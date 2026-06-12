# ============================================================
# program_generation.R — CaTrack  (v2 — Nippard framework)
#
# Rewrite (2026-06): volume targets driven by methodology.R,
# scored exercise selection (no more alphabetical pick),
# goal-aware session templates, time-budget enforcement,
# pull-up baseline branching for assisted/eccentric beginners.
#
# Constants used:
#   VOLUME_TARGETS_HYPERTROPHY_INTERMEDIATE  (methodology.R)
#   GOAL_VOLUME_MULTIPLIERS                  (methodology.R)
#   EXPERIENCE_MAV_POSITION                  (methodology.R)
#   BLOCK_PROFILES                           (methodology.R)
#   WEEK_IN_BLOCK_PROFILES                   (methodology.R)
#   SLOT_TIME_COST                           (methodology.R)
# ============================================================

library(httr2)
library(jsonlite)
library(dplyr)

# ── CREDENTIALS (standalone use only) ────────────────────────
SUPABASE_URL         <- Sys.getenv("SUPABASE_URL",
                                   "https://fowpjdsixqhgaqgdeiph.supabase.co")
SUPABASE_SERVICE_KEY <- Sys.getenv("SUPABASE_SERVICE_KEY",
                                   "YOUR_SERVICE_ROLE_KEY_HERE")

# ── SUPABASE HELPERS ─────────────────────────────────────────
sb_post <- function(table, data, upsert = FALSE) {
  prefer <- if (upsert) "resolution=merge-duplicates" else "return=representation"
  request(paste0(SUPABASE_URL, "/rest/v1/", table)) |>
    req_headers(
      "apikey"        = SUPABASE_SERVICE_KEY,
      "Authorization" = paste("Bearer", SUPABASE_SERVICE_KEY),
      "Content-Type"  = "application/json",
      "Prefer"        = prefer
    ) |>
    req_body_raw(toJSON(data, auto_unbox = TRUE, na = "null")) |>
    req_method("POST") |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
}

sb_get <- function(table, params = "") {
  resp <- request(paste0(SUPABASE_URL, "/rest/v1/", table, params)) |>
    req_headers(
      "apikey"        = SUPABASE_SERVICE_KEY,
      "Authorization" = paste("Bearer", SUPABASE_SERVICE_KEY),
      "Accept"        = "application/json"
    ) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  if (resp$status_code == 200) fromJSON(resp_body_string(resp), simplifyDataFrame = TRUE)
  else { cat("sb_get error:", resp_body_string(resp), "\n"); NULL }
}

sb_patch <- function(table, params, data) {
  request(paste0(SUPABASE_URL, "/rest/v1/", table, params)) |>
    req_headers(
      "apikey"        = SUPABASE_SERVICE_KEY,
      "Authorization" = paste("Bearer", SUPABASE_SERVICE_KEY),
      "Content-Type"  = "application/json",
      "Prefer"        = "return=representation"
    ) |>
    req_body_raw(toJSON(data, auto_unbox = TRUE, na = "null")) |>
    req_method("PATCH") |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
}

sb_insert_one <- function(table, row) {
  resp <- sb_post(table, list(row))
  if (resp$status_code %in% c(200, 201)) {
    result <- fromJSON(resp_body_string(resp), simplifyDataFrame = TRUE)
    if (is.data.frame(result)) result$id[1] else result[[1]]$id
  } else {
    cat(sprintf("Insert error [%s]: %s\n", table, resp_body_string(resp)))
    NULL
  }
}

# ============================================================
# 1. RESOLVE WEEKLY VOLUME TARGET PER MUSCLE
# ============================================================
# target = base MAV in [low, high] interpolated by experience,
#          × goal multiplier × block multiplier × week-in-block multiplier.
# Floored at MV (unless goal multiplier is 0), capped at MRV.
resolve_weekly_target <- function(goal, difficulty, block_variant,
                                  week_in_block, muscle) {
  base <- VOLUME_TARGETS_HYPERTROPHY_INTERMEDIATE[[muscle]]
  if (is.null(base)) return(0L)

  goal_mult  <- GOAL_VOLUME_MULTIPLIERS[[goal]][[muscle]] %||% 1.0
  if (goal_mult <= 0) return(0L)

  exp_pos    <- EXPERIENCE_MAV_POSITION[[difficulty]] %||% 0.5
  mav_target <- base$MAV_low + exp_pos * (base$MAV_high - base$MAV_low)

  block_mult <- BLOCK_PROFILES[[block_variant]]$volume_mult        %||% 1.0
  week_mult  <- WEEK_IN_BLOCK_PROFILES[[as.character(week_in_block)]]$volume_mult %||% 1.0

  target <- mav_target * goal_mult * block_mult * week_mult

  # Guard rails: NA/NaN from a missing constant or zero-mult muscle
  # bubbling into the integer cast would propagate NA_integer_ into
  # weekly_targets and crash the volume coverage report (`if (NA == 0)`).
  if (!is.finite(target)) return(0L)
  if (target < base$MV)  target <- base$MV
  if (target > base$MRV) target <- base$MRV
  as.integer(round(target))
}

# Convenience: all 13 muscles → weekly target for a given block+week
weekly_targets_for_week <- function(goal, difficulty, block_variant, week_in_block) {
  muscles <- names(VOLUME_TARGETS_HYPERTROPHY_INTERMEDIATE)
  setNames(
    vapply(muscles, function(m)
      resolve_weekly_target(goal, difficulty, block_variant, week_in_block, m),
      integer(1)),
    muscles)
}

# ============================================================
# 2. RIR / RPE RESOLUTION
# ============================================================
resolve_rir <- function(block_variant, week_in_block) {
  base_rir <- BLOCK_PROFILES[[block_variant]]$rir_target %||% 2.0
  week_mod <- WEEK_IN_BLOCK_PROFILES[[as.character(week_in_block)]]$rir_mod %||% 0.0
  max(0, min(5, base_rir + week_mod))
}

rir_to_rpe <- function(rir) max(5, min(10, 10 - rir))

# ============================================================
# 3. SESSION FOCUS (which muscles to bias per session in the week)
# ============================================================
# For each (split, sessions_per_week, goal) returns an ordered list
# of sessions. Each session has:
#   label  — display name on calendar
#   focus  — vector of muscle names to bias exercise selection toward
#   slots  — function(goal, block, week_in_block, baseline) -> slot list
# The slot list captures the *ideal* full session before time-budget trim.

# Helper: slot constructors (closures over goal/block/week)
.make_slot <- function(role, movement_patterns, categories = NULL,
                       label, sets, reps_low, reps_high, rest_s,
                       superset_group = NA, prefer_compound = NULL,
                       is_drop_set = FALSE, reuse_heavy = FALSE) {
  list(
    role             = role,           # "heavy" | "compound" | "isolation" | "superset"
    label            = label,
    movement_patterns = movement_patterns,
    categories       = categories,
    sets             = as.integer(sets),
    rep_range_low    = as.integer(reps_low),
    rep_range_high   = as.integer(reps_high),
    rest_seconds     = as.integer(rest_s),
    prefer_compound  = if (is.null(prefer_compound))
                         role %in% c("heavy","compound") else prefer_compound,
    superset_group   = superset_group,
    set_type         = if (is_drop_set) "drop_set" else "working",
    reuse_heavy      = reuse_heavy
  )
}

# Heavy main lift: rep ranges shift by goal
.heavy_slot <- function(patterns, label, goal, sets = 3L) {
  rl <- if (goal == "strength")  3L else 5L
  rh <- if (goal == "strength")  5L else 8L
  .make_slot("heavy", patterns, NULL, label, sets, rl, rh, 180L)
}

# Back-off using the heavy compound exercise
.backoff_slot <- function(patterns, label, goal) {
  rl <- if (goal == "strength")  5L else 8L
  rh <- if (goal == "strength")  8L else 12L
  .make_slot("compound", patterns, NULL, label, 2L, rl, rh, 150L,
             reuse_heavy = TRUE)
}

# Generic working compound
.compound_slot <- function(patterns, label, goal, categories = NULL, sets = 3L) {
  rl <- switch(goal, strength = 5L, pull_up = 6L, 8L)
  rh <- switch(goal, strength = 8L, pull_up = 10L, 12L)
  .make_slot("compound", patterns, categories, label, sets, rl, rh, 150L)
}

# Isolation / accessory
.iso_slot <- function(patterns, label, categories = NULL, sets = 2L,
                      reps_low = 10L, reps_high = 15L, rest_s = 90L,
                      superset_group = NA, is_drop = FALSE) {
  .make_slot("isolation", patterns, categories, label, sets, reps_low,
             reps_high, rest_s, superset_group = superset_group,
             prefer_compound = FALSE, is_drop_set = is_drop)
}

# ============================================================
# 3a. SESSION TEMPLATES BY GOAL × SPLIT × SESSION INDEX
# ============================================================
build_ideal_session <- function(goal, split_style, sessions_per_week,
                                session_idx, block_variant, week_in_block,
                                pullup_baseline = 0L) {

  # Pull-up baseline branching: for pull_up goal users < 3 strict reps,
  # substitute assisted/eccentric in block A.
  # `pullup_baseline < 3L` returns NA when baseline is NA, which would
  # crash the `&&` chain — coerce to 0L when missing.
  pb <- if (is.null(pullup_baseline) || length(pullup_baseline) == 0 ||
            is.na(pullup_baseline[[1]])) 0L else as.integer(pullup_baseline)
  pull_pattern <- if (goal == "pull_up" && pb < 3L &&
                     block_variant == "A")
                    c("vertical_pull")  # exercise scorer will pick lat pulldown
                  else c("vertical_pull")

  key <- paste(split_style, sessions_per_week, session_idx, sep = "_")

  # Fallback: many split × frequency × goal × session_idx combinations
  # don't have a hand-written template (e.g. upper_lower at 3x/wk, PPL
  # at 2x/wk, running_support + PPL). Returning `list()` would create
  # an empty workout AND propagate NAs into the weekly volume math.
  # Resolve recursively to the full_body equivalent so the user always
  # gets a real program even on under-supported combos.
  result <- .build_session_inner(goal, split_style, sessions_per_week,
                                 session_idx, block_variant, week_in_block,
                                 pull_pattern, key)
  if (length(result) == 0 && split_style != "full_body") {
    cat(sprintf("\n  [info] No %s template for %s/%dx/sess %d — falling back to full_body\n",
                goal, split_style, sessions_per_week, session_idx))
    fb_sessions <- if (sessions_per_week >= 3L) 3L else 2L
    fb_idx      <- ((session_idx - 1L) %% fb_sessions) + 1L
    fb_key      <- paste("full_body", fb_sessions, fb_idx, sep = "_")
    result <- .build_session_inner(goal, "full_body", fb_sessions,
                                   fb_idx, block_variant, week_in_block,
                                   pull_pattern, fb_key)
  }
  return(result)
}

# Helper holding the switch tables. Lives outside build_ideal_session so
# the wrapper can call it recursively for the fallback path without
# re-running the pull_pattern / key set-up.
.build_session_inner <- function(goal, split_style, sessions_per_week,
                                 session_idx, block_variant, week_in_block,
                                 pull_pattern, key) {

  # ── HYPERTROPHY ──────────────────────────────────────────
  if (goal == "hypertrophy") {
    return(switch(key,
      "full_body_3_1" = list(
        .heavy_slot(c("squat"), "Heavy Squat", goal),
        .backoff_slot(c("squat"), "Squat Back-off", goal),
        .compound_slot(c("horizontal_pull"), "Horizontal Row", goal),
        .compound_slot(c("horizontal_push"), "Horizontal Press", goal),
        .iso_slot(c("knee_flexion"), "Leg Curl", c("leg_curl"), is_drop = TRUE),
        .iso_slot(c("shoulder_abduction"), "Lateral Raise", c("lateral_raise"))
      ),
      "full_body_3_2" = list(
        .heavy_slot(c("hinge"), "Heavy Hinge", goal),
        .compound_slot(pull_pattern, "Vertical Pull", goal),
        .compound_slot(c("incline_push"), "Incline Press", goal),
        .iso_slot(c("elbow_extension"), "Tricep", c("triceps"), superset_group = "A"),
        .iso_slot(c("elbow_flexion"), "Bicep", c("biceps"), superset_group = "A"),
        .iso_slot(c("rear_delt_fly"), "Rear Delt", c("rear_delt"))
      ),
      "full_body_3_3" = list(
        .compound_slot(c("squat","lunge"), "Single Leg / Squat Variation", goal,
                       categories = c("single_leg","squat")),
        .compound_slot(c("vertical_push"), "Shoulder Press", goal),
        .compound_slot(c("horizontal_pull"), "Row Variation", goal),
        .iso_slot(c("chest_fly"), "Chest Fly", c("chest_fly"), is_drop = TRUE),
        .iso_slot(c("knee_extension"), "Leg Extension", c("leg_extension")),
        .iso_slot(c("plantarflexion"), "Calves", c("calves"), superset_group = "B"),
        .iso_slot(c("spinal_flexion","anti_extension"), "Core", c("core"),
                  superset_group = "B")
      ),
      "full_body_2_1" = list(
        .heavy_slot(c("squat"), "Heavy Squat", goal),
        .backoff_slot(c("squat"), "Squat Back-off", goal),
        .compound_slot(c("hinge"), "Hinge", goal),
        .compound_slot(c("horizontal_pull"), "Row", goal),
        .iso_slot(c("knee_flexion"), "Leg Curl", c("leg_curl"), is_drop = TRUE),
        .iso_slot(c("rear_delt_fly"), "Rear Delt", c("rear_delt")),
        .iso_slot(c("plantarflexion"), "Calves", c("calves"), superset_group = "B"),
        .iso_slot(c("spinal_flexion"), "Core", c("core"), superset_group = "B")
      ),
      "full_body_2_2" = list(
        .heavy_slot(c("horizontal_push"), "Heavy Press", goal),
        .compound_slot(pull_pattern, "Vertical Pull", goal),
        .compound_slot(c("incline_push"), "Incline Press", goal),
        .compound_slot(c("lunge","squat"), "Single Leg", goal, categories = "single_leg"),
        .iso_slot(c("elbow_extension"), "Tricep", c("triceps"), superset_group = "A"),
        .iso_slot(c("elbow_flexion"), "Bicep", c("biceps"), superset_group = "A"),
        .iso_slot(c("shoulder_abduction"), "Lateral Raise", c("lateral_raise"))
      ),
      "push_pull_legs_3_1" = list(  # PUSH
        .heavy_slot(c("horizontal_push"), "Heavy Bench Press", goal),
        .compound_slot(c("incline_push"), "Incline Press", goal),
        .compound_slot(c("vertical_push"), "Overhead Press", goal),
        .iso_slot(c("chest_fly"), "Chest Fly", c("chest_fly")),
        .iso_slot(c("shoulder_abduction"), "Lateral Raise", c("lateral_raise"),
                  superset_group = "A"),
        .iso_slot(c("elbow_extension"), "Tricep", c("triceps"), superset_group = "A")
      ),
      "push_pull_legs_3_2" = list(  # PULL
        .heavy_slot(pull_pattern, "Heavy Vertical Pull", goal),
        .compound_slot(c("horizontal_pull"), "Horizontal Row", goal),
        .compound_slot(c("horizontal_pull"), "Row Variation", goal),
        .iso_slot(c("elbow_flexion"), "Bicep", c("biceps"), superset_group = "A"),
        .iso_slot(c("rear_delt_fly"), "Rear Delt", c("rear_delt"),
                  superset_group = "A"),
        .iso_slot(c("shoulder_abduction"), "Lateral Raise", c("lateral_raise"))
      ),
      "push_pull_legs_3_3" = list(  # LEGS
        .heavy_slot(c("squat"), "Heavy Squat", goal),
        .backoff_slot(c("squat"), "Squat Back-off", goal),
        .compound_slot(c("hinge"), "Hinge", goal),
        .iso_slot(c("knee_flexion"), "Leg Curl", c("leg_curl"), is_drop = TRUE),
        .iso_slot(c("knee_extension"), "Leg Extension", c("leg_extension"),
                  is_drop = TRUE),
        .iso_slot(c("hip_extension"), "Glute", c("hip_thrust","glute_accessory")),
        .iso_slot(c("plantarflexion"), "Calves", c("calves"), superset_group = "B"),
        .iso_slot(c("spinal_flexion"), "Core", c("core"), superset_group = "B")
      ),
      "upper_lower_4_1" = list(  # UPPER A
        .heavy_slot(c("horizontal_push"), "Heavy Bench Press", goal),
        .backoff_slot(c("horizontal_push"), "Press Back-off", goal),
        .compound_slot(pull_pattern, "Vertical Pull", goal),
        .compound_slot(c("horizontal_pull"), "Row", goal),
        .iso_slot(c("elbow_extension"), "Tricep", c("triceps"), superset_group = "A"),
        .iso_slot(c("elbow_flexion"), "Bicep", c("biceps"), superset_group = "A"),
        .iso_slot(c("shoulder_abduction"), "Lateral Raise", c("lateral_raise"))
      ),
      "upper_lower_4_2" = list(  # LOWER A
        .heavy_slot(c("squat"), "Heavy Squat", goal),
        .backoff_slot(c("squat"), "Squat Back-off", goal),
        .compound_slot(c("hinge"), "Hinge", goal),
        .iso_slot(c("knee_flexion"), "Leg Curl", c("leg_curl"), is_drop = TRUE),
        .iso_slot(c("knee_extension"), "Leg Extension", c("leg_extension"),
                  is_drop = TRUE),
        .iso_slot(c("plantarflexion"), "Calves", c("calves"), superset_group = "B"),
        .iso_slot(c("spinal_flexion"), "Core", c("core"), superset_group = "B")
      ),
      "upper_lower_4_3" = list(  # UPPER B
        .compound_slot(c("vertical_push"), "Overhead Press", goal),
        .compound_slot(c("incline_push"), "Incline Press", goal),
        .compound_slot(c("horizontal_pull"), "Row Variation", goal),
        .iso_slot(c("chest_fly"), "Chest Fly", c("chest_fly")),
        .iso_slot(c("elbow_flexion"), "Bicep", c("biceps"), superset_group = "A"),
        .iso_slot(c("elbow_extension"), "Tricep", c("triceps"), superset_group = "A"),
        .iso_slot(c("rear_delt_fly"), "Rear Delt", c("rear_delt"))
      ),
      "upper_lower_4_4" = list(  # LOWER B
        .heavy_slot(c("hinge"), "Heavy Hinge", goal),
        .compound_slot(c("lunge","squat"), "Single Leg", goal, categories = "single_leg"),
        .compound_slot(c("hip_extension"), "Hip Thrust", goal, categories = "hip_thrust"),
        .iso_slot(c("knee_flexion"), "Leg Curl", c("leg_curl"), is_drop = TRUE),
        .iso_slot(c("abduction"), "Glute Accessory", c("glute_accessory")),
        .iso_slot(c("plantarflexion"), "Calves", c("calves"), superset_group = "B"),
        .iso_slot(c("spinal_flexion"), "Core", c("core"), superset_group = "B")
      ),
      list()
    ))
  }

  # ── STRENGTH ─────────────────────────────────────────────
  if (goal == "strength") {
    # Bias: more main-lift sets, fewer accessories
    return(switch(key,
      "full_body_3_1" = list(
        .heavy_slot(c("squat"), "Heavy Squat", goal, sets = 4L),
        .backoff_slot(c("squat"), "Squat Back-off", goal),
        .compound_slot(c("horizontal_pull"), "Row", goal),
        .iso_slot(c("knee_flexion"), "Leg Curl", c("leg_curl"))
      ),
      "full_body_3_2" = list(
        .heavy_slot(c("horizontal_push"), "Heavy Bench Press", goal, sets = 4L),
        .backoff_slot(c("horizontal_push"), "Bench Back-off", goal),
        .compound_slot(pull_pattern, "Vertical Pull", goal),
        .iso_slot(c("elbow_extension"), "Tricep", c("triceps"))
      ),
      "full_body_3_3" = list(
        .heavy_slot(c("hinge"), "Heavy Deadlift", goal, sets = 4L),
        .compound_slot(c("vertical_push"), "Overhead Press", goal),
        .compound_slot(c("horizontal_pull"), "Row", goal),
        .iso_slot(c("spinal_flexion","anti_extension"), "Core", c("core"))
      ),
      "full_body_2_1" = list(
        .heavy_slot(c("squat"), "Heavy Squat", goal, sets = 5L),
        .heavy_slot(c("horizontal_push"), "Heavy Bench Press", goal, sets = 3L),
        .compound_slot(c("horizontal_pull"), "Row", goal),
        .iso_slot(c("knee_flexion"), "Leg Curl", c("leg_curl"))
      ),
      "full_body_2_2" = list(
        .heavy_slot(c("hinge"), "Heavy Deadlift", goal, sets = 4L),
        .heavy_slot(c("vertical_push"), "Heavy Overhead Press", goal, sets = 3L),
        .compound_slot(pull_pattern, "Vertical Pull", goal),
        .iso_slot(c("elbow_extension"), "Tricep", c("triceps"))
      ),
      "push_pull_legs_3_1" = list(
        .heavy_slot(c("horizontal_push"), "Heavy Bench Press", goal, sets = 5L),
        .backoff_slot(c("horizontal_push"), "Bench Back-off", goal),
        .compound_slot(c("vertical_push"), "Overhead Press", goal),
        .iso_slot(c("elbow_extension"), "Tricep", c("triceps"))
      ),
      "push_pull_legs_3_2" = list(
        .heavy_slot(pull_pattern, "Weighted Pull-up / Heavy Pulldown", goal, sets = 4L),
        .compound_slot(c("horizontal_pull"), "Heavy Row", goal),
        .compound_slot(c("horizontal_pull"), "Row Variation", goal),
        .iso_slot(c("elbow_flexion"), "Bicep", c("biceps"))
      ),
      "push_pull_legs_3_3" = list(
        .heavy_slot(c("squat"), "Heavy Squat", goal, sets = 5L),
        .heavy_slot(c("hinge"), "Heavy Deadlift", goal, sets = 3L),
        .compound_slot(c("squat","lunge"), "Squat/Lunge Accessory", goal),
        .iso_slot(c("spinal_flexion"), "Core", c("core"))
      ),
      "upper_lower_4_1" = list(
        .heavy_slot(c("horizontal_push"), "Heavy Bench Press", goal, sets = 5L),
        .backoff_slot(c("horizontal_push"), "Bench Back-off", goal),
        .compound_slot(pull_pattern, "Vertical Pull", goal),
        .compound_slot(c("horizontal_pull"), "Row", goal),
        .iso_slot(c("elbow_extension"), "Tricep", c("triceps"))
      ),
      "upper_lower_4_2" = list(
        .heavy_slot(c("squat"), "Heavy Squat", goal, sets = 5L),
        .backoff_slot(c("squat"), "Squat Back-off", goal),
        .compound_slot(c("hinge"), "Hinge", goal),
        .iso_slot(c("knee_flexion"), "Leg Curl", c("leg_curl"))
      ),
      "upper_lower_4_3" = list(
        .heavy_slot(c("vertical_push"), "Heavy Overhead Press", goal, sets = 5L),
        .compound_slot(c("incline_push"), "Incline Press", goal),
        .compound_slot(c("horizontal_pull"), "Row Variation", goal),
        .iso_slot(c("elbow_flexion"), "Bicep", c("biceps"))
      ),
      "upper_lower_4_4" = list(
        .heavy_slot(c("hinge"), "Heavy Deadlift", goal, sets = 5L),
        .compound_slot(c("lunge","squat"), "Single Leg", goal, categories = "single_leg"),
        .compound_slot(c("hip_extension"), "Hip Thrust", goal, categories = "hip_thrust"),
        .iso_slot(c("plantarflexion"), "Calves", c("calves"))
      ),
      list()
    ))
  }

  # ── PULL-UP FOCUS ────────────────────────────────────────
  if (goal == "pull_up") {
    # Pulling emphasis on every session, with light lower-body work
    # so full-body splits don't turn into upper-only programs.
    return(switch(key,
      "full_body_3_1" = list(
        .heavy_slot(pull_pattern, "Heavy Vertical Pull", goal),
        .backoff_slot(pull_pattern, "Vertical Pull Back-off", goal),
        .compound_slot(c("horizontal_pull"), "Horizontal Row", goal),
        .compound_slot(c("squat"), "Squat (Maintenance)", goal, sets = 2L),
        .iso_slot(c("elbow_flexion"), "Bicep — wide grip", c("biceps")),
        .iso_slot(c("rear_delt_fly"), "Rear Delt", c("rear_delt"))
      ),
      "full_body_3_2" = list(
        .heavy_slot(c("horizontal_pull"), "Heavy Row", goal),
        .compound_slot(pull_pattern, "Vertical Pull Variation", goal),
        .compound_slot(c("hinge"), "Hinge (Maintenance)", goal, sets = 2L),
        .compound_slot(c("horizontal_push"), "Chest Press (Maintenance)", goal, sets = 2L),
        .iso_slot(c("elbow_flexion"), "Bicep — chin grip", c("biceps")),
        .iso_slot(c("spinal_flexion"), "Core", c("core"))
      ),
      "full_body_3_3" = list(
        .heavy_slot(pull_pattern, "Heavy Vertical Pull", goal),
        .compound_slot(c("horizontal_pull"), "Row Variation", goal),
        .compound_slot(c("lunge","squat"), "Single Leg (Maintenance)", goal,
                       categories = "single_leg", sets = 2L),
        .iso_slot(c("elbow_flexion"), "Bicep", c("biceps"), superset_group = "A"),
        .iso_slot(c("rear_delt_fly"), "Rear Delt", c("rear_delt"), superset_group = "A"),
        .iso_slot(c("spinal_flexion","anti_extension"), "Core", c("core"))
      ),
      "full_body_2_1" = list(
        .heavy_slot(pull_pattern, "Heavy Vertical Pull", goal),
        .compound_slot(c("horizontal_pull"), "Row", goal),
        .compound_slot(c("squat"), "Squat (Maintenance)", goal, sets = 2L),
        .compound_slot(c("horizontal_push"), "Chest Press (Maintenance)", goal, sets = 2L),
        .iso_slot(c("elbow_flexion"), "Bicep", c("biceps")),
        .iso_slot(c("rear_delt_fly"), "Rear Delt", c("rear_delt"))
      ),
      "full_body_2_2" = list(
        .heavy_slot(c("horizontal_pull"), "Heavy Row", goal),
        .compound_slot(pull_pattern, "Vertical Pull", goal),
        .compound_slot(c("hinge"), "Hinge (Maintenance)", goal, sets = 2L),
        .iso_slot(c("elbow_flexion"), "Bicep", c("biceps"), superset_group = "A"),
        .iso_slot(c("rear_delt_fly"), "Rear Delt", c("rear_delt"), superset_group = "A"),
        .iso_slot(c("spinal_flexion"), "Core", c("core"))
      ),
      "push_pull_legs_3_1" = list(  # Push (light/maint for pull-up goal)
        .compound_slot(c("horizontal_push"), "Chest Press", goal, sets = 2L),
        .compound_slot(c("vertical_push"), "Overhead Press", goal, sets = 2L),
        .iso_slot(c("rear_delt_fly"), "Rear Delt", c("rear_delt")),
        .iso_slot(c("shoulder_abduction"), "Lateral Raise", c("lateral_raise"))
      ),
      "push_pull_legs_3_2" = list(  # Pull — the main day
        .heavy_slot(pull_pattern, "Heavy Vertical Pull", goal),
        .backoff_slot(pull_pattern, "Vertical Pull Back-off", goal),
        .compound_slot(c("horizontal_pull"), "Heavy Row", goal),
        .compound_slot(c("horizontal_pull"), "Row Variation", goal),
        .iso_slot(c("elbow_flexion"), "Bicep — wide", c("biceps"), superset_group = "A"),
        .iso_slot(c("elbow_flexion"), "Bicep — chin", c("biceps"), superset_group = "A")
      ),
      "push_pull_legs_3_3" = list(  # Legs (minimal)
        .compound_slot(c("squat"), "Squat", goal, sets = 2L),
        .compound_slot(c("hinge"), "Hinge", goal, sets = 2L),
        .iso_slot(c("spinal_flexion","anti_extension"), "Core", c("core"))
      ),
      list()
    ))
  }

  # ── RUNNING SUPPORT ──────────────────────────────────────
  # Heavy single-leg emphasis (each leg trains independently to mimic
  # running's unilateral load), eccentric hamstring work (Nordic-style
  # — biggest documented hamstring-injury reducer for runners), split
  # calf work (gastroc + soleus), and anti-rotation core. Direct
  # bilateral squat work kept modest so heavy training doesn't compete
  # with run mileage for recovery.
  if (goal == "running_support") {
    return(switch(key,
      "full_body_3_1" = list(  # Quad-dominant single-leg day
        .heavy_slot(c("lunge","squat"), "Heavy Single Leg", goal,
                    categories = "single_leg"),
        .compound_slot(c("squat"), "Squat Variation", goal, sets = 2L),
        .iso_slot(c("knee_flexion"), "Nordic / Eccentric Hamstring", c("leg_curl")),
        .iso_slot(c("plantarflexion"), "Calves — standing (gastroc)", c("calves"),
                  superset_group = "B"),
        .iso_slot(c("anti_extension","rotation"), "Anti-rotation Core",
                  c("core"), superset_group = "B")
      ),
      "full_body_3_2" = list(  # Posterior chain + hip stability
        .heavy_slot(c("hinge"), "Heavy Hinge / RDL", goal),
        .compound_slot(c("hip_extension"), "Hip Thrust", goal, categories = "hip_thrust"),
        .compound_slot(c("abduction"), "Hip Abduction (band/cable)", goal,
                       categories = "glute_accessory"),
        .iso_slot(c("knee_flexion"), "Nordic / Eccentric Hamstring",
                  c("leg_curl"), is_drop = TRUE),
        .iso_slot(c("plantarflexion"), "Calves — seated (soleus)", c("calves")),
        .iso_slot(c("anti_extension"), "Dead Bug / Pallof", c("core"))
      ),
      "full_body_3_3" = list(  # Plyo + balance + upper maintenance
        .compound_slot(c("squat","lunge"), "Step-up / Bulgarian Split", goal,
                       categories = "single_leg"),
        .compound_slot(c("hinge"), "Single Leg RDL", goal),
        .compound_slot(c("locomotion"), "Loaded Carry", goal, sets = 2L),
        .iso_slot(c("plantarflexion"), "Calves — standing (gastroc)", c("calves"),
                  is_drop = TRUE),
        .iso_slot(c("rotation","spinal_flexion"), "Rotational Core", c("core")),
        .compound_slot(c("horizontal_pull"), "Row (upper maintenance)", goal, sets = 2L)
      ),
      "full_body_2_1" = list(
        .heavy_slot(c("lunge","squat"), "Heavy Single Leg", goal,
                    categories = "single_leg"),
        .compound_slot(c("hinge"), "Hinge / RDL", goal),
        .compound_slot(c("hip_extension"), "Hip Thrust", goal, categories = "hip_thrust"),
        .iso_slot(c("knee_flexion"), "Nordic / Eccentric Hamstring", c("leg_curl")),
        .iso_slot(c("plantarflexion"), "Calves — gastroc", c("calves"),
                  superset_group = "B"),
        .iso_slot(c("anti_extension","rotation"), "Anti-rotation Core",
                  c("core"), superset_group = "B")
      ),
      "full_body_2_2" = list(
        .heavy_slot(c("hinge"), "Heavy Hinge / Deadlift", goal),
        .compound_slot(c("squat","lunge"), "Step-up / Bulgarian Split", goal,
                       categories = "single_leg"),
        .compound_slot(c("abduction"), "Hip Abduction", goal,
                       categories = "glute_accessory"),
        .compound_slot(c("locomotion"), "Loaded Carry", goal, sets = 2L),
        .iso_slot(c("plantarflexion"), "Calves — seated (soleus)", c("calves"),
                  is_drop = TRUE),
        .iso_slot(c("anti_extension"), "Dead Bug / Plank", c("core"))
      ),
      "upper_lower_4_1" = list(
        .compound_slot(c("horizontal_push"), "Chest Press (maint)", goal, sets = 2L),
        .compound_slot(c("horizontal_pull"), "Row", goal, sets = 2L),
        .compound_slot(c("vertical_push"), "Overhead Press (maint)", goal, sets = 2L),
        .iso_slot(c("spinal_flexion","anti_extension"), "Core", c("core"))
      ),
      "upper_lower_4_2" = list(
        .heavy_slot(c("squat"), "Heavy Squat", goal),
        .compound_slot(c("lunge","squat"), "Single Leg", goal, categories = "single_leg"),
        .iso_slot(c("knee_flexion"), "Leg Curl", c("leg_curl")),
        .iso_slot(c("plantarflexion"), "Calves — standing", c("calves"), superset_group = "B"),
        .iso_slot(c("spinal_flexion"), "Core", c("core"), superset_group = "B")
      ),
      "upper_lower_4_3" = list(
        .compound_slot(c("horizontal_pull"), "Row", goal),
        .compound_slot(c("incline_push"), "Incline Push (maint)", goal, sets = 2L),
        .iso_slot(c("rear_delt_fly"), "Rear Delt", c("rear_delt")),
        .iso_slot(c("anti_extension","rotation"), "Anti-rotation Core", c("core"))
      ),
      "upper_lower_4_4" = list(
        .heavy_slot(c("hinge"), "Heavy Hinge", goal),
        .compound_slot(c("hip_extension"), "Hip Thrust", goal, categories = "hip_thrust"),
        .compound_slot(c("lunge","squat"), "Single Leg", goal, categories = "single_leg"),
        .iso_slot(c("plantarflexion"), "Calves — seated (soleus)", c("calves"), is_drop = TRUE),
        .iso_slot(c("abduction"), "Hip Abduction", c("glute_accessory"))
      ),
      list()
    ))
  }

  # ── FUNCTIONAL ───────────────────────────────────────────
  # Heavy carries on every session (single biggest "real world"
  # transfer), unilateral pressing/pulling (asymmetric load = trunk
  # work without dedicated abs), rotational/anti-rotation core every
  # day, KB/explosive work where equipment allows. Direct arm
  # isolation is intentionally pulled back so the time goes to
  # patterns that show up outside the gym.
  if (goal == "functional") {
    return(switch(key,
      "full_body_3_1" = list(  # Squat + push + carry
        .heavy_slot(c("squat"), "Heavy Goblet/Front Squat", goal),
        .compound_slot(c("lunge","squat"), "Reverse Lunge / Step-up",
                       goal, categories = "single_leg"),
        .compound_slot(c("vertical_push"), "Single Arm OH Press", goal),
        .compound_slot(c("locomotion"), "Farmer's / Suitcase Carry",
                       goal, sets = 3L),
        .iso_slot(c("anti_extension","rotation"), "Pallof / Anti-rotation",
                  c("core"))
      ),
      "full_body_3_2" = list(  # Hinge + pull + rotational
        .heavy_slot(c("hinge"), "Heavy Hinge / KB Swing", goal),
        .compound_slot(c("horizontal_pull"), "Single Arm Row", goal),
        .compound_slot(c("vertical_push"), "Push Press / OH Press", goal),
        .compound_slot(c("locomotion"), "Suitcase Carry (asymmetric)",
                       goal, sets = 2L),
        .iso_slot(c("rotation","spinal_flexion"), "Russian Twist / Cable Chop",
                  c("core"), superset_group = "B"),
        .iso_slot(c("plantarflexion"), "Calves", c("calves"), superset_group = "B")
      ),
      "full_body_3_3" = list(  # Single-leg + multi-planar + carry
        .compound_slot(c("squat","lunge"), "Bulgarian Split / Pistol Progression",
                       goal, categories = "single_leg"),
        .compound_slot(pull_pattern, "Vertical Pull (chin/pulldown)", goal),
        .compound_slot(c("incline_push","horizontal_push"), "Landmine Press / Push-up",
                       goal),
        .compound_slot(c("locomotion"), "Trap Bar / Sled Carry", goal, sets = 3L),
        .iso_slot(c("anti_extension"), "Dead Bug / Hollow Hold", c("core"))
      ),
      "full_body_2_1" = list(
        .heavy_slot(c("squat"), "Heavy Goblet/Front Squat", goal),
        .compound_slot(c("horizontal_pull"), "Single Arm Row", goal),
        .compound_slot(c("lunge","squat"), "Reverse Lunge / Step-up",
                       goal, categories = "single_leg"),
        .compound_slot(c("locomotion"), "Farmer's Carry", goal, sets = 3L),
        .iso_slot(c("anti_extension","rotation"), "Pallof / Anti-rotation",
                  c("core"), superset_group = "B"),
        .iso_slot(c("plantarflexion"), "Calves", c("calves"), superset_group = "B")
      ),
      "full_body_2_2" = list(
        .heavy_slot(c("hinge"), "Heavy Hinge / KB Swing", goal),
        .compound_slot(c("horizontal_push"), "Landmine Press / Push-up", goal),
        .compound_slot(pull_pattern, "Pulldown / Chin-up", goal),
        .compound_slot(c("locomotion"), "Suitcase Carry (asymmetric)",
                       goal, sets = 3L),
        .iso_slot(c("rotation","spinal_flexion"), "Cable Chop / Russian Twist",
                  c("core"))
      ),
      list()
    ))
  }

  list()
}

# ============================================================
# 4. ELIGIBLE EXERCISES (filter by equipment)
# ============================================================
get_eligible_exercises <- function(user_equipment) {
  all_ex <- sb_get("exercises", "?select=*&order=category,name")
  if (is.null(all_ex) || nrow(all_ex) == 0) stop("Could not fetch exercise library")

  eligible <- all_ex[vapply(seq_len(nrow(all_ex)), function(i) {
    required <- tryCatch(all_ex$equipment_required[[i]], error = \(e) character(0))
    # Strip NAs that come from Supabase JSON arrays so downstream
    # `all(... == "bodyweight")` can't yield NA and crash the `if`.
    required <- required[!is.na(required)]
    if (length(required) == 0) return(TRUE)
    if (all(required == "bodyweight")) return(TRUE)
    isTRUE(all(required %in% c(user_equipment, "bodyweight")))
  }, logical(1)), ]

  cat(sprintf("  Exercise library: %d total, %d eligible\n",
              nrow(all_ex), nrow(eligible)))
  eligible
}

# ============================================================
# 5. EXERCISE SCORING + PICK (replaces alphabetical pick)
# ============================================================
# Block equipment preference (broadly aligned with Nippard variety theme).
BLOCK_EQUIPMENT_PREF <- list(
  A = c("barbell","squat_rack","ez_bar","trap_bar"),
  B = c("cable_machine","lat_pulldown_machine","hack_squat_machine",
        "leg_press_machine","leg_extension_machine",
        "seated_leg_curl_machine","lying_leg_curl_machine",
        "chest_press_machine","pec_dec_machine","hip_thrust_machine",
        "hip_abduction_machine","calf_raise_machine","ab_machine",
        "incline_press_machine","shoulder_press_machine","row_machine",
        "lateral_raise_machine"),
  C = c("dumbbells","bodyweight","resistance_bands","pullup_bar","dip_bars","bench")
)

# Score: higher is better.
score_exercise <- function(exercise, slot, block_variant, used_ids,
                           muscle_deficit, goal, pullup_baseline = 0L) {
  if (exercise$id %in% used_ids) return(-Inf)

  # Determine primary muscles for this exercise
  prim <- tryCatch(unlist(exercise$primary_muscles), error = \(e) character(0))
  sec  <- tryCatch(unlist(exercise$secondary_muscles), error = \(e) character(0))

  # Base: 1.0 if compound slot wants compound and this is compound.
  # isTRUE() wraps `slot$prefer_compound` defensively — if the slot
  # constructor ever yields NA here, an unwrapped `if(NA)` would crash.
  base <- if (isTRUE(slot$prefer_compound)) {
    if (isTRUE(exercise$is_compound)) 1.0 else -0.5
  } else {
    if (isTRUE(exercise$is_compound)) 0.2 else 1.0
  }

  # Muscle deficit bonus: how much do the muscles this hits need volume?
  # Coerce NA deficit (possible if a muscle target collapsed to NA upstream)
  # to 0 so sum() doesn't propagate NA into the score.
  safe_def <- function(m) {
    v <- muscle_deficit[[m]] %||% 0
    if (is.na(v)) 0 else v
  }
  prim_def <- sum(vapply(prim, safe_def, numeric(1)))
  sec_def  <- sum(vapply(sec,  safe_def, numeric(1))) * 0.5
  muscle_score <- (prim_def + sec_def) / 5.0  # normalize roughly

  # Block equipment preference. isTRUE() guards against `any(NA)` -> NA
  # crashing the `if`, which can happen if eq contains NA entries from
  # a malformed Supabase array column.
  eq <- tryCatch(unlist(exercise$equipment_required), error = \(e) character(0))
  pref <- BLOCK_EQUIPMENT_PREF[[block_variant]] %||% character(0)
  block_score <- if (isTRUE(length(pref) > 0 && any(eq %in% pref))) 0.6 else 0

  # Pull-up baseline override: for vertical_pull slots in Block A when baseline < 3,
  # prefer lat pulldown / band-assisted variants (downrank "Pull-up" and "Chin-up").
  pb <- if (is.null(pullup_baseline) || length(pullup_baseline) == 0 ||
            is.na(pullup_baseline[[1]])) 0L else as.integer(pullup_baseline)
  baseline_score <- 0
  if (goal == "pull_up" && block_variant == "A" && pb < 3L &&
      isTRUE(exercise$movement_pattern == "vertical_pull")) {
    nm <- tolower(as.character(exercise$name %||% ""))
    if (grepl("pulldown", nm))               baseline_score <- 1.5
    else if (grepl("pull-?up|chin-?up", nm)) baseline_score <- -1.5
  }

  base + muscle_score + block_score + baseline_score
}

pick_exercise_scored <- function(exercises, slot, block_variant, used_ids,
                                 muscle_deficit, goal, pullup_baseline) {
  if (is.null(exercises) || nrow(exercises) == 0) return(NULL)

  cands <- exercises
  if (!is.null(slot$movement_patterns))
    cands <- cands[cands$movement_pattern %in% slot$movement_patterns, ]
  if (!is.null(slot$categories))
    cands <- cands[cands$category %in% slot$categories, ]

  if (nrow(cands) == 0) return(NULL)

  scores <- vapply(seq_len(nrow(cands)), function(i)
    score_exercise(cands[i, ], slot, block_variant, used_ids,
                   muscle_deficit, goal, pullup_baseline),
    numeric(1))

  best_idx <- which.max(scores)
  if (length(best_idx) == 0 || !is.finite(scores[best_idx])) return(NULL)
  cands[best_idx, ]
}

# ============================================================
# 6. SLOT TIME COST + BUDGET TRIM
# ============================================================
slot_time_cost <- function(slot) {
  cost <- switch(slot$role,
    heavy      = SLOT_TIME_COST$heavy_compound,
    compound   = SLOT_TIME_COST$compound,
    isolation  = SLOT_TIME_COST$isolation,
    SLOT_TIME_COST$compound)
  # Supersets are paired — share fixed rest, count as 1.5× a solo
  if (!is.na(slot$superset_group %||% NA))
    cost <- cost * 0.75   # each leg of a superset pair is 0.75× → pair = 1.5×
  cost
}

trim_to_budget <- function(slots, budget_min) {
  if (length(slots) == 0) return(slots)
  total <- sum(vapply(slots, slot_time_cost, numeric(1)))
  while (total > budget_min && length(slots) > 2) {
    # Drop the last isolation; if no isolations, drop last slot
    iso_idx <- which(vapply(slots, \(s) s$role == "isolation", logical(1)))
    drop_idx <- if (length(iso_idx) > 0) tail(iso_idx, 1) else length(slots)
    slots <- slots[-drop_idx]
    total <- sum(vapply(slots, slot_time_cost, numeric(1)))
  }
  slots
}

# ============================================================
# 7. INSTANTIATE A SESSION
#    Pick exercises for each slot, applying RIR/RPE math
# ============================================================
instantiate_session <- function(slots, exercises, goal, difficulty,
                                block_variant, week_in_block,
                                weekly_targets, sets_already_scheduled,
                                pullup_baseline = 0L) {
  used_ids    <- character(0)
  heavy_ex_id <- NULL
  result      <- list()
  order_idx   <- 1L

  rir <- resolve_rir(block_variant, week_in_block)
  rpe <- rir_to_rpe(rir)

  # Build initial deficit map: how short of weekly target are we per muscle?
  muscle_deficit <- list()
  for (m in names(weekly_targets)) {
    muscle_deficit[[m]] <- max(0, weekly_targets[[m]] - (sets_already_scheduled[[m]] %||% 0))
  }

  for (slot in slots) {
    if (isTRUE(slot$reuse_heavy) && !is.null(heavy_ex_id)) {
      chosen <- exercises[exercises$id == heavy_ex_id, ]
    } else {
      chosen <- pick_exercise_scored(exercises, slot, block_variant,
                                     used_ids, muscle_deficit, goal,
                                     pullup_baseline)
    }
    if (is.null(chosen) || nrow(chosen) == 0) next
    if (slot$role == "heavy") heavy_ex_id <- chosen$id

    # Week-in-block volume modifier on sets
    week_mult <- WEEK_IN_BLOCK_PROFILES[[as.character(week_in_block)]]$volume_mult %||% 1.0
    adj_sets  <- max(1L, round(slot$sets * week_mult))

    # Update deficit map
    prim <- tryCatch(unlist(chosen$primary_muscles), error = \(e) character(0))
    for (m in prim) {
      muscle_deficit[[m]] <- max(0, (muscle_deficit[[m]] %||% 0) - adj_sets)
    }

    # Warm-up sets: 2 for heavy, 1 for compound, 0 for isolation
    warmup_n <- switch(slot$role, heavy = 2L, compound = 1L, 0L)

    result[[order_idx]] <- list(
      exercise_id    = chosen$id,
      exercise_name  = chosen$name,
      slot_label     = slot$label,
      exercise_order = order_idx,
      prescribed_sets = as.integer(adj_sets),
      rep_range_low  = as.integer(slot$rep_range_low),
      rep_range_high = as.integer(slot$rep_range_high),
      rpe_target     = round(rpe, 1),
      rest_seconds   = as.integer(slot$rest_seconds),
      set_type       = slot$set_type,
      warmup_sets    = warmup_n,
      superset_group = slot$superset_group %||% NA_character_
    )

    used_ids  <- c(used_ids, chosen$id)
    order_idx <- order_idx + 1L
  }

  # Return scheduled sets per muscle so caller can carry into next session
  sets_added <- list()
  for (ea in result) {
    ex <- exercises[exercises$id == ea$exercise_id, ]
    if (nrow(ex) == 0) next
    prim <- tryCatch(unlist(ex$primary_muscles), error = \(e) character(0))
    for (m in prim) {
      sets_added[[m]] <- (sets_added[[m]] %||% 0) + ea$prescribed_sets
    }
  }

  list(exercises = result, sets_added = sets_added)
}

# ============================================================
# 8. SPLIT SCHEDULE (labels + count)
# ============================================================
get_split_schedule <- function(split_style, sessions_per_week, goal) {
  result <- switch(paste(split_style, sessions_per_week, sep = "_"),
    "full_body_3" = list(
      labels = c("Full Body A", "Full Body B", "Full Body C"),
      n_sessions = 3L
    ),
    "full_body_2" = list(
      labels = c("Full Body (Lower Focus)", "Full Body (Upper Focus)"),
      n_sessions = 2L
    ),
    "push_pull_legs_3" = list(
      labels = c("Push", "Pull", "Legs"),
      n_sessions = 3L
    ),
    "upper_lower_4" = list(
      labels = c("Upper A", "Lower A", "Upper B", "Lower B"),
      n_sessions = 4L
    ),
    "upper_lower_2" = list(
      labels = c("Upper", "Lower"),
      n_sessions = 2L
    ),
    "upper_lower_3" = list(
      labels = c("Upper", "Lower", "Upper"),
      n_sessions = 3L
    ),
    "push_pull_legs_2" = list(
      # PPL only makes sense at 3+ sessions; at 2x/wk we run two full body
      # sessions instead so the user still gets balanced volume.
      labels = c("Full Body (Lower Focus)", "Full Body (Upper Focus)"),
      n_sessions = 2L
    ),
    NULL
  )

  # Final fallback — always honour the user's sessions_per_week
  # so day_offset indexing can't run off the end into NA dates.
  if (is.null(result)) {
    spw <- as.integer(sessions_per_week)
    if (is.na(spw) || spw < 1L) spw <- 3L
    labels <- c("Full Body A", "Full Body B", "Full Body C", "Full Body D")[seq_len(min(spw, 4L))]
    result <- list(labels = labels, n_sessions = length(labels))
  }
  result
}

# ============================================================
# 9. MAIN: generate_program
# ============================================================
generate_program <- function(
    user_id,
    goal             = "hypertrophy",   # hypertrophy | strength | pull_up |
    #                                     running_support | functional
    difficulty       = "intermediate",
    sessions_per_week = 3L,
    split_style      = "full_body",
    session_length_minutes = 45L,
    pullup_baseline  = 0L,
    equipment        = NULL,
    block_number     = 1L,
    start_date       = Sys.Date(),
    program_name     = NULL
) {
  cat("\n=== CaTrack Program Generator (v2 — Nippard framework) ===\n")
  cat(sprintf("User:       %s\n", user_id))
  cat(sprintf("Goal:       %s\n", goal))
  cat(sprintf("Difficulty: %s\n", difficulty))
  cat(sprintf("Frequency:  %dx/week\n", sessions_per_week))
  cat(sprintf("Split:      %s\n", split_style))
  cat(sprintf("Session:    %d min budget\n", session_length_minutes))
  if (goal == "pull_up")
    cat(sprintf("Pull-up baseline: %d strict\n", pullup_baseline))
  cat(sprintf("Block:      %d (variant %s)\n",
              block_number, c("A","B","C")[((block_number - 1L) %% 3L) + 1L]))
  cat(sprintf("Start:      %s\n", start_date))
  cat("==========================================================\n\n")

  # ── Equipment ────────────────────────────────────────────
  if (is.null(equipment)) {
    profile <- sb_get("user_profiles",
                      paste0("?id=eq.", user_id, "&select=equipment_available"))
    equipment <- if (!is.null(profile) && nrow(profile) > 0)
      profile$equipment_available[[1]]
    else
      c("dumbbells","bench","cable_machine","lat_pulldown_machine",
        "leg_press_machine","pullup_bar","bodyweight")
  }

  exercises <- get_eligible_exercises(equipment)
  schedule  <- get_split_schedule(split_style, sessions_per_week, goal)

  # ── Program record ───────────────────────────────────────
  # Default name is "<display_name>'s Block N — Goal". We always
  # read the display name fresh from the user's profile so callers
  # don't have to pass it. Falls back to goal/split label if no
  # profile name exists yet.
  if (is.null(program_name)) {
    goal_label <- tools::toTitleCase(gsub("_", " ", goal))
    nm <- tryCatch({
      prof <- sb_get("user_profiles",
                     paste0("?id=eq.", user_id, "&select=display_name"))
      if (!is.null(prof) && nrow(prof) > 0)
        trimws(as.character(prof$display_name[[1]] %||% ""))
      else ""
    }, error = \(e) "")
    if (nchar(nm) > 0 && nm != "NA") {
      # Strip trailing apostrophe-s if the user's name already ends in 's
      possessive <- if (substr(nm, nchar(nm), nchar(nm)) %in% c("s", "S"))
                      paste0(nm, "'") else paste0(nm, "'s")
      program_name <- sprintf("%s Block %d — %s",
                              possessive, block_number, goal_label)
    } else {
      split_label  <- tools::toTitleCase(gsub("_", " ", split_style))
      program_name <- sprintf("%s %s — Block %d", goal_label, split_label, block_number)
    }
  }

  program_row <- list(
    user_id           = user_id,
    name              = program_name,
    goal              = goal,
    difficulty        = difficulty,
    sessions_per_week = as.integer(sessions_per_week),
    split_style       = split_style,
    session_length_minutes = as.integer(session_length_minutes),
    equipment_snapshot = I(equipment),
    block_number      = as.integer(block_number),
    total_weeks       = 12L,
    start_date        = as.character(start_date),
    is_active         = TRUE
  )

  program_id <- sb_insert_one("programs", program_row)
  if (is.null(program_id)) stop("Failed to create program record")
  cat(sprintf("\nProgram created: %s\n", program_id))

  # Anything past this point that throws would leave a stub program row
  # with no workouts attached. Wrap the generation loop so we delete the
  # orphan record on failure instead of leaving it for the user.
  total_sessions <- 0L; total_exercises <- 0L
  cleanup_on_fail <- function(e) {
    cat(sprintf("\n  ERROR during generation: %s\n", conditionMessage(e)))
    cat("  Rolling back partial program...\n")
    tryCatch({
      # Best-effort cascade — orphaned set_logs / exercises shouldn't
      # happen since we delete before any logs can be written, but be
      # thorough in case the user retries quickly.
      wkts <- sb_get("workouts",
                     paste0("?program_id=eq.", program_id, "&select=id"))
      if (!is.null(wkts) && nrow(wkts) > 0) {
        wid_list <- paste0("(", paste(wkts$id, collapse = ","), ")")
        request(paste0(SUPABASE_URL, "/rest/v1/workout_exercises?workout_id=in.", wid_list)) |>
          req_headers("apikey" = SUPABASE_SERVICE_KEY,
                      "Authorization" = paste("Bearer", SUPABASE_SERVICE_KEY)) |>
          req_method("DELETE") |>
          req_error(is_error = \(r) FALSE) |>
          req_perform()
        request(paste0(SUPABASE_URL, "/rest/v1/workouts?program_id=eq.", program_id)) |>
          req_headers("apikey" = SUPABASE_SERVICE_KEY,
                      "Authorization" = paste("Bearer", SUPABASE_SERVICE_KEY)) |>
          req_method("DELETE") |>
          req_error(is_error = \(r) FALSE) |>
          req_perform()
      }
      request(paste0(SUPABASE_URL, "/rest/v1/programs?id=eq.", program_id)) |>
        req_headers("apikey" = SUPABASE_SERVICE_KEY,
                    "Authorization" = paste("Bearer", SUPABASE_SERVICE_KEY)) |>
        req_method("DELETE") |>
        req_error(is_error = \(r) FALSE) |>
        req_perform()
    }, error = \(ce) cat(sprintf("  Cleanup also failed: %s\n", conditionMessage(ce))))
    stop(conditionMessage(e), call. = FALSE)
  }

  tryCatch({

  for (week in 1:12) {
    block_variant <- c("A","B","C")[ceiling(week / 4)]
    week_in_block <- ((week - 1) %% 4) + 1

    cat(sprintf("\n  Week %02d [Block %s %s]",
                week, block_variant,
                BLOCK_PROFILES[[block_variant]]$label))

    # Weekly targets for this week (block × week-in-block modifiers applied)
    week_targets <- weekly_targets_for_week(goal, difficulty, block_variant, week_in_block)
    sets_scheduled <- list()  # accumulates as we add sessions

    for (sess_idx in seq_len(schedule$n_sessions)) {
      session_label <- schedule$labels[sess_idx]

      # Build ideal slot list, then trim to budget
      slots <- build_ideal_session(goal, split_style, sessions_per_week,
                                   sess_idx, block_variant, week_in_block,
                                   pullup_baseline)
      slots <- trim_to_budget(slots, session_length_minutes)

      # Scheduled date — sessions spaced evenly across the week
      day_offset <- switch(as.character(sessions_per_week),
        "2" = c(0L, 3L),
        "3" = c(0L, 2L, 4L),
        "4" = c(0L, 1L, 3L, 4L),
        c(0L, 2L, 4L)
      )
      sched_date <- start_date + ((week - 1) * 7L) + day_offset[sess_idx]

      workout_row <- list(
        program_id     = program_id,
        user_id        = user_id,
        week_number    = as.integer(week),
        session_number = as.integer(sess_idx),
        session_label  = session_label,
        scheduled_date = as.character(sched_date)
      )
      workout_id <- sb_insert_one("workouts", workout_row)
      if (is.null(workout_id)) { cat("\n    WARN: workout insert failed\n"); next }

      sess <- instantiate_session(slots, exercises, goal, difficulty,
                                  block_variant, week_in_block,
                                  week_targets, sets_scheduled,
                                  pullup_baseline)

      # Carry sets forward so later sessions know what's already covered
      for (m in names(sess$sets_added))
        sets_scheduled[[m]] <- (sets_scheduled[[m]] %||% 0) + sess$sets_added[[m]]

      # Write workout_exercises rows
      if (length(sess$exercises) > 0) {
        ex_rows <- lapply(sess$exercises, function(ea) {
          list(
            workout_id      = workout_id,
            exercise_id     = ea$exercise_id,
            exercise_order  = as.integer(ea$exercise_order),
            prescribed_sets = as.integer(ea$prescribed_sets),
            rep_range_low   = as.integer(ea$rep_range_low),
            rep_range_high  = as.integer(ea$rep_range_high),
            rpe_target      = ea$rpe_target,
            rest_seconds    = as.integer(ea$rest_seconds),
            set_type        = ea$set_type,
            warmup_sets     = as.integer(ea$warmup_sets),
            superset_group  = ea$superset_group
          )
        })
        resp <- sb_post("workout_exercises", ex_rows)
        if (resp$status_code %in% c(200, 201)) {
          total_exercises <- total_exercises + length(sess$exercises)
        } else {
          cat(sprintf("\n    WARN: exercise insert failed: %s\n",
                      resp_body_string(resp)))
        }
      }

      total_sessions <- total_sessions + 1L
      cat(".")
    }

    # Volume coverage report at week's end (just to console for now)
    if (week_in_block == 2) {
      cat(sprintf("\n      Week %d volume coverage:", week))
      for (m in names(week_targets)) {
        tgt <- week_targets[[m]]
        if (is.null(tgt) || is.na(tgt) || tgt == 0) next
        got <- sets_scheduled[[m]] %||% 0
        if (is.na(got)) got <- 0
        marker <- if (got >= tgt) "✓" else
                  if (got >= tgt * 0.7) "~" else "!"
        cat(sprintf(" %s%s %d/%d", marker, m, got, week_targets[[m]]))
      }
    }
  }

  }, error = cleanup_on_fail)
  # end tryCatch wrapping the per-week generation loop

  cat("\n\nDone!\n")
  cat(sprintf("  Program ID:  %s\n", program_id))
  cat(sprintf("  Sessions:    %d\n", total_sessions))
  cat(sprintf("  Exercises:   %d prescriptions written\n", total_exercises))

  cat("\n── Week 1 Preview ──────────────────────────────────\n")
  tryCatch(preview_program(program_id, week = 1),
           error = \(e) cat(sprintf("Preview error (non-fatal): %s\n",
                                    conditionMessage(e))))

  invisible(program_id)
}

# ============================================================
# 10. PREVIEW: print a week of the generated program
# ============================================================
preview_program <- function(program_id, week = 1) {
  workouts <- sb_get("workouts",
    sprintf("?program_id=eq.%s&week_number=eq.%d&order=session_number",
            program_id, week))
  if (is.null(workouts) || nrow(workouts) == 0) {
    cat("No workouts found for this week.\n"); return(invisible(NULL))
  }

  for (i in seq_len(nrow(workouts))) {
    wo <- workouts[i, ]
    cat(sprintf("\n  %s (%s)\n", wo$session_label, wo$scheduled_date))

    ex_list <- sb_get("workout_exercises",
      sprintf("?workout_id=eq.%s&select=*,exercises(name,category)&order=exercise_order",
              wo$id))
    if (!is.null(ex_list) && nrow(ex_list) > 0) {
      for (j in seq_len(nrow(ex_list))) {
        e <- ex_list[j, ]
        ss <- if (!is.null(e$superset_group) && !is.na(e$superset_group))
                sprintf("[%s] ", e$superset_group) else "    "
        wu <- if (e$warmup_sets > 0) sprintf(" (+%dWU)", e$warmup_sets) else ""
        cat(sprintf("  %d. %s%s%s — %dx %d-%d @ RPE %.1f\n",
                    e$exercise_order, ss, e$exercises$name, wu,
                    e$prescribed_sets, e$rep_range_low, e$rep_range_high,
                    e$rpe_target))
      }
    }
  }
  invisible(NULL)
}

# ============================================================
# 11. REGENERATE FROM WEEK: swap a single exercise forward
# ============================================================
regenerate_from_week <- function(program_id, user_id, from_week,
                                 swapped_exercise_id, replacement_exercise_id) {
  future_wkts <- sb_get("workouts",
    sprintf("?program_id=eq.%s&week_number=gte.%d&select=id",
            program_id, from_week))
  if (is.null(future_wkts) || nrow(future_wkts) == 0) {
    cat("No future workouts to update.\n"); return(invisible(NULL))
  }
  future_we <- sb_get("workout_exercises",
    sprintf("?workout_id=in.(%s)&exercise_id=eq.%s",
            paste(future_wkts$id, collapse = ","),
            swapped_exercise_id))
  if (is.null(future_we) || nrow(future_we) == 0) {
    cat("No future instances of this exercise.\n"); return(invisible(NULL))
  }
  for (we_id in future_we$id)
    sb_patch("workout_exercises", sprintf("?id=eq.%s", we_id),
             list(exercise_id = replacement_exercise_id, is_swapped = TRUE))
  cat(sprintf("Updated %d future sessions.\n", nrow(future_we)))
  invisible(nrow(future_we))
}
