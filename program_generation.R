# ============================================================
# program_generation.R
# CaTrack — 12-week program generation engine
# Session 2 of 7
#
# Run this from RStudio to generate a full 12-week block
# for a user and write it to Supabase.
#
# Usage:
#   source("program_generation.R")
#   generate_program(user_id = "your-user-uuid-here")
# ============================================================

library(httr2)
library(jsonlite)
library(dplyr)

# ── CREDENTIALS ─────────────────────────────────────────────
SUPABASE_URL         <- Sys.getenv("SUPABASE_URL",         "https://fowpjdsixqhgaqgdeiph.supabase.co")
SUPABASE_SERVICE_KEY <- Sys.getenv("SUPABASE_SERVICE_KEY", "YOUR_SERVICE_ROLE_KEY_HERE")

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
  else {
    cat("sb_get error:", resp_body_string(resp), "\n")
    NULL
  }
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

# Post one row and return its ID
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
# 1. GOAL PARAMETERS
#    Maps goal + difficulty to rep ranges, RPE, volume targets
# ============================================================
get_goal_params <- function(goal, difficulty) {
  # Base parameters by goal (following Nippard MEV/MRV framework)
  base <- switch(goal,
                 hypertrophy = list(
                   rep_low = 8L, rep_high = 15L, rpe_target = 8.0,
                   compound_sets = 3L, isolation_sets = 2L,
                   weekly_sets = list(
                     quads = 10L, hamstrings = 9L, glutes = 9L,
                     chest = 8L, lats = 10L, mid_back = 6L,
                     front_delts = 4L, mid_delts = 6L, rear_delts = 4L,
                     biceps = 6L, triceps = 6L, calves = 6L, core = 4L
                   )
                 ),
                 strength = list(
                   rep_low = 3L, rep_high = 6L, rpe_target = 8.5,
                   compound_sets = 4L, isolation_sets = 2L,
                   weekly_sets = list(
                     quads = 8L, hamstrings = 8L, glutes = 6L,
                     chest = 6L, lats = 8L, mid_back = 6L,
                     front_delts = 4L, mid_delts = 4L, rear_delts = 4L,
                     biceps = 4L, triceps = 4L, calves = 4L, core = 4L
                   )
                 ),
                 fat_loss = list(
                   rep_low = 10L, rep_high = 20L, rpe_target = 8.0,
                   compound_sets = 3L, isolation_sets = 2L,
                   weekly_sets = list(
                     quads = 9L, hamstrings = 8L, glutes = 8L,
                     chest = 6L, lats = 9L, mid_back = 6L,
                     front_delts = 4L, mid_delts = 6L, rear_delts = 4L,
                     biceps = 6L, triceps = 6L, calves = 6L, core = 6L
                   )
                 ),
                 pull_up = list(
                   rep_low = 5L, rep_high = 10L, rpe_target = 8.5,
                   compound_sets = 4L, isolation_sets = 2L,
                   weekly_sets = list(
                     quads = 6L, hamstrings = 6L, glutes = 6L,
                     chest = 6L, lats = 14L, mid_back = 10L,
                     front_delts = 4L, mid_delts = 4L, rear_delts = 6L,
                     biceps = 8L, triceps = 4L, calves = 4L, core = 6L
                   )
                 ),
                 running_support = list(
                   rep_low = 8L, rep_high = 15L, rpe_target = 8.0,
                   compound_sets = 3L, isolation_sets = 2L,
                   weekly_sets = list(
                     quads = 10L, hamstrings = 10L, glutes = 12L,
                     chest = 4L, lats = 6L, mid_back = 4L,
                     front_delts = 2L, mid_delts = 4L, rear_delts = 4L,
                     biceps = 4L, triceps = 4L, calves = 8L, core = 8L
                   )
                 ),
                 functional = list(
                   rep_low = 8L, rep_high = 15L, rpe_target = 8.0,
                   compound_sets = 3L, isolation_sets = 2L,
                   weekly_sets = list(
                     quads = 8L, hamstrings = 8L, glutes = 9L,
                     chest = 6L, lats = 8L, mid_back = 6L,
                     front_delts = 4L, mid_delts = 6L, rear_delts = 4L,
                     biceps = 4L, triceps = 4L, calves = 6L, core = 8L
                   )
                 ),
                 # Default fallback
                 list(
                   rep_low = 8L, rep_high = 12L, rpe_target = 8.0,
                   compound_sets = 3L, isolation_sets = 2L,
                   weekly_sets = list(
                     quads = 9L, hamstrings = 8L, glutes = 9L,
                     chest = 7L, lats = 9L, mid_back = 6L,
                     front_delts = 4L, mid_delts = 6L, rear_delts = 4L,
                     biceps = 6L, triceps = 6L, calves = 6L, core = 4L
                   )
                 )
  )
  
  # Difficulty modifiers
  diff_mod <- switch(difficulty,
                     beginner     = list(set_mult = 0.75, rpe_mod = -0.5, heavy_sets = 1L),
                     intermediate = list(set_mult = 1.0,  rpe_mod = 0.0,  heavy_sets = 1L),
                     advanced     = list(set_mult = 1.25, rpe_mod = 0.5,  heavy_sets = 2L),
                     list(set_mult = 1.0, rpe_mod = 0.0, heavy_sets = 1L)
  )
  
  c(base, diff_mod)
}

# ============================================================
# 2. WEEK PROGRESSION PARAMETERS
#    4-week micro-cycle: Base → Build → Peak → Deload
# ============================================================
get_week_params <- function(week_in_block) {
  # week_in_block is 1-4 (position within a 4-week cycle)
  switch(as.character(week_in_block),
         "1" = list(  # Base week: establish working weights
           rpe_mod       = -1.0,   # RPE target -1 (lighter, groove the pattern)
           volume_mod    = 0.80,   # 80% of prescribed sets
           rep_mod       = 0L,     # at low end of rep range
           label         = "Base"
         ),
         "2" = list(  # Build week: push to middle of rep range
           rpe_mod       = 0.0,
           volume_mod    = 1.0,
           rep_mod       = 0L,
           label         = "Build"
         ),
         "3" = list(  # Peak week: push to top of rep range, heavier
           rpe_mod       = 0.5,
           volume_mod    = 1.0,
           rep_mod       = 0L,
           label         = "Peak"
         ),
         "4" = list(  # Deload: same exercises, 60% volume, RPE -2
           rpe_mod       = -2.0,
           volume_mod    = 0.60,
           rep_mod       = 0L,
           label         = "Deload"
         )
  )
}

# ============================================================
# 3. EXERCISE SELECTION
#    Fetches exercises from Supabase filtered by equipment
# ============================================================
get_eligible_exercises <- function(user_equipment) {
  # Pull full exercise library
  all_ex <- sb_get("exercises", "?select=*&order=category,name")
  if (is.null(all_ex) || nrow(all_ex) == 0) stop("Could not fetch exercise library")
  
  # Filter: exercise is eligible if ALL required equipment is in user's list
  # (bodyweight exercises are always eligible)
  eligible <- all_ex[sapply(seq_len(nrow(all_ex)), function(i) {
    required <- all_ex$equipment_required[[i]]
    if (length(required) == 0 || all(required == "bodyweight")) return(TRUE)
    all(required %in% c(user_equipment, "bodyweight"))
  }), ]
  
  cat(sprintf("  Exercise library: %d total, %d eligible with your equipment\n",
              nrow(all_ex), nrow(eligible)))
  eligible
}

# Pick best exercise for a slot, excluding already-used ones.
#
# Block-variant rotation strategy (mirrors Nippard's approach):
#   Block A → prefer barbell / free-weight compounds (heaviest progressive overload)
#   Block B → prefer machine / cable (constant tension, safer fatigue accumulation)
#   Block C → prefer dumbbell / unilateral / bodyweight (ROM, balance, novelty)
#
# This ensures each 4-week block uses meaningfully different exercises rather
# than the same exercise repeated or a random alphabetical pick.
pick_exercise <- function(exercises, movement_patterns = NULL, categories = NULL,
                          exclude_ids = character(0), prefer_compound = TRUE,
                          goal = "hypertrophy", block_variant = "A") {
  candidates <- exercises

  # Filter by movement pattern or category
  if (!is.null(movement_patterns))
    candidates <- candidates[candidates$movement_pattern %in% movement_patterns, ]
  if (!is.null(categories))
    candidates <- candidates[candidates$category %in% categories, ]

  # Exclude already-assigned exercises this session
  candidates <- candidates[!candidates$id %in% exclude_ids, ]

  if (nrow(candidates) == 0) return(NULL)

  # Prefer compound vs isolation based on flag
  if (prefer_compound && any(candidates$is_compound))
    candidates <- candidates[candidates$is_compound, ]

  if (nrow(candidates) == 0) return(NULL)

  # ── Block-variant equipment preference ────────────────────────────────
  # Defines which equipment signals to prefer for each training block
  block_equip_pref <- list(
    "A" = c("barbell", "squat_rack", "ez_bar", "trap_bar"),          # free weight
    "B" = c("cable_machine", "lat_pulldown_machine", "hack_squat_machine",
            "leg_press_machine", "leg_extension_machine",
            "lying_leg_curl_machine", "seated_leg_curl_machine",
            "chest_press_machine", "pec_dec_machine",
            "hip_thrust_machine", "hip_abduction_machine",
            "calf_raise_machine", "ab_machine"),                      # machines
    "C" = c("dumbbells", "bodyweight", "resistance_bands",
            "pullup_bar", "dip_bars", "bench")                        # dumbbells / BW
  )
  pref <- block_equip_pref[[block_variant]] %||% character(0)

  # Try to find a candidate that uses preferred equipment
  if (length(pref) > 0) {
    preferred <- candidates[vapply(seq_len(nrow(candidates)), function(i) {
      eq <- tryCatch(
        if (is.list(candidates$equipment_required)) candidates$equipment_required[[i]]
        else strsplit(gsub('[{}"]', '', candidates$equipment_required[i]), ",")[[1]],
        error = \(e) character(0))
      any(trimws(eq) %in% pref)
    }, logical(1)), , drop = FALSE]

    if (nrow(preferred) > 0) candidates <- preferred
  }

  # Within the filtered pool, sort by name for determinism then pick first
  candidates <- candidates[order(candidates$name), , drop = FALSE]
  candidates[1L, ]
}

# ============================================================
# 4. SESSION TEMPLATES
#    Defines the slot structure for each session type
# ============================================================

# Returns a list of slot definitions for one session
build_session_slots <- function(session_type, goal, difficulty) {
  p <- get_goal_params(goal, difficulty)
  
  # Heavy compound: strength-style loading
  heavy_slot <- function(movement_patterns, categories = NULL, label = "Main Compound") {
    rep_low  <- if (goal == "strength") 3L else if (goal %in% c("pull_up","strength")) 4L else 5L
    rep_high <- if (goal == "strength") 5L else 8L
    list(
      label            = label,
      movement_patterns = movement_patterns,
      categories        = categories,
      prefer_compound  = TRUE,
      sets             = p$heavy_sets + 1L,  # +1 because we drop to back-off
      rep_range_low    = rep_low,
      rep_range_high   = rep_high,
      rpe_target       = p$rpe_target + 0.5,
      rest_seconds     = 180L,
      set_type         = "working",
      warmup_sets      = 2L,
      is_heavy         = TRUE
    )
  }
  
  # Back-off set: same movement, lighter, more reps
  backoff_slot <- function(movement_patterns, categories = NULL, label = "Back-off") {
    list(
      label            = label,
      movement_patterns = movement_patterns,
      categories        = categories,
      prefer_compound  = TRUE,
      sets             = 2L,
      rep_range_low    = p$rep_low - 2L,
      rep_range_high   = p$rep_low + 2L,
      rpe_target       = p$rpe_target - 0.5,
      rest_seconds     = 150L,
      set_type         = "working",
      warmup_sets      = 0L,
      is_heavy         = FALSE,
      reuse_heavy      = TRUE  # pick same exercise as heavy slot
    )
  }
  
  # Working compound: moderate load, hypertrophy reps
  compound_slot <- function(movement_patterns, categories = NULL, label = "Compound") {
    list(
      label            = label,
      movement_patterns = movement_patterns,
      categories        = categories,
      prefer_compound  = TRUE,
      sets             = p$compound_sets,
      rep_range_low    = p$rep_low,
      rep_range_high   = p$rep_high,
      rpe_target       = p$rpe_target,
      rest_seconds     = 150L,
      set_type         = "working",
      warmup_sets      = 1L,
      is_heavy         = FALSE
    )
  }
  
  # Isolation / accessory
  iso_slot <- function(movement_patterns, categories = NULL, label = "Accessory",
                       drop = FALSE, superset_group = NA_character_) {
    list(
      label            = label,
      movement_patterns = movement_patterns,
      categories        = categories,
      prefer_compound  = FALSE,
      sets             = p$isolation_sets,
      rep_range_low    = p$rep_low + 2L,
      rep_range_high   = p$rep_high + 3L,
      rpe_target       = p$rpe_target + 0.5,
      rest_seconds     = 90L,
      set_type         = if (drop) "drop_set" else "working",
      warmup_sets      = 0L,
      superset_group   = superset_group,
      is_heavy         = FALSE
    )
  }
  
  # ── SESSION TYPE DEFINITIONS ─────────────────────────────
  switch(session_type,
         
         # ── 3x FULL BODY ─────────────────────────────────────────
         # Session A: Squat-dominant + Horizontal pull + accessories
         "full_body_A" = list(
           heavy_slot(c("squat"), label = "Heavy Squat"),
           backoff_slot(c("squat"), label = "Squat Back-off"),
           compound_slot(c("horizontal_pull"), label = "Horizontal Pull"),
           iso_slot(c("knee_flexion"), c("leg_curl"), label = "Leg Curl", drop = TRUE),
           iso_slot(c("rear_delt_fly","shoulder_abduction"), label = "Rear Delt / Lateral Raise",
                    drop = TRUE, superset_group = NA_character_),
           iso_slot(c("horizontal_push","chest_fly"), label = "Chest Accessory")
         ),
         
         # Session B: Press-dominant upper + Vertical pull + accessories
         "full_body_B" = list(
           heavy_slot(c("horizontal_push"), label = "Heavy Press"),
           backoff_slot(c("horizontal_push","incline_push"), label = "Press Back-off"),
           compound_slot(c("vertical_pull"), label = "Vertical Pull"),
           compound_slot(c("vertical_push"), label = "Shoulder Press"),
           iso_slot(c("elbow_extension"), c("triceps"), label = "Tricep",
                    superset_group = "A"),
           iso_slot(c("elbow_flexion"), c("biceps"), label = "Bicep",
                    superset_group = "A")
         ),
         
         # Session C: Hinge-dominant lower + Vertical pull + accessories
         "full_body_C" = list(
           heavy_slot(c("hinge"), label = "Heavy Hinge"),
           backoff_slot(c("hinge","lunge"), label = "Hinge Back-off"),
           compound_slot(c("squat","lunge"), c("single_leg"), label = "Quad Accessory"),
           iso_slot(c("knee_extension"), c("leg_extension"), label = "Leg Extension", drop = TRUE),
           iso_slot(c("plantarflexion"), c("calves"), label = "Calves",
                    superset_group = "B"),
           iso_slot(c("spinal_flexion","anti_extension"), c("core"), label = "Core",
                    superset_group = "B")
         ),
         
         # ── 3x PUSH / PULL / LEGS ────────────────────────────────
         "push" = list(
           heavy_slot(c("horizontal_push"), label = "Heavy Horizontal Push"),
           compound_slot(c("incline_push"), label = "Incline Push"),
           compound_slot(c("vertical_push"), label = "Overhead Press"),
           iso_slot(c("elbow_extension"), c("triceps"), label = "Tricep",
                    superset_group = "A"),
           iso_slot(c("shoulder_abduction"), c("lateral_raise"), label = "Lateral Raise",
                    superset_group = "A"),
           iso_slot(c("chest_fly"), label = "Chest Fly")
         ),
         
         "pull" = list(
           heavy_slot(c("vertical_pull"), label = "Heavy Vertical Pull"),
           compound_slot(c("horizontal_pull"), label = "Horizontal Row"),
           compound_slot(c("horizontal_pull"), label = "Row Variation"),
           iso_slot(c("elbow_flexion"), c("biceps"), label = "Bicep",
                    superset_group = "A"),
           iso_slot(c("rear_delt_fly"), c("rear_delt"), label = "Rear Delt",
                    superset_group = "A"),
           iso_slot(c("shoulder_abduction"), c("lateral_raise"), label = "Lateral Raise")
         ),
         
         "legs" = list(
           heavy_slot(c("squat"), label = "Heavy Squat"),
           backoff_slot(c("squat"), label = "Squat Back-off"),
           compound_slot(c("hinge"), label = "Hinge"),
           iso_slot(c("knee_flexion"), c("leg_curl"), label = "Leg Curl", drop = TRUE),
           iso_slot(c("knee_extension"), c("leg_extension"), label = "Leg Extension", drop = TRUE),
           iso_slot(c("hip_extension"), c("hip_thrust","glute_accessory"), label = "Glute"),
           iso_slot(c("plantarflexion"), c("calves"), label = "Calves",
                    superset_group = "B"),
           iso_slot(c("spinal_flexion"), c("core"), label = "Core",
                    superset_group = "B")
         ),
         
         # ── 2x FULL BODY ─────────────────────────────────────────
         "fb2_A" = list(  # Lower dominant
           heavy_slot(c("squat"), label = "Heavy Squat"),
           backoff_slot(c("squat"), label = "Squat Back-off"),
           compound_slot(c("hinge"), label = "Hinge"),
           compound_slot(c("horizontal_pull"), label = "Row"),
           iso_slot(c("knee_flexion"), c("leg_curl"), label = "Leg Curl", drop = TRUE),
           iso_slot(c("shoulder_abduction","rear_delt_fly"), label = "Delt Accessory"),
           iso_slot(c("plantarflexion"), c("calves"), label = "Calves",
                    superset_group = "B"),
           iso_slot(c("spinal_flexion"), c("core"), label = "Core",
                    superset_group = "B")
         ),
         
         "fb2_B" = list(  # Upper dominant
           heavy_slot(c("horizontal_push"), label = "Heavy Press"),
           compound_slot(c("vertical_pull"), label = "Vertical Pull"),
           compound_slot(c("incline_push"), label = "Incline Push"),
           compound_slot(c("horizontal_pull"), label = "Row"),
           compound_slot(c("lunge","squat"), c("single_leg"), label = "Single Leg"),
           iso_slot(c("elbow_extension"), c("triceps"), label = "Tricep",
                    superset_group = "A"),
           iso_slot(c("elbow_flexion"), c("biceps"), label = "Bicep",
                    superset_group = "A")
         ),
         
         # ── 4x UPPER / LOWER ─────────────────────────────────────
         "upper_A" = list(
           heavy_slot(c("horizontal_push"), label = "Heavy Press"),
           backoff_slot(c("horizontal_push"), label = "Press Back-off"),
           compound_slot(c("vertical_pull"), label = "Vertical Pull"),
           compound_slot(c("horizontal_pull"), label = "Row"),
           iso_slot(c("elbow_extension"), c("triceps"), label = "Tricep",
                    superset_group = "A"),
           iso_slot(c("elbow_flexion"), c("biceps"), label = "Bicep",
                    superset_group = "A"),
           iso_slot(c("shoulder_abduction"), c("lateral_raise"), label = "Lateral Raise")
         ),
         
         "upper_B" = list(
           compound_slot(c("vertical_push"), label = "Overhead Press"),
           compound_slot(c("incline_push"), label = "Incline Push"),
           compound_slot(c("horizontal_pull"), label = "Row Variation"),
           iso_slot(c("chest_fly"), label = "Chest Fly"),
           iso_slot(c("elbow_flexion"), c("biceps"), label = "Bicep",
                    superset_group = "A"),
           iso_slot(c("elbow_extension"), c("triceps"), label = "Tricep",
                    superset_group = "A"),
           iso_slot(c("rear_delt_fly"), c("rear_delt"), label = "Rear Delt")
         ),
         
         "lower_A" = list(
           heavy_slot(c("squat"), label = "Heavy Squat"),
           backoff_slot(c("squat"), label = "Squat Back-off"),
           compound_slot(c("hinge"), label = "Hinge"),
           iso_slot(c("knee_flexion"), c("leg_curl"), label = "Leg Curl", drop = TRUE),
           iso_slot(c("knee_extension"), c("leg_extension"), label = "Leg Extension", drop = TRUE),
           iso_slot(c("plantarflexion"), c("calves"), label = "Calves",
                    superset_group = "B"),
           iso_slot(c("spinal_flexion"), c("core"), label = "Core",
                    superset_group = "B")
         ),
         
         "lower_B" = list(
           heavy_slot(c("hinge"), label = "Heavy Hinge"),
           compound_slot(c("lunge","squat"), c("single_leg"), label = "Single Leg"),
           compound_slot(c("hip_extension"), c("hip_thrust"), label = "Hip Thrust"),
           iso_slot(c("knee_flexion"), c("leg_curl"), label = "Leg Curl", drop = TRUE),
           iso_slot(c("abduction"), c("glute_accessory"), label = "Glute Accessory"),
           iso_slot(c("plantarflexion"), c("calves"), label = "Calves",
                    superset_group = "B"),
           iso_slot(c("spinal_flexion"), c("core"), label = "Core",
                    superset_group = "B")
         ),
         
         # Fallback
         list()
  )
}

# ============================================================
# 5. SPLIT SCHEDULE
#    Returns ordered list of session types for each split
# ============================================================
get_split_schedule <- function(split_style, sessions_per_week, goal) {
  switch(paste(split_style, sessions_per_week, sep = "_"),
         
         # 3x Full Body (default for most goals)
         "full_body_3" = list(
           sessions = c("full_body_A", "full_body_B", "full_body_C"),
           labels   = c("Full Body A", "Full Body B", "Full Body C")
         ),
         
         # 2x Full Body
         "full_body_2" = list(
           sessions = c("fb2_A", "fb2_B"),
           labels   = c("Full Body (Lower Focus)", "Full Body (Upper Focus)")
         ),
         
         # 3x Push/Pull/Legs
         "push_pull_legs_3" = list(
           sessions = c("push", "pull", "legs"),
           labels   = c("Push", "Pull", "Legs")
         ),
         
         # 4x Upper/Lower
         "upper_lower_4" = list(
           sessions = c("upper_A", "lower_A", "upper_B", "lower_B"),
           labels   = c("Upper A", "Lower A", "Upper B", "Lower B")
         ),
         
         # 2x Upper/Lower (alternating)
         "upper_lower_2" = list(
           sessions = c("upper_A", "lower_A"),
           labels   = c("Upper", "Lower")
         ),
         
         # Default: 3x Full Body
         list(
           sessions = c("full_body_A", "full_body_B", "full_body_C"),
           labels   = c("Full Body A", "Full Body B", "Full Body C")
         )
  )
}

# ============================================================
# 6. INSTANTIATE ONE SESSION
#    Given a template and exercise pool, return a list of
#    exercise assignments ready to write to workout_exercises
# ============================================================
instantiate_session <- function(slots, exercises, goal, difficulty,
                                week_params, block_variant = "A") {
  used_ids    <- character(0)
  heavy_ex_id <- NULL  # track the heavy compound for back-off reuse
  result      <- list()
  order_idx   <- 1L
  
  for (slot in slots) {
    # Back-off slots reuse the heavy compound exercise
    if (isTRUE(slot$reuse_heavy) && !is.null(heavy_ex_id)) {
      chosen <- exercises[exercises$id == heavy_ex_id, ]
    } else {
      chosen <- pick_exercise(
        exercises        = exercises,
        movement_patterns = slot$movement_patterns,
        categories       = slot$categories,
        exclude_ids      = used_ids,
        prefer_compound  = slot$prefer_compound,
        goal             = goal,
        block_variant    = block_variant
      )
    }
    
    if (is.null(chosen) || nrow(chosen) == 0) next
    
    # Track heavy compound for back-off
    if (isTRUE(slot$is_heavy)) heavy_ex_id <- chosen$id
    
    # Apply week progression modifiers
    adj_rpe  <- min(10, max(6, slot$rpe_target + week_params$rpe_mod))
    adj_sets <- max(1L, round(slot$sets * week_params$volume_mod))
    
    result[[order_idx]] <- list(
      exercise_id    = chosen$id,
      exercise_name  = chosen$name,   # for display/debugging
      slot_label     = slot$label,
      exercise_order = order_idx,
      prescribed_sets = as.integer(adj_sets),
      rep_range_low  = as.integer(slot$rep_range_low),
      rep_range_high = as.integer(slot$rep_range_high),
      rpe_target     = adj_rpe,
      rest_seconds   = as.integer(slot$rest_seconds),
      set_type       = slot$set_type,
      warmup_sets    = as.integer(slot$warmup_sets %||% 0L),
      superset_group = slot$superset_group %||% NA_character_
    )
    
    used_ids  <- c(used_ids, chosen$id)
    order_idx <- order_idx + 1L
  }
  
  result
}

# %||% is defined in global.R — do not redefine here

# ============================================================
# 7. MAIN: generate_program
#    Creates a full 12-week program in Supabase
# ============================================================
generate_program <- function(
    user_id,
    goal             = "hypertrophy",   # hypertrophy | strength | fat_loss |
    #                                     pull_up | running_support | functional
    difficulty       = "intermediate",  # beginner | intermediate | advanced
    sessions_per_week = 3L,
    split_style      = "full_body",     # full_body | push_pull_legs | upper_lower
    equipment        = NULL,            # vector of equipment IDs; NULL = fetch from profile
    block_number     = 1L,
    start_date       = Sys.Date(),
    program_name     = NULL
) {
  cat("\n=== CaTrack Program Generator ===\n")
  cat(sprintf("User:       %s\n", user_id))
  cat(sprintf("Goal:       %s\n", goal))
  cat(sprintf("Difficulty: %s\n", difficulty))
  cat(sprintf("Frequency:  %dx/week\n", sessions_per_week))
  cat(sprintf("Split:      %s\n", split_style))
  cat(sprintf("Block:      %d\n", block_number))
  cat(sprintf("Start:      %s\n", start_date))
  cat("=================================\n\n")
  
  # ── Get user equipment if not provided ────────────────────
  if (is.null(equipment)) {
    profile <- sb_get("user_profiles", paste0("?id=eq.", user_id, "&select=equipment_available"))
    equipment <- if (!is.null(profile) && nrow(profile) > 0)
      profile$equipment_available[[1]]
    else
      c("dumbbells", "bench", "cable_machine", "lat_pulldown_machine",
        "leg_press_machine", "pullup_bar", "bodyweight")
    cat(sprintf("Equipment from profile: %s\n", paste(equipment, collapse = ", ")))
  }
  
  # ── Get eligible exercises ─────────────────────────────────
  exercises <- get_eligible_exercises(equipment)
  
  # ── Build schedule ────────────────────────────────────────
  schedule <- get_split_schedule(split_style, sessions_per_week, goal)
  n_session_types <- length(schedule$sessions)
  cat(sprintf("\nSplit: %d session types per week\n", n_session_types))
  for (i in seq_along(schedule$sessions))
    cat(sprintf("  Day %d: %s\n", i, schedule$labels[i]))
  
  # ── Create program record ─────────────────────────────────
  if (is.null(program_name)) {
    # Replace underscores with spaces before title-casing
    goal_label  <- tools::toTitleCase(gsub("_", " ", goal))
    split_label <- tools::toTitleCase(gsub("_", " ", split_style))
    program_name <- sprintf("%s %s — Block %d", goal_label, split_label, block_number)
  }
  
  program_row <- list(
    user_id           = user_id,
    name              = program_name,
    goal              = goal,
    difficulty        = difficulty,
    sessions_per_week = as.integer(sessions_per_week),
    split_style       = split_style,
    equipment_snapshot = I(equipment),
    block_number      = as.integer(block_number),
    total_weeks       = 12L,
    start_date        = as.character(start_date),
    is_active         = TRUE
  )
  
  program_id <- sb_insert_one("programs", program_row)
  if (is.null(program_id)) stop("Failed to create program record")
  cat(sprintf("\nProgram created: %s\n", program_id))
  
  # ── Generate 12 weeks ────────────────────────────────────
  total_sessions <- 0L
  total_exercises <- 0L
  
  for (week in 1:12) {
    # Determine block variant (A=wk1-4, B=wk5-8, C=wk9-12)
    block_variant <- c("A","B","C")[ceiling(week / 4)]
    week_in_block <- ((week - 1) %% 4) + 1
    week_params   <- get_week_params(week_in_block)
    
    cat(sprintf("\n  Week %02d [Block %s, %s]", week, block_variant, week_params$label))
    
    for (sess_idx in seq_along(schedule$sessions)) {
      session_type  <- schedule$sessions[sess_idx]
      session_label <- schedule$labels[sess_idx]
      
      # Calculate scheduled date (week start + session offset)
      # Sessions spaced evenly across the week
      day_offset <- switch(as.character(sessions_per_week),
                           "2" = c(0L, 3L),
                           "3" = c(0L, 2L, 4L),
                           "4" = c(0L, 1L, 3L, 4L),
                           c(0L, 2L, 4L)
      )
      sched_date <- start_date + ((week - 1) * 7L) + day_offset[sess_idx]
      
      # Create workout record
      workout_row <- list(
        program_id    = program_id,
        user_id       = user_id,
        week_number   = as.integer(week),
        session_number = as.integer(sess_idx),
        session_label = session_label,
        scheduled_date = as.character(sched_date)
      )
      workout_id <- sb_insert_one("workouts", workout_row)
      if (is.null(workout_id)) {
        cat(sprintf("\n    WARN: Failed to create workout w%d-s%d\n", week, sess_idx))
        next
      }
      
      # Get session slots and instantiate exercises
      slots  <- build_session_slots(session_type, goal, difficulty)
      ex_assignments <- instantiate_session(
        slots, exercises, goal, difficulty, week_params, block_variant
      )
      
      # Write workout_exercises
      if (length(ex_assignments) > 0) {
        ex_rows <- lapply(ex_assignments, function(ea) {
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
          total_exercises <- total_exercises + length(ex_assignments)
        } else {
          cat(sprintf("\n    WARN: Exercise insert failed: %s\n", resp_body_string(resp)))
        }
      }
      
      total_sessions <- total_sessions + 1L
      cat(".")
    }
  }
  
  cat(sprintf("\n\nDone!\n"))
  cat(sprintf("  Program ID:  %s\n", program_id))
  cat(sprintf("  Sessions:    %d\n", total_sessions))
  cat(sprintf("  Exercises:   %d prescriptions written\n", total_exercises))
  
  # ── Preview week 1 ────────────────────────────────────────
  cat("\n── Week 1 Preview ──────────────────────────────────\n")
  preview_program(program_id)
  
  invisible(program_id)
}

# ============================================================
# 8. PREVIEW: print a week of the generated program
# ============================================================
preview_program <- function(program_id, week = 1) {
  workouts <- sb_get("workouts",
                     sprintf("?program_id=eq.%s&week_number=eq.%d&order=session_number", program_id, week))
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
                    e$exercise_order, ss,
                    e$exercises$name,
                    wu,
                    e$prescribed_sets,
                    e$rep_range_low, e$rep_range_high,
                    e$rpe_target))
      }
    }
  }
  invisible(NULL)
}

# ============================================================
# 9. REGENERATE: update remaining sessions after a swap
# ============================================================
regenerate_from_week <- function(program_id, user_id, from_week,
                                 swapped_exercise_id, replacement_exercise_id) {
  # Fetch program details
  prog <- sb_get("programs", sprintf("?id=eq.%s", program_id))
  if (is.null(prog) || nrow(prog) == 0) stop("Program not found")
  prog <- prog[1, ]
  
  # Fetch all future workout_exercises with the swapped exercise
  future_we <- sb_get("workout_exercises",
                      sprintf(
                        "?workout_id=in.(%s)&exercise_id=eq.%s",
                        paste(
                          sb_get("workouts",
                                 sprintf("?program_id=eq.%s&week_number=gte.%d&select=id", program_id, from_week)
                          )$id,
                          collapse = ","
                        ),
                        swapped_exercise_id
                      )
  )
  
  if (is.null(future_we) || nrow(future_we) == 0) {
    cat("No future instances of this exercise found.\n")
    return(invisible(NULL))
  }
  
  # Update each with the replacement exercise
  for (we_id in future_we$id) {
    sb_patch("workout_exercises",
             sprintf("?id=eq.%s", we_id),
             list(exercise_id = replacement_exercise_id, is_swapped = TRUE))
  }
  
  cat(sprintf("Updated %d future sessions: swapped exercise %s → %s\n",
              nrow(future_we), swapped_exercise_id, replacement_exercise_id))
  invisible(nrow(future_we))
}

# ============================================================
# EXAMPLE USAGE (comment out after first run)
# ============================================================

# Step 1: Make sure you have a user in auth.users.
#         For testing, use the Supabase service role to create one:
#
# TEST_USER_ID <- "paste-a-uuid-here"  # get from Supabase Auth dashboard
#
# Step 2: Generate the program
#
# program_id <- generate_program(
#   user_id          = TEST_USER_ID,
#   goal             = "hypertrophy",
#   difficulty       = "intermediate",
#   sessions_per_week = 3L,
#   split_style      = "full_body",
#   equipment        = c(
#     "barbell", "dumbbells", "ez_bar", "squat_rack", "bench",
#     "cable_machine", "lat_pulldown_machine", "leg_press_machine",
#     "hack_squat_machine", "leg_extension_machine",
#     "seated_leg_curl_machine", "hip_thrust_machine",
#     "hip_abduction_machine", "calf_raise_machine",
#     "pec_dec_machine", "chest_press_machine",
#     "incline_press_machine", "shoulder_press_machine",
#     "lateral_raise_machine", "ab_machine", "pullup_bar",
#     "seated_calf_raise_machine", "dip_bars"
#   ),
#   start_date       = Sys.Date()
# )
#
# Step 3: Preview any week
# preview_program(program_id, week = 5)   # see block B variation
# preview_program(program_id, week = 9)   # see block C variation