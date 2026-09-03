# ============================================================
# seed_variety_2026_09c.R — CatRack
# Evidence-based variety additions, batch 3 (2026-09 refactor).
#
# Fills the last thin spots: barbell-free "big lift" alternates for a
# dumbbell-only day, single-arm / floor pressing (unilateral -> anti-rotation
# core + cuts limb asymmetry: Gonzalo-Skok 2017; Zhang 2023 meta), and a couple
# of dumbbell/bodyweight accessory gap-fillers. All flow into EXISTING generator
# slots by movement_pattern + category, so the variety engine rotates them in.
#
# Idempotent: skips any exercise whose name already exists. Safe to re-run.
#   Rscript seed_variety_2026_09c.R
# ============================================================

if (nchar(Sys.getenv("SUPABASE_SERVICE_KEY")) == 0 && file.exists(".Renviron"))
  readRenviron(".Renviron")

library(httr2); library(jsonlite)
URL <- Sys.getenv("SUPABASE_URL"); KEY <- Sys.getenv("SUPABASE_SERVICE_KEY")
stopifnot("SUPABASE_URL not set" = nchar(URL) > 0, "SUPABASE_SERVICE_KEY not set" = nchar(KEY) > 0)

sb_get <- function(path) {
  r <- request(paste0(URL, "/rest/v1/", path)) |>
    req_headers(apikey = KEY, Authorization = paste("Bearer", KEY)) |>
    req_error(is_error = \(x) FALSE) |> req_perform()
  if (r$status_code != 200) return(NULL)
  fromJSON(resp_body_string(r), simplifyDataFrame = TRUE)
}
sb_insert_one <- function(row) {
  request(paste0(URL, "/rest/v1/exercises")) |>
    req_headers(apikey = KEY, Authorization = paste("Bearer", KEY),
                "Content-Type" = "application/json", Prefer = "return=representation") |>
    req_body_raw(toJSON(row, auto_unbox = TRUE, na = "null")) |>
    req_method("POST") |> req_error(is_error = \(x) FALSE) |> req_perform()
}
ex <- function(name, category, movement_pattern, primary, secondary = character(0),
               equipment = "dumbbells", is_compound = TRUE, rl = 8L, rh = 12L,
               sets = 3L, rpe = 8.0, rest = 90L, set_type = "working",
               sub1 = NA_character_, sub2 = NA_character_, note = NA_character_) {
  list(name = name, category = category, movement_pattern = movement_pattern,
       primary_muscles = I(primary), secondary_muscles = I(secondary),
       equipment_required = I(equipment), is_compound = is_compound,
       default_rep_range_low = as.integer(rl), default_rep_range_high = as.integer(rh),
       default_sets = as.integer(sets), default_rpe_target = rpe,
       rest_seconds = as.integer(rest), set_type = set_type,
       substitution_1 = sub1, substitution_2 = sub2, coaching_note = note)
}

new_ex <- list(
  # ── PRESSES — floor + single-arm (dumbbell, joint-friendly, anti-rotation) ──
  ex("DB Floor Press", "horizontal_push", "horizontal_push", c("chest"), c("triceps", "front_delts"),
     c("dumbbells"), TRUE, 8L, 12L, 3L, 8.0, 120L, "working",
     "DB Chest Press", "Barbell Bench Press",
     "Lie on the floor and press. The floor caps the descent — easier on the shoulders and a great triceps/lockout emphasis. No bench required."),
  ex("Single-Arm DB Floor Press", "horizontal_push", "horizontal_push", c("chest"), c("triceps", "front_delts", "core"),
     c("dumbbells"), TRUE, 8L, 12L, 3L, 8.0, 90L, "working",
     "DB Floor Press", "DB Chest Press",
     "One dumbbell, pressed from the floor. The offset load forces the core to resist rotation while you press — chest work plus anti-rotation trunk stability."),
  ex("Single-Arm DB Overhead Press", "vertical_push", "vertical_push", c("front_delts", "mid_delts"), c("triceps", "core"),
     c("dumbbells"), TRUE, 6L, 10L, 3L, 8.0, 90L, "working",
     "Seated DB Shoulder Press", "Standing DB Arnold Press",
     "Press one DB overhead standing. Bracing against the offset load turns it into serious oblique/anti-lateral-flexion work on top of the shoulder press."),

  # ── DUMBBELL 'BIG LIFT' ALTERNATES — for a barbell-free day ──
  ex("Dumbbell Deadlift", "hinge", "hinge", c("hamstrings", "glutes", "quads"), c("lower_back", "core"),
     c("dumbbells"), TRUE, 6L, 10L, 3L, 8.0, 150L, "working",
     "Barbell Deadlift", "DB RDL",
     "Deadlift from the floor with a DB in each hand. A loadable full-hinge pattern when a barbell isn't available; brace hard and drive the floor away."),
  ex("Single-Arm DB RDL", "hinge", "hinge", c("hamstrings", "glutes"), c("core", "lower_back"),
     c("dumbbells"), TRUE, 8L, 12L, 3L, 8.0, 90L, "working",
     "B-Stance RDL", "DB RDL",
     "RDL holding one DB on the opposite side; resist the pull into rotation. Unilateral hamstring/glute work that also hammers anti-rotation — evens out left/right."),
  ex("DB Front Squat", "squat", "squat", c("quads"), c("glutes", "core", "adductors"),
     c("dumbbells"), TRUE, 8L, 12L, 3L, 8.0, 120L, "working",
     "DB Goblet Squat", "Front Squat",
     "Two DBs racked on the shoulders, squat upright and deep. Keeps the torso tall and biases the quads — a barbell-free front-squat stand-in."),

  # ── GLUTE — dumbbell hip thrust (barbell-free) ──
  ex("DB Hip Thrust", "hip_thrust", "hip_extension", c("glutes"), c("hamstrings", "core"),
     c("dumbbells", "bench"), FALSE, 10L, 15L, 3L, 8.0, 90L, "working",
     "Barbell Hip Thrust", "Single-Leg Hip Thrust",
     "Upper back on a bench, a DB across the hips, drive to full extension and squeeze. All the hip-thrust glute stimulus without a barbell."),

  # ── TRICEPS — dumbbell + bodyweight options ──
  ex("DB Skull Crusher", "triceps", "elbow_extension", c("triceps"), character(0),
     c("dumbbells", "bench"), FALSE, 10L, 15L, 3L, 8.0, 75L, "working",
     "EZ Bar Skull Crusher", "Overhead DB Tricep Extension",
     "Lying triceps extension with DBs (neutral grip is easy on the elbows). Lower to the ears, keep the upper arms still."),
  ex("Bench Dips", "triceps", "elbow_extension", c("triceps"), c("chest", "front_delts"),
     c("bench"), FALSE, 8L, 15L, 3L, 8.0, 75L, "working",
     "Dips", "Cable Tricep Pushdown",
     "Hands on a bench behind you, lower and press back up. Bodyweight triceps you can do anywhere; put your feet up or a plate on the lap to load."),

  # ── FUNCTIONAL ROW — anti-rotation (dumbbell) ──
  ex("Renegade Row", "horizontal_pull", "horizontal_pull", c("lats", "mid_back"), c("biceps", "core", "rear_delts"),
     c("dumbbells"), TRUE, 8L, 12L, 3L, 8.0, 90L, "working",
     "One-Arm Dumbbell Row", "Chest Supported DB Row",
     "In a plank on two DBs, row one at a time while keeping the hips square. A back builder and a brutal anti-rotation core drill in one — very functional.")
)

existing <- sb_get("exercises?select=name")
have <- if (!is.null(existing) && nrow(existing) > 0) tolower(trimws(existing$name)) else character(0)
to_add <- Filter(function(e) !(tolower(e$name) %in% have), new_ex)

cat(sprintf("Library: %d existing | %d new to add (%d already present)\n",
            length(have), length(to_add), length(new_ex) - length(to_add)))
for (e in to_add) {
  r <- sb_insert_one(e)
  cat(sprintf("  [%s] %-28s HTTP %d\n",
              if (r$status_code %in% c(200, 201)) "ok" else "ERR", e$name, r$status_code))
  if (!r$status_code %in% c(200, 201)) cat("      ", resp_body_string(r), "\n")
}
cat("\nDone.\n")
