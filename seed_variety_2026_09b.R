# ============================================================
# seed_variety_2026_09b.R — CatRack
# Evidence-based variety additions, batch 2 (2026-09 refactor).
#
# Same approach as seed_variety_2026_09.R: fill real gaps (bodyweight/band/DB
# options for machine-gated patterns, complementary angles, more core &
# posterior-chain durability) with exercises that flow into the EXISTING
# generator slots by movement_pattern + category.
#
# Evidence pillars:
#  • Systematic, non-redundant variation -> regional hypertrophy (Kassiano 2022;
#    Costa 2021).
#  • Long-muscle-length training -> more growth, esp. distal (Strey 2026 meta).
#  • Running economy <- heavy strength + plyometrics; calf/soleus & trunk
#    durability (Eihara 2022; Balsalobre-Fernandez 2016).
#  • Unilateral work cuts limb asymmetry and improves sprint / change-of-
#    direction / single-leg power / balance; bilateral stays best for max
#    strength -> keep both (Gonzalo-Skok 2017; Zhang 2023 meta; Duan 2024).
#
# Idempotent: skips any exercise whose name already exists. Safe to re-run.
#   Rscript seed_variety_2026_09b.R
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
               equipment = "bodyweight", is_compound = FALSE, rl = 8L, rh = 12L,
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
  # ── CALVES — dynamic bodyweight / DB options (was machine-dependent). Calf &
  #    soleus strength matter for running economy and Achilles durability. ──
  ex("Single-Leg Calf Raise", "calves", "plantarflexion", c("calves"), character(0),
     "bodyweight", FALSE, 8L, 15L, 3L, 8.0, 60L, "working",
     "Standing Calf Raise", "DB Seated Calf Raise",
     "One leg on a step, full stretch at the bottom, full contraction at the top, no bouncing. Balance forces each side to do its own work. Hold a DB to load."),
  ex("DB Seated Calf Raise", "calves", "plantarflexion", c("calves"), character(0),
     "dumbbells", FALSE, 12L, 20L, 3L, 8.0, 60L, "working",
     "Seated Calf Raise", "Single-Leg Calf Raise",
     "Seated with DBs on the knees, bent knee biases the SOLEUS — the endurance calf muscle that drives running economy. Slow, full range."),

  # ── QUAD, lengthened, band (owner has bands; no leg-extension machine). ──
  ex("Spanish Squat", "leg_extension", "knee_extension", c("quads"), c("glutes"),
     "resistance_bands", FALSE, 10L, 15L, 3L, 8.0, 75L, "working",
     "Sissy Squat", "Leg Extension",
     "Loop a band behind the knees anchored in front; sit straight down keeping shins vertical. Big quad tension with a knee-friendly, near-vertical torso. Pause at the bottom to make it brutal."),

  # ── HINGE — loadable UNILATERAL-biased option (asymmetry + posterior chain). ──
  ex("B-Stance RDL", "hinge", "hinge", c("hamstrings", "glutes"), c("lower_back", "adductors"),
     "dumbbells", TRUE, 8L, 12L, 3L, 8.0, 90L, "working",
     "Single-Leg RDL", "DB RDL",
     "Staggered stance, ~70% of the weight on the front leg, back toe as a kickstand. Most of a single-leg RDL's unilateral hamstring stimulus with far less balance tax, so you can load it."),

  # ── HORIZONTAL PULL — scalable bodyweight row (was all machine/DB/barbell). ──
  ex("Inverted Row", "horizontal_pull", "horizontal_pull", c("lats", "mid_back"), c("biceps", "rear_delts", "core"),
     c("barbell", "squat_rack"), TRUE, 8L, 15L, 3L, 8.0, 90L, "working",
     "Chest Supported DB Row", "Seated Cable Row",
     "Set a bar in the rack at hip height, hang underneath and row your chest to it, body in a straight line. Raise the bar to make it easier, lower it (or elevate the feet) to make it harder."),

  # ── REAR DELT / UPPER-BACK POSTURE — band option (owner has bands). ──
  ex("Band Pull-Apart", "rear_delt", "rear_delt_fly", c("rear_delts", "mid_back"), c("traps"),
     "resistance_bands", FALSE, 15L, 25L, 3L, 7.0, 45L, "working",
     "Cable Face Pull", "Reverse Flys",
     "Band at arm's length, pull it apart to the chest by squeezing the shoulder blades, no shrug. High-rep rear-delt and postural work; a great superset filler."),

  # ── CHEST — different fly angle (upper-chest bias). ──
  ex("Low to High Cable Fly", "chest_fly", "chest_fly", c("chest"), c("front_delts"),
     "cable_machine", FALSE, 12L, 15L, 3L, 9.0, 75L, "working",
     "Cable Chest Flies", "DB Fly",
     "Cables set low, sweep hands up and together to eye level. Biases the upper chest and complements the flat/high-to-low fly angles for fuller development."),

  # ── VERTICAL PUSH — bodyweight + explosive DB options. ──
  ex("Pike Push-up", "vertical_push", "vertical_push", c("front_delts", "mid_delts"), c("triceps", "core"),
     "bodyweight", TRUE, 6L, 12L, 3L, 8.0, 90L, "working",
     "Seated DB Shoulder Press", "Push-up",
     "Hips high in an inverted-V, lower the crown of the head toward the floor and press back up. Bodyweight overhead pressing; elevate the feet to load the shoulders more."),
  ex("DB Push Press", "vertical_push", "vertical_push", c("front_delts", "mid_delts"), c("triceps", "quads", "core"),
     "dumbbells", TRUE, 6L, 10L, 3L, 8.0, 120L, "working",
     "Seated DB Shoulder Press", "Standing DB Arnold Press",
     "A small dip-and-drive from the legs launches the DBs overhead, then control them down. Lets you overload the shoulders and trains total-body power transfer — a strong functional press."),

  # ── CORE — anti-extension & anti-lateral-flexion durability (runners/athletes). ──
  ex("Hollow Body Hold", "core", "anti_extension", c("core"), c("hip_flexors"),
     "bodyweight", FALSE, 15L, 45L, 3L, 8.0, 60L, "isometric",
     "Plank", "Dead Bug",
     "On your back, press the low back into the floor and lift shoulders and legs into a shallow banana shape. The gold-standard anti-extension brace. Log seconds held; tuck the knees to regress."),
  ex("Side Plank", "core", "anti_extension", c("core", "obliques"), c("glutes"),
     "bodyweight", FALSE, 20L, 45L, 3L, 8.0, 45L, "isometric",
     "Copenhagen Plank", "Plank",
     "On one forearm, body in a straight line, hips stacked and lifted. Anti-lateral-flexion strength for the obliques and QL — key trunk stability for running. Log seconds per side."),
  ex("Dead Bug", "core", "anti_extension", c("core"), c("hip_flexors"),
     "bodyweight", FALSE, 8L, 15L, 3L, 7.0, 45L, "working",
     "Hollow Body Hold", "Bird-Dog",
     "On your back, low back glued to the floor, slowly lower an opposite arm and leg then switch. Teaches you to brace the trunk while the limbs move — exactly what running and carrying demand. Reps per side.")
)

existing <- sb_get("exercises?select=name")
have <- if (!is.null(existing) && nrow(existing) > 0) tolower(trimws(existing$name)) else character(0)
to_add <- Filter(function(e) !(tolower(e$name) %in% have), new_ex)

cat(sprintf("Library: %d existing | %d new to add (%d already present)\n",
            length(have), length(to_add), length(new_ex) - length(to_add)))
for (e in to_add) {
  r <- sb_insert_one(e)
  cat(sprintf("  [%s] %-26s HTTP %d\n",
              if (r$status_code %in% c(200, 201)) "ok" else "ERR", e$name, r$status_code))
  if (!r$status_code %in% c(200, 201)) cat("      ", resp_body_string(r), "\n")
}
cat("\nDone.\n")
