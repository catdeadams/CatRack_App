# ============================================================
# seed_variety_2026_09.R — CatRack
# Evidence-based variety additions (2026-09 refactor, phase 2).
#
# Rationale (peer-reviewed):
#  • Systematic exercise variation that targets different regions / force-length
#    profiles enhances regional hypertrophy & strength; redundant or random
#    swapping does not (Kassiano 2022 systematic review; Costa 2021).
#  • Training at LONG muscle lengths drives greater (esp. distal) hypertrophy
#    (Strey 2026 meta-analysis; Maeo 2022 overhead triceps; McMahon 2026).
#  • Running economy: heavy resistance + plyometrics (Eihara 2022; Llanos-Lagos
#    2024; Balsalobre-Fernandez 2016).
#
# Every exercise here fills a real gap: a bodyweight/DB/cable option where a
# pattern was machine-gated, a lengthened-position variant, more loaded-carry
# and rotational-core variety, or frontal-plane single-leg work. They flow into
# the EXISTING generator slots by movement_pattern + category, so the variety
# engine rotates them in automatically.
#
# Idempotent: skips any exercise whose name already exists. Safe to re-run.
#   Rscript seed_variety_2026_09.R    (or source() in RStudio)
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
  # ── QUAD isolation — was machine-ONLY (Leg Extension). These are bodyweight
  #    and bias the LENGTHENED position (strong regional-hypertrophy evidence). ──
  ex("Reverse Nordic Curl", "leg_extension", "knee_extension", c("quads"), c("core"),
     "bodyweight", FALSE, 6L, 12L, 3L, 8.0, 90L, "working",
     "Leg Extension", "Sissy Squat",
     "Kneel tall, then lean back as far as you can control, keeping hips extended. Trains the quads (esp. rectus femoris) at long muscle length — a potent, joint-friendly growth stimulus."),
  ex("Sissy Squat", "leg_extension", "knee_extension", c("quads"), c("core"),
     "bodyweight", FALSE, 8L, 15L, 3L, 8.0, 90L, "working",
     "Reverse Nordic Curl", "Leg Extension",
     "Hold a support, rise on the balls of the feet and lean back, driving knees forward as you lower. Deep-stretch quad work; add a plate to the chest to progress."),

  # ── HAMSTRING curl — was machine-only + Nordic. Bodyweight knee-flexion. ──
  ex("Slider Leg Curl", "leg_curl", "knee_flexion", c("hamstrings"), c("glutes", "calves"),
     "bodyweight", FALSE, 8L, 15L, 3L, 8.0, 90L, "working",
     "Nordic Ham Curl", "Lying Leg Curl",
     "Lie on your back, heels on sliders/towels, bridge the hips up, then curl the heels toward you and extend out slowly. Keep hips high the whole set. Do it single-leg to progress."),

  # ── GLUTE — bodyweight unilateral option. ──
  ex("Single-Leg Hip Thrust", "hip_thrust", "hip_extension", c("glutes"), c("hamstrings", "core"),
     "bodyweight", FALSE, 10L, 20L, 3L, 8.0, 75L, "working",
     "Glute Bridge", "Barbell Hip Thrust",
     "Shoulders on a bench, drive through one heel to full hip extension, other knee tucked. Squeeze hard at the top, keep ribs down. Add a DB on the hip to load."),

  # ── LOADED CARRIES — was basically Farmer's/Suitcase for DB users. Different
  #    hold positions train different trunk/shoulder demands (functional core). ──
  ex("Overhead Carry", "conditioning", "locomotion", c("core", "shoulders"), c("traps", "forearms"),
     "dumbbells", TRUE, 20L, 40L, 3L, 8.0, 90L, "carry",
     "Front Rack Carry", "Farmer's Carry",
     "Press one or two DBs overhead, elbows locked, and walk with ribs down. Brutal anti-extension core + overhead shoulder stability. Log the time (sec) or steps in the reps field."),
  ex("Front Rack Carry", "conditioning", "locomotion", c("core"), c("upper_back", "forearms"),
     "dumbbells", TRUE, 20L, 45L, 3L, 8.0, 90L, "carry",
     "Farmer's Carry", "Zercher Carry",
     "DBs racked at the shoulders, elbows high, walk tall. Loads the upper back and anti-flexion core. Log the time (sec) or steps in the reps field."),
  ex("Zercher Carry", "conditioning", "locomotion", c("core"), c("upper_back", "biceps", "forearms"),
     "barbell", TRUE, 20L, 40L, 3L, 8.0, 120L, "carry",
     "Front Rack Carry", "Farmer's Carry",
     "Barbell in the crooks of the elbows, chest up, walk. Punishing upper-back and bracing demand. Log the time (sec) or steps in the reps field."),

  # ── ROTATIONAL CORE — was only Russian Twists + one Wood Chop. ──
  ex("Landmine Rotation", "core", "rotation", c("core", "obliques"), c("front_delts"),
     "barbell", FALSE, 8L, 12L, 3L, 8.0, 75L, "working",
     "Wood Chop", "Low to High Cable Chop",
     "Barbell end in both hands at arms' length; rotate it side to side from the hips and trunk, not the arms. Anti-rotation on the way, rotation on the way through — great athletic core."),
  ex("Low to High Cable Chop", "core", "rotation", c("core", "obliques"), c("front_delts"),
     "cable_machine", FALSE, 12L, 15L, 3L, 8.0, 75L, "working",
     "Wood Chop", "Landmine Rotation",
     "Cable set low; sweep from the outside hip up across the body. Complements the high-to-low Wood Chop for the other diagonal."),

  # ── HIP-STABILITY / ADDUCTOR — runner + functional durability (isometric). ──
  ex("Copenhagen Plank", "core", "anti_extension", c("adductors", "core"), c("glutes"),
     "bodyweight", FALSE, 15L, 40L, 3L, 8.0, 60L, "isometric",
     "Pallof Press", "Side Plank",
     "Side plank with the TOP leg's foot/shin on a bench, bottom leg lifted to it. Builds groin/adductor strength — a key runner & change-of-direction durability piece. Log seconds held."),

  # ── FRONTAL-PLANE / UNILATERAL — running & functional carryover; sagittal
  #    lunges alone miss side-to-side strength and adductor length. ──
  ex("Cossack Squat", "single_leg", "lunge", c("quads", "glutes", "adductors"), c("hamstrings", "core"),
     "dumbbells", TRUE, 8L, 12L, 3L, 8.0, 90L, "working",
     "Lateral Lunge", "Bulgarian Split Squat",
     "Shift side to side over one deeply-bent leg, the other straight, heels down. Loads the working quad/glute and lengthens the trailing adductor — strong lateral-stability + mobility work."),
  ex("Lateral Lunge", "single_leg", "lunge", c("quads", "glutes"), c("adductors", "hamstrings"),
     "dumbbells", TRUE, 8L, 12L, 3L, 8.0, 90L, "working",
     "Cossack Squat", "DB Reverse Lunge",
     "Step wide to one side, sit back into that hip, push back to center. Frontal-plane strength the sagittal lunges miss."),
  ex("Skater Squat", "single_leg", "lunge", c("quads", "glutes"), c("hamstrings", "core"),
     "bodyweight", TRUE, 8L, 15L, 3L, 8.0, 90L, "working",
     "Bulgarian Split Squat", "Single Leg Squat to Box",
     "Single-leg squat reaching the rear knee toward the floor, torso hinged, arms as a counterweight. Big balance + single-leg strength demand; hold a light DB to load."),

  # ── LENGTHENED-BIAS accessories where the pattern lacked one. ──
  ex("Bayesian Cable Curl", "biceps", "elbow_flexion", c("biceps"), c("brachialis"),
     "cable_machine", FALSE, 10L, 15L, 3L, 9.0, 75L, "working",
     "Incline Dumbbell Curl", "Cable Curl",
     "Face away from a low cable, elbow behind the torso, curl. Keeps tension in the fully lengthened biceps position — where the growth stimulus is strongest."),
  ex("Deficit Push-up", "horizontal_push", "horizontal_push", c("chest", "triceps", "front_delts"), c("core"),
     "bodyweight", TRUE, 8L, 15L, 3L, 8.0, 90L, "working",
     "DB Chest Press", "Push-up",
     "Hands on two DBs or blocks so the chest sinks below the hands. Full lengthened-position stretch on the chest with no equipment. Elevate the feet to add load."),

  # ── QUAD-biased squat pattern for DB users (adds squat variety). ──
  ex("Heel-Elevated Goblet Squat", "squat", "squat", c("quads"), c("glutes", "core", "adductors"),
     "dumbbells", TRUE, 8L, 15L, 3L, 8.0, 120L, "working",
     "DB Goblet Squat", "Front Squat",
     "Goblet squat with heels on a small wedge/plate. Keeps the torso upright and shifts emphasis onto the quads through a deeper knee-forward range.")
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
