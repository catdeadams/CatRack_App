# ============================================================
# db_migrate_2026_09.R — CatRack data migration (2026-09 refactor)
#
# Idempotent, non-destructive fixes to the `exercises` table:
#   1. Rename "Kettlebell Swing" (dumbbell-based) -> "Dumbbell Swing"
#      so users without a kettlebell aren't shown a "kettlebell" lift.
#   2. Fix "Nordic Ham Curl" equipment (was lat_pulldown_machine, wrong)
#      -> bodyweight, so the hamstring/knee-flexion slot can always fill.
#   3. Add the plyometric + isometric-hold library the running-support
#      program needs (seed_running_exercises.R was never run on this DB),
#      plus bodyweight/cable hip-abduction fallbacks so the running
#      hip-stability slot never silently drops.
#
# Safe to re-run: renames only fire when the old row still exists, and
# new exercises are skipped if their name is already present.
#
# RUN once:  source("db_migrate_2026_09.R")   (RStudio, .Renviron loaded)
#   or:      Rscript db_migrate_2026_09.R
# The app never sources this — it is a dev/admin script.
# ============================================================

if (nchar(Sys.getenv("SUPABASE_SERVICE_KEY")) == 0 && file.exists(".Renviron"))
  readRenviron(".Renviron")

library(httr2)
library(jsonlite)

URL <- Sys.getenv("SUPABASE_URL")
KEY <- Sys.getenv("SUPABASE_SERVICE_KEY")
stopifnot("SUPABASE_URL not set"         = nchar(URL) > 0,
          "SUPABASE_SERVICE_KEY not set" = nchar(KEY) > 0)

sb_get <- function(path) {
  r <- request(paste0(URL, "/rest/v1/", path)) |>
    req_headers(apikey = KEY, Authorization = paste("Bearer", KEY)) |>
    req_error(is_error = \(x) FALSE) |> req_perform()
  if (r$status_code != 200) return(NULL)
  fromJSON(resp_body_string(r), simplifyDataFrame = TRUE)
}

sb_patch_id <- function(id, fields) {
  request(paste0(URL, "/rest/v1/exercises?id=eq.", id)) |>
    req_headers(apikey = KEY, Authorization = paste("Bearer", KEY),
                "Content-Type" = "application/json",
                Prefer = "return=representation") |>
    req_body_raw(toJSON(fields, auto_unbox = TRUE, na = "null")) |>
    req_method("PATCH") |> req_error(is_error = \(x) FALSE) |> req_perform()
}

sb_insert_one <- function(row) {
  request(paste0(URL, "/rest/v1/exercises")) |>
    req_headers(apikey = KEY, Authorization = paste("Bearer", KEY),
                "Content-Type" = "application/json", Prefer = "return=representation") |>
    req_body_raw(toJSON(row, auto_unbox = TRUE, na = "null")) |>
    req_method("POST") |> req_error(is_error = \(x) FALSE) |> req_perform()
}

# ── 1 + 2. Renames / equipment fixes (by current name, idempotent) ──
fix_by_name <- function(old_name, fields, note) {
  row <- sb_get(sprintf("exercises?name=eq.%s&select=id",
                        URLencode(old_name, reserved = TRUE)))
  if (is.null(row) || nrow(row) == 0) {
    cat(sprintf("  [skip] '%s' not found (already migrated?)\n", old_name)); return(invisible())
  }
  r <- sb_patch_id(row$id[1], fields)
  cat(sprintf("  [%s] %s -> HTTP %d\n", if (r$status_code %in% c(200,204)) "ok" else "ERR",
              note, r$status_code))
  if (!r$status_code %in% c(200,204)) cat("      ", resp_body_string(r), "\n")
}

cat("Renames / equipment fixes:\n")
fix_by_name("Kettlebell Swing",
  list(name = "Dumbbell Swing",
       coaching_note = "Hip-hinge power move holding one dumbbell vertically by the top head. Snap the hips to float the bell to shoulder height — power comes from the hips, not the arms."),
  "Kettlebell Swing -> Dumbbell Swing")
fix_by_name("Nordic Ham Curl",
  list(equipment_required = I(c("bodyweight")),
       coaching_note = "Anchor your feet (under a loaded bar, a heavy DB, or a partner). Keep hips extended and lower as slowly as you can, then push back up. Eccentric-only is fine until you can control the full range."),
  "Nordic Ham Curl equipment -> bodyweight")

# ── 3. New exercises (plyometric / isometric / abduction fallbacks) ──
ex <- function(name, category, movement_pattern, primary,
               secondary = character(0), equipment = "bodyweight",
               is_compound = FALSE, rl = 8L, rh = 12L, sets = 3L,
               rpe = 8.0, rest = 120L, set_type = "working",
               note = NA_character_) {
  list(name = name, category = category, movement_pattern = movement_pattern,
       primary_muscles = I(primary), secondary_muscles = I(secondary),
       equipment_required = I(equipment), is_compound = is_compound,
       default_rep_range_low = as.integer(rl), default_rep_range_high = as.integer(rh),
       default_sets = as.integer(sets), default_rpe_target = rpe,
       rest_seconds = as.integer(rest), set_type = set_type, coaching_note = note)
}

new_ex <- list(
  # Plyometrics — bodyweight, low reps, maximal intent (running economy + power)
  ex("Squat Jump","plyometric","plyometric", c("quads","glutes"), c("calves"), "bodyweight", TRUE, 5L,8L, 3L, 8.0, 120L, "plyometric",
     "Sink to a quarter/half squat and jump as high as possible. Land softly and reset each rep — quality over fatigue."),
  ex("Box Jump","plyometric","plyometric", c("quads","glutes"), c("calves"), "bodyweight", TRUE, 5L,8L, 3L, 8.0, 120L, "plyometric",
     "Explode onto a sturdy box, land soft with knees tracking over toes. Step down (don't jump down) to save the knees."),
  ex("Broad Jump","plyometric","plyometric", c("glutes","quads"), c("hamstrings","calves"), "bodyweight", TRUE, 4L,6L, 3L, 8.0, 120L, "plyometric",
     "Horizontal max-effort jump; swing the arms, land balanced. Full reset between reps."),
  ex("Tuck Jump","plyometric","plyometric", c("quads","glutes"), c("calves"), "bodyweight", TRUE, 6L,10L, 3L, 8.0, 90L, "plyometric",
     "Jump and pull knees toward chest. Soft, quiet landings; stop the set when height drops."),
  ex("Pogo Hops","plyometric","plyometric", c("calves"), character(0), "bodyweight", FALSE, 10L,20L, 3L, 8.0, 90L, "plyometric",
     "Stiff-ankle rebound hops off the balls of the feet, minimal knee bend. Trains Achilles/ankle stiffness for running."),
  ex("Bounding","plyometric","plyometric", c("glutes","hamstrings"), c("calves","quads"), "bodyweight", TRUE, 6L,12L, 3L, 8.0, 120L, "plyometric",
     "Exaggerated running leaps for distance — drive the knee, cover ground. Great single-leg power/economy work for runners."),
  ex("Lateral Skater Jump","plyometric","plyometric", c("glutes"), c("quads","calves"), "bodyweight", TRUE, 6L,10L, 3L, 8.0, 90L, "plyometric",
     "Bound side to side, landing on one leg and stabilizing. Frontal-plane power + hip stability for runners."),
  ex("Depth Jump","plyometric","plyometric", c("quads","glutes"), c("calves"), "bodyweight", TRUE, 4L,6L, 3L, 8.0, 150L, "plyometric",
     "Step off a low box, land, and immediately rebound as high as possible. Advanced — minimize ground contact time."),

  # Isometric holds — REP FIELD = SECONDS (set_type isometric); tendon/soleus/quad durability
  ex("Wall Sit","isometric","knee_extension", c("quads"), c("glutes"), "bodyweight", FALSE, 20L,45L, 3L, 8.0, 90L, "isometric",
     "Back flat on a wall, thighs parallel to the floor. Hold for time. Add load by holding a plate/DB on the lap."),
  ex("Single-Leg Wall Sit","isometric","knee_extension", c("quads"), c("glutes"), "bodyweight", FALSE, 15L,30L, 3L, 8.0, 90L, "isometric",
     "Wall sit on one leg, other foot off the floor. Keep hips level. Hold for time."),
  ex("Split Squat Iso Hold","isometric","lunge", c("quads","glutes"), c("hamstrings"), "bodyweight", FALSE, 20L,45L, 3L, 8.0, 90L, "isometric",
     "Hold the bottom of a split squat, front shin vertical. Add a DB in the goblet position to progress."),
  ex("Single-Leg Calf Raise Hold","isometric","plantarflexion", c("calves"), character(0), "bodyweight", FALSE, 20L,45L, 3L, 8.0, 75L, "isometric",
     "Hold the top of a single-leg calf raise (heel high). Builds Achilles/gastroc stiffness for running."),
  ex("Standing Calf Raise Hold","isometric","plantarflexion", c("calves"), character(0), "bodyweight", FALSE, 20L,45L, 3L, 8.0, 75L, "isometric",
     "Hold the top of a two-leg standing calf raise. Straight knee targets the gastroc."),
  ex("Seated Calf Raise Hold","isometric","plantarflexion", c("calves"), character(0), "bodyweight", FALSE, 30L,60L, 3L, 8.0, 75L, "isometric",
     "Hold the top of a seated calf raise (knee bent) — biases the soleus, the key endurance calf muscle for runners."),

  # Hip-abduction fallbacks so the running hip-stability slot never drops
  ex("Side-Lying Hip Abduction","glute_accessory","abduction", c("glutes","hip_abductors"), character(0), "bodyweight", FALSE, 12L,20L, 3L, 8.0, 60L, "working",
     "Lie on your side, raise the top leg with the toe pointed slightly down and the hip stacked. Slow and controlled — glute medius stability for runners."),
  ex("Cable Hip Abduction","glute_accessory","abduction", c("glutes","hip_abductors"), c("adductors"), "cable_machine", FALSE, 12L,20L, 3L, 8.0, 60L, "working",
     "Ankle strap on the outside leg, stand side-on to a low cable and lift the leg out. Keep the torso still — resist leaning.")
)

existing <- sb_get("exercises?select=name")
have <- if (!is.null(existing) && nrow(existing) > 0) tolower(trimws(existing$name)) else character(0)
to_add <- Filter(function(e) !(tolower(e$name) %in% have), new_ex)

cat(sprintf("\nNew exercises: %d in library | %d to add (%d already present)\n",
            length(have), length(to_add), length(new_ex) - length(to_add)))
for (e in to_add) {
  r <- sb_insert_one(e)
  cat(sprintf("  [%s] %-28s HTTP %d\n",
              if (r$status_code %in% c(200,201)) "ok" else "ERR", e$name, r$status_code))
  if (!r$status_code %in% c(200,201)) cat("      ", resp_body_string(r), "\n")
}
cat("\nMigration done.\n")
