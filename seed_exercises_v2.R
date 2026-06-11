# ============================================================
# seed_exercises_v2.R — CaTrack
# Adds exercises needed by the redesigned program generator.
#
# Fills gaps for:
#   - Pull-up Focus goal (assisted + eccentric progressions)
#   - Functional / Running Support goals (loaded carries, KB swing)
#
# USAGE:
#   1. Make sure SUPABASE_SERVICE_KEY is in your .Renviron
#   2. source("seed_exercises_v2.R")
#
# Safe to re-run — uses upsert on the unique exercise name.
# ============================================================

library(httr2)
library(jsonlite)

SUPABASE_URL         <- Sys.getenv("SUPABASE_URL",
                                   "https://fowpjdsixqhgaqgdeiph.supabase.co")
SUPABASE_SERVICE_KEY <- Sys.getenv("SUPABASE_SERVICE_KEY", "")

if (nchar(SUPABASE_SERVICE_KEY) == 0)
  stop("SUPABASE_SERVICE_KEY not set. Add it to .Renviron and restart R.")

# Upsert keyed on the `name` unique constraint so a re-run merges
# rather than colliding on PK. on_conflict tells PostgREST which
# constraint resolution=merge-duplicates should target.
sb_upsert <- function(table, data) {
  request(paste0(SUPABASE_URL, "/rest/v1/", table, "?on_conflict=name")) |>
    req_headers(
      "apikey"        = SUPABASE_SERVICE_KEY,
      "Authorization" = paste("Bearer", SUPABASE_SERVICE_KEY),
      "Content-Type"  = "application/json",
      "Prefer"        = "resolution=merge-duplicates"
    ) |>
    req_body_raw(toJSON(data, auto_unbox = TRUE, na = "null")) |>
    req_method("POST") |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
}

ex <- function(name, category, movement_pattern,
               primary_muscles, secondary_muscles, equipment_required,
               is_compound, rep_low, rep_high, sets, rpe, rest, set_type,
               sub1 = NA_character_, sub2 = NA_character_,
               note = NA_character_, youtube = NA_character_) {
  list(
    name                   = name,
    category               = category,
    movement_pattern       = movement_pattern,
    primary_muscles        = I(primary_muscles),
    secondary_muscles      = I(secondary_muscles),
    equipment_required     = I(equipment_required),
    is_compound            = is_compound,
    default_rep_range_low  = as.integer(rep_low),
    default_rep_range_high = as.integer(rep_high),
    default_sets           = as.integer(sets),
    default_rpe_target     = rpe,
    rest_seconds           = as.integer(rest),
    set_type               = set_type,
    substitution_1         = sub1,
    substitution_2         = sub2,
    coaching_note          = note,
    youtube_url            = youtube
  )
}

new_exercises <- list(

  # ── Pull-up baseline progressions (for users with < 3 strict) ──
  ex("Band Assisted Pull-up","vertical_pull","vertical_pull",
     c("lats"), c("biceps","mid_back","rear_delts"),
     c("pullup_bar","resistance_bands"), TRUE, 5,10,3, 8.0, 150, "working",
     "Eccentric Pull-up","Lat Pulldown",
     "Loop band over bar, foot/knee in band. Full hang at bottom, chin over bar at top. Reduce band thickness as you get stronger.",
     "https://www.youtube.com/watch?v=0AcrUcLNu1s"),

  ex("Eccentric Pull-up","vertical_pull","vertical_pull",
     c("lats"), c("biceps","mid_back","rear_delts"),
     c("pullup_bar"), TRUE, 3,6,3, 9.0, 180, "working",
     "Band Assisted Pull-up","Lat Pulldown",
     "Jump or step to the top (chin over bar). Lower yourself as slowly as possible — aim for 5+ seconds on the way down. Builds the strength for full pull-ups.",
     "https://www.youtube.com/watch?v=7-NPmS_RJWE"),

  # ── Loaded carries (for Functional + general core) ────────────
  ex("Farmer's Carry","conditioning","locomotion",
     c("core","traps"), c("forearms","calves","glutes"),
     c("dumbbells"), TRUE, 1,1,3, 8.0, 90, "timed",
     "Suitcase Carry","Trap Bar Carry",
     "Hold heavy dumbbells at sides. Walk 30-60s with ribs down, shoulders packed, normal stride. Grip and core get destroyed.",
     "https://www.youtube.com/watch?v=Goq__lU88SE"),

  ex("Suitcase Carry","conditioning","locomotion",
     c("core","obliques"), c("traps","forearms","glutes"),
     c("dumbbells"), TRUE, 1,1,3, 8.0, 90, "timed",
     "Farmer's Carry","Side Plank",
     "One heavy dumbbell, one side only. Resist the lateral lean — anti-lateral-flexion core work. 30-45s per side.",
     "https://www.youtube.com/watch?v=mtuB4XHwcCQ"),

  ex("Trap Bar Carry","conditioning","locomotion",
     c("core","traps"), c("forearms","calves","glutes"),
     c("trap_bar"), TRUE, 1,1,3, 8.0, 120, "timed",
     "Farmer's Carry","Suitcase Carry",
     "Heavier than DB Farmer's — load 1.5-2× bodyweight. Short walk 20-40s. Brutal for grip, traps, core."),

  # ── Kettlebell foundational (Functional goal) ─────────────────
  ex("KB Swing","hinge","hinge",
     c("glutes","hamstrings"), c("core","lats","forearms"),
     c("kettlebell"), TRUE, 10,20,4, 8.0, 90, "working",
     "Barbell RDL","Trap Bar Deadlift",
     "Hip-hinge dominant — bell floats up from hip drive, not arms. Pack lats, hard glute squeeze at top. Power generation, not curl.",
     "https://www.youtube.com/watch?v=cKx8xE8jJZs"),

  ex("KB Goblet Squat","squat","squat",
     c("quads","glutes"), c("core","adductors"),
     c("kettlebell"), TRUE, 8,15,3, 8.0, 120, "working",
     "DB Goblet Squat","Barbell Back Squat",
     "Hold bell at chest. Deep squat, elbows inside knees at bottom. Great for groove work and beginners.",
     "https://www.youtube.com/watch?v=MxsFDhcyFyE"),

  ex("KB Turkish Get-up","conditioning","locomotion",
     c("core","shoulders"), c("glutes","hamstrings","quads"),
     c("kettlebell"), TRUE, 3,5,2, 8.0, 120, "working",
     "Single Arm KB Press","Suitcase Carry",
     "Lie down with bell pressed overhead. Stand up in 7 stages without dropping arm. Slow, controlled. Time under load is the point.",
     "https://www.youtube.com/watch?v=0bWRPC49-KI")
)

# ── Seed ─────────────────────────────────────────────────────
cat(sprintf("Upserting %d new exercises...\n", length(new_exercises)))
resp <- sb_upsert("exercises", new_exercises)
if (resp$status_code %in% c(200, 201, 204)) {
  cat("Done.\n")
} else {
  cat(sprintf("ERROR %d:\n%s\n", resp$status_code, resp_body_string(resp)))
}
