# ============================================================
# seed_running_exercises.R
# One-time seed for the running_support redesign: adds plyometric
# and isometric-hold exercises the new full-body running program
# needs. Safe to re-run — it skips any exercise whose name already
# exists (case-insensitive), so it never creates duplicates.
#
# HOW TO RUN (once):
#   1. Open this file in RStudio with SUPABASE_URL and
#      SUPABASE_SERVICE_KEY set in your .Renviron (same ones the app
#      uses). The service key is required to write to the table.
#   2. Source the whole file:  source("seed_running_exercises.R")
#   3. Check the printed HTTP 201s. Then regenerate a running_support
#      program to see the new exercises appear.
#
# This is a dev/admin script — the app never sources it (global.R
# only sources its fixed screen files), so it has no runtime effect.
# ============================================================

library(httr2)
library(jsonlite)

SUPABASE_URL <- Sys.getenv("SUPABASE_URL")
KEY          <- Sys.getenv("SUPABASE_SERVICE_KEY")
stopifnot("SUPABASE_URL not set"         = nchar(SUPABASE_URL) > 0,
          "SUPABASE_SERVICE_KEY not set" = nchar(KEY) > 0)

sb_get <- function(path) {
  resp <- request(paste0(SUPABASE_URL, "/rest/v1/", path)) |>
    req_headers(apikey = KEY, Authorization = paste("Bearer", KEY)) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  if (resp$status_code != 200) return(NULL)
  fromJSON(resp_body_string(resp), simplifyDataFrame = TRUE)
}

sb_insert_one <- function(row) {
  request(paste0(SUPABASE_URL, "/rest/v1/exercises")) |>
    req_headers(apikey = KEY, Authorization = paste("Bearer", KEY),
                "Content-Type" = "application/json",
                Prefer = "return=representation") |>
    req_body_raw(toJSON(row, auto_unbox = TRUE, na = "null")) |>
    req_method("POST") |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
}

# Helper: build one exercise row. Arrays are wrapped in I() so a single
# value still serializes as a JSON array (e.g. ["calves"], not "calves").
ex <- function(name, category, movement_pattern, primary,
               secondary = character(0), equipment = "bodyweight",
               is_compound = FALSE, rl = 8L, rh = 12L) {
  list(
    name                  = name,
    category              = category,
    movement_pattern      = movement_pattern,
    primary_muscles       = I(primary),
    secondary_muscles     = I(secondary),
    equipment_required    = I(equipment),
    is_compound           = is_compound,
    default_rep_range_low  = as.integer(rl),
    default_rep_range_high = as.integer(rh)
  )
}

new_ex <- list(
  # ── Plyometrics (category "plyometric", movement_pattern "plyometric") ──
  # Bodyweight, low reps, maximal intent. Improve running economy at
  # slower speeds and complement heavy lifting ("complex" training).
  ex("Squat Jump",          "plyometric", "plyometric", c("quads","glutes"),      c("calves"),     is_compound = TRUE, rl = 5L,  rh = 8L),
  ex("Box Jump",            "plyometric", "plyometric", c("quads","glutes"),      c("calves"),     is_compound = TRUE, rl = 5L,  rh = 8L),
  ex("Broad Jump",          "plyometric", "plyometric", c("glutes","quads"),      c("hamstrings"), is_compound = TRUE, rl = 4L,  rh = 6L),
  ex("Tuck Jump",           "plyometric", "plyometric", c("quads","glutes"),      c("calves"),     is_compound = TRUE, rl = 6L,  rh = 10L),
  ex("Pogo Hops",           "plyometric", "plyometric", c("calves"),              character(0),    is_compound = FALSE, rl = 10L, rh = 20L),
  ex("Bounding",            "plyometric", "plyometric", c("glutes","hamstrings"), c("calves"),     is_compound = TRUE, rl = 6L,  rh = 12L),
  ex("Lateral Skater Jump", "plyometric", "plyometric", c("glutes"),              c("quads"),      is_compound = TRUE, rl = 6L,  rh = 10L),
  ex("Depth Jump",          "plyometric", "plyometric", c("quads","glutes"),      c("calves"),     is_compound = TRUE, rl = 4L,  rh = 6L),

  # ── Isometric holds (category "isometric") — REP FIELD = SECONDS ──
  # Heavy sustained holds for tendon stiffness (Achilles/soleus) and
  # trunk/quad durability. Bodyweight; add external load once you can
  # hold past the range.
  ex("Wall Sit",                   "isometric", "knee_extension", c("quads"),          c("glutes"), rl = 20L, rh = 45L),
  ex("Single-Leg Wall Sit",        "isometric", "knee_extension", c("quads"),          c("glutes"), rl = 15L, rh = 30L),
  ex("Split Squat Iso Hold",       "isometric", "lunge",          c("quads","glutes"), c("hamstrings"), rl = 20L, rh = 45L),
  ex("Single-Leg Calf Raise Hold", "isometric", "plantarflexion", c("calves"),         character(0), rl = 20L, rh = 45L),
  ex("Standing Calf Raise Hold",   "isometric", "plantarflexion", c("calves"),         character(0), rl = 20L, rh = 45L),
  ex("Seated Calf Raise Hold",     "isometric", "plantarflexion", c("calves"),         character(0), rl = 30L, rh = 60L)
)

# Skip anything already in the table (case-insensitive name match)
existing <- sb_get("exercises?select=name")
have <- if (!is.null(existing) && nrow(existing) > 0)
  tolower(trimws(existing$name)) else character(0)
to_add <- Filter(function(e) !(tolower(e$name) %in% have), new_ex)

cat(sprintf("Exercise library: %d existing | %d new to add\n",
            length(have), length(to_add)))

if (length(to_add) == 0) {
  cat("Nothing to add — all seed exercises already present.\n")
} else {
  for (e in to_add) {
    r <- sb_insert_one(e)
    cat(sprintf("  %-28s -> HTTP %d\n", e$name, r$status_code))
    if (!r$status_code %in% c(200, 201))
      cat("     RESPONSE: ", resp_body_string(r), "\n")
  }
}
cat("Done.\n")
