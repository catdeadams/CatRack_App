# ============================================================
# seed_pullup_subs.R — CaTrack
# Updates the substitution_1 / substitution_2 fields for every
# pull-up and chin-up variant so the in-app "Swap Exercise" button
# always offers an EASIER progression (band-assisted, eccentric,
# lat pulldown) rather than another full-strict bodyweight variant.
#
# Safe to re-run — uses PATCH on the unique exercise name.
# ============================================================

library(httr2)
library(jsonlite)

SUPABASE_URL         <- Sys.getenv("SUPABASE_URL",
                                   "https://fowpjdsixqhgaqgdeiph.supabase.co")
SUPABASE_SERVICE_KEY <- Sys.getenv("SUPABASE_SERVICE_KEY", "")

if (nchar(SUPABASE_SERVICE_KEY) == 0)
  stop("SUPABASE_SERVICE_KEY not set. Add it to .Renviron and restart R.")

sb_patch_name <- function(name, fields) {
  esc <- URLencode(name, reserved = TRUE)
  request(paste0(SUPABASE_URL, "/rest/v1/exercises?name=eq.", esc)) |>
    req_headers(
      "apikey"        = SUPABASE_SERVICE_KEY,
      "Authorization" = paste("Bearer", SUPABASE_SERVICE_KEY),
      "Content-Type"  = "application/json",
      "Prefer"        = "return=representation"
    ) |>
    req_body_raw(toJSON(fields, auto_unbox = TRUE, na = "null")) |>
    req_method("PATCH") |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
}

# Order of preference for the EASIER swap:
#   1) Band Assisted Pull-up   (load reduction via band)
#   2) Eccentric Pull-up       (slow lower; builds strict strength)
#   3) Lat Pulldown            (machine; selectable load)
updates <- list(
  "Pull-up" = list(
    substitution_1 = "Band Assisted Pull-up",
    substitution_2 = "Eccentric Pull-up"
  ),
  "Chin-up" = list(
    substitution_1 = "Band Assisted Pull-up",
    substitution_2 = "Eccentric Pull-up"
  ),
  # Keep machine variants pointing to the bodyweight progression as
  # the "harder" swap path — the scorer will only pick those when
  # the user actually has enough strict reps.
  "Lat Pulldown" = list(
    substitution_1 = "Neutral Grip Lat Pulldown",
    substitution_2 = "Band Assisted Pull-up"
  ),
  "Neutral Grip Lat Pulldown" = list(
    substitution_1 = "Lat Pulldown",
    substitution_2 = "Band Assisted Pull-up"
  )
)

cat(sprintf("Updating %d pull-up/chin-up substitutions...\n", length(updates)))
for (nm in names(updates)) {
  resp <- sb_patch_name(nm, updates[[nm]])
  if (resp$status_code %in% c(200, 201, 204)) {
    cat(sprintf("  %s ✓\n", nm))
  } else {
    cat(sprintf("  %s ✗ [%d] %s\n", nm,
                resp$status_code, resp_body_string(resp)))
  }
}
cat("Done.\n")
