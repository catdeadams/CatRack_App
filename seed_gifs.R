# ============================================================
# seed_gifs.R — CaTrack
# Pre-populate gif_url for all exercises using ExerciseDB.
#
# USAGE:
#   Step 1: Run with DRY_RUN <- TRUE  (default)
#           Review the printed table — look for UNMATCHED rows
#           Adjust MANUAL_CROSSWALK entries if needed
#   Step 2: Set DRY_RUN <- FALSE and re-run to write to Supabase
#
# API calls used: ~11 (bodyPartList + 1 per body part)
# ============================================================

library(httr2)
library(jsonlite)

# ── CONFIG ───────────────────────────────────────────────────
DRY_RUN <- TRUE   # TRUE = preview only; FALSE = write to Supabase

SUPABASE_URL         <- "https://fowpjdsixqhgaqgdeiph.supabase.co"
SUPABASE_SERVICE_KEY <- Sys.getenv("SUPABASE_SERVICE_KEY",
  "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6ImZvd3BqZHNpeHFoZ2FxZ2RlaXBoIiwicm9sZSI6InNlcnZpY2Vfcm9sZSIsImlhdCI6MTc3NzUwNDY2MSwiZXhwIjoyMDkzMDgwNjYxfQ.c4P0LohsMcZEV9V98kNX12ohVv13At1cIdLBft_UT9I")
EXERCISEDB_API_KEY   <- Sys.getenv("EXERCISEDB_API_KEY", "")

# ── MANUAL CROSSWALK ─────────────────────────────────────────
# Maps OUR exercise names -> ExerciseDB exercise names.
# Auto-matching handles most cases; add entries here for anything
# that shows as UNMATCHED after a dry run.
MANUAL_CROSSWALK <- c(
  "Barbell Back Squat"            = "barbell squat",
  "DB Goblet Squat"               = "dumbbell goblet squat",
  "DB Reverse Lunge"              = "dumbbell walking lunge",
  "Barbell Reverse Lunge"         = "barbell walking lunge",
  "Single Leg Squat to Box"       = "pistol squat",
  "Barbell RDL"                   = "barbell romanian deadlift",
  "Straight Leg Deadlift"         = "barbell straight leg deadlift",
  "DB RDL"                        = "dumbbell romanian deadlift",
  "45 Degree Hip Extension"       = "hyperextension",
  "Barbell Hip Thrust"            = "barbell hip thrust",
  "Hip Thrust Machine"            = "barbell hip thrust",
  "Glute Bridge"                  = "glute bridge",
  "Hip Abduction Machine"         = "hip abduction",
  "Clamshell"                     = "hip abduction",
  "Nordic Ham Curl"               = "nordic hamstring curl",
  "Calves on Machine"             = "standing calf raises",
  "Calves on Leg Press"           = "leg press calf raise",
  "DB Chest Press"                = "dumbbell bench press",
  "Machine Chest Press"           = "chest press",
  "DB Incline Press"              = "dumbbell incline bench press",
  "Machine Incline Press"         = "incline chest press",
  "Pec Dec Flies"                 = "pec deck fly",
  "Cable Chest Flies"             = "cable fly",
  "DB Fly"                        = "dumbbell fly",
  "Single Arm Cable Row"          = "cable seated row",
  "Chest Supported DB Row"        = "dumbbell bent over row",
  "Helms Row"                     = "dumbbell bent over row",
  "Machine Row"                   = "seated cable row",
  "Neutral Grip Lat Pulldown"     = "lat pulldown",
  "Standing DB Arnold Press"      = "arnold press",
  "Seated Barbell Overhead Press" = "barbell shoulder press",
  "Machine Shoulder Press"        = "shoulder press",
  "DB Lateral Raise"              = "dumbbell lateral raise",
  "Cable Lateral Raise"           = "cable lateral raise",
  "Machine Lateral Raise"         = "dumbbell lateral raise",
  "EZ Bar Skull Crusher"          = "ez barbell triceps extension",
  "Cable Tricep Pushdown"         = "cable pushdown",
  "Cable Rope Pushdown"           = "cable rope pushdown",
  "SA Overhead Tricep Extension"  = "dumbbell one arm overhead triceps extension",
  "EZ Bar Curl"                   = "ez barbell curl",
  "DB Curl"                       = "dumbbell bicep curl",
  "Barbell Curl"                  = "barbell curl",
  "Cable Curl"                    = "cable curl",
  "Face Away Cable Curl"          = "cable bicep curl",
  "Preacher Curl"                 = "ez barbell preacher curl",
  "DB Hammer Curl"                = "dumbbell hammer curl",
  "Reverse Pec Dec"               = "cable reverse fly",
  "Cable Face Pull"               = "cable pull through",
  "Cable Crunch"                  = "cable crunch",
  "Hanging Leg Raise"             = "hanging leg raise",
  "Wood Chop"                     = "cable wood chop",
  "Ab Machine"                    = "cable crunch",
  "Sled Push"                     = "sled push",
  "Sled Pull"                     = "sled pull"
)

# ── SUPABASE HELPERS ─────────────────────────────────────────
sb_get_all <- function(table, params = "") {
  resp <- request(paste0(SUPABASE_URL, "/rest/v1/", table, params)) |>
    req_headers(
      "apikey"        = SUPABASE_SERVICE_KEY,
      "Authorization" = paste("Bearer", SUPABASE_SERVICE_KEY),
      "Accept"        = "application/json"
    ) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  if (resp$status_code != 200) {
    stop("Supabase error: ", resp_body_string(resp))
  }
  fromJSON(resp_body_string(resp), simplifyDataFrame = TRUE)
}

sb_update_gif <- function(exercise_id, gif_url) {
  request(paste0(SUPABASE_URL, "/rest/v1/exercises?id=eq.", exercise_id)) |>
    req_headers(
      "apikey"        = SUPABASE_SERVICE_KEY,
      "Authorization" = paste("Bearer", SUPABASE_SERVICE_KEY),
      "Content-Type"  = "application/json",
      "Prefer"        = "return=minimal"
    ) |>
    req_body_raw(toJSON(list(gif_url = gif_url), auto_unbox = TRUE)) |>
    req_method("PATCH") |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
}

# ── FETCH ALL EXERCISEDB EXERCISES ───────────────────────────
fetch_all_edb <- function() {
  oss_base   <- "https://oss.exercisedb.dev"
  rapid_base <- "https://exercisedb.p.rapidapi.com"

  rapid_headers <- if (nchar(EXERCISEDB_API_KEY) > 0)
    list("X-RapidAPI-Key" = EXERCISEDB_API_KEY,
         "X-RapidAPI-Host" = "exercisedb.p.rapidapi.com")
  else list()

  make_oss  <- function(url) request(url) |> req_error(is_error = \(r) FALSE)
  make_rapid <- function(url) {
    req <- request(url) |> req_error(is_error = \(r) FALSE)
    if (length(rapid_headers) > 0) req <- do.call(req_headers, c(list(req), rapid_headers))
    req
  }

  parse_ex <- function(resp) {
    if (is.null(resp) || resp$status_code != 200) return(NULL)
    rows <- tryCatch(fromJSON(resp_body_string(resp), simplifyDataFrame = TRUE), error = \(e) NULL)
    if (is.data.frame(rows) && nrow(rows) > 0 && "gifUrl" %in% names(rows)) {
      keep <- intersect(c("id", "name", "gifUrl", "bodyPart", "equipment", "target"), names(rows))
      rows[, keep]
    } else NULL
  }

  fetch_by_bodypart <- function(base, make_req_fn) {
    bp_resp <- tryCatch(req_perform(make_req_fn(paste0(base, "/exercises/bodyPartList"))),
                        error = \(e) NULL)
    if (is.null(bp_resp) || bp_resp$status_code != 200) return(NULL)
    body_parts <- tryCatch(fromJSON(resp_body_string(bp_resp)), error = \(e) NULL)
    if (is.null(body_parts) || length(body_parts) == 0) return(NULL)
    cat(sprintf("Body parts (%d): %s\n\n", length(body_parts), paste(body_parts, collapse = ", ")))
    all_rows <- list()
    for (bp in body_parts) {
      cat(sprintf("  Fetching %-15s ... ", bp))
      Sys.sleep(0.4)
      url  <- sprintf("%s/exercises/bodyPart/%s?limit=500", base,
                      utils::URLencode(bp, reserved = TRUE))
      rows <- parse_ex(tryCatch(req_perform(make_req_fn(url)), error = \(e) NULL))
      if (!is.null(rows)) { all_rows <- c(all_rows, list(rows)); cat(sprintf("%d exercises\n", nrow(rows))) }
      else cat("(no usable data)\n")
    }
    if (length(all_rows) == 0) NULL else unique(do.call(rbind, all_rows))
  }

  # Strategy 1: OSS bulk fetch (fastest, no rate limit)
  cat("Trying OSS bulk fetch...\n")
  resp <- tryCatch(req_perform(make_oss(paste0(oss_base, "/exercises?limit=2000"))), error = \(e) NULL)
  edb  <- parse_ex(resp)
  if (!is.null(edb) && nrow(edb) > 0) {
    cat(sprintf("OSS bulk: %d exercises loaded\n\n", nrow(edb)))
    return(unique(edb))
  }
  cat(sprintf("OSS bulk failed (HTTP %s) — trying OSS by body part...\n",
              if (!is.null(resp)) resp$status_code else "ERROR"))

  # Strategy 2: OSS by body part
  edb <- fetch_by_bodypart(oss_base, make_oss)
  if (!is.null(edb) && nrow(edb) > 0) {
    cat(sprintf("\nOSS by body part: %d exercises loaded\n\n", nrow(edb)))
    return(edb)
  }
  cat("OSS by body part failed — trying RapidAPI...\n")

  # Strategy 3: RapidAPI (requires key)
  if (nchar(EXERCISEDB_API_KEY) == 0)
    stop("All OSS methods failed and EXERCISEDB_API_KEY is not set in .Renviron")

  edb <- fetch_by_bodypart(rapid_base, make_rapid)
  if (!is.null(edb) && nrow(edb) > 0) {
    cat(sprintf("\nRapidAPI: %d exercises loaded\n\n", nrow(edb)))
    return(edb)
  }
  stop("RapidAPI returned no usable data. Check your EXERCISEDB_API_KEY.")
}

# ── MATCHING HELPERS ─────────────────────────────────────────
norm <- function(x) tolower(trimws(gsub("[^a-z0-9 ]", " ", tolower(x))))

# Remove common equipment prefixes for fuzzy fallback
strip_prefix <- function(x) {
  trimws(sub(
    "^(barbell|dumbbell|db|cable|machine|ez.?bar|kettlebell|smith|trap bar|single.?arm|sa )\\s+",
    "", norm(x), ignore.case = TRUE))
}

# Return best matching ExerciseDB row for one of our exercise names
find_match <- function(our_name, edb) {
  if (is.null(edb) || nrow(edb) == 0)
    return(list(row = NULL, method = "UNMATCHED"))

  # 1. Manual crosswalk
  manual <- MANUAL_CROSSWALK[our_name]
  if (!is.na(manual) && nchar(manual) > 0) {
    idx <- which(norm(edb$name) == norm(manual))
    if (length(idx) > 0)
      return(list(row = edb[idx[1], ], method = "manual"))
    # Manual entry specified but not found in EDB — flag it
    return(list(row = NULL, method = paste0("manual_not_found:", manual)))
  }

  on  <- norm(our_name)
  en  <- norm(edb$name)

  # 2. Exact name match
  idx <- which(en == on)
  if (length(idx) > 0) return(list(row = edb[idx[1], ], method = "exact"))

  # 3. Exact match after stripping equipment prefix from ours
  sp  <- strip_prefix(our_name)
  idx <- which(en == sp)
  if (length(idx) > 0) return(list(row = edb[idx[1], ], method = "strip_prefix"))

  # 4. EDB name is a substring of ours (or vice versa)
  idx <- which(sapply(en, function(e) grepl(e, on, fixed = TRUE) ||
                                      grepl(on, e, fixed = TRUE)))
  if (length(idx) > 0) {
    # Pick shortest EDB name (most specific)
    idx <- idx[which.min(nchar(edb$name[idx]))]
    return(list(row = edb[idx[1], ], method = "substring"))
  }

  # 5. Fuzzy (Levenshtein), normalized similarity
  dists <- adist(on, en)[1, ]
  sims  <- 1 - dists / pmax(nchar(on), nchar(en), 1)
  best  <- which.max(sims)
  if (sims[best] >= 0.55)
    return(list(row = edb[best, ], method = sprintf("fuzzy(%.0f%%)", sims[best]*100)))

  list(row = NULL, method = "UNMATCHED")
}

# ── LOAD OUR EXERCISES FROM SUPABASE ─────────────────────────
cat("Loading exercises from Supabase...\n")
our_exs <- sb_get_all("exercises", "?select=id,name,gif_url&order=name")
cat(sprintf("Our exercises: %d\n\n", nrow(our_exs)))

# ── FETCH EXERCISEDB ─────────────────────────────────────────
edb <- fetch_all_edb()

# ── MATCH ────────────────────────────────────────────────────
cat(sprintf("%-40s %-10s %-40s %s\n",
            "OUR EXERCISE", "METHOD", "EDB MATCH", "GIF URL"))
cat(strrep("-", 130), "\n")

results <- lapply(seq_len(nrow(our_exs)), function(i) {
  ex   <- our_exs[i, ]
  name <- ex$name

  # Skip if already has a gif_url in the DB
  if (!is.null(ex$gif_url) && !is.na(ex$gif_url) && nchar(as.character(ex$gif_url)) > 5) {
    cat(sprintf("%-40s %-10s %s\n",
                substr(name, 1, 39), "SKIP", "(already set)"))
    return(list(id = ex$id, name = name, gif_url = ex$gif_url,
                method = "already_set", matched = TRUE))
  }

  m <- find_match(name, edb)

  if (!is.null(m$row)) {
    gif <- m$row$gifUrl
    cat(sprintf("%-40s %-10s %-40s %s\n",
                substr(name, 1, 39), m$method,
                substr(m$row$name, 1, 39),
                substr(gif, 1, 50)))
    list(id = ex$id, name = name, gif_url = gif,
         edb_name = m$row$name, method = m$method, matched = TRUE)
  } else {
    cat(sprintf("%-40s %-10s <<< NO MATCH >>>\n",
                substr(name, 1, 39), m$method))
    list(id = ex$id, name = name, gif_url = NULL,
         edb_name = NA, method = m$method, matched = FALSE)
  }
})

# ── SUMMARY ──────────────────────────────────────────────────
n_matched   <- sum(sapply(results, \(r) isTRUE(r$matched)))
n_unmatched <- sum(sapply(results, \(r) !isTRUE(r$matched)))

cat("\n", strrep("=", 60), "\n")
cat(sprintf("Matched:   %d / %d\n", n_matched, nrow(our_exs)))
cat(sprintf("Unmatched: %d\n", n_unmatched))

unmatched <- Filter(\(r) !isTRUE(r$matched), results)
if (length(unmatched) > 0) {
  cat("\nUNMATCHED — add to MANUAL_CROSSWALK:\n")
  for (r in unmatched) cat(sprintf('  %-40s -> ???\n', paste0('"', r$name, '"')))
}

# ── WRITE TO SUPABASE ────────────────────────────────────────
if (!DRY_RUN) {
  to_write <- Filter(\(r) isTRUE(r$matched) && r$method != "already_set" &&
                       !is.null(r$gif_url) && nchar(r$gif_url) > 5, results)
  cat(sprintf("\nWriting %d gif_url values to Supabase...\n", length(to_write)))

  ok <- 0; fail <- 0
  for (r in to_write) {
    resp <- sb_update_gif(r$id, r$gif_url)
    if (resp$status_code %in% c(200, 201, 204)) {
      ok <- ok + 1
    } else {
      cat(sprintf("  FAIL %-35s HTTP %s\n", r$name, resp$status_code))
      fail <- fail + 1
    }
    Sys.sleep(0.05)
  }
  cat(sprintf("Done. %d written, %d failed.\n", ok, fail))
} else {
  cat(sprintf(
    "\nDRY RUN — no changes written.\nSet DRY_RUN <- FALSE and re-run to update Supabase.\n"))
}
