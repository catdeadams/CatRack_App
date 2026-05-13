# ============================================================
# workout_screen.R — CaTrack
# Active session screen: per-set logging, rest timer,
# last-session pre-fill, RPE, notes, exercise swap.
# Source this from global.R
# ============================================================

# ── TIMER JS ─────────────────────────────────────────────────
rest_timer_js <- "
var catrackTimer      = null;
var catrackSeconds    = 0;
var catrackTotal      = 0;
var catrackLastTotal  = 0;

function showTimerSection() {
  var el = document.getElementById('rest-timer-section');
  if (el) el.style.display = 'flex';
}
function hideTimerSection() {
  var el = document.getElementById('rest-timer-section');
  if (el) el.style.display = 'none';
}
function startRestTimer(seconds) {
  clearInterval(catrackTimer);
  catrackLastTotal = seconds;
  catrackSeconds   = seconds;
  catrackTotal     = seconds;
  showTimerSection();
  updateTimerDisplay();
  catrackTimer = setInterval(function() {
    catrackSeconds--;
    updateTimerDisplay();
    if (catrackSeconds <= 0) {
      clearInterval(catrackTimer);
      var bar  = document.getElementById('rest-timer-bar');
      var disp = document.getElementById('rest-timer-display');
      if (bar)  bar.style.width = '0%';
      if (disp) { disp.innerText = 'Rest done!'; disp.style.color = '#1D9E75'; }
      if (navigator.vibrate) navigator.vibrate([300, 100, 300]);
      setTimeout(hideTimerSection, 3000);
    }
  }, 1000);
}
function restartTimer() {
  if (catrackLastTotal > 0) startRestTimer(catrackLastTotal);
}
function updateTimerDisplay() {
  var m   = Math.floor(catrackSeconds / 60);
  var s   = catrackSeconds % 60;
  var pct = catrackTotal > 0 ? Math.round(100 * catrackSeconds / catrackTotal) : 100;
  var disp = document.getElementById('rest-timer-display');
  var bar  = document.getElementById('rest-timer-bar');
  if (disp) { disp.innerText = m + ':' + (s < 10 ? '0' : '') + s; disp.style.color = '#f0f0f0'; }
  if (bar)  bar.style.width = pct + '%';
}
function stopTimer() {
  clearInterval(catrackTimer);
  hideTimerSection();
}
"

# ── SESSION ELAPSED TIMER JS ─────────────────────────────────
# Uses window globals so the timer survives Shiny re-renders.
session_timer_js <- "
(function() {
  if (!window.catrackWsStart) window.catrackWsStart = Date.now();
  if (window.catrackElapsedInterval) clearInterval(window.catrackElapsedInterval);

  function updateElapsed() {
    var el = document.getElementById('session-elapsed');
    if (!el) return;
    var elapsed = Math.floor((Date.now() - window.catrackWsStart) / 1000);
    var m = Math.floor(elapsed / 60);
    var s = elapsed % 60;
    el.innerText = m + ':' + (s < 10 ? '0' : '') + s;
  }

  window.catrackElapsedInterval = setInterval(updateElapsed, 1000);
  updateElapsed();
})();
"

# ── FETCH WORKOUT DATA ───────────────────────────────────────
fetch_workout_data <- function(workout_id, user_id, token) {
  workout <- sb_select("workouts",
                       sprintf("?id=eq.%s&user_id=eq.%s", workout_id, user_id),
                       token = token)
  if (is.null(workout)) return(NULL)
  we <- sb_select("workout_exercises",
                  sprintf("?workout_id=eq.%s&select=*,exercises(*)&order=exercise_order",
                          workout_id),
                  token = token)
  if (is.null(we)) return(list(workout = workout, exercises = NULL, last_logs = NULL))
  ex_ids  <- paste(we$exercise_id, collapse = ",")
  last_logs <- sb_select("workout_set_logs",
                         sprintf("?user_id=eq.%s&workout_exercise_id=in.(%s)&select=*,workout_exercises(exercise_id)&order=logged_at.desc",
                                 user_id, paste(we$id, collapse = ",")),
                         token = token)
  hist_logs <- list()
  for (eid in we$exercise_id) {
    h <- sb_select("workout_set_logs",
                   sprintf(paste0("?user_id=eq.%s",
                                  "&workout_exercise_id=in.(%s)",
                                  "&is_warmup=eq.false",
                                  "&select=weight_lbs,reps_completed,rpe_actual,notes,logged_at",
                                  "&order=logged_at.desc&limit=5"),
                           user_id,
                           sprintf("(select id from workout_exercises where exercise_id=eq.%s)", eid)),
                   token = token)
    if (!is.null(h)) hist_logs[[eid]] <- h
  }
  list(workout = workout, exercises = we, last_logs = last_logs, hist_logs = hist_logs)
}

# Simpler last-performance fetch that actually works with Supabase REST
fetch_last_performance <- function(exercise_id, user_id, token) {
  sb_select("last_exercise_log",
            sprintf("?user_id=eq.%s&exercise_id=eq.%s", user_id, exercise_id),
            token = token)
}

# ── EXERCISE HISTORY ─────────────────────────────────────────
fetch_exercise_history <- function(exercise_id, user_id, token, n_sessions = 5) {
  we_ids <- tryCatch(
    sb_select("workout_exercises",
              sprintf("?exercise_id=eq.%s&select=id", exercise_id),
              token = token),
    error = \(e) NULL)
  if (is.null(we_ids) || nrow(we_ids) == 0) return(NULL)
  id_list <- paste0("(", paste(we_ids$id, collapse = ","), ")")
  logs <- tryCatch(
    sb_select("workout_set_logs",
              sprintf(paste0("?user_id=eq.%s&is_warmup=eq.false",
                             "&workout_exercise_id=in.%s",
                             "&select=weight_lbs,reps_completed,rpe_actual,logged_at",
                             "&order=logged_at.desc&limit=60"),
                      user_id, id_list),
              token = token),
    error = \(e) NULL)
  if (is.null(logs) || nrow(logs) == 0) return(NULL)
  logs$date  <- as.Date(as.POSIXct(logs$logged_at, tz = "UTC"))
  logs$wt    <- as.numeric(logs$weight_lbs)
  logs$reps  <- as.integer(logs$reps_completed)
  logs$rpe   <- as.numeric(logs$rpe_actual)
  dates <- unique(logs$date[order(logs$date, decreasing = TRUE)])[seq_len(n_sessions)]
  do.call(rbind, lapply(dates, function(d) {
    day  <- logs[logs$date == d, ]
    best <- day[which.max(replace(day$wt, is.na(day$wt), -Inf)), ]
    data.frame(date   = d, wt = best$wt, reps = best$reps,
               rpe    = best$rpe, n_sets = nrow(day), stringsAsFactors = FALSE)
  }))
}

# ── EXERCISEDB GIF HELPERS ───────────────────────────────────
fetch_exercise_gif <- function(exercise_name, api_key) {
  if (is.null(api_key) || nchar(api_key) == 0)
    return(list(url = NULL, error = "no_key"))

  try_query <- function(q) {
    encoded <- utils::URLencode(tolower(trimws(q)), reserved = TRUE)
    url <- paste0("https://exercisedb.p.rapidapi.com/exercises/name/", encoded,
                  "?limit=10&offset=0")
    resp <- tryCatch(
      request(url) |>
        req_headers("X-RapidAPI-Key"  = api_key,
                    "X-RapidAPI-Host" = "exercisedb.p.rapidapi.com") |>
        req_error(is_error = \(r) FALSE) |>
        req_perform(),
      error = \(e) NULL)
    if (is.null(resp))  return(list(url = NULL, error = "request_failed"))
    if (resp$status_code != 200) {
      body <- tryCatch(substr(resp_body_string(resp), 1, 200), error = \(e) "")
      return(list(url = NULL, error = paste0("http_", resp$status_code, ": ", body)))
    }
    body_str <- tryCatch(resp_body_string(resp), error = \(e) "")
    data <- tryCatch(fromJSON(body_str, simplifyDataFrame = TRUE), error = \(e) NULL)
    if (is.data.frame(data) && nrow(data) > 0) {
      gif <- tryCatch(data$gifUrl[1], error = \(e) NULL)
      if (!is.null(gif) && !is.na(gif) && nchar(gif) > 0)
        return(list(url = gif, error = NULL))
    }
    if (is.list(data) && !is.null(data$exercises) &&
        is.data.frame(data$exercises) && nrow(data$exercises) > 0) {
      gif <- tryCatch(data$exercises$gifUrl[1], error = \(e) NULL)
      if (!is.null(gif) && !is.na(gif) && nchar(gif) > 0)
        return(list(url = gif, error = NULL))
    }
    list(url = NULL, error = paste0("no_match(", substr(body_str, 1, 80), ")"))
  }

  # Only 2 attempts max to conserve the free-tier daily quota (50 req/day):
  # full name first, then fall back to last word only.
  words   <- strsplit(trimws(exercise_name), "\\s+")[[1]]
  queries <- unique(c(exercise_name, tail(words, 1)))

  last_err <- "no_match"
  for (q in queries) {
    res <- try_query(q)
    message(sprintf("[GIF] query='%s' -> %s", q, if (!is.null(res$url)) "FOUND" else res$error))
    if (!is.null(res$url)) return(list(url = res$url, error = NULL))
    last_err <- res$error %||% last_err
  }
  list(url = NULL, error = last_err)
}

cache_exercise_gif <- function(exercise_id, gif_url, svc_token) {
  tryCatch(
    sb_update("exercises",
              sprintf("?id=eq.%s", exercise_id),
              list(gif_url = gif_url),
              token = svc_token),
    error = \(e) NULL)
}

# ── EXERCISE GROUP BUILDER ───────────────────────────────────
# Groups exercises by superset_group (adjacent rows with the same non-null
# superset_group share a block; each exercise with no superset_group is solo).
build_exercise_groups <- function(exercises) {
  group_order <- character(0)
  group_map   <- list()
  for (i in seq_len(nrow(exercises))) {
    sg <- tryCatch(trimws(as.character(exercises$superset_group[i])), error = \(e) "")
    if (nchar(sg) == 0 || sg == "NA") {
      key <- paste0("solo_", i)
    } else {
      key <- paste0("ss_", sg)
    }
    if (!(key %in% group_order)) group_order <- c(group_order, key)
    group_map[[key]] <- c(group_map[[key]], i)
  }
  list(order = group_order, map = group_map)
}

# ── WORKOUT SCREEN UI ────────────────────────────────────────
workout_screen_ui <- function(workout, exercises, last_perf_map,
                              set_logs_rv, timer_active,
                              gif_map = list(), history_map = list()) {

  if (is.null(workout) || is.null(exercises)) {
    return(div(style = "padding:40px; text-align:center; color:#555;",
               "Loading session..."))
  }

  wo        <- if (is.data.frame(workout)) workout[1, ] else workout
  is_review <- isTRUE(
    !is.null(wo$completed_at) && !is.na(wo$completed_at) &&
      nchar(as.character(wo$completed_at)) > 5
  )
  n_ex      <- nrow(exercises)
  completed <- sum(sapply(seq_len(n_ex), function(i) {
    we_id <- exercises$id[i]
    logs  <- set_logs_rv[[we_id]]
    !is.null(logs) && length(logs) >= exercises$prescribed_sets[i]
  }))
  pct <- if (n_ex > 0) round(100 * completed / n_ex) else 0L

  # Build exercise groups for block-style display
  grp         <- build_exercise_groups(exercises)
  group_order <- grp$order
  group_map   <- grp$map

  # Block accent colors (cycle through these)
  BLOCK_COLORS <- c("#1D9E75", "#2979FF", "#9C27B0", "#FF7043", "#FF9800", "#00ACC1")

  tagList(
    tags$head(
      tags$script(HTML(rest_timer_js)),
      tags$script(HTML(session_timer_js))
    ),

    # ── Top nav row ───────────────────────────────────────────
    div(style = paste0(
          "display:flex; align-items:center; gap:10px;",
          "margin-bottom:14px; padding:0 2px;"),
        tags$button("←",
                    style = paste0("background:#1e1e1e; border:none; border-radius:10px;",
                                   "width:38px; height:38px; font-size:17px; color:#aaa;",
                                   "cursor:pointer; flex-shrink:0; display:flex;",
                                   "align-items:center; justify-content:center;"),
                    onclick = "Shiny.setInputValue('close_workout', Math.random(), {priority:'event'})"),
        div(style = "flex:1; text-align:center;",
            div(style = "font-size:10px; color:#444; text-transform:uppercase; letter-spacing:0.07em;",
                paste0("Week ", wo$week_number, " · Day ", wo$session_number)),
            div(style = "font-size:15px; font-weight:700; color:#f0f0f0; line-height:1.2;",
                wo$session_label)
        ),
        div(style = "width:38px; text-align:right;") # spacer to center title
    ),

    # ── Session elapsed timer ─────────────────────────────────
    if (!is_review)
      div(style = "text-align:center; margin-bottom:12px;",
          div(id    = "session-elapsed",
              style = paste0("font-size:44px; font-weight:700; color:#f0f0f0;",
                             "letter-spacing:-1px; font-variant-numeric:tabular-nums;",
                             "line-height:1;"),
              "0:00"),
          div(style = "font-size:10px; color:#333; text-transform:uppercase;",
              "elapsed")
      ),

    # ── Segmented progress bar ────────────────────────────────
    div(style = "margin-bottom:16px;",
        div(style = "display:flex; justify-content:space-between; margin-bottom:5px;",
            div(style = "font-size:11px; color:#444;",
                paste0(completed, " / ", n_ex, " done")),
            div(style = "font-size:11px; font-weight:700; color:#1D9E75;",
                paste0(pct, "%"))
        ),
        div(style = "display:flex; gap:2px; height:4px;",
            lapply(seq_len(n_ex), function(i) {
              we_id <- exercises$id[i]
              logs  <- set_logs_rv[[we_id]]
              done  <- !is.null(logs) && length(logs) >= exercises$prescribed_sets[i]
              div(style = paste0("flex:1; border-radius:2px; background:",
                                 if (done) "#1D9E75" else "#222", ";"))
            })
        )
    ),

    # ── Review mode banner ────────────────────────────────────
    if (is_review)
      div(style = paste0("background:#061a12; border:1px solid #0F6E56; border-radius:10px;",
                         "padding:10px 14px; margin-bottom:12px;",
                         "display:flex; align-items:center; gap:10px;"),
          div(style = "color:#4ade80; font-size:17px;", "✓"),
          div(
            div(style = "font-size:13px; font-weight:600; color:#4ade80;",
                "Session Complete — Review Mode"),
            div(style = "font-size:11px; color:#555; margin-top:2px;",
                tryCatch(
                  paste0("Completed ",
                         format(as.POSIXct(wo$completed_at), "%b %d at %I:%M %p")),
                  error = \(e) "Previously completed"
                ))
          )
      ),

    # ── Rest timer bar (floating, hidden until a set is logged) ──
    div(id    = "rest-timer-section",
        style = paste0("background:#111; border:1px solid #1D9E75; border-radius:12px;",
                       "padding:12px 16px; margin-bottom:12px;",
                       "display:none; align-items:center; gap:12px;"),
        div(style = "font-size:13px; color:#1D9E75;", "▶"),
        div(style = "flex:1;",
            div(id    = "rest-timer-display",
                style = paste0("font-size:24px; font-weight:700; color:#f0f0f0;",
                               "font-variant-numeric:tabular-nums; line-height:1;"),
                ""),
            div(style = "background:#222; border-radius:3px; height:3px; margin-top:6px;",
                div(id    = "rest-timer-bar",
                    style = paste0("height:100%; background:#1D9E75; border-radius:3px;",
                                   "width:100%; transition:width 1s linear;")))
        ),
        div(style = "display:flex; gap:6px;",
            tags$button("⟳",
                        style = paste0("background:#1a2a1f; border:1px solid #0F6E56;",
                                       "border-radius:8px; padding:6px 9px; font-size:15px;",
                                       "color:#1D9E75; cursor:pointer; line-height:1;"),
                        onclick = "restartTimer()"),
            tags$button("Skip",
                        style = paste0("background:#1e1e1e; border:none; border-radius:8px;",
                                       "padding:6px 10px; font-size:11px; color:#555;",
                                       "cursor:pointer; white-space:nowrap;"),
                        onclick = "stopTimer()")
        )
    ),

    # ── Exercise blocks ───────────────────────────────────────
    lapply(seq_along(group_order), function(g_idx) {
      key         <- group_order[[g_idx]]
      row_indices <- group_map[[key]]
      is_ss       <- startsWith(key, "ss_")
      block_color <- BLOCK_COLORS[((g_idx - 1L) %% length(BLOCK_COLORS)) + 1L]

      # Block completion
      block_done <- all(sapply(row_indices, function(i) {
        we_id <- exercises$id[i]
        logs  <- set_logs_rv[[we_id]]
        !is.null(logs) && length(logs) >= exercises$prescribed_sets[i]
      }))

      # Meta from first exercise in the block
      first_we   <- exercises[row_indices[1], ]
      n_sets     <- first_we$prescribed_sets
      rest_s     <- first_we$rest_seconds
      rest_label <- if (rest_s >= 60)
        paste0(round(rest_s / 60, 1), " min rest")
      else
        paste0(rest_s, "s rest")

      accent <- if (block_done) "#1D9E75" else block_color

      div(style = paste0(
            "background:#141414; border-radius:14px; margin-bottom:12px;",
            "overflow:hidden; border:1px solid ",
            if (block_done) "#0F6E56" else "#1c1c1c", ";"),

          # Block header
          div(style = paste0(
                "background:", accent, "18;",
                "border-bottom:1px solid ", accent, "30;",
                "padding:10px 14px;",
                "display:flex; justify-content:space-between; align-items:center;"),
              div(style = "display:flex; align-items:center; gap:8px;",
                  # Label pill
                  div(style = paste0(
                        "background:", accent, "; color:#fff;",
                        "font-size:10px; font-weight:700;",
                        "border-radius:6px; padding:3px 9px;",
                        "text-transform:uppercase; letter-spacing:0.05em;"),
                      if (is_ss) "Superset" else paste0("Block ", g_idx)
                  ),
                  div(style = "font-size:11px; color:#444;",
                      paste0(n_sets, " sets · ", rest_label))
              ),
              # Right badge: done check OR "×N" sets badge
              if (block_done)
                div(style = paste0("color:#1D9E75; font-size:15px; font-weight:700;"), "✓")
              else
                div(style = paste0(
                      "background:", accent, "18;",
                      "color:", accent, ";",
                      "font-size:10px; font-weight:700;",
                      "border-radius:10px; padding:2px 8px;",
                      "border:1px solid ", accent, "40;"),
                    paste0("×", n_sets))
          ),

          # Exercises inside the block
          lapply(seq_along(row_indices), function(ex_in_block) {
            i        <- row_indices[ex_in_block]
            we       <- exercises[i, ]
            ex_info  <- tryCatch(we$exercises, error = \(e) NULL)
            ex_name  <- if (!is.null(ex_info) && !is.null(ex_info$name))
              ex_info$name else paste("Exercise", i)
            ex_cat   <- if (!is.null(ex_info) && !is.null(ex_info$category))
              ex_info$category else ""
            yt_url   <- if (!is.null(ex_info) &&
                            !is.null(ex_info$youtube_url) &&
                            !is.na(ex_info$youtube_url))
              ex_info$youtube_url else NULL
            note_tip <- if (!is.null(ex_info) &&
                            !is.null(ex_info$coaching_note) &&
                            !is.na(ex_info$coaching_note))
              ex_info$coaching_note else NULL

            # Muscle display string
            prim_raw <- if (!is.null(ex_info) && !is.null(ex_info$primary_muscles))
              ex_info$primary_muscles else NULL
            muscles_display <- tryCatch({
              m <- unlist(prim_raw)
              m <- m[nchar(trimws(m)) > 0]
              if (length(m) > 0)
                tools::toTitleCase(paste(gsub("_", " ", head(m, 2)), collapse = ", "))
              else
                tools::toTitleCase(gsub("_", " ", ex_cat))
            }, error = \(e) tools::toTitleCase(gsub("_", " ", ex_cat)))

            # GIF: session cache → DB cached → NULL
            gif_url <- gif_map[[we$exercise_id]] %||%
              tryCatch({
                g <- ex_info$gif_url
                if (!is.na(g) && nchar(g) > 0) g else NULL
              }, error = \(e) NULL)

            last     <- last_perf_map[[we$exercise_id]]
            we_logs  <- set_logs_rv[[we$id]] %||% list()
            n_logged <- length(we_logs)
            is_complete <- n_logged >= we$prescribed_sets

            ex_last_note <- tryCatch({
              last_entry <- if (length(we_logs) > 0) we_logs[[length(we_logs)]] else NULL
              if (!is.null(last_entry) && !is.null(last_entry$notes) &&
                  !is.na(last_entry$notes)) last_entry$notes else ""
            }, error = \(e) "")

            # Separator between exercises in a superset
            sep <- if (is_ss && ex_in_block > 1)
              div(style = paste0(
                    "display:flex; align-items:center; gap:8px;",
                    "padding:0 14px; margin:0;"),
                  div(style = "flex:1; height:1px; background:#1e1e1e;"),
                  div(style = "font-size:10px; color:#333; font-weight:600;",
                      "SUPERSET"),
                  div(style = "flex:1; height:1px; background:#1e1e1e;")
              )
            else NULL

            tagList(
              sep,
              div(style = "padding:12px 14px;",

                  # ── Exercise header: thumbnail + info + action btns ──
                  div(style = "display:flex; align-items:flex-start; gap:10px; margin-bottom:10px;",

                      # GIF thumbnail or placeholder
                      if (!is.null(gif_url) && nchar(gif_url) > 0) {
                        tags$img(
                          src     = gif_url,
                          alt     = ex_name,
                          loading = "lazy",
                          style   = paste0(
                            "width:58px; height:58px; border-radius:10px;",
                            "object-fit:cover; background:#111; flex-shrink:0;"))
                      } else {
                        div(style = paste0(
                              "width:58px; height:58px; border-radius:10px;",
                              "background:#1e1e1e; flex-shrink:0;",
                              "display:flex; align-items:center; justify-content:center;",
                              "font-size:22px;"),
                            "🏋️")
                      },

                      # Name + tags
                      div(style = "flex:1; min-width:0;",
                          div(style = paste0(
                                "font-size:15px; font-weight:700; color:#f0f0f0;",
                                "line-height:1.2; margin-bottom:4px;"),
                              ex_name,
                              if (is_complete)
                                span(style = "color:#1D9E75; margin-left:6px; font-size:13px;",
                                     "✓")),
                          div(style = "display:flex; flex-wrap:wrap; gap:4px; align-items:center;",
                              if (nchar(muscles_display) > 0)
                                span(style = paste0(
                                       "font-size:10px; color:#555;",
                                       "background:#1e1e1e; border-radius:4px; padding:2px 7px;"),
                                     paste0("🎯 ", muscles_display)),
                              if (nchar(ex_cat) > 0)
                                span(style = paste0(
                                       "font-size:10px; color:#333;",
                                       "background:#1a1a1a; border-radius:4px; padding:2px 7px;"),
                                     tools::toTitleCase(gsub("_", " ", ex_cat)))
                          ),
                          div(style = "font-size:11px; color:#333; margin-top:4px;",
                              paste0(we$rep_range_low, "–", we$rep_range_high, " reps · RPE ",
                                     we$rpe_target))
                      ),

                      # Action buttons (stacked)
                      div(style = "display:flex; flex-direction:column; gap:4px; flex-shrink:0;",
                          if (!is.null(yt_url))
                            tags$button("▶",
                                        style = paste0(
                                          "background:#1e1e1e; border:none; border-radius:7px;",
                                          "padding:6px 8px; font-size:12px; color:#888;",
                                          "cursor:pointer;"),
                                        title   = "Watch demo",
                                        onclick = sprintf(
                                          "window.open('%s','_blank'); return false;", yt_url)),
                          tags$button("⇄",
                                      style = paste0(
                                        "background:#1e1e1e; border:none; border-radius:7px;",
                                        "padding:6px 8px; font-size:13px; color:#888;",
                                        "cursor:pointer;"),
                                      title   = "Swap exercise",
                                      onclick = sprintf(
                                        "Shiny.setInputValue('swap_exercise','%s|%s',{priority:'event'})",
                                        we$id, we$exercise_id)),
                          if (nchar(EXERCISEDB_API_KEY) > 0 &&
                              (is.null(gif_url) || nchar(gif_url %||% "") == 0))
                            tags$button("🎬",
                                        style = paste0(
                                          "background:#1e1e1e; border:none; border-radius:7px;",
                                          "padding:6px 8px; font-size:12px; color:#555;",
                                          "cursor:pointer;"),
                                        title   = "Load form demo",
                                        onclick = sprintf(
                                          "Shiny.setInputValue('load_exercise_gif','%s',{priority:'event'})",
                                          paste0(we$exercise_id, "|",
                                                 gsub("'", "", ex_name, fixed = TRUE))))
                      )
                  ),

                  # Coaching tip
                  if (!is.null(note_tip))
                    div(style = paste0(
                          "background:#061a12; border-left:2px solid #1D9E75;",
                          "border-radius:0 7px 7px 0; padding:7px 10px;",
                          "font-size:11px; color:#5DCAA5; margin-bottom:10px; line-height:1.4;"),
                        note_tip),

                  # Last performance reference
                  if (!is.null(last))
                    div(style = "font-size:11px; color:#333; margin-bottom:8px;",
                        paste0("Last: ",
                               if (!is.na(last$weight_lbs))
                                 paste0(last$weight_lbs, " lbs × ")
                               else "BW × ",
                               last$reps_completed, " reps",
                               if (!is.na(last$rpe_actual))
                                 paste0(" @ RPE ", last$rpe_actual) else "")),

                  # Warmup sets note
                  if (!is.null(we$warmup_sets) && !is.na(we$warmup_sets) && we$warmup_sets > 0)
                    div(style = "font-size:11px; color:#333; margin-bottom:8px;",
                        paste0("⬆ ", we$warmup_sets, " warm-up set(s)")),

                  # ── Set logging grid ─────────────────────────────────
                  div(
                    # Column headers
                    div(style = paste0(
                          "display:grid;",
                          "grid-template-columns:24px 1fr 1fr 50px 34px;",
                          "gap:4px; padding:0 2px 5px; border-bottom:1px solid #1a1a1a;",
                          "margin-bottom:5px;"),
                        div(style = "font-size:9px; color:#2e2e2e; text-transform:uppercase; text-align:center;", "SET"),
                        div(style = "font-size:9px; color:#2e2e2e; text-transform:uppercase;", "WEIGHT (lbs)"),
                        div(style = "font-size:9px; color:#2e2e2e; text-transform:uppercase;", "REPS"),
                        div(style = "font-size:9px; color:#2e2e2e; text-transform:uppercase;", "RPE"),
                        div()
                    ),

                    # Set rows
                    lapply(seq_len(we$prescribed_sets), function(s) {
                      set_key   <- paste0(we$id, "_s", s)
                      log_entry <- if (s <= length(we_logs)) we_logs[[s]] else NULL
                      is_logged <- !is.null(log_entry)

                      def_weight <- if (is_logged) log_entry$weight_lbs
                        else if (s > 1 && length(we_logs) >= s - 1) we_logs[[s-1]]$weight_lbs
                        else if (!is.null(last)) last$weight_lbs
                        else NA
                      def_reps <- if (is_logged) log_entry$reps_completed
                        else if (!is.null(last)) last$reps_completed
                        else we$rep_range_low
                      def_rpe  <- if (is_logged) log_entry$rpe_actual
                        else if (s > 1 && length(we_logs) >= s - 1) we_logs[[s-1]]$rpe_actual
                        else if (!is.null(last) && !is.na(last$rpe_actual))
                          round(last$rpe_actual)
                        else NA

                      input_bg  <- if (is_logged) "#071a10" else "#0d0d0d"
                      input_bdr <- if (is_logged) "#0F6E56" else "#1e1e1e"

                      div(style = paste0(
                            "display:grid;",
                            "grid-template-columns:24px 1fr 1fr 50px 34px;",
                            "gap:4px; align-items:center;",
                            "padding:4px 2px; border-radius:7px; margin-bottom:3px;",
                            if (is_logged) " background:#071a10;" else ""),

                          # Set number
                          div(style = paste0(
                                "font-size:12px; font-weight:700; text-align:center; ",
                                if (is_logged) "color:#1D9E75;" else "color:#2e2e2e;"),
                              s),

                          # Weight input
                          tags$input(
                            type        = "number",
                            id          = paste0("w_", set_key),
                            value       = if (!is.na(def_weight %||% NA)) def_weight else "",
                            placeholder = if (!is.null(last) && !is.na(last$weight_lbs))
                              as.character(last$weight_lbs) else "lbs",
                            min = "0", step = "2.5",
                            style = paste0(
                              "background:", input_bg, ";",
                              "border:1px solid ", input_bdr, ";",
                              "color:#f0f0f0; border-radius:7px; padding:6px 8px;",
                              "font-size:13px; width:100%; box-sizing:border-box;"),
                            class        = "ct-set-input",
                            `data-we-id` = we$id,
                            `data-set`   = s,
                            `data-type`  = "weight"
                          ),

                          # Reps input
                          tags$input(
                            type        = "number",
                            id          = paste0("r_", set_key),
                            value       = if (!is.na(def_reps %||% NA)) def_reps else "",
                            placeholder = as.character(we$rep_range_low),
                            min = "0", step = "1",
                            style = paste0(
                              "background:", input_bg, ";",
                              "border:1px solid ", input_bdr, ";",
                              "color:#f0f0f0; border-radius:7px; padding:6px 8px;",
                              "font-size:13px; width:100%; box-sizing:border-box;"),
                            class        = "ct-set-input",
                            `data-we-id` = we$id,
                            `data-set`   = s,
                            `data-type`  = "reps"
                          ),

                          # RPE input
                          tags$input(
                            type        = "number",
                            id          = paste0("rpe_", set_key),
                            value       = if (!is.na(def_rpe %||% NA)) as.integer(def_rpe) else "",
                            placeholder = as.character(as.integer(we$rpe_target)),
                            min = "0", max = "10", step = "1",
                            style = paste0(
                              "background:", input_bg, ";",
                              "border:1px solid ", input_bdr, ";",
                              "color:#f0f0f0; border-radius:7px; padding:6px 8px;",
                              "font-size:13px; width:100%; box-sizing:border-box;"),
                            `data-we-id` = we$id,
                            `data-set`   = s,
                            `data-type`  = "rpe"
                          ),

                          # Log / check button
                          if (is_logged)
                            div(style = "text-align:center; color:#1D9E75; font-size:17px;", "✓")
                          else
                            tags$button(
                              "✓",
                              style = paste0(
                                "background:#1D9E75; color:#fff; border:none; border-radius:7px;",
                                "font-size:15px; font-weight:700; cursor:pointer;",
                                "width:34px; height:34px; display:flex;",
                                "align-items:center; justify-content:center;"),
                              onclick = sprintf(
                                "Shiny.setInputValue('log_set','%s|%d',{priority:'event'})",
                                we$id, s))
                      )
                    }),

                    # Notes textarea
                    div(style = "margin-top:8px;",
                        tags$textarea(
                          id          = paste0("note_", we$id),
                          placeholder = "Notes (optional)...",
                          style       = paste0(
                            "background:#0d0d0d; border:1px solid #1a1a1a;",
                            "color:#aaa; border-radius:8px; padding:8px 10px;",
                            "font-size:12px; width:100%; box-sizing:border-box;",
                            "resize:none; min-height:32px; font-family:inherit;",
                            "line-height:1.4;"),
                          ex_last_note
                        )
                    )
                  ),  # end set grid

                  # Exercise history (collapsible, lazy-loaded)
                  {
                    hist <- history_map[[we$exercise_id]]
                    if (!is.null(hist) && is.data.frame(hist) && nrow(hist) > 0) {
                      div(style = "margin-top:8px;",
                          tags$details(
                            tags$summary(
                              style = paste0(
                                "font-size:11px; color:#333; cursor:pointer; padding:3px 0;",
                                "list-style:none; -webkit-user-select:none; user-select:none;"),
                              "📊 History"),
                            div(style = "margin-top:6px; display:flex; flex-direction:column; gap:3px;",
                                lapply(seq_len(nrow(hist)), function(h) {
                                  r <- hist[h, ]
                                  div(style = paste0(
                                        "display:flex; justify-content:space-between;",
                                        "padding:4px 7px; background:#0d0d0d; border-radius:5px;",
                                        "font-size:11px;"),
                                      span(style = "color:#333;", format(r$date, "%b %d")),
                                      span(style = "color:#888;",
                                           paste0(if (!is.na(r$wt)) paste0(r$wt, " lbs") else "BW",
                                                  " × ", r$reps, " reps",
                                                  if (!is.na(r$rpe)) paste0("  RPE ", r$rpe) else "",
                                                  if (r$n_sets > 1) paste0("  (", r$n_sets, " sets)") else "")))
                                })
                            )
                          )
                      )
                    } else if (is.null(hist)) {
                      div(style = "margin-top:8px;",
                          tags$button(
                            "📊 History",
                            style = paste0(
                              "background:none; border:1px solid #1e1e1e;",
                              "border-radius:6px; padding:5px 10px;",
                              "font-size:11px; color:#2e2e2e; cursor:pointer;"),
                            onclick = sprintf(
                              "Shiny.setInputValue('load_exercise_history','%s',{priority:'event'})",
                              we$exercise_id))
                      )
                    }
                  }

              ) # end exercise padding div
            ) # end tagList
          }) # end lapply exercises in block
      ) # end block card
    }), # end lapply groups

    # ── Action buttons ────────────────────────────────────────
    div(style = "margin-top:20px; padding-bottom:28px;",
        if (is_review) {
          tags$button(
            "← Back to Calendar",
            style = paste0(
              "width:100%; background:#1e1e1e; color:#aaa; border:none;",
              "border-radius:12px; padding:14px; font-size:14px;",
              "font-weight:600; cursor:pointer;"),
            onclick = "Shiny.setInputValue('close_workout', Math.random(), {priority:'event'})")
        } else {
          tagList(
            tags$button(
              "Finish Session",
              style = paste0(
                "width:100%; background:#1D9E75; color:#fff; border:none;",
                "border-radius:14px; padding:16px; font-size:16px;",
                "font-weight:700; cursor:pointer; letter-spacing:0.02em;",
                "box-shadow:0 4px 20px rgba(29,158,117,0.35);"),
              onclick = "Shiny.setInputValue('finish_session', Math.random(), {priority:'event'})"),
            tags$button(
              "← Close",
              style = paste0(
                "width:100%; background:none; color:#444; border:none;",
                "border-radius:12px; padding:12px; font-size:13px;",
                "cursor:pointer; margin-top:8px;"),
              onclick = "Shiny.setInputValue('close_workout', Math.random(), {priority:'event'})")
          )
        }
    )
  ) # end tagList
}

# ── SWAP MODAL UI ─────────────────────────────────────────────
swap_modal_ui <- function(we_id, exercise_id, suggestions) {
  div(style = paste0(
        "position:fixed; top:0; left:0; right:0; bottom:0;",
        "background:rgba(0,0,0,0.88); z-index:200;",
        "display:flex; align-items:flex-end; justify-content:center;"),
      div(style = paste0(
            "background:#141414; border-radius:20px 20px 0 0;",
            "width:100%; max-width:480px; padding:24px;",
            "border-top:1px solid #222;"),

          div(style = "width:40px; height:4px; background:#333; border-radius:2px; margin:0 auto 20px;"),

          div(style = "font-size:17px; font-weight:700; margin-bottom:4px; color:#f0f0f0;",
              "Swap Exercise"),
          div(style = "font-size:12px; color:#444; margin-bottom:18px;",
              "Same muscles, same movement — tap to pick"),

          if (is.null(suggestions)) {
            div(style = "color:#444; text-align:center; padding:24px;",
                "Finding substitutes...")
          } else if (length(suggestions) == 0) {
            div(style = "color:#444; text-align:center; padding:24px;",
                "No substitutes found with your equipment.")
          } else {
            tagList(
              lapply(seq_along(suggestions), function(i) {
                s           <- suggestions[[i]]
                muscles_str <- tryCatch(
                  paste(unlist(s$primary_muscles), collapse = ", "),
                  error = \(e) "")
                label <- if (i == 1) "Best match" else if (i == 2) "Alternative" else "Option"
                div(style = paste0(
                      "background:#1a1a1a; border:1px solid #242424;",
                      "border-radius:12px; padding:14px 16px; margin-bottom:8px;",
                      "cursor:pointer; transition:border-color 0.15s;"),
                    onclick = sprintf(
                      "Shiny.setInputValue('confirm_swap','%s|%s|session',{priority:'event'})",
                      we_id, s$id),
                    div(style = "display:flex; justify-content:space-between; align-items:flex-start;",
                        div(style = "font-size:14px; font-weight:600; color:#f0f0f0;",
                            s$name),
                        div(style = paste0(
                              "font-size:10px; color:#1D9E75; font-weight:700;",
                              "text-transform:uppercase; letter-spacing:0.07em;",
                              "margin-left:8px; flex-shrink:0;"),
                            label)
                    ),
                    div(style = "font-size:11px; color:#444; margin-top:5px;",
                        paste0("🎯 ",
                               tools::toTitleCase(gsub("_", " ", muscles_str)),
                               " · ",
                               s$default_rep_range_low %||% 8, "–",
                               s$default_rep_range_high %||% 12, " reps"))
                )
              }),
              div(style = "display:flex; gap:8px; margin-top:10px;",
                  tags$button(
                    "Cancel",
                    style = paste0(
                      "flex:1; background:#1e1e1e; color:#666; border:none;",
                      "border-radius:10px; padding:12px; font-size:13px; cursor:pointer;"),
                    onclick = "Shiny.setInputValue('cancel_swap', 1, {priority:'event'})"),
                  tags$button(
                    "Swap rest of block →",
                    style = paste0(
                      "flex:2; background:#0a1f16; color:#1D9E75;",
                      "border:1px solid #1D9E75; border-radius:10px;",
                      "padding:12px; font-size:13px; font-weight:600; cursor:pointer;"),
                    onclick = sprintf(
                      "Shiny.setInputValue('confirm_swap','%s|%s|block',{priority:'event'})",
                      we_id, suggestions[[1]]$id))
              )
            )
          }
      )
  )
}

# ── WORKOUT SERVER LOGIC ─────────────────────────────────────
setup_workout_server <- function(input, output, session, rv) {

  # ── Open a workout ───────────────────────────────────────────
  observeEvent(input$open_workout, {
    rv$active_workout_id <- input$open_workout
    rv$page    <- "workout"
    rv$nav_tab <- "log"

    # Reset session elapsed timer in the browser
    session$sendCustomMessage("reset_session_timer", list())

    withProgress(message = "Loading session...", value = 0.5, {
      data <- tryCatch({
        wo <- sb_select("workouts",
                        sprintf("?id=eq.%s", rv$active_workout_id),
                        token = rv$token)
        we <- sb_select("workout_exercises",
                        sprintf("?workout_id=eq.%s&select=*,exercises(*)&order=exercise_order",
                                rv$active_workout_id),
                        token = rv$token)
        list(workout = wo, exercises = we)
      }, error = \(e) { message("Load workout error: ", e$message); NULL })

      if (!is.null(data)) {
        rv$active_workout   <- data$workout
        rv$active_exercises <- data$exercises

        rv$last_perf_map <- list()
        if (!is.null(data$exercises)) {
          for (i in seq_len(nrow(data$exercises))) {
            eid  <- data$exercises$exercise_id[i]
            last <- tryCatch(
              fetch_last_performance(eid, rv$user_id, rv$token),
              error = \(e) NULL)
            if (!is.null(last)) rv$last_perf_map[[eid]] <- last[1, ]
          }
        }
      }
    })

    # Load existing set logs from Supabase so re-opening shows prior work
    rv$set_logs <- list()
    if (!is.null(data) && !is.null(data$exercises) && nrow(data$exercises) > 0) {
      we_ids <- paste(data$exercises$id, collapse = ",")
      existing <- tryCatch(
        sb_select("workout_set_logs",
                  sprintf("?user_id=eq.%s&workout_exercise_id=in.(%s)&order=set_number",
                          rv$user_id, we_ids),
                  token = rv$token),
        error = \(e) NULL)
      if (!is.null(existing) && nrow(existing) > 0) {
        for (i in seq_len(nrow(existing))) {
          row   <- existing[i, ]
          we_id <- row$workout_exercise_id
          set_n <- as.integer(row$set_number)
          if (is.null(rv$set_logs[[we_id]])) rv$set_logs[[we_id]] <- list()
          rv$set_logs[[we_id]][[set_n]] <- list(
            weight_lbs     = row$weight_lbs,
            reps_completed = row$reps_completed,
            rpe_actual     = row$rpe_actual,
            notes          = row$notes,
            set_number     = set_n
          )
        }
        message(sprintf("Loaded %d existing set logs for workout %s",
                        nrow(existing), rv$active_workout_id))
      }
    }

    rv$swap_we_id       <- NULL
    rv$swap_ex_id       <- NULL
    rv$swap_suggestions <- NULL
  })

  # ── Close workout ────────────────────────────────────────────
  observeEvent(input$close_workout, {
    rv$page    <- "dashboard"
    rv$nav_tab <- "dashboard"
    rv$active_workout_id <- NULL
  })

  # ── Log a set ────────────────────────────────────────────────
  observeEvent(input$log_set, {
    parts   <- strsplit(input$log_set, "\\|")[[1]]
    if (length(parts) < 2) return()
    we_id   <- parts[1]
    set_num <- as.integer(parts[2])
    set_key  <- paste0("w_",   we_id, "_s", set_num)
    reps_key <- paste0("r_",   we_id, "_s", set_num)
    rpe_key  <- paste0("rpe_", we_id, "_s", set_num)
    note_key <- paste0("note_", we_id)

    weight <- tryCatch(as.numeric(input[[set_key]]),  error = \(e) NA)
    reps   <- tryCatch(as.integer(input[[reps_key]]), error = \(e) NA)
    rpe    <- tryCatch(as.numeric(input[[rpe_key]]),  error = \(e) NA)
    notes  <- input[[note_key]] %||% ""

    if (is.na(reps)) {
      showNotification("Please enter reps before logging.", type = "warning")
      return()
    }

    log_row <- list(
      workout_exercise_id = we_id,
      user_id             = rv$user_id,
      set_number          = as.integer(set_num),
      weight_lbs          = if (is.na(weight)) NULL else weight,
      reps_completed      = as.integer(reps),
      rpe_actual          = if (is.na(rpe)) NULL else rpe,
      is_warmup           = FALSE,
      notes               = if (nchar(notes) > 0) notes else NULL
    )

    resp <- sb_insert("workout_set_logs", log_row, token = rv$token)

    if (resp$status_code %in% c(200, 201)) {
      current           <- rv$set_logs[[we_id]] %||% list()
      current[[set_num]] <- log_row
      rv$set_logs[[we_id]] <- current

      # Start rest timer
      if (!is.null(rv$active_exercises)) {
        we_row <- rv$active_exercises[rv$active_exercises$id == we_id, ]
        if (nrow(we_row) > 0) {
          rest_s <- we_row$rest_seconds[1]
          session$sendCustomMessage("start_rest_timer", list(seconds = rest_s))
        }
      }

      showNotification(paste0("Set ", set_num, " logged ✓"), type = "message", duration = 2)
    } else {
      showNotification("Error saving set. Try again.", type = "error")
    }
  })

  # JS handler for rest timer + session timer reset
  output$start_timer_js <- renderUI({
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('start_rest_timer', function(msg) {
         startRestTimer(msg.seconds);
       });
       Shiny.addCustomMessageHandler('reset_session_timer', function(msg) {
         window.catrackWsStart = Date.now();
       });"
    ))
  })

  # ── Finish session ───────────────────────────────────────────
  observeEvent(input$finish_session, {
    if (is.null(rv$active_workout_id)) return()

    duration_mins <- tryCatch(
      if (!is.null(rv$session_start_time))
        as.integer(as.numeric(Sys.time() - rv$session_start_time, units = "mins"))
      else NA_integer_,
      error = \(e) NA_integer_)

    update_data <- list(completed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"))
    if (!is.na(duration_mins)) update_data$duration_minutes <- duration_mins

    resp <- sb_update("workouts",
                      sprintf("?id=eq.%s", rv$active_workout_id),
                      update_data,
                      token = rv$token)

    message(sprintf("Finish session response: %d", resp$status_code))
    showNotification("Session complete! Great work 💪", type = "message", duration = 4)

    rv$all_logs <- NULL
    rv$prs      <- NULL

    Sys.sleep(0.3)
    tryCatch({
      workouts <- sb_select("workouts",
                            sprintf("?program_id=eq.%s&order=week_number,session_number",
                                    rv$program$id),
                            token = rv$token)
      if (safe_nrow(workouts) > 0) rv$workouts <- workouts
    }, error = \(e) message("Reload workouts error: ", e$message))

    rv$active_workout_id <- NULL
    rv$active_workout    <- NULL
    rv$active_exercises  <- NULL
    rv$set_logs          <- list()
    rv$page              <- "dashboard"
    rv$nav_tab           <- "dashboard"
  })

  # ── Exercise swap ────────────────────────────────────────────
  observeEvent(input$swap_exercise, {
    parts <- strsplit(input$swap_exercise, "\\|")[[1]]
    if (length(parts) < 2) return()
    rv$swap_we_id       <- parts[1]
    rv$swap_ex_id       <- parts[2]
    rv$swap_suggestions <- NULL

    we_row <- if (!is.null(rv$active_exercises))
      rv$active_exercises[rv$active_exercises$id == rv$swap_we_id, ] else NULL

    tryCatch({
      current_ex <- sb_select("exercises",
                              sprintf("?id=eq.%s", rv$swap_ex_id), token = rv$token)
      if (!is.null(current_ex)) {
        muscles    <- paste(current_ex$primary_muscles[[1]], collapse = ", ")
        user_equip <- rv$profile$equipment_available[[1]] %||%
          c("dumbbells", "bench", "cable_machine", "pullup_bar", "bodyweight")
        suggestions <- get_swap_suggestions(
          exercise_name   = current_ex$name,
          primary_muscles = muscles,
          user_equipment  = user_equip,
          user_token      = rv$token,
          exclude_ex_id   = rv$swap_ex_id
        )
        rv$swap_suggestions <- suggestions
      }
    }, error = function(e) {
      message("Swap suggestion error: ", e$message)
      rv$swap_suggestions <- list()
    })
  })

  observeEvent(input$cancel_swap, {
    rv$swap_we_id       <- NULL
    rv$swap_ex_id       <- NULL
    rv$swap_suggestions <- NULL
  })

  observeEvent(input$confirm_swap, {
    parts <- strsplit(input$confirm_swap, "\\|")[[1]]
    if (length(parts) < 3) return()
    we_id     <- parts[1]
    new_ex_id <- parts[2]
    scope     <- parts[3]

    sb_update("workout_exercises",
              sprintf("?id=eq.%s", we_id),
              list(exercise_id = new_ex_id, is_swapped = TRUE),
              token = rv$token)

    sb_insert("exercise_swaps",
              list(user_id = rv$user_id, workout_exercise_id = we_id,
                   original_exercise_id    = rv$swap_ex_id,
                   replacement_exercise_id = new_ex_id, scope = scope),
              token = rv$token)

    if (scope == "block" && !is.null(rv$active_workout) && !is.null(rv$program)) {
      current_week <- rv$active_workout$week_number[1]
      tryCatch(
        regenerate_from_week(rv$program$id, rv$user_id,
                             current_week, rv$swap_ex_id, new_ex_id),
        error = \(e) message("Block regenerate error: ", e$message))
    }

    we <- sb_select("workout_exercises",
                    sprintf("?workout_id=eq.%s&select=*,exercises(*)&order=exercise_order",
                            rv$active_workout_id), token = rv$token)
    rv$active_exercises <- we
    rv$swap_we_id       <- NULL
    rv$swap_ex_id       <- NULL
    rv$swap_suggestions <- NULL

    showNotification(
      paste0("Swapped! ",
             if (scope == "block") "Updated for rest of block." else "This session only."),
      type = "message", duration = 3)
  })

  # ── Load exercise history ────────────────────────────────────
  observeEvent(input$load_exercise_history, {
    req(rv$token, rv$user_id)
    ex_id <- trimws(input$load_exercise_history %||% "")
    if (nchar(ex_id) == 0 || !is.null(rv$exercise_history[[ex_id]])) return()
    hist <- fetch_exercise_history(ex_id, rv$user_id, rv$token)
    rv$exercise_history[[ex_id]] <- if (!is.null(hist)) hist else data.frame()
  })

  # ── Load & cache exercise GIF ────────────────────────────────
  observeEvent(input$load_exercise_gif, {
    req(rv$token)
    raw   <- input$load_exercise_gif %||% ""
    parts <- strsplit(raw, "\\|")[[1]]
    if (length(parts) < 2) return()
    ex_id   <- parts[1]
    ex_name <- paste(parts[-1], collapse = "|")

    if (!is.null(rv$exercise_gifs[[ex_id]])) return()

    result  <- fetch_exercise_gif(ex_name, EXERCISEDB_API_KEY)
    gif_url <- result$url
    if (is.null(gif_url)) {
      err <- result$error %||% "unknown"
      if (grepl("429", err)) {
        showNotification("ExerciseDB daily limit reached (50 req/day). Try again tomorrow.",
                         type = "warning", duration = 6)
      } else {
        showNotification(paste0("GIF not found [", err, "]"), type = "warning", duration = 6)
      }
      rv$exercise_gifs[[ex_id]] <- ""
      return()
    }

    rv$exercise_gifs[[ex_id]] <- gif_url
    svc <- if (nchar(SUPABASE_SERVICE_KEY) > 0) SUPABASE_SERVICE_KEY else rv$token
    cache_exercise_gif(ex_id, gif_url, svc)
  })
}

# ── SWAP SUGGESTIONS ─────────────────────────────────────────
# Returns up to 3 exercise substitutes that target the same muscles
# as the exercise being swapped, filtered by the user's equipment.
#
# Strategy (in priority order):
#  1. Use the curated substitution_1 / substitution_2 fields on the exercise
#     record — these are hand-picked same-muscle-group alternatives
#  2. Fall back to scoring all equipment-eligible exercises by primary_muscles
#     overlap with the original exercise
get_swap_suggestions <- function(exercise_name, primary_muscles,
                                 user_equipment, user_token, exclude_ex_id) {

  parse_arr <- function(val) {
    if (is.list(val))      return(tolower(trimws(unlist(val))))
    if (is.character(val)) return(tolower(trimws(strsplit(gsub('[{}\\[\\]"]', '', val[1]), ",")[[1]])))
    character(0)
  }

  has_equipment <- function(req_val) {
    req <- parse_arr(req_val)
    req <- req[nchar(req) > 0]
    if (length(req) == 0) return(TRUE)
    all(req %in% c(tolower(user_equipment), "bodyweight"))
  }

  results   <- list()
  found_ids <- character(0)

  # ── Step 1: curated substitutions ────────────────────────────
  cur <- sb_select("exercises",
    sprintf("?id=eq.%s&select=substitution_1,substitution_2", exclude_ex_id),
    token = user_token)

  sub_names <- character(0)
  if (!is.null(cur) && nrow(cur) > 0) {
    s1 <- tryCatch(as.character(cur$substitution_1[1]), error = \(e) NA_character_)
    s2 <- tryCatch(as.character(cur$substitution_2[1]), error = \(e) NA_character_)
    sub_names <- na.omit(c(s1, s2))
    sub_names <- sub_names[nchar(trimws(sub_names)) > 0]
  }

  for (nm in sub_names) {
    match <- sb_select("exercises",
      sprintf(paste0("?name=ilike.%s",
                     "&select=id,name,category,primary_muscles,equipment_required,",
                     "default_rep_range_low,default_rep_range_high&limit=1"),
              URLencode(nm, reserved = TRUE)),
      token = user_token)
    if (is.null(match) || nrow(match) == 0) next
    if (!has_equipment(match$equipment_required[[1]])) next
    results   <- c(results, list(as.list(match[1, ])))
    found_ids <- c(found_ids, match$id[1])
    if (length(results) >= 3) return(results)
  }

  # ── Step 2: muscle-overlap scoring ───────────────────────────
  target_muscles <- tolower(trimws(strsplit(primary_muscles, ",\\s*")[[1]]))
  target_muscles <- target_muscles[nchar(target_muscles) > 0]

  pool <- sb_select("exercises",
    sprintf(paste0("?id=neq.%s",
                   "&select=id,name,category,primary_muscles,equipment_required,",
                   "default_rep_range_low,default_rep_range_high"),
            exclude_ex_id),
    token = user_token)

  if (is.null(pool) || nrow(pool) == 0) return(results)

  pool <- pool[!pool$id %in% found_ids, ]
  ok   <- vapply(seq_len(nrow(pool)), function(i)
    has_equipment(pool$equipment_required[[i]]), logical(1))
  pool <- pool[ok, , drop = FALSE]
  if (nrow(pool) == 0) return(results)

  scores <- vapply(seq_len(nrow(pool)), function(i) {
    m <- parse_arr(pool$primary_muscles[[i]])
    m <- m[nchar(m) > 0]
    length(intersect(m, target_muscles))
  }, integer(1))

  matched <- pool[scores > 0, , drop = FALSE]
  mscores <- scores[scores > 0]
  if (nrow(matched) > 0) {
    matched <- matched[order(-mscores), , drop = FALSE]
    need    <- max(0L, 3L - length(results))
    results <- c(results,
      lapply(seq_len(min(need, nrow(matched))),
             function(i) as.list(matched[i, ])))
  }

  results
}
