# ============================================================
# workout_screen.R — CaTrack
# Active session screen: per-set logging, rest timer,
# last-session pre-fill, RPE, notes, exercise swap.
# Source this from global.R
# ============================================================

# ── TIMER JS ─────────────────────────────────────────────────
rest_timer_js <- "
// Use window globals so the timer state persists across Shiny re-renders.
// var declarations reset to null each re-render, causing multiple orphan intervals
// that all fire vibrate simultaneously when they expire.
if (typeof window.catrackTimer     === 'undefined') window.catrackTimer     = null;
if (typeof window.catrackSeconds   === 'undefined') window.catrackSeconds   = 0;
if (typeof window.catrackTotal     === 'undefined') window.catrackTotal     = 0;
if (typeof window.catrackLastTotal === 'undefined') window.catrackLastTotal = 0;

function showTimerSection() {
  var el = document.getElementById('rest-timer-section');
  if (el) el.style.display = 'flex';
}
function hideTimerSection() {
  var el = document.getElementById('rest-timer-section');
  if (el) el.style.display = 'none';
}
function startRestTimer(seconds) {
  clearInterval(window.catrackTimer);
  window.catrackLastTotal = seconds;
  window.catrackSeconds   = seconds;
  window.catrackTotal     = seconds;
  showTimerSection();
  updateTimerDisplay();
  window.catrackTimer = setInterval(function() {
    window.catrackSeconds--;
    updateTimerDisplay();
    if (window.catrackSeconds <= 0) {
      clearInterval(window.catrackTimer);
      window.catrackTimer = null;
      var bar  = document.getElementById('rest-timer-bar');
      var disp = document.getElementById('rest-timer-display');
      if (bar)  bar.style.width = '0%';
      if (disp) { disp.innerText = 'Rest done!'; disp.style.color = '#1D9E75'; }
      if (navigator.vibrate) navigator.vibrate([200, 100, 200]);
      setTimeout(hideTimerSection, 3000);
    }
  }, 1000);
}
function restartTimer() {
  if (window.catrackLastTotal > 0) startRestTimer(window.catrackLastTotal);
}
function updateTimerDisplay() {
  var m   = Math.floor(window.catrackSeconds / 60);
  var s   = window.catrackSeconds % 60;
  var pct = window.catrackTotal > 0 ? Math.round(100 * window.catrackSeconds / window.catrackTotal) : 100;
  var disp = document.getElementById('rest-timer-display');
  var bar  = document.getElementById('rest-timer-bar');
  if (disp) { disp.innerText = m + ':' + (s < 10 ? '0' : '') + s; disp.style.color = '#f0f0f0'; }
  if (bar)  bar.style.width = pct + '%';
}
function stopTimer() {
  clearInterval(window.catrackTimer);
  window.catrackTimer = null;
  hideTimerSection();
}
"

# ── SESSION ELAPSED TIMER JS ─────────────────────────────────
# Persists start time in localStorage keyed by workout ID so the
# timer survives both Shiny re-renders AND full session reconnects.
make_session_timer_js <- function(workout_id) {
  sprintf("
(function() {
  var storageKey = 'catrack_ws_start_%s';
  var stored = localStorage.getItem(storageKey);
  var now = Date.now();
  // Resume if stored start is < 6 hours old (same session)
  if (stored && (now - parseInt(stored, 10)) < 6 * 3600 * 1000) {
    window.catrackWsStart = parseInt(stored, 10);
  } else {
    window.catrackWsStart = now;
    localStorage.setItem(storageKey, String(now));
  }
  if (window.catrackElapsedInterval) clearInterval(window.catrackElapsedInterval);
  function updateElapsed() {
    var el = document.getElementById('session-elapsed');
    if (!el) return;
    var elapsed = Math.floor((Date.now() - window.catrackWsStart) / 1000);
    var m = Math.floor(elapsed / 60);
    var s = elapsed %% 60;
    el.innerText = m + ':' + (s < 10 ? '0' : '') + s;
  }
  window.catrackElapsedInterval = setInterval(updateElapsed, 1000);
  updateElapsed();
})();
", workout_id)
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
                             "&select=weight_lbs,reps_completed,rpe_actual,notes,logged_at",
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
    note_val <- tryCatch({
      n <- as.character(best$notes %||% "")
      if (n %in% c("", "NA", "{}", "[]", "null")) NA_character_ else n
    }, error = \(e) NA_character_)
    data.frame(date   = d, wt = best$wt, reps = best$reps,
               rpe    = best$rpe, n_sets = nrow(day),
               note   = note_val, stringsAsFactors = FALSE)
  }))
}

# ── LAST PERFORMANCE (multi-row for rep-matched pre-fill) ────
# Returns up to 15 recent working set logs so the renderer can
# pick the most recent one whose reps match the current target.
fetch_last_performance <- function(exercise_id, user_id, token) {
  we_ids <- tryCatch(
    sb_select("workout_exercises",
              sprintf("?exercise_id=eq.%s&select=id", exercise_id),
              token = token),
    error = \(e) NULL)
  if (is.null(we_ids) || nrow(we_ids) == 0) return(NULL)
  id_list <- paste0("(", paste(we_ids$id, collapse = ","), ")")
  tryCatch(
    sb_select("workout_set_logs",
              sprintf(paste0("?user_id=eq.%s&is_warmup=eq.false",
                             "&workout_exercise_id=in.%s",
                             "&select=weight_lbs,reps_completed,rpe_actual,logged_at",
                             "&order=logged_at.desc&limit=15"),
                      user_id, id_list),
              token = token),
    error = \(e) NULL)
}

# ── SUGGESTED WEIGHT (Nippard-style intensity-from-RIR scaling) ──
# Given last logged performance, estimate 1RM (Epley/Brzycki blend)
# and scale it to the current block's RIR + the rep range midpoint.
# The lifter is free to override — this is a suggestion, not a
# prescription. Returns NA if there's no last-performance data.
#
# Math: %1RM ≈ 1 / (1 + (target_reps + RIR) / 30)   [Epley inverse]
#       suggested = e1RM × %1RM, rounded to 2.5 lb plate increments
suggest_weight <- function(last_log, target_reps, week_number) {
  if (is.null(last_log)) return(NA_real_)
  w <- tryCatch(as.numeric(last_log$weight_lbs),    error = \(e) NA_real_)
  r <- tryCatch(as.integer(last_log$reps_completed), error = \(e) NA_integer_)
  if (is.na(w) || is.na(r) || w <= 0 || r <= 0) return(NA_real_)

  # estimate_1rm() lives in progress_screen.R — sourced before runtime
  e1rm <- tryCatch(estimate_1rm(w, r), error = \(e) NA_real_)
  if (is.null(e1rm) || is.na(e1rm) || e1rm <= 0) return(NA_real_)

  block <- tryCatch(c("A","B","C")[ceiling(as.integer(week_number) / 4)],
                    error = \(e) "B")
  rir <- tryCatch(BLOCK_PROFILES[[block]]$rir_target, error = \(e) 2.0)
  if (is.null(rir) || is.na(rir)) rir <- 2.0

  effort_reps <- as.numeric(target_reps) + as.numeric(rir)
  pct_1rm     <- 1 / (1 + effort_reps / 30)

  round(e1rm * pct_1rm / 2.5) * 2.5
}

# ── EXERCISE GROUP BUILDER ───────────────────────────────────
# Groups exercises by superset_group (adjacent rows with the same non-null
# superset_group share a block; each exercise with no superset_group is solo).
build_exercise_groups <- function(exercises) {
  group_order <- character(0)
  group_map   <- list()
  for (i in seq_len(nrow(exercises))) {
    sg_raw <- tryCatch(exercises$superset_group[i], error = \(e) NA)
    sg <- if (!is.null(sg_raw) && length(sg_raw) == 1 && !is.na(sg_raw))
      trimws(as.character(sg_raw)) else ""
    if (nchar(sg) == 0) {
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
                              history_map = list()) {

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
      tags$script(HTML(make_session_timer_js(
        tryCatch(as.character(wo$id[1]), error = \(e) "unknown"))))
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
                                       "padding:6px 10px; font-size:11px; color:#888;",
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
                  div(style = "font-size:11px; color:#888;",
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

            # Rep-matched last performance: find the most recent log whose
            # reps fall within this exercise's target range. Falls back to
            # the most recent log if no rep-matched entry exists.
            last <- tryCatch({
              df <- last_perf_map[[we$exercise_id]]
              if (is.null(df) || nrow(df) == 0) {
                NULL
              } else {
                rep_lo <- tryCatch(as.integer(we$rep_range_low),  error = \(e) NA_integer_)
                rep_hi <- tryCatch(as.integer(we$rep_range_high), error = \(e) NA_integer_)
                if (!is.na(rep_lo) && !is.na(rep_hi)) {
                  matched <- df[!is.na(df$reps_completed) &
                                df$reps_completed >= rep_lo &
                                df$reps_completed <= (rep_hi + 2L), ]
                  if (nrow(matched) > 0) matched[1, ] else df[1, ]
                } else df[1, ]
              }
            }, error = \(e) NULL)
            we_logs  <- set_logs_rv[[we$id]] %||% list()
            n_logged <- length(we_logs)
            is_complete <- n_logged >= we$prescribed_sets

            ex_last_note <- tryCatch({
              last_entry <- if (length(we_logs) > 0) we_logs[[length(we_logs)]] else NULL
              raw <- if (!is.null(last_entry)) last_entry$notes else NULL
              s   <- tryCatch(as.character(raw %||% ""), error = \(e) "")
              if (s %in% c("", "NA", "{}", "[]", "null")) "" else s
            }, error = \(e) "")

            # Separator between exercises in a superset
            sep <- if (is_ss && ex_in_block > 1)
              div(style = paste0(
                    "display:flex; align-items:center; gap:8px;",
                    "padding:0 14px; margin:0;"),
                  div(style = "flex:1; height:1px; background:#1e1e1e;"),
                  div(style = "font-size:10px; color:#666; font-weight:600;",
                      "SUPERSET"),
                  div(style = "flex:1; height:1px; background:#1e1e1e;")
              )
            else NULL

            tagList(
              sep,
              div(style = "padding:12px 14px;",

                  # ── Exercise header: info + action btns ──
                  div(style = "display:flex; align-items:flex-start; gap:10px; margin-bottom:10px;",

                      # Name + tags
                      div(style = "flex:1; min-width:0;",
                          div(style = paste0(
                                "font-size:15px; font-weight:700; color:#f0f0f0;",
                                "line-height:1.2; margin-bottom:4px;"),
                              ex_name,
                              if (isTRUE(we$is_swapped))
                                span(style = paste0(
                                       "font-size:9px; color:#FF9800;",
                                       "background:#2d1e00; border:1px solid #FF980050;",
                                       "border-radius:4px; padding:2px 5px;",
                                       "margin-left:6px; vertical-align:middle;"),
                                     "SUBST"),
                              if (is_complete)
                                span(style = "color:#1D9E75; margin-left:6px; font-size:13px;",
                                     "✓")),
                          div(style = "display:flex; flex-wrap:wrap; gap:4px; align-items:center;",
                              if (nchar(muscles_display) > 0)
                                span(style = paste0(
                                       "font-size:10px; color:#aaa;",
                                       "background:#1e1e1e; border-radius:4px; padding:2px 7px;"),
                                     muscles_display),
                              if (nchar(ex_cat) > 0)
                                span(style = paste0(
                                       "font-size:10px; color:#777;",
                                       "background:#1a1a1a; border-radius:4px; padding:2px 7px;"),
                                     tools::toTitleCase(gsub("_", " ", ex_cat)))
                          ),
                          div(style = "font-size:11px; color:#888; margin-top:4px;",
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
                                        we$id, we$exercise_id))
                      )
                  ),

                  # Coaching tip
                  if (!is.null(note_tip))
                    div(style = paste0(
                          "background:#061a12; border-left:2px solid #1D9E75;",
                          "border-radius:0 7px 7px 0; padding:7px 10px;",
                          "font-size:11px; color:#5DCAA5; margin-bottom:10px; line-height:1.4;"),
                        note_tip),

                  # Last performance reference + smart suggestion
                  {
                    target_reps_mid <- (we$rep_range_low + we$rep_range_high) / 2
                    suggested_wt    <- if (!is.null(last))
                      suggest_weight(last, target_reps_mid, wo$week_number)
                      else NA_real_
                    if (!is.null(last)) {
                      tagList(
                        div(style = "font-size:11px; color:#888; margin-bottom:2px;",
                            paste0("Last: ",
                                   if (!is.na(last$weight_lbs))
                                     paste0(last$weight_lbs, " lbs × ")
                                   else "BW × ",
                                   last$reps_completed, " reps",
                                   if (!is.na(last$rpe_actual))
                                     paste0(" @ RPE ", last$rpe_actual) else "")),
                        if (!is.na(suggested_wt) && suggested_wt > 0)
                          div(style = "font-size:11px; color:#5DCAA5; margin-bottom:8px;",
                              paste0("Suggested today: ", suggested_wt, " lbs × ",
                                     we$rep_range_low, "–", we$rep_range_high, " reps"))
                        else
                          div(style = "margin-bottom:8px;")
                      )
                    }
                  },

                  # Warmup sets note
                  if (!is.null(we$warmup_sets) && !is.na(we$warmup_sets) && we$warmup_sets > 0)
                    div(style = "font-size:11px; color:#777; margin-bottom:8px;",
                        paste0(we$warmup_sets, " warm-up set(s) before working sets")),

                  # Drop set notice
                  if (!is.null(we$set_type) && !is.na(we$set_type) && we$set_type == "drop_set")
                    div(style = paste0(
                          "background:#1a1200; border-left:2px solid #FF9800;",
                          "border-radius:0 7px 7px 0; padding:6px 10px;",
                          "font-size:11px; color:#FF9800; margin-bottom:8px;"),
                        "Last set: DROP SET — reduce weight ~50% and push for max reps"),

                  # ── Set logging grid ─────────────────────────────────
                  div(
                    # Column headers
                    div(style = paste0(
                          "display:grid;",
                          "grid-template-columns:24px 1fr 1fr 50px 34px;",
                          "gap:4px; padding:0 2px 5px; border-bottom:1px solid #222;",
                          "margin-bottom:5px;"),
                        div(style = "font-size:9px; color:#666; text-transform:uppercase; text-align:center;", "SET"),
                        div(style = "font-size:9px; color:#666; text-transform:uppercase;", "WEIGHT (lbs)"),
                        div(style = "font-size:9px; color:#666; text-transform:uppercase;", "REPS"),
                        div(style = "font-size:9px; color:#666; text-transform:uppercase;", "RPE"),
                        div()
                    ),

                    # Set rows — each set has its own notes input below it
                    # so notes are *per-set*, not shared across the exercise.
                    lapply(seq_len(we$prescribed_sets), function(s) {
                      set_key   <- paste0(we$id, "_s", s)
                      note_key  <- paste0("note_", we$id, "_s", s)
                      log_entry <- if (s <= length(we_logs)) we_logs[[s]] else NULL
                      # editing=TRUE re-opens a previously logged set for correction —
                      # the green ✓ button toggles this on, log_set toggles it off.
                      is_editing <- isTRUE(tryCatch(log_entry$editing, error = \(e) FALSE))
                      is_logged <- !is.null(log_entry) && !is_editing
                      is_drop   <- !is.null(we$set_type) && !is.na(we$set_type) &&
                                   we$set_type == "drop_set" && s == we$prescribed_sets

                      # Per-set note: defaults to that set's saved note (or the
                      # most recent set's note as a pre-fill for the next set).
                      set_note_default <- tryCatch({
                        raw <- if (is_logged || is_editing) log_entry$notes
                          else if (s > 1 && length(we_logs) >= s - 1) we_logs[[s-1]]$notes
                          else NULL
                        n <- as.character(raw %||% "")
                        if (n %in% c("", "NA", "{}", "[]", "null")) "" else n
                      }, error = \(e) "")

                      # When editing a previously logged set, defaults must
                      # come from the stored values, NOT from a fresh
                      # suggestion — otherwise the lifter can't correct a typo
                      # without retyping everything from scratch.
                      def_weight <- if (is_logged || is_editing) log_entry$weight_lbs
                        else if (is_drop && s > 1 && length(we_logs) >= s - 1) {
                          # Pre-fill drop set at ~50% of previous set
                          prev_w <- we_logs[[s-1]]$weight_lbs
                          if (!is.null(prev_w) && !is.na(prev_w)) round(prev_w * 0.5 / 2.5) * 2.5
                          else NA
                        }
                        else if (s > 1 && length(we_logs) >= s - 1) we_logs[[s-1]]$weight_lbs
                        else if (!is.null(last)) {
                          # First set with no current data: use Nippard-style
                          # suggestion derived from e1RM × block RIR target.
                          tr_mid <- (we$rep_range_low + we$rep_range_high) / 2
                          sugg   <- suggest_weight(last, tr_mid, wo$week_number)
                          if (!is.na(sugg) && sugg > 0) sugg else last$weight_lbs
                        }
                        else NA
                      def_reps <- if (is_logged || is_editing) log_entry$reps_completed
                        else if (!is.null(last)) last$reps_completed
                        else we$rep_range_low
                      def_rpe  <- tryCatch({
                        raw <- if (is_logged || is_editing) log_entry$rpe_actual
                          else if (s > 1 && length(we_logs) >= s - 1) we_logs[[s-1]]$rpe_actual
                          else if (!is.null(last) && !is.na(last$rpe_actual)) last$rpe_actual
                          else NA
                        if (!is.null(raw) && length(raw) > 0 && !is.na(raw))
                          as.integer(round(as.numeric(raw))) else NA
                      }, error = \(e) NA)

                      input_bg  <- if (is_logged) "#071a10" else if (is_drop) "#1a1200" else "#0d0d0d"
                      input_bdr <- if (is_logged) "#0F6E56" else if (is_drop) "#FF9800" else "#1e1e1e"

                      tagList(
                      div(style = paste0(
                            "display:grid;",
                            "grid-template-columns:24px 1fr 1fr 50px 34px;",
                            "gap:4px; align-items:center;",
                            "padding:4px 2px; border-radius:7px; margin-bottom:3px;",
                            if (is_logged) " background:#071a10;"
                            else if (is_drop) " background:#1a1200;"
                            else ""),

                          # Set number (shows "DROP" for drop set row)
                          div(style = paste0(
                                "font-size:10px; font-weight:700; text-align:center; ",
                                if (is_logged && is_drop) "color:#FF9800;"
                                else if (is_logged) "color:#1D9E75;"
                                else if (is_drop) "color:#FF9800;"
                                else "color:#777;"),
                              if (is_drop) "DROP" else s),

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
                          # Completed = filled green ✓ (tap to edit)
                          # Incomplete = empty grey circle (tap to log)
                          if (is_logged)
                            tags$button(
                              "✓",
                              title = "Tap to edit",
                              style = paste0(
                                "background:#1D9E75; color:#fff; border:none;",
                                "border-radius:7px; font-size:17px; font-weight:700;",
                                "cursor:pointer;",
                                "width:34px; height:34px; display:flex;",
                                "align-items:center; justify-content:center; line-height:1;"),
                              onclick = sprintf(
                                "Shiny.setInputValue('edit_set','%s|%d',{priority:'event'})",
                                we$id, s))
                          else
                            tags$button(
                              "○",
                              style = paste0(
                                "background:#1a1a1a; color:#555; border:1.5px solid #333;",
                                "border-radius:7px; font-size:18px; cursor:pointer;",
                                "width:34px; height:34px; display:flex;",
                                "align-items:center; justify-content:center; line-height:1;"),
                              onclick = sprintf(
                                "Shiny.setInputValue('log_set','%s|%d',{priority:'event'})",
                                we$id, s))
                      ),
                      # Per-set notes input — small, inline, one per set.
                      # localStorage-backed so reconnects don't wipe a draft.
                      div(style = "margin:0 0 6px 28px;",
                          tags$input(
                            type        = "text",
                            id          = note_key,
                            value       = set_note_default,
                            placeholder = sprintf("Set %d note (optional)...", s),
                            style       = paste0(
                              "background:#0d0d0d; border:1px solid #1a1a1a;",
                              "color:#aaa; border-radius:6px; padding:5px 8px;",
                              "font-size:11px; width:calc(100% - 28px); box-sizing:border-box;",
                              "font-family:inherit;")
                          ),
                          tags$script(HTML(sprintf("
(function() {
  var key = 'catrack_note_%s_s%d';
  var inp = document.getElementById('%s');
  if (!inp) return;
  if (inp.value === '') {
    var saved = localStorage.getItem(key);
    if (saved) inp.value = saved;
  }
  inp.addEventListener('input', function() {
    localStorage.setItem(key, inp.value);
  });
})();
", we$id, s, note_key)))
                      )
                      )  # end tagList wrapping set row + note
                    })
                  ),  # end set grid

                  # Exercise history (collapsible, lazy-loaded)
                  {
                    hist <- history_map[[we$exercise_id]]
                    if (!is.null(hist) && is.data.frame(hist) && nrow(hist) > 0) {
                      div(style = "margin-top:8px;",
                          # open = TRUE renders <details open> so history shows immediately
                          tags$details(open = TRUE,
                            tags$summary(
                              style = paste0(
                                "font-size:11px; color:#888; cursor:pointer; padding:3px 0;",
                                "list-style:none; -webkit-user-select:none; user-select:none;"),
                              "History"),
                            div(style = "margin-top:6px; display:flex; flex-direction:column; gap:4px;",
                                lapply(seq_len(nrow(hist)), function(h) {
                                  r <- hist[h, ]
                                  has_note <- !is.na(r$note) && nchar(r$note) > 0
                                  div(style = paste0(
                                        "padding:5px 7px; background:#0d0d0d; border-radius:5px;",
                                        "font-size:11px;"),
                                      div(style = "display:flex; justify-content:space-between;",
                                          span(style = "color:#777;", format(r$date, "%b %d")),
                                          span(style = "color:#aaa;",
                                               paste0(if (!is.na(r$wt)) paste0(r$wt, " lbs") else "BW",
                                                      " × ", r$reps, " reps",
                                                      if (!is.na(r$rpe)) paste0("  RPE ", r$rpe) else "",
                                                      if (r$n_sets > 1) paste0("  (", r$n_sets, " sets)") else ""))),
                                      if (has_note)
                                        div(style = "font-size:10px; color:#666; font-style:italic; margin-top:2px;",
                                            r$note)
                                  )
                                })
                            )
                          )
                      )
                    } else if (is.null(hist)) {
                      div(style = "margin-top:8px;",
                          tags$button(
                            "History",
                            style = paste0(
                              "background:none; border:1px solid #333;",
                              "border-radius:6px; padding:5px 10px;",
                              "font-size:11px; color:#777; cursor:pointer;"),
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
            "width:100%; max-width:480px; padding:24px 24px 32px;",
            "border-top:1px solid #222;"),

          tags$script(HTML(sprintf("
            window._swapWeId   = '%s';
            window._swapExId   = null;
            window._swapScope  = 'session';

            function swapSelectEx(el, exId) {
              document.querySelectorAll('.swap-card').forEach(function(c) {
                c.style.border = '1px solid #242424';
                c.style.background = '#1a1a1a';
              });
              el.style.border = '1px solid #1D9E75';
              el.style.background = '#0a1f16';
              window._swapExId = exId;
              document.getElementById('do-swap-btn').style.opacity = '1';
              document.getElementById('do-swap-btn').disabled = false;
            }

            function swapSelectScope(scope) {
              window._swapScope = scope;
              var btns = document.querySelectorAll('.swap-scope-btn');
              btns.forEach(function(b) {
                b.style.background   = '#1e1e1e';
                b.style.color        = '#888';
                b.style.border       = '1px solid #333';
                b.style.fontWeight   = '400';
              });
              var active = document.getElementById('swap-scope-' + scope);
              if (active) {
                active.style.background  = '#0a1f16';
                active.style.color       = '#1D9E75';
                active.style.border      = '1px solid #1D9E75';
                active.style.fontWeight  = '700';
              }
            }

            function doSwap() {
              if (!window._swapExId) return;
              Shiny.setInputValue('confirm_swap',
                window._swapWeId + '|' + window._swapExId + '|' + window._swapScope,
                {priority:'event'});
            }
            // Init scope button state
            setTimeout(function() { swapSelectScope('session'); }, 50);
          ", we_id))),

          div(style = "width:40px; height:4px; background:#333; border-radius:2px; margin:0 auto 20px;"),

          div(style = "font-size:17px; font-weight:700; margin-bottom:4px; color:#f0f0f0;",
              "Swap Exercise"),
          div(style = "font-size:12px; color:#555; margin-bottom:16px;",
              "Pick a replacement, choose scope, then confirm."),

          if (is.null(suggestions)) {
            div(style = "color:#444; text-align:center; padding:24px;",
                "Finding substitutes...")
          } else if (length(suggestions) == 0) {
            div(style = "color:#444; text-align:center; padding:24px;",
                "No substitutes found with your equipment.")
          } else {
            tagList(
              # Selectable exercise cards
              lapply(seq_along(suggestions), function(i) {
                s           <- suggestions[[i]]
                muscles_str <- tryCatch(
                  tools::toTitleCase(gsub("_", " ",
                    paste(unlist(s$primary_muscles), collapse = ", "))),
                  error = \(e) "")
                match_label <- if (i == 1) "BEST MATCH" else if (i == 2) "ALTERNATIVE" else "OPTION"
                label_color <- if (i == 1) "#1D9E75" else "#888"
                div(
                  class   = "swap-card",
                  style   = paste0(
                    "background:#1a1a1a; border:1px solid #242424;",
                    "border-radius:12px; padding:14px 16px; margin-bottom:8px;",
                    "cursor:pointer; transition:border 0.15s, background 0.15s;"),
                  onclick = sprintf("swapSelectEx(this,'%s')", s$id),
                  div(style = "display:flex; justify-content:space-between; align-items:center;",
                      div(style = "font-size:14px; font-weight:600; color:#f0f0f0;", s$name),
                      div(style = sprintf(
                            "font-size:10px; color:%s; font-weight:700; letter-spacing:0.07em;",
                            label_color),
                          match_label)
                  ),
                  div(style = "font-size:11px; color:#555; margin-top:3px;",
                      paste0(muscles_str,
                             " · ", s$default_rep_range_low %||% 8,
                             "–", s$default_rep_range_high %||% 12, " reps"))
                )
              }),

              # Scope toggle
              div(style = "margin:16px 0 12px;",
                  div(style = "font-size:11px; color:#555; text-transform:uppercase;
                               letter-spacing:0.07em; margin-bottom:8px;",
                      "Apply swap to"),
                  div(style = "display:flex; gap:8px;",
                      tags$button(
                        "This session",
                        id    = "swap-scope-session",
                        class = "swap-scope-btn",
                        style = paste0(
                          "flex:1; border-radius:8px; padding:10px;",
                          "font-size:13px; cursor:pointer;"),
                        onclick = "swapSelectScope('session')"),
                      tags$button(
                        "Rest of block",
                        id    = "swap-scope-block",
                        class = "swap-scope-btn",
                        style = paste0(
                          "flex:1; border-radius:8px; padding:10px;",
                          "font-size:13px; cursor:pointer;"),
                        onclick = "swapSelectScope('block')")
                  )
              ),

              # Confirm button
              tags$button(
                "Confirm Swap",
                id    = "do-swap-btn",
                style = paste0(
                  "width:100%; background:#1D9E75; color:#fff; border:none;",
                  "border-radius:12px; padding:14px; font-size:15px;",
                  "font-weight:700; cursor:pointer; margin-bottom:10px;",
                  "opacity:0.35;"),
                disabled = NA,
                onclick  = "doSwap()"),

              tags$button(
                "Cancel",
                style = paste0(
                  "width:100%; background:none; color:#ccc; border:1px solid #333;",
                  "border-radius:10px; padding:10px; font-size:13px; cursor:pointer;"),
                onclick = "Shiny.setInputValue('cancel_swap', 1, {priority:'event'})")
            )
          }
      )
  )
}

# ── WORKOUT PREVIEW UI ───────────────────────────────────────
# Read-only view of an upcoming session. Shown when the user taps
# an incomplete session card on the dashboard. From here they can
# choose Start Session (→ logging screen) or Back.
workout_preview_ui <- function(workout, exercises) {
  if (is.null(workout) || is.null(exercises))
    return(div(style = "padding:40px; text-align:center; color:#555;",
               "Loading preview..."))

  wo   <- if (is.data.frame(workout)) workout[1, ] else workout
  n_ex <- nrow(exercises)

  # ── Estimated duration ─────────────────────────────────────
  est_min <- tryCatch({
    total_sec <- sum(vapply(seq_len(n_ex), function(i) {
      n_sets <- as.integer(exercises$prescribed_sets[i] %||% 3L)
      rest   <- as.integer(exercises$rest_seconds[i]    %||% 120L)
      warm   <- as.integer(exercises$warmup_sets[i]     %||% 0L)
      set_time  <- 45L + rest       # ~45s execute + rest
      warm_time <- 30L + 60L
      n_sets * set_time + warm * warm_time
    }, numeric(1)))
    round(total_sec / 60)
  }, error = \(e) NA)

  # ── Muscle stimulus tally (primary muscles × prescribed sets) ──
  muscle_tally <- list()
  for (i in seq_len(n_ex)) {
    n_sets  <- as.integer(exercises$prescribed_sets[i] %||% 0L)
    ex_info <- tryCatch(exercises$exercises[i, ], error = \(e) NULL)
    if (is.null(ex_info)) next
    prim <- tryCatch(unlist(ex_info$primary_muscles), error = \(e) character(0))
    for (m in prim)
      muscle_tally[[m]] <- (muscle_tally[[m]] %||% 0L) + n_sets
  }
  if (length(muscle_tally) > 0)
    muscle_tally <- muscle_tally[order(-unlist(muscle_tally))]

  tagList(
    # Top nav
    div(style = paste0("display:flex; align-items:center; gap:10px;",
                       "margin-bottom:14px; padding:0 2px;"),
        tags$button("←",
          style = paste0("background:#1e1e1e; border:none; border-radius:10px;",
                         "width:38px; height:38px; font-size:17px; color:#aaa;",
                         "cursor:pointer; flex-shrink:0;"),
          onclick = "Shiny.setInputValue('close_preview', Math.random(), {priority:'event'})"),
        div(style = "flex:1; text-align:center;",
            div(style = "font-size:10px; color:#444; text-transform:uppercase; letter-spacing:0.07em;",
                paste0("Week ", wo$week_number, " · Day ", wo$session_number,
                       " · Preview")),
            div(style = "font-size:15px; font-weight:700; color:#f0f0f0; line-height:1.2;",
                wo$session_label)),
        div(style = "width:38px;")
    ),

    # Stats grid
    div(style = "display:grid; grid-template-columns:1fr 1fr 1fr; gap:8px;
                 margin-bottom:14px;",
        div(style = "background:#161616; border:1px solid #222; border-radius:10px;
                     padding:12px 8px; text-align:center;",
            div(style = "font-size:18px; font-weight:700; color:#1D9E75;",
                if (!is.na(est_min)) paste0(est_min, " min") else "—"),
            div(style = "font-size:10px; color:#555; text-transform:uppercase;
                         letter-spacing:0.06em; margin-top:2px;", "Est. duration")),
        div(style = "background:#161616; border:1px solid #222; border-radius:10px;
                     padding:12px 8px; text-align:center;",
            div(style = "font-size:18px; font-weight:700; color:#f0f0f0;",
                sum(as.integer(exercises$prescribed_sets), na.rm = TRUE)),
            div(style = "font-size:10px; color:#555; text-transform:uppercase;
                         letter-spacing:0.06em; margin-top:2px;", "Total sets")),
        div(style = "background:#161616; border:1px solid #222; border-radius:10px;
                     padding:12px 8px; text-align:center;",
            div(style = "font-size:18px; font-weight:700; color:#f0f0f0;", n_ex),
            div(style = "font-size:10px; color:#555; text-transform:uppercase;
                         letter-spacing:0.06em; margin-top:2px;", "Exercises"))
    ),

    # Muscle stimulus
    if (length(muscle_tally) > 0)
      div(style = "background:#161616; border:1px solid #222; border-radius:10px;
                   padding:12px 14px; margin-bottom:14px;",
          div(class = "ct-section-title", "MUSCLE STIMULUS (working sets)"),
          div(style = "display:flex; flex-wrap:wrap; gap:5px;",
              lapply(names(muscle_tally), function(m)
                span(style = paste0("font-size:11px; color:#5DCAA5;",
                                     "background:#061a12; border:1px solid #0F6E56;",
                                     "border-radius:5px; padding:3px 8px;"),
                     paste0(tools::toTitleCase(gsub("_", " ", m)),
                            " · ", muscle_tally[[m]]))
              )
          )
      ),

    # Exercise list (read-only)
    lapply(seq_len(n_ex), function(i) {
      we      <- exercises[i, ]
      ex_info <- tryCatch(we$exercises, error = \(e) NULL)
      ex_name <- if (!is.null(ex_info) && !is.null(ex_info$name))
                   ex_info$name else paste("Exercise", i)
      note <- tryCatch({
        n <- ex_info$coaching_note
        if (!is.na(n) && nchar(n) > 0) n else NULL
      }, error = \(e) NULL)
      prim_raw <- tryCatch(unlist(ex_info$primary_muscles), error = \(e) character(0))
      muscles  <- tools::toTitleCase(paste(gsub("_", " ", head(prim_raw, 2)),
                                            collapse = ", "))
      ss <- tryCatch(we$superset_group, error = \(e) NA)
      is_ss <- !is.null(ss) && length(ss) == 1 && !is.na(ss) && nchar(as.character(ss)) > 0

      div(style = paste0("background:#141414; border:1px solid #1c1c1c;",
                         "border-radius:12px; padding:12px 14px; margin-bottom:8px;"),
          div(style = "display:flex; gap:10px; align-items:flex-start;",
              div(style = "flex:1; min-width:0;",
                  div(style = "display:flex; align-items:center; gap:6px; flex-wrap:wrap;",
                      div(style = "font-size:14px; font-weight:700; color:#f0f0f0;",
                          ex_name),
                      if (is_ss)
                        span(style = paste0("font-size:9px; color:#5DCAA5;",
                                             "background:#061a12; border:1px solid #0F6E56;",
                                             "border-radius:4px; padding:2px 5px;"),
                             paste0("Superset ", ss))
                  ),
                  if (nchar(muscles) > 0)
                    div(style = "font-size:10px; color:#888; margin-top:2px;", muscles),
                  div(style = "font-size:11px; color:#5DCAA5; margin-top:4px;",
                      paste0(we$prescribed_sets, " × ",
                             we$rep_range_low, "–", we$rep_range_high, " reps",
                             "  ·  RPE ", we$rpe_target,
                             "  ·  Rest ",
                             if (we$rest_seconds >= 60)
                               paste0(round(we$rest_seconds / 60, 1), " min")
                             else paste0(we$rest_seconds, "s")))
              )
          ),
          if (!is.null(note))
            div(style = paste0("background:#061a12; border-left:2px solid #1D9E75;",
                               "border-radius:0 6px 6px 0; padding:6px 10px;",
                               "font-size:11px; color:#5DCAA5; margin-top:8px;",
                               "line-height:1.4;"),
                note)
      )
    }),

    # Action buttons
    div(style = "margin-top:20px; padding-bottom:32px;",
        tags$button("Start Session →",
          style = paste0("width:100%; background:#1D9E75; color:#fff; border:none;",
                         "border-radius:14px; padding:16px; font-size:16px;",
                         "font-weight:700; cursor:pointer; letter-spacing:0.02em;",
                         "box-shadow:0 4px 20px rgba(29,158,117,0.35);"),
          onclick = sprintf(
            "Shiny.setInputValue('start_from_preview','%s',{priority:'event'})",
            wo$id)),
        tags$button("← Back to Calendar",
          style = paste0("width:100%; background:none; color:#555; border:none;",
                         "border-radius:12px; padding:12px; font-size:13px;",
                         "cursor:pointer; margin-top:8px;"),
          onclick = "Shiny.setInputValue('close_preview', Math.random(), {priority:'event'})")
    )
  )
}

# ── WORKOUT SERVER LOGIC ─────────────────────────────────────
setup_workout_server <- function(input, output, session, rv) {

  # ── Preview an upcoming workout ─────────────────────────────
  observeEvent(input$open_preview, {
    rv$preview_workout_id <- input$open_preview
    rv$page    <- "preview"
    rv$nav_tab <- "dashboard"

    withProgress(message = "Loading preview...", value = 0.5, {
      data <- tryCatch({
        wo <- sb_select("workouts",
          sprintf("?id=eq.%s", rv$preview_workout_id), token = rv$token)
        we <- sb_select("workout_exercises",
          sprintf("?workout_id=eq.%s&select=*,exercises(*)&order=exercise_order",
                  rv$preview_workout_id), token = rv$token)
        list(workout = wo, exercises = we)
      }, error = \(e) { message("Preview load error: ", e$message); NULL })

      if (!is.null(data)) {
        rv$preview_workout   <- data$workout
        rv$preview_exercises <- data$exercises
      }
    })
  })

  # ── Close preview → back to dashboard ───────────────────────
  observeEvent(input$close_preview, {
    rv$preview_workout_id <- NULL
    rv$preview_workout    <- NULL
    rv$preview_exercises  <- NULL
    rv$page    <- "dashboard"
    rv$nav_tab <- "dashboard"
  })

  # ── Start session from preview ──────────────────────────────
  # Clears preview state, then immediately runs the open_workout logic.
  # Done inline (rather than re-firing open_workout) to keep it synchronous.
  observeEvent(input$start_from_preview, {
    wid <- input$start_from_preview
    if (is.null(wid) || nchar(wid) == 0) return()

    rv$preview_workout_id <- NULL
    rv$preview_workout    <- NULL
    rv$preview_exercises  <- NULL

    rv$active_workout_id <- wid
    rv$page    <- "workout"
    rv$nav_tab <- "log"
    session$sendCustomMessage("reset_session_timer", list())
    rv$session_start_time <- Sys.time()

    withProgress(message = "Loading session...", value = 0.5, {
      data <- tryCatch({
        wo <- sb_select("workouts",
          sprintf("?id=eq.%s", wid), token = rv$token)
        we <- sb_select("workout_exercises",
          sprintf("?workout_id=eq.%s&select=*,exercises(*)&order=exercise_order",
                  wid), token = rv$token)
        list(workout = wo, exercises = we)
      }, error = \(e) { message("Start session load error: ", e$message); NULL })

      if (!is.null(data)) {
        rv$active_workout   <- data$workout
        rv$active_exercises <- data$exercises
        rv$last_perf_map    <- list()
        if (!is.null(data$exercises)) {
          for (i in seq_len(nrow(data$exercises))) {
            eid  <- data$exercises$exercise_id[i]
            last <- tryCatch(
              fetch_last_performance(eid, rv$user_id, rv$token),
              error = \(e) NULL)
            if (!is.null(last) && nrow(last) > 0)
              rv$last_perf_map[[eid]] <- last
          }
        }
      }

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
              id             = row$id,
              weight_lbs     = row$weight_lbs,
              reps_completed = row$reps_completed,
              rpe_actual     = row$rpe_actual,
              notes          = row$notes,
              set_number     = set_n
            )
          }
        }
      }

      rv$swap_we_id       <- NULL
      rv$swap_ex_id       <- NULL
      rv$swap_suggestions <- NULL
    })
  })

  # Note: there's no standalone open_workout observer anymore — every
  # entry point goes through open_preview → start_from_preview, which
  # handles all the load logic above. If a future caller needs to skip
  # the preview, add an observer here that delegates to the same load.

  # ── Close workout ────────────────────────────────────────────
  observeEvent(input$close_workout, {
    rv$page    <- "dashboard"
    rv$nav_tab <- "dashboard"
    rv$active_workout_id <- NULL
  })

  # ── Edit a logged set: flip it back into input mode ─────────
  # Triggered by tapping the green ✓ button on a logged set row.
  # The set retains its server id so the subsequent log_set call
  # PATCHes the existing row instead of inserting a duplicate.
  observeEvent(input$edit_set, {
    parts <- strsplit(input$edit_set, "\\|")[[1]]
    if (length(parts) < 2) return()
    we_id   <- parts[1]
    set_num <- as.integer(parts[2])
    current <- rv$set_logs[[we_id]] %||% list()
    if (length(current) >= set_num && !is.null(current[[set_num]])) {
      current[[set_num]]$editing <- TRUE
      rv$set_logs[[we_id]] <- current
    }
  })

  # ── Log (or update) a set ────────────────────────────────────
  # If the local entry already has an id, we PATCH that row.
  # Otherwise INSERT a new row and capture its id from the response.
  observeEvent(input$log_set, {
    parts   <- strsplit(input$log_set, "\\|")[[1]]
    if (length(parts) < 2) return()
    we_id   <- parts[1]
    set_num <- as.integer(parts[2])
    set_key  <- paste0("w_",   we_id, "_s", set_num)
    reps_key <- paste0("r_",   we_id, "_s", set_num)
    rpe_key  <- paste0("rpe_", we_id, "_s", set_num)
    # Per-set note key — falls back to the legacy per-exercise field
    # for any saved drafts still pending in localStorage.
    note_key        <- paste0("note_", we_id, "_s", set_num)
    legacy_note_key <- paste0("note_", we_id)

    weight <- tryCatch(as.numeric(input[[set_key]]),   error = \(e) NA)
    reps   <- tryCatch({
      v <- as.numeric(input[[reps_key]])
      if (is.na(v)) NA_integer_ else as.integer(round(v))
    }, error = \(e) NA_integer_)
    rpe    <- tryCatch({
      v <- as.numeric(input[[rpe_key]])
      if (is.na(v)) NA_integer_ else as.integer(min(10L, max(0L, round(v))))
    }, error = \(e) NA_integer_)
    notes  <- input[[note_key]] %||% input[[legacy_note_key]] %||% ""

    if (is.na(reps) || reps <= 0) {
      showNotification("Please enter reps before logging.", type = "warning")
      return()
    }

    # Check for an existing entry (edit case): if it has an id,
    # PATCH that row instead of inserting a duplicate.
    existing    <- if (length(rv$set_logs[[we_id]]) >= set_num)
                     rv$set_logs[[we_id]][[set_num]] else NULL
    existing_id <- tryCatch(existing$id, error = \(e) NULL)

    log_row <- list(
      workout_exercise_id = we_id,
      user_id             = rv$user_id,
      set_number          = as.integer(set_num),
      weight_lbs          = if (is.na(weight)) NULL else weight,
      reps_completed      = as.integer(reps),
      rpe_actual          = if (is.na(rpe)) NULL else as.integer(rpe),
      is_warmup           = FALSE,
      notes               = if (nchar(notes) > 0) notes else NULL
    )

    is_edit <- !is.null(existing_id) && nchar(as.character(existing_id)) > 0
    resp    <- if (is_edit)
                 sb_update("workout_set_logs",
                           sprintf("?id=eq.%s", existing_id),
                           log_row, token = rv$token)
               else
                 sb_insert("workout_set_logs", log_row, token = rv$token)

    if (resp$status_code %in% c(200, 201, 204)) {
      # On insert, capture the new row id so future taps can edit it
      if (!is_edit) {
        new_id <- tryCatch({
          body <- fromJSON(resp_body_string(resp), simplifyDataFrame = TRUE)
          if (is.data.frame(body)) body$id[1] else body[[1]]$id
        }, error = \(e) NULL)
        log_row$id <- new_id
      } else {
        log_row$id <- existing_id
      }
      log_row$editing <- FALSE

      current             <- rv$set_logs[[we_id]] %||% list()
      current[[set_num]]  <- log_row
      rv$set_logs[[we_id]] <- current

      # Only start the rest timer for fresh logs — edits don't need a rest
      if (!is_edit && !is.null(rv$active_exercises)) {
        we_row <- rv$active_exercises[rv$active_exercises$id == we_id, ]
        if (nrow(we_row) > 0) {
          rest_s <- we_row$rest_seconds[1]
          session$sendCustomMessage("start_rest_timer", list(seconds = rest_s))
        }
      }

      showNotification(
        paste0("Set ", set_num,
               if (is_edit) " updated ✓" else " logged ✓"),
        type = "message", duration = 2)
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

    completed_at_str <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
    update_data <- list(completed_at = completed_at_str)
    if (!is.na(duration_mins)) update_data$duration_minutes <- duration_mins

    resp <- sb_update("workouts",
                      sprintf("?id=eq.%s", rv$active_workout_id),
                      update_data,
                      token = rv$token)

    message(sprintf("Finish session response: %d", resp$status_code))

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

    # Build summary data before clearing in-memory state
    rv$summary_data <- list(
      workout_id    = rv$active_workout_id,
      workout       = rv$active_workout,
      exercises     = rv$active_exercises,
      set_logs      = rv$set_logs,
      duration_mins = duration_mins,
      completed_at  = completed_at_str,
      notes         = ""
    )

    rv$active_workout_id <- NULL
    rv$active_workout    <- NULL
    rv$active_exercises  <- NULL
    rv$set_logs          <- list()
    rv$page              <- "summary"
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

    # Detect if swapping back to original exercise; if so, clear the SUBST badge
    orig_swap <- tryCatch(
      sb_select("exercise_swaps",
                sprintf("?workout_exercise_id=eq.%s&order=created_at.desc&limit=1", we_id),
                token = rv$token),
      error = \(e) NULL)
    is_back_to_original <- !is.null(orig_swap) && nrow(orig_swap) > 0 &&
      orig_swap$original_exercise_id[1] == new_ex_id

    sb_update("workout_exercises",
              sprintf("?id=eq.%s", we_id),
              list(exercise_id = new_ex_id, is_swapped = !is_back_to_original),
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
