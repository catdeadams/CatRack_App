# ============================================================
# workout_screen.R — CaTrack
# Active session screen: per-set logging, rest timer,
# last-session pre-fill, RPE, notes, exercise swap.
# Source this from global.R
# ============================================================

# ── REST TIMER JS ────────────────────────────────────────────
rest_timer_js <- "
var catrackTimer = null;
var catrackSeconds = 0;

function startRestTimer(seconds) {
  clearInterval(catrackTimer);
  catrackSeconds = seconds;
  updateTimerDisplay();
  catrackTimer = setInterval(function() {
    catrackSeconds--;
    updateTimerDisplay();
    if (catrackSeconds <= 0) {
      clearInterval(catrackTimer);
      document.getElementById('rest-timer-bar').style.width = '0%';
      document.getElementById('rest-timer-display').innerText = 'Rest done!';
      document.getElementById('rest-timer-display').style.color = '#4ade80';
    }
  }, 1000);
}

function updateTimerDisplay() {
  var m = Math.floor(catrackSeconds / 60);
  var s = catrackSeconds % 60;
  var pct = 100;
  document.getElementById('rest-timer-display').innerText =
    m + ':' + (s < 10 ? '0' : '') + s;
  document.getElementById('rest-timer-display').style.color = '#e8ff47';
}

function stopTimer() {
  clearInterval(catrackTimer);
  document.getElementById('rest-timer-display').innerText = 'Resting...';
}
"

# ── FETCH WORKOUT DATA ───────────────────────────────────────
fetch_workout_data <- function(workout_id, user_id, token) {
  # Get workout info
  workout <- sb_select("workouts",
                       sprintf("?id=eq.%s&user_id=eq.%s", workout_id, user_id),
                       token = token)
  
  if (is.null(workout)) return(NULL)
  
  # Get prescribed exercises with exercise details
  we <- sb_select("workout_exercises",
                  sprintf("?workout_id=eq.%s&select=*,exercises(*)&order=exercise_order",
                          workout_id),
                  token = token)
  
  if (is.null(we)) return(list(workout = workout, exercises = NULL, last_logs = NULL))
  
  # Get last logged set for each exercise (for pre-filling)
  ex_ids <- paste(we$exercise_id, collapse = ",")
  last_logs <- sb_select("workout_set_logs",
                         sprintf("?user_id=eq.%s&workout_exercise_id=in.(%s)&select=*,workout_exercises(exercise_id)&order=logged_at.desc",
                                 user_id, paste(we$id, collapse = ",")),
                         token = token)
  
  # Also fetch historical last performance for each exercise
  # (could be from any past session, not just this workout)
  hist_logs <- list()
  for (eid in we$exercise_id) {
    # Get the most recent set logged for this exercise by this user
    h <- sb_select("workout_set_logs",
                   sprintf(paste0("?user_id=eq.%s",
                                  "&workout_exercise_id=in.(%s)",
                                  "&is_warmup=eq.false",
                                  "&select=weight_lbs,reps_completed,rpe_actual,notes,logged_at",
                                  "&order=logged_at.desc&limit=5"),
                           user_id,
                           # Get workout_exercise IDs where exercise_id matches
                           sprintf("(select id from workout_exercises where exercise_id=eq.%s)", eid)),
                   token = token)
    if (!is.null(h)) hist_logs[[eid]] <- h
  }
  
  list(workout = workout, exercises = we, last_logs = last_logs, hist_logs = hist_logs)
}

# Simpler last-performance fetch that actually works with Supabase REST
fetch_last_performance <- function(exercise_id, user_id, token) {
  # Use the last_exercise_log view we created in the schema
  sb_select("last_exercise_log",
            sprintf("?user_id=eq.%s&exercise_id=eq.%s", user_id, exercise_id),
            token = token)
}

# ── WORKOUT SCREEN UI ────────────────────────────────────────
workout_screen_ui <- function(workout, exercises, last_perf_map,
                              set_logs_rv, timer_active) {
  if (is.null(workout) || is.null(exercises)) {
    return(div(style = "padding:40px; text-align:center; color:#555;",
               "Loading session..."))
  }
  
  wo        <- if (is.data.frame(workout)) workout[1, ] else workout
  n_ex      <- nrow(exercises)
  completed <- sum(sapply(seq_len(n_ex), function(i) {
    we_id <- exercises$id[i]
    logs  <- set_logs_rv[[we_id]]
    !is.null(logs) && length(logs) >= exercises$prescribed_sets[i]
  }))
  
  pct <- if (n_ex > 0) round(100 * completed / n_ex) else 0L
  
  tagList(
    tags$head(tags$script(HTML(rest_timer_js))),
    
    # ── Session header ─────────────────────────────────────
    div(style = "display:flex; align-items:center; gap:12px; margin-bottom:16px;",
        tags$button("←", class = "ct-btn-secondary ct-btn-sm",
                    style = "width:36px; padding:8px; font-size:16px;",
                    onclick = "Shiny.setInputValue('close_workout', Math.random(), {priority:'event'})"),
        div(style = "flex:1;",
            div(style = "font-size:11px; color:#666; text-transform:uppercase;
                     letter-spacing:0.06em;",
                paste0("Week ", wo$week_number, " · Day ", wo$session_number)),
            div(style = "font-size:17px; font-weight:700;", wo$session_label)
        ),
        div(style = "text-align:right;",
            div(style = "font-size:18px; font-weight:700; color:#e8ff47;",
                paste0(pct, "%")),
            div(style = "font-size:10px; color:#555;",
                paste0(completed, "/", n_ex, " done"))
        )
    ),
    
    # Session progress bar
    div(class = "ct-progress-bar",
        div(class = "ct-progress-fill", style = sprintf("width:%d%%", pct))),
    
    # ── Rest timer ─────────────────────────────────────────
    div(id = "rest-timer-section",
        style = "background:#1a1a1a; border-radius:10px; padding:12px 16px;
                 margin:12px 0; display:flex; align-items:center; gap:12px;",
        div(style = "font-size:20px;", "⏱"),
        div(style = "flex:1;",
            div(id = "rest-timer-display",
                style = "font-size:22px; font-weight:700; color:#555;", "Resting..."),
            div(style = "background:#2a2a2a; border-radius:3px; height:3px; margin-top:4px;",
                div(id = "rest-timer-bar",
                    style = "height:100%; background:#e8ff47; border-radius:3px;
                       width:0%; transition:width 0.5s;"))
        ),
        tags$button("Skip", class = "ct-btn-secondary ct-btn-sm",
                    style = "width:50px; font-size:11px;",
                    onclick = "stopTimer()")
    ),
    
    # ── Exercise cards ──────────────────────────────────────
    lapply(seq_len(n_ex), function(i) {
      we       <- exercises[i, ]
      ex_info  <- tryCatch(we$exercises, error = \(e) NULL)
      ex_name  <- if (!is.null(ex_info) && !is.null(ex_info$name))
        ex_info$name else paste("Exercise", i)
      ex_cat   <- if (!is.null(ex_info) && !is.null(ex_info$category))
        ex_info$category else ""
      yt_url   <- if (!is.null(ex_info) && !is.null(ex_info$youtube_url) &&
                      !is.na(ex_info$youtube_url)) ex_info$youtube_url else NULL
      note_tip <- if (!is.null(ex_info) && !is.null(ex_info$coaching_note) &&
                      !is.na(ex_info$coaching_note)) ex_info$coaching_note else NULL
      
      # Last performance for pre-filling
      last <- last_perf_map[[we$exercise_id]]
      
      # Current set logs for this exercise
      we_logs <- set_logs_rv[[we$id]] %||% list()
      n_logged <- length(we_logs)
      is_complete <- n_logged >= we$prescribed_sets
      
      # Superset label
      ss_label <- if (!is.null(we$superset_group) && !is.na(we$superset_group) &&
                      nchar(we$superset_group) > 0)
        paste0(we$superset_group, " · ") else ""
      
      div(class = "ct-workout-card",
          style = paste0(
            "background:#1a1a1a; border:1.5px solid ",
            if (is_complete) "#4ade80" else "#2a2a2a",
            "; border-radius:12px; padding:14px; margin-bottom:10px;"),
          
          # Exercise header row
          div(style = "display:flex; justify-content:space-between;
                     align-items:flex-start; margin-bottom:10px;",
              div(style = "flex:1;",
                  div(style = "font-size:10px; color:#555; text-transform:uppercase;
                         letter-spacing:0.05em; margin-bottom:2px;",
                      paste0(ss_label, toupper(ex_cat))),
                  div(style = "font-size:15px; font-weight:700; color:#f0f0f0;",
                      ex_name,
                      if (is_complete) span(style="color:#4ade80; margin-left:6px;", "✓"))
              ),
              div(style = "display:flex; gap:6px; align-items:center;",
                  # YouTube demo button — window.open avoids blank Shiny window
                  if (!is.null(yt_url))
                    tags$button("▶ Demo",
                                style = "background:#2a2a2a; border:none; border-radius:6px;
                         padding:5px 8px; font-size:11px; color:#aaa; cursor:pointer;",
                                onclick = sprintf("window.open('%s','_blank'); return false;", yt_url)),
                  # Swap button
                  tags$button("⇄",
                              style = "background:#2a2a2a; border:none; border-radius:6px;
                       padding:5px 8px; font-size:14px; color:#aaa; cursor:pointer;",
                              title = "Swap exercise",
                              onclick = sprintf(
                                "Shiny.setInputValue('swap_exercise', '%s|%s', {priority:'event'})",
                                we$id, we$exercise_id))
              )
          ),
          
          # Prescription row
          div(style = "display:flex; gap:8px; margin-bottom:10px;",
              div(style = "background:#2a2a2a; border-radius:6px; padding:5px 10px;
                       font-size:11px; color:#aaa;",
                  paste0(we$prescribed_sets, " sets")),
              div(style = "background:#2a2a2a; border-radius:6px; padding:5px 10px;
                       font-size:11px; color:#aaa;",
                  paste0(we$rep_range_low, "–", we$rep_range_high, " reps")),
              div(style = "background:#2a2a2a; border-radius:6px; padding:5px 10px;
                       font-size:11px; color:#aaa;",
                  paste0("RPE ", we$rpe_target)),
              div(style = "background:#2a2a2a; border-radius:6px; padding:5px 10px;
                       font-size:11px; color:#aaa;",
                  paste0(round(we$rest_seconds / 60, 1), " min rest"))
          ),
          
          # Coaching tip
          if (!is.null(note_tip))
            div(style = "background:#1e2200; border-left:3px solid #e8ff47;
                       border-radius:0 6px 6px 0; padding:7px 10px;
                       font-size:11px; color:#999; margin-bottom:10px;
                       line-height:1.4;",
                note_tip),
          
          # Last session reference
          if (!is.null(last))
            div(style = "font-size:11px; color:#555; margin-bottom:8px;",
                paste0("Last time: ",
                       if (!is.na(last$weight_lbs)) paste0(last$weight_lbs, " lbs") else "BW",
                       " × ", last$reps_completed, " reps",
                       if (!is.na(last$rpe_actual)) paste0(" @ RPE ", last$rpe_actual) else "")),
          
          # Warmup sets note
          if (!is.null(we$warmup_sets) && !is.na(we$warmup_sets) && we$warmup_sets > 0)
            div(style = "font-size:11px; color:#444; margin-bottom:8px;",
                paste0("⬆ ", we$warmup_sets, " warm-up set(s) before working sets")),
          
          # ── Per-set logging ────────────────────────────────
          div(style = "display:flex; flex-direction:column; gap:6px;",
              
              # Grab the last note at the exercise level (outside the set loop)
              # so the textarea can reference it safely
              {
                ex_last_note <- tryCatch({
                  last_entry <- if (length(we_logs) > 0) we_logs[[length(we_logs)]] else NULL
                  if (!is.null(last_entry) && !is.null(last_entry$notes) &&
                      !is.na(last_entry$notes)) last_entry$notes else ""
                }, error = \(e) "")
                NULL  # expression result discarded; ex_last_note is now in scope
              },
              
              # Column headers
              div(style = "display:grid; grid-template-columns:30px 1fr 1fr 60px 32px;
                       gap:4px; padding:0 2px;",
                  div(style="font-size:10px;color:#444;","SET"),
                  div(style="font-size:10px;color:#444;","WEIGHT (lbs)"),
                  div(style="font-size:10px;color:#444;","REPS"),
                  div(style="font-size:10px;color:#444;","RPE"),
                  div()
              ),
              
              # Set rows
              lapply(seq_len(we$prescribed_sets), function(s) {
                set_key   <- paste0(we$id, "_s", s)
                log_entry <- if (s <= length(we_logs)) we_logs[[s]] else NULL
                is_logged <- !is.null(log_entry)
                
                # Default values: from this session's prior log, or last session
                def_weight <- if (is_logged) log_entry$weight_lbs
                else if (s > 1 && length(we_logs) >= s-1)
                  we_logs[[s-1]]$weight_lbs
                else if (!is.null(last)) last$weight_lbs
                else NA
                def_reps   <- if (is_logged) log_entry$reps_completed
                else if (!is.null(last)) last$reps_completed
                else we$rep_range_low
                # RPE: same carry-forward logic — previous set this session, then last session
                def_rpe    <- if (is_logged) log_entry$rpe_actual
                else if (s > 1 && length(we_logs) >= s-1)
                  we_logs[[s-1]]$rpe_actual
                else if (!is.null(last) && !is.na(last$rpe_actual))
                  round(last$rpe_actual)
                else NA
                
                div(style = paste0(
                  "display:grid; grid-template-columns:30px 1fr 1fr 60px 32px;",
                  "gap:4px; align-items:center; padding:4px 2px;",
                  if (is_logged) "background:#0a1a0a; border-radius:6px;" else ""),
                  
                  # Set number
                  div(style = paste0("font-size:13px; font-weight:700; text-align:center; ",
                                     if (is_logged) "color:#4ade80;" else "color:#555;"),
                      s),
                  
                  # Weight input
                  tags$input(
                    type        = "number",
                    id          = paste0("w_", set_key),
                    value       = if (!is.na(def_weight %||% NA)) def_weight else "",
                    placeholder = if (!is.null(last) && !is.na(last$weight_lbs))
                      as.character(last$weight_lbs) else "lbs",
                    min         = "0", step = "2.5",
                    style       = paste0(
                      "background:", if (is_logged) "#0f2a0f" else "#1e1e1e", ";",
                      "border:1.5px solid ", if (is_logged) "#4ade80" else "#2a2a2a", ";",
                      "color:#f0f0f0; border-radius:8px; padding:7px 8px;",
                      "font-size:13px; width:100%; box-sizing:border-box;"),
                    class = "ct-set-input",
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
                    min         = "0", step = "1",
                    style       = paste0(
                      "background:", if (is_logged) "#0f2a0f" else "#1e1e1e", ";",
                      "border:1.5px solid ", if (is_logged) "#4ade80" else "#2a2a2a", ";",
                      "color:#f0f0f0; border-radius:8px; padding:7px 8px;",
                      "font-size:13px; width:100%; box-sizing:border-box;"),
                    class = "ct-set-input",
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
                    min         = "0", max = "10", step = "1",
                    style       = paste0(
                      "background:", if (is_logged) "#0f2a0f" else "#1e1e1e", ";",
                      "border:1.5px solid ", if (is_logged) "#4ade80" else "#2a2a2a", ";",
                      "color:#f0f0f0; border-radius:8px; padding:7px 8px;",
                      "font-size:13px; width:100%; box-sizing:border-box;"),
                    `data-we-id` = we$id,
                    `data-set`   = s,
                    `data-type`  = "rpe"
                  ),
                  
                  # Log set button / check
                  if (is_logged)
                    div(style="text-align:center;color:#4ade80;font-size:16px;", "✓")
                  else
                    tags$button("✓",
                                style = paste0(
                                  "background:#e8ff47; color:#000; border:none; border-radius:8px;",
                                  "font-size:16px; font-weight:700; cursor:pointer;",
                                  "width:32px; height:32px; display:flex;",
                                  "align-items:center; justify-content:center;"),
                                onclick = sprintf(
                                  "Shiny.setInputValue('log_set', '%s|%d', {priority:'event'})",
                                  we$id, s))
                )
              }),
              
              # Notes field
              div(style = "margin-top:6px;",
                  tags$textarea(
                    id          = paste0("note_", we$id),
                    placeholder = "Notes (e.g. felt strong, try 5 lbs more next time...)",
                    style       = paste0(
                      "background:#1e1e1e; border:1.5px solid #2a2a2a;",
                      "color:#f0f0f0; border-radius:8px; padding:8px 10px;",
                      "font-size:12px; width:100%; box-sizing:border-box;",
                      "resize:none; min-height:36px; font-family:inherit;",
                      "line-height:1.4;"),
                    ex_last_note
                  )
              )
          )
      )
    }),
    
    # ── Finish session button ───────────────────────────────
    div(style = "margin-top:16px;",
        tags$button("Finish Session 🏁",
                    class = "ct-btn-primary",
                    onclick = "Shiny.setInputValue('finish_session', Math.random(), {priority:'event'})"),
        tags$button("Cancel",
                    class = "ct-btn-secondary",
                    style = "margin-top:8px;",
                    onclick = "Shiny.setInputValue('close_workout', Math.random(), {priority:'event'})")
    )
  )
}

# ── SWAP UI ─────────────────────────────────────────────────
swap_modal_ui <- function(we_id, exercise_id, suggestions) {
  div(style = "position:fixed; top:0; left:0; right:0; bottom:0;
               background:rgba(0,0,0,0.85); z-index:200;
               display:flex; align-items:flex-end; justify-content:center;",
      div(style = "background:#1a1a1a; border-radius:16px 16px 0 0;
                 width:100%; max-width:480px; padding:24px;",
          
          div(style="font-size:16px; font-weight:700; margin-bottom:6px;", "Swap Exercise"),
          div(style="font-size:13px; color:#666; margin-bottom:16px;",
              "Replace for this session only, or for the rest of the block."),
          
          if (is.null(suggestions)) {
            div(style="color:#555; text-align:center; padding:20px;", "Loading suggestions...")
          } else {
            tagList(
              lapply(seq_along(suggestions), function(i) {
                s <- suggestions[[i]]
                div(style="background:#2a2a2a; border-radius:10px; padding:12px;
                       margin-bottom:8px; cursor:pointer;",
                    onclick = sprintf(
                      "Shiny.setInputValue('confirm_swap', '%s|%s|session', {priority:'event'})",
                      we_id, s$id),
                    div(style="font-size:14px; font-weight:600; color:#f0f0f0;", s$name),
                    div(style="font-size:11px; color:#666; margin-top:3px;",
                        paste0(s$default_rep_range_low, "–", s$default_rep_range_high,
                               " reps · ", paste(s$primary_muscles, collapse=", ")))
                )
              }),
              div(style="display:flex; gap:8px; margin-top:4px;",
                  tags$button("Cancel", class="ct-btn-secondary ct-btn-sm",
                              onclick="Shiny.setInputValue('cancel_swap', 1, {priority:'event'})"),
                  if (!is.null(suggestions) && length(suggestions) > 0)
                    tags$button("Swap rest of block",
                                style="flex:1; background:#333; color:#ddd; border:none;
                       border-radius:8px; padding:10px; cursor:pointer; font-size:13px;",
                                onclick = sprintf(
                                  "Shiny.setInputValue('confirm_swap', '%s|%s|block', {priority:'event'})",
                                  we_id, suggestions[[1]]$id))
                  else NULL
              )
            )
          }
      )
  )
}

# ── WORKOUT SERVER LOGIC ─────────────────────────────────────
# Call this inside server() — it registers all workout-related observers
setup_workout_server <- function(input, output, session, rv) {
  
  # ── Open a workout ──────────────────────────────────────────
  observeEvent(input$open_workout, {
    rv$active_workout_id <- input$open_workout
    rv$page    <- "workout"
    rv$nav_tab <- "log"
    
    # Load workout data
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
        
        # Pre-fetch last performance for each exercise
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
    
    # Initialise set log reactive storage
    rv$set_logs <- list()
    rv$swap_we_id    <- NULL
    rv$swap_ex_id    <- NULL
    rv$swap_suggestions <- NULL
  })
  
  # ── Close workout ───────────────────────────────────────────
  observeEvent(input$close_workout, {
    rv$page    <- "dashboard"
    rv$nav_tab <- "dashboard"
    rv$active_workout_id <- NULL
  })
  
  # ── Log a set ───────────────────────────────────────────────
  observeEvent(input$log_set, {
    parts <- strsplit(input$log_set, "\\|")[[1]]
    if (length(parts) < 2) return()
    we_id     <- parts[1]
    set_num   <- as.integer(parts[2])
    set_key   <- paste0("w_",   we_id, "_s", set_num)
    reps_key  <- paste0("r_",   we_id, "_s", set_num)
    rpe_key   <- paste0("rpe_", we_id, "_s", set_num)
    note_key  <- paste0("note_", we_id)
    
    weight <- tryCatch(as.numeric(input[[set_key]]),  error = \(e) NA)
    reps   <- tryCatch(as.integer(input[[reps_key]]), error = \(e) NA)
    rpe    <- tryCatch(as.numeric(input[[rpe_key]]),  error = \(e) NA)
    notes  <- input[[note_key]] %||% ""
    
    if (is.na(reps)) {
      showNotification("Please enter reps before logging.", type = "warning")
      return()
    }
    
    # Write to Supabase
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
      # Update local reactive set log list
      current <- rv$set_logs[[we_id]] %||% list()
      current[[set_num]] <- log_row
      rv$set_logs[[we_id]] <- current
      
      # Find rest duration for this exercise and start timer
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
  
  # JS handler for rest timer
  output$start_timer_js <- renderUI({
    tags$script(HTML(sprintf(
      "Shiny.addCustomMessageHandler('start_rest_timer', function(msg) {
         startRestTimer(msg.seconds);
       });"
    )))
  })
  
  # ── Finish session ──────────────────────────────────────────
  observeEvent(input$finish_session, {
    if (is.null(rv$active_workout_id)) return()
    
    # Mark workout as completed
    resp <- sb_update("workouts",
                      sprintf("?id=eq.%s", rv$active_workout_id),
                      list(completed_at    = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
                           duration_minutes = as.integer(
                             as.numeric(Sys.time() - rv$session_start_time, units = "mins"))),
                      token = rv$token)
    
    showNotification("Session complete! Great work 💪", type = "message", duration = 4)
    
    # Clear cached progress so charts refresh with new data
    rv$all_logs <- NULL
    rv$prs      <- NULL
    
    # Reload workouts and go to dashboard
    workouts <- sb_select("workouts",
                          sprintf("?program_id=eq.%s&order=week_number,session_number", rv$program$id),
                          token = rv$token)
    rv$workouts <- workouts
    rv$page     <- "dashboard"
    rv$nav_tab  <- "dashboard"
  })
  
  # ── Exercise swap ───────────────────────────────────────────
  observeEvent(input$swap_exercise, {
    parts <- strsplit(input$swap_exercise, "\\|")[[1]]
    if (length(parts) < 2) return()
    rv$swap_we_id    <- parts[1]
    rv$swap_ex_id    <- parts[2]
    rv$swap_suggestions <- NULL  # loading state
    
    # Get current exercise's muscles/equipment for context
    we_row <- if (!is.null(rv$active_exercises))
      rv$active_exercises[rv$active_exercises$id == rv$swap_we_id, ] else NULL
    
    # Fetch suggestions via Claude API
    tryCatch({
      current_ex <- sb_select("exercises",
                              sprintf("?id=eq.%s", rv$swap_ex_id), token = rv$token)
      
      if (!is.null(current_ex)) {
        muscles   <- paste(current_ex$primary_muscles[[1]], collapse=", ")
        user_equip <- rv$profile$equipment_available[[1]] %||%
          c("dumbbells","bench","cable_machine","pullup_bar","bodyweight")
        
        suggestions <- get_swap_suggestions(
          exercise_name    = current_ex$name,
          primary_muscles  = muscles,
          user_equipment   = user_equip,
          user_token       = rv$token,
          exclude_ex_id    = rv$swap_ex_id
        )
        rv$swap_suggestions <- suggestions
      }
    }, error = function(e) {
      message("Swap suggestion error: ", e$message)
      rv$swap_suggestions <- list()
    })
  })
  
  observeEvent(input$cancel_swap, {
    rv$swap_we_id    <- NULL
    rv$swap_ex_id    <- NULL
    rv$swap_suggestions <- NULL
  })
  
  observeEvent(input$confirm_swap, {
    parts <- strsplit(input$confirm_swap, "\\|")[[1]]
    if (length(parts) < 3) return()
    we_id      <- parts[1]
    new_ex_id  <- parts[2]
    scope      <- parts[3]   # "session" or "block"
    
    # Update this workout_exercise
    sb_update("workout_exercises",
              sprintf("?id=eq.%s", we_id),
              list(exercise_id = new_ex_id, is_swapped = TRUE),
              token = rv$token)
    
    # Log the swap
    sb_insert("exercise_swaps",
              list(user_id = rv$user_id, workout_exercise_id = we_id,
                   original_exercise_id = rv$swap_ex_id,
                   replacement_exercise_id = new_ex_id, scope = scope),
              token = rv$token)
    
    # If block scope, update all future sessions
    if (scope == "block" && !is.null(rv$active_workout) && !is.null(rv$program)) {
      current_week <- rv$active_workout$week_number[1]
      tryCatch(
        regenerate_from_week(rv$program$id, rv$user_id,
                             current_week, rv$swap_ex_id, new_ex_id),
        error = \(e) message("Block regenerate error: ", e$message))
    }
    
    # Reload exercises for this workout
    we <- sb_select("workout_exercises",
                    sprintf("?workout_id=eq.%s&select=*,exercises(*)&order=exercise_order",
                            rv$active_workout_id), token = rv$token)
    rv$active_exercises <- we
    rv$swap_we_id       <- NULL
    rv$swap_ex_id       <- NULL
    rv$swap_suggestions <- NULL
    
    showNotification(
      paste0("Swapped! ", if (scope=="block") "Updated for rest of block." else "This session only."),
      type = "message", duration = 3)
  })
}

# ── CLAUDE SWAP SUGGESTIONS ──────────────────────────────────
get_swap_suggestions <- function(exercise_name, primary_muscles,
                                 user_equipment, user_token, exclude_ex_id) {
  
  # First try: find substitutions in our own DB
  # Fetch all exercises with equipment info, filter in R
  db_subs <- sb_select("exercises",
                       sprintf("?id=neq.%s&select=id,name,category,primary_muscles,equipment_required,default_rep_range_low,default_rep_range_high",
                               exclude_ex_id),
                       token = user_token)
  
  if (!is.null(db_subs) && nrow(db_subs) > 0) {
    # Filter by: user has required equipment
    db_subs_filtered <- db_subs[sapply(seq_len(nrow(db_subs)), function(i) {
      req <- tryCatch(
        if (is.list(db_subs$equipment_required)) db_subs$equipment_required[[i]]
        else strsplit(gsub("[{}]","",db_subs$equipment_required[i]),",")[[1]],
        error = \(e) character(0))
      req <- trimws(req)
      if (length(req) == 0 || all(req == "")) return(TRUE)
      all(req %in% c(user_equipment, "bodyweight"))
    }), ]
    
    if (!is.null(db_subs_filtered) && nrow(db_subs_filtered) >= 2) {
      return(lapply(seq_len(min(3, nrow(db_subs_filtered))), function(i)
        as.list(db_subs_filtered[i, ])))
    }
  }
  
  # Fallback: use Claude API for suggestions
  if (nchar(ANTHROPIC_API_KEY) == 0) return(list())
  
  prompt <- sprintf(
    paste0("You are a strength training coach. Suggest 3 exercise substitutions for '%s'",
           " that target %s. The user has this equipment: %s.",
           " Return ONLY a JSON array like:",
           " [{\"name\":\"Exercise Name\",\"reason\":\"brief reason\"}].",
           " No other text."),
    exercise_name, primary_muscles, paste(user_equipment, collapse=", "))
  
  resp <- tryCatch(
    request("https://api.anthropic.com/v1/messages") |>
      req_headers(
        "x-api-key"         = ANTHROPIC_API_KEY,
        "anthropic-version" = "2023-06-01",
        "content-type"      = "application/json"
      ) |>
      req_body_raw(toJSON(list(
        model      = "claude-sonnet-4-20250514",
        max_tokens = 300L,
        messages   = list(list(role="user", content=prompt))
      ), auto_unbox=TRUE)) |>
      req_error(is_error = \(r) FALSE) |>
      req_perform(),
    error = \(e) NULL)
  
  if (is.null(resp) || resp$status_code != 200) return(list())
  
  body    <- fromJSON(resp_body_string(resp))
  raw_txt <- tryCatch(body$content[[1]]$text, error=\(e) "")
  clean   <- gsub("```json|```", "", raw_txt)
  
  suggestions <- tryCatch(fromJSON(clean, simplifyDataFrame=FALSE), error=\(e) list())
  
  # Look up each suggested name in our DB
  results <- list()
  for (s in suggestions) {
    match <- sb_select("exercises",
                       sprintf("?name=ilike.*%s*&select=id,name,primary_muscles,default_rep_range_low,default_rep_range_high&limit=1",
                               URLencode(s$name, reserved=TRUE)),
                       token = user_token)
    if (!is.null(match)) {
      results <- c(results, list(as.list(match[1, ])))
    } else {
      results <- c(results, list(list(
        id   = NA, name = s$name,
        primary_muscles = list(primary_muscles),
        default_rep_range_low = 8L, default_rep_range_high = 12L
      )))
    }
  }
  results
}