# ============================================================
# workout_summary.R — CaTrack
# Post-session summary screen: stats, volume, notes.
# Shown after finish_session; also viewable from program calendar.
# ============================================================

# ── Helper: single stat tile ─────────────────────────────────
summary_stat_tile <- function(label, value, color = "#1D9E75") {
  div(style = paste0(
        "background:#161616; border-radius:10px; padding:12px 8px; text-align:center;",
        "border:1px solid #1e1e1e;"),
      div(style = paste0("font-size:20px; font-weight:700; color:", color, "; line-height:1;"),
          value),
      div(style = "font-size:10px; color:#555; text-transform:uppercase; letter-spacing:0.07em; margin-top:4px;",
          label)
  )
}

# ── Main summary UI ───────────────────────────────────────────
workout_summary_ui <- function(summary_data, program = NULL) {
  if (is.null(summary_data)) return(div())

  wo        <- summary_data$workout
  exercises <- summary_data$exercises
  set_logs  <- summary_data$set_logs %||% list()

  # ── Compute stats ─────────────────────────────────────────
  total_prescribed <- if (!is.null(exercises) && nrow(exercises) > 0)
    sum(exercises$prescribed_sets, na.rm = TRUE) else 0L

  # set_logs is a sparse list keyed by set_number — a logged set 3 with
  # sets 1-2 skipped leaves NULL holes that length() would miscount.
  total_logged <- sum(sapply(set_logs, function(s) sum(!vapply(s, is.null, logical(1)))))

  pct_complete <- if (total_prescribed > 0)
    round(100 * total_logged / total_prescribed) else 0L

  total_volume <- tryCatch(
    sum(sapply(unlist(set_logs, recursive = FALSE), function(log) {
      w <- as.numeric(log$weight_lbs %||% 0)
      r <- as.integer(log$reps_completed %||% 0)
      if (is.na(w) || is.na(r)) 0 else w * r
    }), na.rm = TRUE),
    error = \(e) 0)

  duration_str <- tryCatch({
    dm <- as.integer(summary_data$duration_mins %||% NA)
    if (!is.na(dm) && dm > 0) paste0(dm, " min") else "—"
  }, error = \(e) "—")

  # Week / block info
  wo_row <- if (!is.null(wo) && is.data.frame(wo) && nrow(wo) > 0) wo[1, ] else NULL
  week_n   <- tryCatch(as.integer(wo_row$week_number),   error = \(e) NA)
  day_n    <- tryCatch(as.integer(wo_row$session_number), error = \(e) NA)
  sess_lbl <- tryCatch(as.character(wo_row$session_label), error = \(e) "")
  block_ch <- if (!is.na(week_n)) c("A","B","C")[ceiling(week_n / 4)] else "?"

  prog_name <- tryCatch(as.character(program$name %||% ""), error = \(e) "")

  # ── Per-exercise breakdown ────────────────────────────────
  ex_rows <- if (!is.null(exercises) && nrow(exercises) > 0) {
    lapply(seq_len(nrow(exercises)), function(i) {
      we       <- exercises[i, ]
      we_logs  <- set_logs[[we$id]] %||% list()
      # Drop sparse NULL holes so counts/notes reflect only real sets.
      we_logs  <- Filter(Negate(is.null), we_logs)
      n_done   <- length(we_logs)
      ex_name  <- tryCatch(we$exercises$name, error = \(e) paste("Exercise", i))

      ex_vol <- tryCatch(
        sum(sapply(we_logs, function(l) {
          w <- as.numeric(l$weight_lbs %||% 0)
          r <- as.integer(l$reps_completed %||% 0)
          if (is.na(w) || is.na(r)) 0 else w * r
        }), na.rm = TRUE),
        error = \(e) 0)

      best_set <- tryCatch({
        weights <- sapply(we_logs, \(l) as.numeric(l$weight_lbs %||% 0))
        reps    <- sapply(we_logs, \(l) as.integer(l$reps_completed %||% 0))
        idx <- which.max(replace(weights, is.na(weights), -Inf))
        if (length(idx) > 0 && weights[idx] > 0)
          paste0(weights[idx], " lbs × ", reps[idx])
        else if (length(we_logs) > 0 && !is.na(reps[1]) && reps[1] > 0)
          paste0(reps[1], " reps")
        else "—"
      }, error = \(e) "—")

      {
        div(style = paste0(
              "padding:10px 0; border-bottom:1px solid #1a1a1a;"),
            div(style = "display:flex; justify-content:space-between; align-items:center;",
                div(
                  div(style = "font-size:13px; color:#f0f0f0; font-weight:600; margin-bottom:2px;",
                      ex_name),
                  div(style = "font-size:11px; color:#555;",
                      paste0(n_done, " / ", we$prescribed_sets, " sets",
                             if (nchar(best_set) > 0 && best_set != "—")
                               paste0("  ·  best: ", best_set) else ""))
                ),
                div(style = "text-align:right; flex-shrink:0; margin-left:12px;",
                    if (ex_vol > 0) {
                      div(style = "font-size:13px; font-weight:700; color:#1D9E75;",
                          paste0(format(round(ex_vol), big.mark = ","), " lbs"))
                    } else {
                      div(style = "font-size:12px; color:#333;", "—")
                    }
                )
            ),
            # Per-set breakdown — each set's weight × reps × RPE next to its note
            if (length(we_logs) > 0)
              div(style = "margin-top:6px; display:flex; flex-direction:column; gap:2px;",
                  lapply(seq_along(we_logs), function(si) {
                    l   <- we_logs[[si]]
                    w   <- suppressWarnings(as.numeric(l$weight_lbs %||% NA))
                    rp  <- suppressWarnings(as.integer(l$reps_completed %||% NA))
                    rpe <- suppressWarnings(as.numeric(l$rpe_actual %||% NA))
                    sn  <- suppressWarnings(as.integer(l$set_number %||% si))
                    note <- {
                      n <- as.character(l$notes %||% "")
                      if (trimws(n) %in% c("", "NA", "NULL", "{}", "[]", "null")) "" else trimws(n)
                    }
                    div(
                      div(style = "display:flex; justify-content:space-between; gap:8px; font-size:11px;",
                          span(style = "color:#666;",
                               paste0("Set ", if (!is.na(sn)) sn else si)),
                          span(style = "color:#aaa;",
                               paste0(if (!is.na(w) && w > 0) paste0(w, " lbs") else "BW",
                                      " × ", if (!is.na(rp)) rp else "—", " reps",
                                      if (!is.na(rpe)) paste0("  ·  RPE ", rpe) else ""))),
                      if (nchar(note) > 0)
                        div(style = paste0(
                              "font-size:10px; color:#5DCAA5; font-style:italic;",
                              "padding:1px 0 2px 6px; border-left:2px solid #1D9E7540;"),
                            paste0("\U0001F4DD ", note))
                    )
                  }))
        )
      }
    })
  } else list()

  # ── Build UI ─────────────────────────────────────────────
  tagList(
    # Header
    div(style = "text-align:center; padding:24px 0 16px;",
        div(style = paste0(
              "width:56px; height:56px; border-radius:50%;",
              "background:#061a12; border:2px solid #1D9E75;",
              "display:flex; align-items:center; justify-content:center;",
              "margin:0 auto 12px;"),
            div(style = "color:#1D9E75; font-size:28px; font-weight:700;", "✓")),
        div(style = "font-size:20px; font-weight:700; color:#f0f0f0; margin-bottom:4px;",
            "Session Complete"),
        if (nchar(sess_lbl) > 0)
          div(style = "font-size:13px; color:#888;", sess_lbl)
    ),

    # Program / week / block context
    div(style = paste0(
          "background:#111; border-radius:10px; padding:10px 14px;",
          "margin-bottom:16px; display:flex; justify-content:space-between;",
          "align-items:center; border:1px solid #1a1a1a;"),
        div(
          if (nchar(prog_name) > 0)
            div(style = "font-size:12px; font-weight:600; color:#f0f0f0;", prog_name),
          div(style = "font-size:11px; color:#555; margin-top:2px;",
              paste0(
                if (!is.na(week_n)) paste0("Week ", week_n, "  ·  ") else "",
                if (!is.na(day_n))  paste0("Day ", day_n, "  ·  ") else "",
                "Block ", block_ch
              ))
        ),
        div(style = paste0("font-size:10px; color:#1D9E75; background:#061a12;",
                           "border:1px solid #0F6E56; border-radius:6px; padding:3px 8px;",
                           "font-weight:700;"),
            paste0(pct_complete, "% complete"))
    ),

    # Stats grid
    div(style = "display:grid; grid-template-columns:1fr 1fr 1fr; gap:8px; margin-bottom:20px;",
        summary_stat_tile("Sets Done",  paste0(total_logged, " / ", total_prescribed)),
        summary_stat_tile("Volume",
                          if (total_volume > 0)
                            paste0(format(round(total_volume / 1000, 1), nsmall = 1), "k lbs")
                          else "—"),
        summary_stat_tile("Duration", duration_str)
    ),

    # Exercise breakdown
    div(style = "margin-bottom:20px;",
        div(class = "ct-section-title", "EXERCISE BREAKDOWN"),
        div(style = "background:#111; border-radius:10px; padding:0 12px; border:1px solid #1a1a1a;",
            ex_rows)
    ),

    # Session notes
    div(style = "margin-bottom:20px;",
        div(class = "ct-section-title", "SESSION NOTES"),
        tags$textarea(
          id          = "summary_session_notes",
          placeholder = "How did this feel? Any PRs, pain points, weight adjustments...",
          style       = paste0(
            "background:#0d0d0d; border:1px solid #1e1e1e; color:#ccc;",
            "border-radius:10px; padding:10px 12px; font-size:13px;",
            "width:100%; box-sizing:border-box; resize:none;",
            "min-height:90px; font-family:inherit; line-height:1.5;"),
          summary_data$notes %||% ""
        )
    ),

    # Done button
    div(style = "padding-bottom:32px;",
        tags$button(
          "Done — Back to Program",
          style = paste0(
            "width:100%; background:#1D9E75; color:#fff; border:none;",
            "border-radius:14px; padding:16px; font-size:16px;",
            "font-weight:700; cursor:pointer; letter-spacing:0.02em;",
            "box-shadow:0 4px 20px rgba(29,158,117,0.3);"),
          onclick = "Shiny.setInputValue('dismiss_summary', Math.random(), {priority:'event'})")
    )
  )
}

# ── Summary server ────────────────────────────────────────────
setup_summary_server <- function(input, output, session, rv) {

  observeEvent(input$dismiss_summary, {
    # Save notes if user typed anything
    notes <- trimws(input$summary_session_notes %||% "")
    if (nchar(notes) > 0 && !is.null(rv$summary_data$workout_id)) {
      tryCatch(
        sb_update("workouts",
                  sprintf("?id=eq.%s", rv$summary_data$workout_id),
                  list(session_notes = notes),
                  token = rv$token),
        error = \(e) message("Summary note save error: ", e$message))
    }

    rv$summary_data <- NULL
    rv$page         <- "dashboard"
    rv$nav_tab      <- "dashboard"
  })

  # Allow opening summary for a completed workout from the calendar
  observeEvent(input$view_summary, {
    req(rv$token, rv$user_id)
    workout_id <- input$view_summary
    if (is.null(workout_id) || nchar(workout_id) == 0) return()

    withProgress(message = "Loading summary...", value = 0.5, {
      wo <- tryCatch(
        sb_select("workouts", sprintf("?id=eq.%s", workout_id), token = rv$token),
        error = \(e) NULL)
      we <- tryCatch(
        sb_select("workout_exercises",
                  sprintf("?workout_id=eq.%s&select=*,exercises(*)&order=exercise_order",
                          workout_id),
                  token = rv$token),
        error = \(e) NULL)
      existing_logs <- tryCatch({
        if (!is.null(we) && nrow(we) > 0) {
          we_ids <- paste(we$id, collapse = ",")
          sb_select("workout_set_logs",
                    sprintf("?user_id=eq.%s&workout_exercise_id=in.(%s)&order=set_number",
                            rv$user_id, we_ids),
                    token = rv$token)
        } else NULL
      }, error = \(e) NULL)

      # Rebuild set_logs structure
      logs <- list()
      if (!is.null(existing_logs) && nrow(existing_logs) > 0) {
        for (i in seq_len(nrow(existing_logs))) {
          row   <- existing_logs[i, ]
          wid   <- row$workout_exercise_id
          set_n <- as.integer(row$set_number)
          if (is.null(logs[[wid]])) logs[[wid]] <- list()
          logs[[wid]][[set_n]] <- list(
            weight_lbs     = row$weight_lbs,
            reps_completed = row$reps_completed,
            rpe_actual     = row$rpe_actual,
            notes          = row$notes,
            set_number     = set_n
          )
        }
      }

      # Try to read saved session notes
      saved_notes <- tryCatch(as.character(wo$session_notes[1] %||% ""), error = \(e) "")

      rv$summary_data <- list(
        workout_id    = workout_id,
        workout       = wo,
        exercises     = we,
        set_logs      = logs,
        duration_mins = tryCatch(as.integer(wo$duration_minutes[1]), error = \(e) NA),
        completed_at  = tryCatch(as.character(wo$completed_at[1]), error = \(e) ""),
        notes         = saved_notes
      )
    })

    rv$page    <- "summary"
    rv$nav_tab <- "dashboard"
  })
}
