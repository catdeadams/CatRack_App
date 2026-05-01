# ============================================================
# program_screen.R — CaTrack  (Session 10)
# Program overview, past programs, skip workout, rename.
# Source from global.R
# ============================================================

# ── DATA FETCHERS ─────────────────────────────────────────────

fetch_all_programs <- function(user_id, token) {
  sb_select("program_summary",
    sprintf("?user_id=eq.%s&order=created_at.desc", user_id),
    token = token)
}

fetch_program_prs <- function(program_id, user_id, token) {
  # PRs set during a specific program (joined via workouts)
  sb_select("personal_records",
    sprintf("?user_id=eq.%s&order=max_weight_lbs.desc&limit=5", user_id),
    token = token)
}

# ── SKIP WORKOUT ──────────────────────────────────────────────

skip_workout <- function(workout_id, token) {
  sb_update("workouts",
    sprintf("?id=eq.%s", workout_id),
    list(is_skipped = TRUE),
    token = token)
}

unskip_workout <- function(workout_id, token) {
  sb_update("workouts",
    sprintf("?id=eq.%s", workout_id),
    list(is_skipped = FALSE),
    token = token)
}

# ── PROGRAM CARD UI ───────────────────────────────────────────

program_card_ui <- function(prog, is_active = FALSE, prs = NULL) {
  completed  <- as.integer(prog$completed_sessions %||% 0)
  total      <- as.integer(prog$total_sessions     %||% 0)
  skipped    <- as.integer(prog$skipped_sessions   %||% 0)
  consist    <- tryCatch(as.numeric(prog$consistency_pct), error = \(e) NA)
  pct        <- if (total > 0) round(100 * completed / total) else 0L
  name       <- as.character(prog$display_name %||% prog$name %||% "Program")
  goal_label <- tools::toTitleCase(gsub("_", " ", prog$goal %||% ""))
  diff_label <- tools::toTitleCase(prog$difficulty %||% "")
  start_str  <- tryCatch(format(as.Date(prog$start_date), "%b %d, %Y"),
                          error = \(e) "")

  div(style = paste0(
    "background:#161616; border-radius:12px; padding:16px; margin-bottom:10px;",
    "border:1px solid ", if (is_active) "#1D9E75" else "#222", ";"
  ),
    # Header row
    div(style = "display:flex; justify-content:space-between; align-items:flex-start;
                 margin-bottom:12px;",
      div(style = "flex:1; min-width:0;",
        if (is_active)
          div(style = "font-size:10px; font-weight:700; color:#1D9E75; letter-spacing:0.1em;
                       text-transform:uppercase; margin-bottom:4px;", "ACTIVE"),
        div(style = "font-size:15px; font-weight:700; color:#f0f0f0;
                     white-space:nowrap; overflow:hidden; text-overflow:ellipsis;",
            name),
        div(style = "font-size:11px; color:#555; margin-top:3px;",
            paste0(goal_label, " · ", diff_label,
                   " · ", prog$sessions_per_week %||% "?", "x/week",
                   if (nchar(start_str) > 0) paste0(" · Started ", start_str) else ""))
      ),
      div(style = "text-align:right; flex-shrink:0; margin-left:12px;",
        div(style = "font-size:18px; font-weight:700; color:#1D9E75;",
            paste0(pct, "%")),
        div(style = "font-size:10px; color:#555;", "done")
      )
    ),

    # Progress bar
    div(style = "background:#1e1e1e; border-radius:3px; height:4px; margin-bottom:12px;",
      div(style = sprintf(
        "width:%d%%; height:100%%; border-radius:3px; background:%s;",
        pct, if (is_active) "#1D9E75" else "#0F6E56"))),

    # Stats row
    div(style = "display:grid; grid-template-columns:1fr 1fr 1fr; gap:8px; margin-bottom:12px;",
      prog_stat("Sessions", paste0(completed, " / ", total)),
      prog_stat("Skipped",  as.character(skipped)),
      prog_stat("Consistency",
                if (!is.na(consist)) paste0(consist, "%") else "—")
    ),

    # Action buttons
    div(style = "display:flex; gap:6px;",
      if (is_active) {
        tags$button("Rename",
          style = "flex:1; background:#1e1e1e; color:#aaa; border:1px solid #262626;
                   border-radius:8px; padding:8px; font-size:12px; cursor:pointer;",
          onclick = sprintf(
            "Shiny.setInputValue('rename_program','%s',{priority:'event'})", prog$program_id))
      } else {
        tagList(
          tags$button("Re-activate",
            style = "flex:1; background:#061a12; color:#1D9E75; border:1px solid #0F6E56;
                     border-radius:8px; padding:8px; font-size:12px; font-weight:600;
                     cursor:pointer;",
            onclick = sprintf(
              "Shiny.setInputValue('reactivate_program','%s',{priority:'event'})",
              prog$program_id)),
          tags$button("Rename",
            style = "background:#1e1e1e; color:#555; border:1px solid #222;
                     border-radius:8px; padding:8px 12px; font-size:12px; cursor:pointer;",
            onclick = sprintf(
              "Shiny.setInputValue('rename_program','%s',{priority:'event'})",
              prog$program_id))
        )
      }
    )
  )
}

prog_stat <- function(label, value) {
  div(style = "background:#1e1e1e; border-radius:6px; padding:8px 10px;",
    div(style = "font-size:10px; color:#555; text-transform:uppercase;
                 letter-spacing:0.07em; margin-bottom:2px;", label),
    div(style = "font-size:13px; font-weight:600; color:#f0f0f0;", value)
  )
}

# ── RENAME MODAL ──────────────────────────────────────────────

rename_modal_ui <- function(program_id, current_name) {
  div(style = "position:fixed; top:0; left:0; right:0; bottom:0;
               background:rgba(0,0,0,0.85); z-index:200;
               display:flex; align-items:center; justify-content:center;",
    div(style = "background:#161616; border:1px solid #262626; border-radius:16px;
                 width:calc(100% - 48px); max-width:400px; padding:24px;",
      div(style = "font-size:16px; font-weight:700; margin-bottom:6px; color:#f0f0f0;",
          "Rename Program"),
      div(style = "font-size:13px; color:#555; margin-bottom:16px;",
          "Give this program a name that means something to you."),
      tags$input(
        type = "text", id = "rename_input",
        value = current_name,
        style = "background:#1e1e1e; border:1.5px solid #262626; color:#f0f0f0;
                 border-radius:10px; padding:10px 12px; font-size:14px;
                 width:100%; box-sizing:border-box; margin-bottom:14px;"),
      div(style = "display:flex; gap:8px;",
        tags$button("Cancel",
          style = "flex:1; background:#1e1e1e; color:#aaa; border:1px solid #262626;
                   border-radius:8px; padding:10px; cursor:pointer; font-size:13px;",
          onclick = "Shiny.setInputValue('cancel_rename', 1, {priority:'event'})"),
        tags$button("Save",
          style = "flex:1; background:#1D9E75; color:#fff; border:none;
                   border-radius:8px; padding:10px; font-weight:700; cursor:pointer;
                   font-size:13px;",
          onclick = sprintf(
            "Shiny.setInputValue('save_rename','%s|'+document.getElementById('rename_input').value,{priority:'event'})",
            program_id))
      )
    )
  )
}

# ── SKIP CONFIRMATION MODAL ───────────────────────────────────

skip_modal_ui <- function(workout_id, session_label) {
  div(style = "position:fixed; top:0; left:0; right:0; bottom:0;
               background:rgba(0,0,0,0.85); z-index:200;
               display:flex; align-items:flex-end; justify-content:center;",
    div(style = "background:#161616; border-radius:16px 16px 0 0; border:1px solid #222;
                 width:100%; max-width:480px; padding:24px;",
      div(style = "font-size:16px; font-weight:700; color:#f0f0f0; margin-bottom:6px;",
          "Skip this session?"),
      div(style = "font-size:13px; color:#555; margin-bottom:4px;",
          session_label),
      div(style = "font-size:12px; color:#444; margin-bottom:20px; line-height:1.5;",
          "Skipped sessions are greyed out on your calendar and count against your streak. You can un-skip later."),
      div(style = "display:flex; gap:8px;",
        tags$button("Cancel",
          style = "flex:1; background:#1e1e1e; color:#aaa; border:1px solid #262626;
                   border-radius:8px; padding:12px; cursor:pointer; font-size:14px;",
          onclick = "Shiny.setInputValue('cancel_skip', 1, {priority:'event'})"),
        tags$button("Skip Session",
          style = "flex:1; background:#2d0f0f; color:#f87171; border:1px solid #3d1515;
                   border-radius:8px; padding:12px; font-weight:700; cursor:pointer;
                   font-size:14px;",
          onclick = sprintf(
            "Shiny.setInputValue('confirm_skip','%s',{priority:'event'})", workout_id))
      )
    )
  )
}

# ── PROGRAMS PAGE UI ──────────────────────────────────────────

programs_page_ui <- function(active_program, all_programs,
                              rename_program_id = NULL,
                              rename_current_name = NULL) {

  tagList(
    div(style = "margin-bottom:16px;",
      div(style = "font-size:18px; font-weight:700; color:#f0f0f0;", "Programs"),
      div(style = "font-size:12px; color:#555;", "Manage your training blocks")
    ),

    # Rename modal (if active)
    if (!is.null(rename_program_id))
      rename_modal_ui(rename_program_id, rename_current_name %||% ""),

    # Active program card
    if (!is.null(active_program)) {
      tagList(
        div(class = "ct-section-title", "ACTIVE PROGRAM"),
        program_card_ui(active_program, is_active = TRUE)
      )
    },

    # Past programs
    if (!is.null(all_programs) && nrow(all_programs) > 1) {
      past <- all_programs[!isTRUE(all_programs$is_active), ]
      if (nrow(past) > 0) {
        tagList(
          div(class = "ct-section-title", style = "margin-top:16px;", "PAST PROGRAMS"),
          lapply(seq_len(nrow(past)), function(i)
            program_card_ui(past[i, ], is_active = FALSE))
        )
      }
    },

    # New program button
    div(style = "margin-top:16px;",
      tags$button("+ New Program",
        class = "ct-btn-secondary",
        onclick = "Shiny.setInputValue('go_onboarding', 1, {priority:'event'})")
    )
  )
}

# ── PROGRAM MANAGEMENT SERVER LOGIC ──────────────────────────

setup_program_server <- function(input, output, session, rv) {

  # Load all programs when page opens
  observe({
    req(rv$token, rv$user_id)
    if (rv$page != "programs") return()
    if (is.null(rv$all_programs)) {
      rv$all_programs <- tryCatch(
        fetch_all_programs(rv$user_id, rv$token),
        error = \(e) NULL)
    }
  })

  # ── Skip workout ──────────────────────────────────────────
  observeEvent(input$skip_workout_prompt, {
    parts <- strsplit(input$skip_workout_prompt, "\\|")[[1]]
    if (length(parts) < 2) return()
    rv$skip_workout_id    <- parts[1]
    rv$skip_session_label <- parts[2]
  })

  observeEvent(input$cancel_skip, {
    rv$skip_workout_id    <- NULL
    rv$skip_session_label <- NULL
  })

  observeEvent(input$confirm_skip, {
    req(rv$token)
    wid <- input$confirm_skip
    resp <- skip_workout(wid, rv$token)
    if (resp$status_code %in% c(200, 201, 204)) {
      refresh_workouts()
      showNotification("Session skipped.", type = "message", duration = 2)
    } else {
      showNotification("Error skipping session.", type = "error")
    }
    rv$skip_workout_id    <- NULL
    rv$skip_session_label <- NULL
  })

  # ── Rename program ────────────────────────────────────────
  observeEvent(input$rename_program, {
    pid     <- input$rename_program
    all_p   <- rv$all_programs
    cur_name <- if (!is.null(all_p) && nrow(all_p) > 0) {
      row <- all_p[all_p$program_id == pid, ]
      if (nrow(row) > 0) as.character(row$display_name[1]) else ""
    } else ""
    rv$rename_program_id    <- pid
    rv$rename_current_name  <- cur_name
  })

  observeEvent(input$cancel_rename, {
    rv$rename_program_id   <- NULL
    rv$rename_current_name <- NULL
  })

  observeEvent(input$save_rename, {
    req(rv$token)
    parts    <- strsplit(input$save_rename, "\\|", fixed = TRUE)[[1]]
    if (length(parts) < 2) return()
    pid      <- parts[1]
    new_name <- paste(parts[-1], collapse = "|")  # handle | in name
    new_name <- trimws(new_name)
    if (nchar(new_name) == 0) {
      showNotification("Name cannot be empty.", type = "warning"); return()
    }

    resp <- sb_update("programs",
      sprintf("?id=eq.%s", pid),
      list(custom_name = new_name),
      token = rv$token)

    if (resp$status_code %in% c(200, 201, 204)) {
      # Refresh program data
      rv$all_programs <- tryCatch(
        fetch_all_programs(rv$user_id, rv$token), error = \(e) NULL)
      # Also refresh active program name if it's the same program
      if (!is.null(rv$program) && rv$program$id == pid) {
        prog <- sb_select("programs",
          sprintf("?id=eq.%s", pid), token = rv$token)
        if (!is.null(prog)) rv$program <- prog[1, ]
      }
      showNotification(paste0("Renamed to \"", new_name, "\""),
                       type = "message", duration = 3)
    } else {
      showNotification("Error saving name.", type = "error")
    }
    rv$rename_program_id   <- NULL
    rv$rename_current_name <- NULL
  })

  # ── Re-activate past program ──────────────────────────────
  observeEvent(input$reactivate_program, {
    req(rv$token, rv$user_id)
    pid <- input$reactivate_program

    # Deactivate current active program
    if (!is.null(rv$program)) {
      sb_update("programs",
        sprintf("?user_id=eq.%s&is_active=eq.true", rv$user_id),
        list(is_active = FALSE, completed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")),
        token = rv$token)
    }

    # Activate selected program
    resp <- sb_update("programs",
      sprintf("?id=eq.%s", pid),
      list(is_active = TRUE, completed_at = NULL),
      token = rv$token)

    if (resp$status_code %in% c(200, 201, 204)) {
      # Reload active program
      prog <- sb_select("programs",
        sprintf("?id=eq.%s", pid), token = rv$token)
      if (!is.null(prog)) rv$program <- prog[1, ]

      workouts <- sb_select("workouts",
        sprintf("?program_id=eq.%s&order=week_number,session_number", pid),
        token = rv$token)
      rv$workouts <- workouts

      rv$all_programs <- tryCatch(
        fetch_all_programs(rv$user_id, rv$token), error = \(e) NULL)

      rv$page    <- "dashboard"
      rv$nav_tab <- "dashboard"
      showNotification("Program re-activated!", type = "message", duration = 3)
    } else {
      showNotification("Error re-activating program.", type = "error")
    }
  })
}
