# ============================================================
# profile_screen.R — CaTrack  (Session 11)
# Profile editing, password reset, streak tracking
# ============================================================

# ── STREAK CALCULATION ────────────────────────────────────────

# Returns list(current=N, longest=N, at_risk=TRUE/FALSE)
calculate_streak <- function(workouts, program) {
  if (is.null(workouts) || is.null(program)) return(list(current=0L, longest=0L, at_risk=FALSE))

  tryCatch({
    start    <- as.Date(program$start_date)
    spw      <- as.integer(program$sessions_per_week %||% 3L)
    today    <- Sys.Date()

    # Build a data frame of weeks with completion info
    # Each row = one program week
    n_weeks  <- as.integer(program$total_weeks %||% 12L)

    week_stats <- lapply(seq_len(n_weeks), function(w) {
      week_start <- start + (w - 1) * 7L
      week_end   <- week_start + 6L

      # Only count past weeks (don't count future ones)
      if (week_start > today) return(NULL)

      week_wos <- workouts[
        !is.na(workouts$week_number) &
        as.integer(workouts$week_number) == w, ]

      completed <- if (nrow(week_wos) > 0)
        sum(!is.na(week_wos$completed_at) &
            nchar(as.character(week_wos$completed_at)) > 5)
      else 0L

      # is_skipped is a vector; isTRUE() collapses vectors to FALSE so
      # the prior sum() returned at most 1. Use element-wise check.
      skipped <- if (nrow(week_wos) > 0 && "is_skipped" %in% names(week_wos))
        sum(week_wos$is_skipped %in% TRUE)
      else 0L

      # Week is "complete" if you hit your target (skips count against you)
      # Target = sessions_per_week, skips don't reduce the target
      is_complete <- completed >= spw

      # Is this the current partial week?
      is_current  <- week_start <= today && today <= week_end

      list(week = w, completed = completed, skipped = skipped,
           is_complete = is_complete, is_current = is_current,
           week_end = week_end)
    })

    # Remove NULLs (future weeks)
    week_stats <- Filter(Negate(is.null), week_stats)

    if (length(week_stats) == 0)
      return(list(current=0L, longest=0L, at_risk=FALSE))

    # Current streak: consecutive complete weeks going backwards from latest full week
    # Exclude current partial week from streak counting
    full_weeks  <- Filter(function(w) !w$is_current, week_stats)
    current_wk  <- Filter(function(w)  w$is_current, week_stats)

    # Count backwards from most recent full week
    current_streak <- 0L
    for (i in rev(seq_along(full_weeks))) {
      if (full_weeks[[i]]$is_complete) current_streak <- current_streak + 1L
      else break
    }

    # Longest streak ever
    longest_streak <- 0L
    run <- 0L
    for (wk in full_weeks) {
      if (wk$is_complete) { run <- run + 1L; if (run > longest_streak) longest_streak <- run }
      else run <- 0L
    }

    # "At risk" = current week exists, not yet complete, today is Thu/Fri/Sat/Sun
    at_risk <- FALSE
    if (length(current_wk) > 0) {
      cw         <- current_wk[[1]]
      days_left  <- as.integer(cw$week_end - today)
      at_risk    <- !cw$is_complete && days_left <= 2L && current_streak > 0L
    }

    list(current = current_streak, longest = longest_streak, at_risk = at_risk)

  }, error = \(e) list(current=0L, longest=0L, at_risk=FALSE))
}

# ── STREAK BADGE UI ──────────────────────────────────────────

streak_badge_ui <- function(streak) {
  if (is.null(streak) || streak$current == 0) return(NULL)

  color <- if (streak$at_risk) "#fbbf24" else "#1D9E75"
  bg    <- if (streak$at_risk) "#1a1200" else "#061a12"
  icon  <- if (streak$at_risk) "⚡" else "🔥"

  div(style = paste0(
    "background:", bg, "; border:1px solid ", color, "; border-radius:8px;",
    "padding:8px 12px; margin-bottom:14px;",
    "display:flex; align-items:center; justify-content:space-between;"
  ),
    div(style = "display:flex; align-items:center; gap:8px;",
      div(style = "font-size:18px;", icon),
      div(
        div(style = paste0("font-size:13px; font-weight:700; color:", color, ";"),
            paste0(streak$current, "-week streak",
                   if (streak$at_risk) " — at risk!" else "!")),
        div(style = "font-size:11px; color:#555;",
            if (streak$at_risk)
              "Complete this week's sessions to keep it going"
            else
              paste0("Longest ever: ", streak$longest, " weeks"))
      )
    ),
    div(style = paste0("font-size:28px; font-weight:700; color:", color, ";"),
        streak$current)
  )
}

# ── PASSWORD RESET ────────────────────────────────────────────

# Call this from server when type=recovery detected in URL
supabase_set_password <- function(access_token, new_password) {
  request(paste0(SUPABASE_URL, "/auth/v1/user")) |>
    req_headers(
      "apikey"        = SUPABASE_ANON_KEY,
      "Authorization" = paste("Bearer", access_token),
      "Content-Type"  = "application/json"
    ) |>
    req_body_raw(toJSON(list(password = new_password), auto_unbox = TRUE)) |>
    req_method("PUT") |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
}

password_reset_ui <- function(error_msg = NULL) {
  div(class = "ct-onboard-step",
    div(class = "ct-logo-wrap", catrack_logo_svg("full")),
    div(class = "ct-tagline", "Set your new password"),

    div(class = "ct-auth-card",
      h5("New Password", style = "font-weight:700; margin-bottom:20px; color:#f0f0f0;"),

      if (!is.null(error_msg))
        div(class = "ct-alert ct-alert-error", error_msg),

      passwordInput("new_password",  "New Password",    placeholder = "At least 8 characters"),
      passwordInput("new_password2", "Confirm Password", placeholder = "Same as above"),

      tags$button("Set Password", class = "ct-btn-primary",
        onclick = "Shiny.setInputValue('submit_new_password', Math.random(), {priority:'event'})")
    )
  )
}

# ── PROFILE PAGE UI ──────────────────────────────────────────
# Simplified layout:
#   1. Header (avatar/name/email)
#   2. Display name (single source of truth; syncs to friends + programs)
#   3. Active Program card — collapsed by default. Tap to expand and
#      see goal/difficulty/split/frequency/equipment. The expanded view
#      has Edit Gear + Edit Frequency buttons that regenerate only the
#      remaining (uncompleted) weeks of the current program.
#   4. Account section.
#
# Goal / difficulty / split are read-only here — those changes are
# disruptive enough to warrant a brand-new program via "+ New Program".

profile_page_ui <- function(profile, user_email, program,
                             expanded   = FALSE,
                             edit_gear  = FALSE,
                             edit_freq  = FALSE,
                             ob_equipment = NULL,
                             save_msg = NULL) {

  disp_name <- profile$display_name %||% ""
  goal      <- as.character(program$goal       %||% profile$goal       %||% "")
  diff      <- as.character(program$difficulty %||% profile$difficulty %||% "")
  spw       <- as.integer(program$sessions_per_week %||% profile$sessions_per_week %||% 3L)
  split     <- as.character(program$split_style %||% profile$split_style %||% "full_body")
  # Equipment defaults: program snapshot → profile → ob_equipment (for the
  # gear-edit modal preview)
  cur_equip <- tryCatch(
    program$equipment_snapshot[[1]] %||% profile$equipment_available[[1]],
    error = \(e) character(0))
  edit_equip <- ob_equipment %||% cur_equip
  prog_name <- as.character(program$name %||% "No active program")

  tagList(
    # ── Header ─────────────────────────────────────────────
    div(style = "text-align:center; padding:20px 0 16px;",
      div(style = "width:60px; height:60px; border-radius:50%; background:#0F6E56;
                   display:flex; align-items:center; justify-content:center;
                   font-size:24px; font-weight:700; color:#E1F5EE;
                   margin:0 auto 12px;",
          toupper(substr(disp_name, 1, 1))),
      div(style = "font-size:18px; font-weight:700; color:#f0f0f0; margin-bottom:4px;",
          disp_name),
      div(style = "font-size:13px; color:#555;", user_email)
    ),

    if (!is.null(save_msg))
      div(class = "ct-alert ct-alert-success", save_msg),

    # ── Display name (single source of truth for "appears to others") ──
    div(style = "background:#161616; border:1px solid #222; border-radius:12px;
                 padding:16px; margin-bottom:10px;",
      div(class = "ct-section-title", "DISPLAY NAME"),
      div(style = "font-size:11px; color:#555; margin-bottom:8px; line-height:1.4;",
          "Shown to friends and used in your program names."),
      div(style = "display:flex; gap:8px; align-items:center;",
        tags$input(type="text", id="profile_name", value=disp_name,
          style="flex:1; background:#1e1e1e; border:1.5px solid #262626;
                 color:#f0f0f0; border-radius:8px; padding:9px 12px; font-size:14px;"),
        tags$button("Save",
          style="background:#1D9E75; color:#fff; border:none; border-radius:8px;
                 padding:9px 16px; font-weight:700; cursor:pointer; font-size:13px;
                 white-space:nowrap;",
          onclick="Shiny.setInputValue('save_display_name',
            document.getElementById('profile_name').value, {priority:'event'})")
      )
    ),

    # ── Active Program card (collapsed → expanded on tap) ──
    if (!is.null(program)) {
      div(style = "background:#161616; border:1px solid #222; border-radius:12px;
                   padding:0; margin-bottom:10px; overflow:hidden;",
        # Clickable header: program name + edit pencil + chevron
        div(style = "padding:14px 16px; cursor:pointer;
                     display:flex; align-items:center; gap:10px;",
            onclick = "Shiny.setInputValue('profile_toggle_program', Math.random(), {priority:'event'})",
          div(style = "flex:1; min-width:0;",
            div(class = "ct-section-title", style = "margin-bottom:4px;", "ACTIVE PROGRAM"),
            div(style = "font-size:15px; font-weight:700; color:#f0f0f0;
                         white-space:nowrap; overflow:hidden; text-overflow:ellipsis;",
                prog_name)
          ),
          # Rename button
          tags$button(HTML("&#9998;"),
            title = "Rename",
            style = paste0(
              "background:#1e1e1e; border:1px solid #2a2a2a; border-radius:50%;",
              "width:30px; height:30px; padding:0; color:#5DCAA5;",
              "font-size:13px; cursor:pointer; line-height:1;",
              "display:inline-flex; align-items:center; justify-content:center;",
              "flex-shrink:0;"),
            onclick = sprintf(
              "event.stopPropagation(); Shiny.setInputValue('rename_program','%s',{priority:'event'})",
              program$id)),
          div(style = "color:#555; font-size:14px; flex-shrink:0;",
              if (expanded) HTML("&#9650;") else HTML("&#9660;"))
        ),

        # Expanded details
        if (expanded)
          div(style = "border-top:1px solid #222; padding:14px 16px;",
            # ── Read-only summary
            div(style = "display:grid; grid-template-columns:1fr 1fr; gap:10px;
                         font-size:12px; margin-bottom:14px;",
              div(div(style = "color:#555; font-size:10px; text-transform:uppercase;
                               letter-spacing:0.07em;", "Goal"),
                  div(style = "color:#f0f0f0; font-weight:600; margin-top:2px;",
                      tools::toTitleCase(gsub("_", " ", goal)))),
              div(div(style = "color:#555; font-size:10px; text-transform:uppercase;
                               letter-spacing:0.07em;", "Difficulty"),
                  div(style = "color:#f0f0f0; font-weight:600; margin-top:2px;",
                      tools::toTitleCase(diff))),
              div(div(style = "color:#555; font-size:10px; text-transform:uppercase;
                               letter-spacing:0.07em;", "Split"),
                  div(style = "color:#f0f0f0; font-weight:600; margin-top:2px;",
                      names(SPLIT_OPTIONS)[SPLIT_OPTIONS == split][1] %||% split)),
              div(div(style = "color:#555; font-size:10px; text-transform:uppercase;
                               letter-spacing:0.07em;", "Frequency"),
                  div(style = "color:#f0f0f0; font-weight:600; margin-top:2px;",
                      paste0(spw, "x / week"))),
              div(div(style = "color:#555; font-size:10px; text-transform:uppercase;
                               letter-spacing:0.07em;", "Equipment"),
                  div(style = "color:#f0f0f0; font-weight:600; margin-top:2px;",
                      paste0(length(cur_equip %||% character(0)), " items")))
            ),
            # ── Edit Gear / Edit Frequency buttons
            div(style = "background:#1a120a; border:1px solid #854F0B; border-radius:8px;
                         padding:10px 12px; margin-bottom:12px;
                         font-size:11px; color:#888; line-height:1.4;",
                "Editing gear or frequency rebuilds the remaining weeks of this program. ",
                "Completed sessions stay logged."),
            div(style = "display:flex; flex-direction:column; gap:8px;",
              tags$button("Edit Gear",
                class = "ct-btn-secondary",
                onclick = "Shiny.setInputValue('profile_open_edit_gear', Math.random(), {priority:'event'})"),
              tags$button("Edit Frequency (2 / 3 days)",
                class = "ct-btn-secondary",
                onclick = "Shiny.setInputValue('profile_open_edit_freq', Math.random(), {priority:'event'})")
            )
          )
      )
    } else {
      div(style = "background:#161616; border:1px solid #222; border-radius:12px;
                   padding:16px; margin-bottom:10px;",
        div(class = "ct-section-title", "ACTIVE PROGRAM"),
        div(style = "font-size:13px; color:#888;", "None yet."),
        tags$button("+ New Program",
          style = "margin-top:10px; background:#1D9E75; color:#fff; border:none;
                   border-radius:8px; padding:10px 14px; font-weight:700; cursor:pointer;
                   font-size:13px;",
          onclick = "Shiny.setInputValue('go_onboarding', 1, {priority:'event'})")
      )
    },

    # ── Edit Gear modal ────────────────────────────────────
    if (isTRUE(edit_gear))
      profile_edit_gear_modal_ui(edit_equip),

    # ── Edit Frequency modal ───────────────────────────────
    if (isTRUE(edit_freq))
      profile_edit_freq_modal_ui(spw),

    # ── Account section ────────────────────────────────────
    div(style = "background:#161616; border:1px solid #222; border-radius:12px;
                 padding:16px; margin-bottom:10px;",
      div(class = "ct-section-title", "ACCOUNT"),
      div(style = "display:flex; flex-direction:column; gap:8px;",
        tags$button("Programming Method",
          class = "ct-btn-secondary",
          onclick = "Shiny.setInputValue('open_methodology', Math.random(), {priority:'event'})"),
        tags$button("View Programs",
          class = "ct-btn-secondary",
          onclick = "Shiny.setInputValue('nav_tab','programs',{priority:'event'})"),
        tags$button("Log Out",
          class = "ct-btn-danger",
          onclick = "Shiny.setInputValue('logout', Math.random(), {priority:'event'})")
      )
    )
  )
}

# ── EDIT GEAR MODAL ──────────────────────────────────────────
# Equipment multi-select. Save triggers regen of remaining weeks.
profile_edit_gear_modal_ui <- function(equip) {
  div(style = "position:fixed; top:0; left:0; right:0; bottom:0;
               background:rgba(0,0,0,0.92); z-index:300;
               display:flex; align-items:flex-start; justify-content:center;
               overflow-y:auto;",
    div(style = "background:#0f0f0f; border:1px solid #1e1e1e;
                 width:100%; max-width:480px; min-height:100vh;
                 padding:20px 20px 40px;",
      div(style = "display:flex; justify-content:space-between; align-items:center;
                   margin-bottom:14px;",
        div(style = "font-size:17px; font-weight:700; color:#f0f0f0;",
            "Edit Equipment"),
        tags$button("✕",
          style = "background:#1e1e1e; border:none; border-radius:8px;
                   width:32px; height:32px; color:#aaa; font-size:16px; cursor:pointer;",
          onclick = "Shiny.setInputValue('profile_close_edit_gear', Math.random(), {priority:'event'})")
      ),
      div(style = "font-size:12px; color:#888; margin-bottom:16px; line-height:1.4;",
          "Saving will rebuild the remaining weeks of your program with the new equipment list. Completed sessions are kept as-is."),

      div(style = "text-align:right; margin-bottom:8px;",
        tags$a("All", href="#",
          onclick="Shiny.setInputValue('equip_select_all',Math.random(),{priority:'event'})"),
        span(style="color:#333;", " · "),
        tags$a("Clear", href="#",
          onclick="Shiny.setInputValue('equip_clear_all',Math.random(),{priority:'event'})")
      ),
      lapply(names(EQUIPMENT_CATEGORIES), function(cat_name) {
        cat_items <- EQUIPMENT_CATEGORIES[[cat_name]]
        tagList(
          div(class = "ct-equip-category", cat_name),
          div(class = "ct-equip-grid",
            lapply(names(cat_items), function(equip_id) {
              is_sel <- equip_id %in% (equip %||% character(0))
              div(class = paste("ct-equip-item", if (is_sel) "selected"),
                onclick = sprintf(
                  "Shiny.setInputValue('toggle_equip','%s',{priority:'event'})", equip_id),
                div(class = "ct-equip-check", if (is_sel) "✓" else ""),
                cat_items[[equip_id]])
            })
          )
        )
      }),

      tags$button("Save & Rebuild Remaining Weeks",
        style = "width:100%; background:#1D9E75; color:#fff; border:none;
                 border-radius:12px; padding:14px; font-size:14px; font-weight:700;
                 cursor:pointer; margin-top:14px;",
        onclick = "Shiny.setInputValue('profile_save_gear', Math.random(), {priority:'event'})"),

      tags$button("Cancel",
        style = "width:100%; background:none; color:#888; border:none;
                 border-radius:8px; padding:10px; font-size:12px; cursor:pointer; margin-top:6px;",
        onclick = "Shiny.setInputValue('profile_close_edit_gear', Math.random(), {priority:'event'})")
    )
  )
}

# ── EDIT FREQUENCY MODAL ─────────────────────────────────────
profile_edit_freq_modal_ui <- function(current_spw) {
  div(style = "position:fixed; top:0; left:0; right:0; bottom:0;
               background:rgba(0,0,0,0.85); z-index:300;
               display:flex; align-items:center; justify-content:center;
               padding:20px;",
    div(style = "background:#161616; border:1px solid #262626; border-radius:16px;
                 width:100%; max-width:380px; padding:22px;",
      div(style = "font-size:16px; font-weight:700; color:#f0f0f0; margin-bottom:4px;",
          "Edit Frequency"),
      div(style = "font-size:12px; color:#888; margin-bottom:16px; line-height:1.4;",
          "Changing days/week rebuilds the remaining weeks of your program. Completed sessions are kept."),
      div(style = "display:flex; gap:8px; margin-bottom:16px;",
        lapply(c(2L, 3L), function(val) {
          is_sel <- isTRUE(as.integer(current_spw) == val)
          div(style = paste0(
            "flex:1; text-align:center; padding:18px 8px; border-radius:10px;",
            "cursor:pointer; border:1.5px solid ",
            if (is_sel) "#1D9E75; background:#061a12;" else "#262626; background:#1e1e1e;"),
            onclick = sprintf(
              "Shiny.setInputValue('profile_pick_freq',%d,{priority:'event'})", val),
            div(style = "font-size:22px; font-weight:700; color:#f0f0f0;", val),
            div(style = "font-size:10px; color:#555;", "days/week"))
        })
      ),
      tags$button("Save & Rebuild Remaining Weeks",
        style = "width:100%; background:#1D9E75; color:#fff; border:none;
                 border-radius:10px; padding:12px; font-size:13px; font-weight:700;
                 cursor:pointer;",
        onclick = "Shiny.setInputValue('profile_save_freq', Math.random(), {priority:'event'})"),
      tags$button("Cancel",
        style = "width:100%; background:none; color:#888; border:none;
                 padding:10px; font-size:12px; cursor:pointer; margin-top:4px;",
        onclick = "Shiny.setInputValue('profile_close_edit_freq', Math.random(), {priority:'event'})")
    )
  )
}

# ── PROFILE SERVER LOGIC ──────────────────────────────────────

setup_profile_server <- function(input, output, session, rv) {

  # ── Detect password reset token from URL ─────────────────
  # JS injected into UI catches the URL hash on page load
  observe({
    token_data <- input$url_recovery_token %||% ""
    if (nchar(token_data) < 10) return()
    # Parse access_token from hash: #access_token=XXX&...&type=recovery
    if (grepl("type=recovery", token_data)) {
      parts   <- strsplit(gsub("^#", "", token_data), "&")[[1]]
      kv      <- setNames(
        sapply(parts, function(p) URLdecode(strsplit(p,"=")[[1]][2])),
        sapply(parts, function(p) strsplit(p,"=")[[1]][1])
      )
      rv$recovery_token <- kv[["access_token"]] %||% NULL
      if (!is.null(rv$recovery_token)) {
        rv$page <- "password_reset"
      }
    }
  })

  # ── Submit new password ───────────────────────────────────
  observeEvent(input$submit_new_password, {
    req(rv$recovery_token)
    pw1 <- input$new_password  %||% ""
    pw2 <- input$new_password2 %||% ""

    if (nchar(pw1) < 8) {
      rv$pw_reset_error <- "Password must be at least 8 characters."; return()
    }
    if (pw1 != pw2) {
      rv$pw_reset_error <- "Passwords don't match."; return()
    }

    resp <- supabase_set_password(rv$recovery_token, pw1)
    if (resp$status_code %in% c(200, 201)) {
      rv$recovery_token <- NULL
      rv$pw_reset_error <- NULL
      rv$page           <- "login"
      rv$auth_error     <- NULL
      showNotification("Password updated! Please log in.",
                       type="message", duration=4)
    } else {
      body <- tryCatch(fromJSON(resp_body_string(resp))$message,
                       error=\(e) "Unknown error")
      rv$pw_reset_error <- paste("Error:", body)
    }
  })

  # ── Toggle the program card open / closed ────────────────
  observeEvent(input$profile_toggle_program, {
    rv$profile_expanded <- !isTRUE(rv$profile_expanded)
  })

  # ── Open / close Edit Gear modal ──────────────────────────
  observeEvent(input$profile_open_edit_gear, {
    # Seed the modal's working set from the program's snapshot
    rv$ob_equipment <- tryCatch(
      rv$program$equipment_snapshot[[1]] %||%
        rv$profile$equipment_available[[1]],
      error = \(e) character(0))
    rv$profile_edit_gear <- TRUE
  })
  observeEvent(input$profile_close_edit_gear, {
    rv$profile_edit_gear <- FALSE
  })

  # ── Open / close Edit Frequency modal ─────────────────────
  observeEvent(input$profile_open_edit_freq, {
    rv$profile_edit_spw  <- as.integer(rv$program$sessions_per_week %||%
                                       rv$profile$sessions_per_week %||% 3L)
    rv$profile_edit_freq <- TRUE
  })
  observeEvent(input$profile_close_edit_freq, {
    rv$profile_edit_freq <- FALSE
    rv$profile_edit_spw  <- NULL
  })
  observeEvent(input$profile_pick_freq, {
    rv$profile_edit_spw <- as.integer(input$profile_pick_freq)
  })

  # ── Save Gear → regen remaining weeks ─────────────────────
  observeEvent(input$profile_save_gear, {
    req(rv$token, rv$program)
    new_equip <- rv$ob_equipment %||% character(0)
    if (length(new_equip) == 0) {
      showNotification("Pick at least one piece of equipment first.",
                       type = "warning", duration = 4); return()
    }
    withProgress(message = "Rebuilding remaining weeks...", value = 0.4, {
      tryCatch({
        regenerate_remaining_weeks(
          program_id    = rv$program$id,
          user_id       = rv$user_id,
          new_equipment = new_equip
        )
        setProgress(0.9)
        # Reload program + workouts
        prog <- sb_select("programs", sprintf("?id=eq.%s", rv$program$id),
                          token = rv$token)
        if (!is.null(prog)) rv$program <- prog[1, ]
        workouts <- sb_select("workouts",
          sprintf("?program_id=eq.%s&order=week_number,session_number",
                  rv$program$id), token = rv$token)
        rv$workouts <- workouts
        showNotification("Equipment updated. Remaining weeks rebuilt.",
                         type = "message", duration = 4)
      }, error = function(e) {
        showNotification(paste("Rebuild failed:", conditionMessage(e)),
                         type = "error", duration = 8)
      })
    })
    rv$profile_edit_gear <- FALSE
  })

  # ── Save Frequency → regen remaining weeks ────────────────
  observeEvent(input$profile_save_freq, {
    req(rv$token, rv$program)
    new_spw <- as.integer(rv$profile_edit_spw %||% 3L)
    if (!new_spw %in% c(2L, 3L)) {
      showNotification("Pick 2 or 3 days/week.", type = "warning"); return()
    }
    if (new_spw == as.integer(rv$program$sessions_per_week %||% 3L)) {
      rv$profile_edit_freq <- FALSE; return()
    }
    withProgress(message = "Rebuilding remaining weeks...", value = 0.4, {
      tryCatch({
        regenerate_remaining_weeks(
          program_id            = rv$program$id,
          user_id               = rv$user_id,
          new_sessions_per_week = new_spw
        )
        setProgress(0.9)
        # Also persist on the user profile so future programs default to it
        sb_update("user_profiles", sprintf("?id=eq.%s", rv$user_id),
                  list(sessions_per_week = new_spw), token = rv$token)
        prog <- sb_select("programs", sprintf("?id=eq.%s", rv$program$id),
                          token = rv$token)
        if (!is.null(prog)) rv$program <- prog[1, ]
        workouts <- sb_select("workouts",
          sprintf("?program_id=eq.%s&order=week_number,session_number",
                  rv$program$id), token = rv$token)
        rv$workouts <- workouts
        pf <- sb_select("user_profiles", sprintf("?id=eq.%s", rv$user_id),
                        token = rv$token)
        if (!is.null(pf)) rv$profile <- pf[1, ]
        showNotification(sprintf("Frequency set to %dx/week. Remaining weeks rebuilt.",
                                 new_spw),
                         type = "message", duration = 4)
      }, error = function(e) {
        showNotification(paste("Rebuild failed:", conditionMessage(e)),
                         type = "error", duration = 8)
      })
    })
    rv$profile_edit_freq <- FALSE
    rv$profile_edit_spw  <- NULL
  })

  # ── Display name save (instant, no regeneration) ─────────
  observeEvent(input$save_display_name, {
    req(rv$token, rv$user_id)
    name <- trimws(input$save_display_name %||% "")
    if (nchar(name) == 0) {
      showNotification("Name cannot be empty.", type="warning"); return()
    }
    resp <- sb_update("user_profiles",
      sprintf("?id=eq.%s", rv$user_id),
      list(display_name = name), token = rv$token)
    if (resp$status_code %in% c(200, 201, 204)) {
      pf <- sb_select("user_profiles", sprintf("?id=eq.%s", rv$user_id), token=rv$token)
      if (!is.null(pf)) rv$profile <- pf[1,]
      showNotification("Name updated!", type="message", duration=2)
    }
  })

  # ── Full profile save + optional regeneration ─────────────
  observeEvent(input$save_profile, {
    req(rv$token, rv$user_id, rv$profile)

    new_goal  <- rv$profile_edit$goal       %||% rv$profile$goal
    new_diff  <- rv$profile_edit$difficulty  %||% rv$profile$difficulty
    new_spw   <- rv$profile_edit$spw         %||% as.integer(rv$profile$sessions_per_week)
    new_split <- rv$profile_edit$split_style %||% rv$profile$split_style
    new_equip <- rv$ob_equipment %||%
                 tryCatch(rv$profile$equipment_available[[1]], error=\(e) character(0))

    # Match onboarding's floor — a profile save that clears equipment
    # would generate a near-empty program on regen.
    if (length(new_equip) == 0) {
      showNotification("Please select at least one piece of equipment before saving.",
                       type = "warning", duration = 4)
      return()
    }

    # Detect what changed
    goal_changed  <- !isTRUE(new_goal  == rv$profile$goal)
    diff_changed  <- !isTRUE(new_diff  == rv$profile$difficulty)
    equip_changed <- !isTRUE(setequal(new_equip,
                      tryCatch(rv$profile$equipment_available[[1]], error=\(e) character(0))))
    spw_changed   <- !isTRUE(new_spw  == as.integer(rv$profile$sessions_per_week))
    split_changed <- !isTRUE(new_split == rv$profile$split_style)

    needs_regen   <- goal_changed || diff_changed || equip_changed ||
                     spw_changed  || split_changed

    withProgress(message = if (needs_regen) "Saving & regenerating..." else "Saving...",
                 value = 0.3, {

      # Save profile
      sb_update("user_profiles",
        sprintf("?id=eq.%s", rv$user_id),
        list(
          goal              = new_goal,
          difficulty        = new_diff,
          sessions_per_week = as.integer(new_spw),
          split_style       = new_split,
          equipment_available = I(new_equip)
        ), token = rv$token)

      setProgress(0.5)

      # Regenerate remaining program weeks if needed
      if (needs_regen && !is.null(rv$program)) {
        tryCatch({
          current_week <- tryCatch({
            start <- as.Date(rv$program$start_date)
            max(1L, as.integer(floor(as.numeric(Sys.Date()-start)/7))+1L)
          }, error=\(e) 1L)

          # Deactivate current program
          sb_update("programs",
            sprintf("?id=eq.%s", rv$program$id),
            list(is_active=FALSE,
                 completed_at=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ")),
            token = rv$token)

          # Generate new program from current week
          setProgress(0.65, detail="Building new program...")
          # Carry forward session length from the existing program and
          # pull-up baseline from the user profile (both optional).
          existing_len <- tryCatch(
            as.integer(rv$program$session_length_minutes %||% 45L), error = \(e) 45L)
          existing_pu <- tryCatch(
            as.integer(rv$profile$pullup_baseline %||% 0L), error = \(e) 0L)
          new_prog_id <- generate_program(
            user_id           = rv$user_id,
            goal              = new_goal,
            difficulty        = new_diff,
            sessions_per_week = as.integer(new_spw),
            split_style       = new_split,
            session_length_minutes = existing_len,
            pullup_baseline   = existing_pu,
            equipment         = new_equip,
            block_number      = as.integer(rv$program$block_number %||% 1L),
            start_date        = tryCatch(as.Date(rv$program$start_date), error = \(e) Sys.Date())
          )

          setProgress(0.9)

          # Reload
          prog <- sb_select("programs",
            sprintf("?id=eq.%s", new_prog_id), token=rv$token)
          if (!is.null(prog)) rv$program <- prog[1,]
          workouts <- sb_select("workouts",
            sprintf("?program_id=eq.%s&order=week_number,session_number", new_prog_id),
            token=rv$token)
          rv$workouts <- workouts

        }, error=\(e) {
          message("Regen error: ", e$message)
          showNotification(paste("Regen failed:", e$message), type="error")
        })
      }

      # Reload profile
      pf <- sb_select("user_profiles", sprintf("?id=eq.%s", rv$user_id), token=rv$token)
      if (!is.null(pf)) rv$profile <- pf[1,]
      rv$profile_edit   <- list()
      rv$all_programs   <- tryCatch(
        fetch_all_programs(rv$user_id, rv$token), error=\(e) NULL)

      showNotification(
        if (needs_regen) "Profile saved and program updated!"
        else "Profile saved!",
        type="message", duration=3)
    })
  })
}
