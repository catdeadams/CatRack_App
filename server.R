# ============================================================
# server.R — CaTrack
# ============================================================

server <- function(input, output, session) {

  # Allow Shiny to transparently reconnect when the websocket drops
  # (Posit Connect idle timeout, phone backgrounding, weak wifi).
  # Without this the user has to manually refresh, which was their
  # #1 complaint and also wipes any unsaved set-note drafts.
  session$allowReconnect(TRUE)

  # ── Reactive state ─────────────────────────────────────────
  rv <- reactiveValues(
    # Auth
    token        = NULL,
    refresh_token = NULL,
    user_id      = NULL,
    user_email   = NULL,
    auth_mode    = "login",   # "login" | "signup"
    auth_error   = NULL,
    
    # App navigation
    page         = "login",   # login | onboarding | dashboard | workout | progress | friends
    nav_tab      = "dashboard",
    
    # User profile + program
    profile      = NULL,
    program      = NULL,
    workouts     = NULL,
    
    # Onboarding
    ob_step      = 1L,
    ob_goal      = "hypertrophy",
    ob_difficulty = "intermediate",
    ob_freq      = 3L,
    ob_split     = "full_body",
    ob_session_length = 45L,
    ob_pullup_baseline = 0L,
    ob_equipment = c(
      "barbell","dumbbells","squat_rack","bench","pullup_bar",
      "cable_machine","lat_pulldown_machine","leg_press_machine"
    ),
    ob_name      = "",
    ob_generating = FALSE,

    # Methodology modal
    show_methodology = FALSE,

    # Workout preview
    preview_workout_id = NULL,
    preview_workout    = NULL,
    preview_exercises  = NULL,
    
    # Progress
    all_logs           = NULL,
    prs                = NULL,
    selected_exercise  = NULL,
    leaderboard        = NULL,
    group_members      = NULL,
    invite_code        = NULL,
    member_streaks     = NULL,
    member_1rm         = NULL,
    hidden_exercises   = character(0),
    activity_feed      = NULL,
    
    # Profile editing
    profile_edit        = list(),
    recovery_token      = NULL,
    pw_reset_error      = NULL,
    streak              = NULL,
    
    # Programs management
    all_programs        = NULL,
    rename_program_id   = NULL,
    rename_current_name = NULL,
    delete_program_id   = NULL,
    delete_program_name = NULL,
    skip_workout_id     = NULL,
    skip_session_label  = NULL,

    # Profile page state
    profile_expanded    = FALSE,
    profile_edit_gear   = FALSE,
    profile_edit_freq   = FALSE,
    profile_edit_spw    = NULL,
    profile_save_msg    = NULL,
    
    # Active workout
    active_workout_id  = NULL,
    active_workout     = NULL,
    active_exercises   = NULL,
    last_perf_map      = list(),
    set_logs           = list(),
    swap_we_id         = NULL,
    swap_ex_id         = NULL,
    swap_suggestions   = NULL,
    session_start_time = NULL,
    exercise_history   = list(),  # exercise_id -> data.frame of recent sessions

    # Workout summary (shown after finish_session)
    summary_data       = NULL,
    summary_notes_saved = FALSE
  )
  
  # ── Helper: safe nrow that never returns NULL ───────────────
  safe_nrow <- function(x) {
    if (is.null(x)) return(0L)
    if (is.data.frame(x)) return(nrow(x))
    if (is.list(x)) return(length(x))
    0L
  }
  
  # ── Helper: load user data after login ─────────────────────
  load_user_data <- function() {
    req(rv$token, rv$user_id)
    
    tryCatch({
      # Load profile
      profile    <- sb_select("user_profiles",
                              sprintf("?id=eq.%s", rv$user_id), token = rv$token)
      rv$profile <- if (safe_nrow(profile) > 0) profile[1, ] else NULL

      if (is.null(rv$profile)) {
        rv$page <- "onboarding"
        return()
      }

      rv$hidden_exercises <- tryCatch({
        raw <- rv$profile$hidden_from_leaderboard
        if (!is.null(raw) && length(raw) > 0) {
          vals <- unlist(raw)
          vals[!is.na(vals) & nchar(vals) > 0]
        } else character(0)
      }, error = \(e) character(0))
      
      # Load active program
      program    <- sb_select("programs",
                              sprintf("?user_id=eq.%s&is_active=eq.true&order=created_at.desc&limit=1",
                                      rv$user_id), token = rv$token)
      rv$program <- if (safe_nrow(program) > 0) program[1, ] else NULL
      
      if (!is.null(rv$program)) {
        workouts    <- sb_select("workouts",
                                 sprintf("?program_id=eq.%s&order=week_number,session_number",
                                         rv$program$id), token = rv$token)
        rv$workouts <- if (safe_nrow(workouts) > 0) workouts else NULL
      }
      # Pre-load all programs for the programs page
      rv$all_programs <- tryCatch(
        fetch_all_programs(rv$user_id, rv$token), error = \(e) NULL)
      
      rv$streak  <- calculate_streak(rv$workouts, rv$program)
      rv$page    <- "dashboard"
      rv$nav_tab <- "dashboard"
      
    }, error = function(e) {
      # Log to console but don't crash the app
      message("load_user_data error: ", conditionMessage(e))
      # If we at least have a token, show dashboard with empty state
      rv$page <- if (is.null(rv$profile)) "onboarding" else "dashboard"
    })
  }
  
  # ── Helper: refresh just the workouts list ─────────────────
  # Call this any time a workout status changes
  refresh_workouts <- function() {
    req(rv$token, rv$program)
    tryCatch({
      workouts <- sb_select("workouts",
                            sprintf("?program_id=eq.%s&order=week_number,session_number",
                                    rv$program$id),
                            token = rv$token)
      rv$workouts <- if (safe_nrow(workouts) > 0) workouts else NULL
      message(sprintf("Workouts refreshed: %d rows, %d completed",
                      safe_nrow(rv$workouts),
                      if (!is.null(rv$workouts) && "completed_at" %in% names(rv$workouts))
                        sum(!is.na(rv$workouts$completed_at) &
                              nchar(as.character(rv$workouts$completed_at)) > 5)
                      else 0L))
      # Recalculate streak any time workouts update
      rv$streak <- calculate_streak(rv$workouts, rv$program)
    }, error = function(e) {
      message("refresh_workouts error: ", conditionMessage(e))
    })
  }
  
  # ── AUTH: switch login/signup mode ─────────────────────────
  observeEvent(input$switch_auth_mode, {
    rv$auth_mode  <- input$switch_auth_mode
    rv$auth_error <- NULL
  })
  
  # ── AUTH: handle login / signup action ─────────────────────
  observeEvent(input$auth_action, {
    rv$auth_error <- NULL
    email    <- trimws(input$auth_email %||% "")
    password <- input$auth_password %||% ""
    
    if (nchar(email) == 0 || nchar(password) == 0) {
      rv$auth_error <- "Please enter your email and password."; return()
    }
    
    if (input$auth_action == "signup") {
      # Validate
      if (nchar(input$auth_password2 %||% "") == 0 ||
          input$auth_password != input$auth_password2) {
        rv$auth_error <- "Passwords don't match."; return()
      }
      if (nchar(input$signup_name %||% "") == 0) {
        rv$auth_error <- "Please enter a display name."; return()
      }
      result <- sb_signup(email, password)
      if (result$status %in% c(200, 201)) {
        # After signup, log in to get token
        result <- sb_login(email, password)
      }
    } else {
      result <- sb_login(email, password)
    }
    
    # Extract token safely — Supabase may nest it differently
    token_val <- tryCatch(result$body$access_token, error = \(e) NULL)
    
    if (result$status %in% c(200, 201) && !is.null(token_val) && nchar(token_val) > 0) {
      rv$token         <- token_val
      rv$refresh_token <- tryCatch(result$body$refresh_token, error = \(e) NULL)
      rv$user_id       <- tryCatch(result$body$user$id,    error = \(e) NULL)
      rv$user_email    <- tryCatch(result$body$user$email, error = \(e) NULL)

      # Persist refresh token to localStorage so screen-timeout doesn't sign out
      if (!is.null(rv$refresh_token) && nchar(rv$refresh_token) > 0) {
        session$sendCustomMessage("save_auth_session", list(
          refresh_token = rv$refresh_token,
          email         = rv$user_email %||% ""
        ))
      }

      if (identical(input$auth_action, "signup")) {
        rv$ob_name <- trimws(input$signup_name %||% "")
        # Persist the signup name immediately so the user never has to
        # retype it later — onboarding pre-fills from the profile and
        # subsequent program regens read it from there too.
        if (!is.null(rv$user_id) && nchar(rv$ob_name) > 0) {
          tryCatch(
            sb_upsert("user_profiles",
                      list(id = rv$user_id, display_name = rv$ob_name),
                      token = rv$token),
            error = \(e) message("Signup display_name save failed: ", e$message))
        }
      }

      load_user_data()
    } else {
      msg <- tryCatch(
        paste(
          result$body$error_description %||%
            result$body$msg %||%
            result$body$message %||%
            "Login failed. Check your email and password."
        ),
        error = \(e) "Login failed. Please try again."
      )
      rv$auth_error <- msg
    }
  })
  
  # ── ONBOARDING: goal selection ──────────────────────────────
  observeEvent(input$select_goal, {
    rv$ob_goal <- input$select_goal
    # Running support is full-body only; lock the split so the summary and
    # generator agree (the split step is hidden for this goal).
    if (identical(rv$ob_goal, "running_support")) rv$ob_split <- "full_body"
  })
  
  # ── ONBOARDING: difficulty ──────────────────────────────────
  observeEvent(input$select_difficulty, {
    rv$ob_difficulty <- input$select_difficulty
  })
  
  # ── ONBOARDING: frequency ──────────────────────────────────
  observeEvent(input$select_frequency, {
    rv$ob_freq <- as.integer(input$select_frequency)
  })
  
  # ── ONBOARDING: split ──────────────────────────────────────
  observeEvent(input$select_split, {
    rv$ob_split <- input$select_split
  })

  # ── ONBOARDING: session length ─────────────────────────────
  observeEvent(input$select_session_length, {
    rv$ob_session_length <- as.integer(input$select_session_length)
  })

  # ── ONBOARDING: pull-up baseline ───────────────────────────
  observeEvent(input$select_pullup_baseline, {
    rv$ob_pullup_baseline <- as.integer(input$select_pullup_baseline)
  })
  
  # ── ONBOARDING: equipment toggle ───────────────────────────
  observeEvent(input$toggle_equip, {
    equip <- input$toggle_equip
    current <- rv$ob_equipment %||% character(0)
    if (equip %in% current)
      rv$ob_equipment <- setdiff(current, equip)
    else
      rv$ob_equipment <- union(current, equip)
  })
  
  observeEvent(input$equip_select_all, {
    rv$ob_equipment <- names(ALL_EQUIPMENT)
  })
  
  observeEvent(input$equip_clear_all, {
    rv$ob_equipment <- character(0)
  })
  
  # ── ONBOARDING: navigation ──────────────────────────────────
  observeEvent(input$onboard_back, {
    if (rv$ob_step > 1) rv$ob_step <- rv$ob_step - 1L
  })
  
  observeEvent(input$onboard_next, {
    # Validate current step
    err <- switch(as.character(rv$ob_step),
                  "1" = if (is.null(rv$ob_goal)) "Please select a goal." else NULL,
                  "2" = if (is.null(rv$ob_difficulty)) "Please select a difficulty." else NULL,
                  "3" = if (is.null(rv$ob_freq) || is.null(rv$ob_split))
                    "Please select frequency and split." else NULL,
                  "4" = if (length(rv$ob_equipment) == 0)
                    "Please select at least one piece of equipment." else NULL,
                  "5" = {
                    name <- trimws(input$display_name %||% "")
                    if (nchar(name) == 0) "Please enter a display name." else NULL
                  },
                  NULL
    )
    
    if (!is.null(err)) {
      showNotification(err, type = "error", duration = 3)
      return()
    }
    
    if (rv$ob_step < 5L) {
      rv$ob_step <- rv$ob_step + 1L
    } else {
      # Final step: save profile and generate program
      if (isTRUE(rv$ob_generating)) return()  # prevent double-fire on rapid taps
      rv$ob_name       <- trimws(input$display_name)
      rv$ob_generating <- TRUE
      
      withProgress(message = "Building your 12-week program...", value = 0, {
        tryCatch({
          # 1. Save user profile
          setProgress(0.15, detail = "Saving profile...")
          profile_data <- list(
            id               = rv$user_id,
            display_name     = rv$ob_name,
            goal             = rv$ob_goal,
            difficulty       = rv$ob_difficulty,
            sessions_per_week = as.integer(rv$ob_freq),
            split_style      = rv$ob_split,
            pullup_baseline  = as.integer(rv$ob_pullup_baseline %||% 0L),
            equipment_available = I(rv$ob_equipment)
          )
          sb_upsert("user_profiles", profile_data, token = rv$token)

          # Deactivate any existing active programs before creating new one
          tryCatch(
            sb_update("programs",
              sprintf("?user_id=eq.%s&is_active=eq.true", rv$user_id),
              list(is_active = FALSE),
              token = rv$token),
            error = \(e) NULL)

          # 2. Generate program (uses service key for bulk writes)
          setProgress(0.4, detail = "Selecting exercises...")
          program_id <- generate_program(
            user_id           = rv$user_id,
            goal              = rv$ob_goal,
            difficulty        = rv$ob_difficulty,
            sessions_per_week = as.integer(rv$ob_freq),
            split_style       = rv$ob_split,
            session_length_minutes = as.integer(rv$ob_session_length %||% 45L),
            pullup_baseline   = as.integer(rv$ob_pullup_baseline %||% 0L),
            equipment         = rv$ob_equipment,
            block_number      = 1L,
            start_date        = Sys.Date()
          )
          
          setProgress(0.9, detail = "Wrapping up...")
          
          # 3. Reload user data
          load_user_data()
          rv$ob_generating <- FALSE
          rv$page <- "dashboard"
          
          showNotification(
            paste0("Program generated! ", rv$ob_freq, " sessions/week for 12 weeks."),
            type = "message", duration = 5)
          
        }, error = function(e) {
          rv$ob_generating <- FALSE
          # generate_program() now rolls back its partial program record
          # before re-throwing, so the user is safe to retry without
          # leaving orphan rows behind.
          showNotification(
            paste("Error generating program:", conditionMessage(e),
                  "— please try again. If the issue persists, change goal/split and retry."),
            type = "error", duration = 10)
        })
      })
    }
  })
  
  # ── NAVIGATION ──────────────────────────────────────────────
  observeEvent(input$go_onboarding, {
    rv$ob_step <- 1L
    rv$page    <- "onboarding"
  })
  
  # ── OPEN WORKOUT — now handled by setup_workout_server ──────
  # (open_workout observer is registered inside setup_workout_server)
  
  # ── RENDER: auth error ──────────────────────────────────────
  output$auth_error <- renderUI({
    if (!is.null(rv$auth_error))
      div(class = "ct-alert ct-alert-error", rv$auth_error)
  })
  
  # ── RENDER: onboarding generating message ──────────────────
  output$onboard_generate_msg <- renderUI({
    if (isTRUE(rv$ob_generating))
      div(class = "ct-alert ct-alert-success",
          "⚙️ Generating your 12-week program...")
  })
  
  # ── MAIN UI ROUTER ──────────────────────────────────────────
  output$main_ui <- renderUI({
    
    page <- rv$page
    
    # ── Login / Signup ──
    if (page == "login") {
      return(tagList(
        login_page_ui(mode = rv$auth_mode),
        if (isTRUE(rv$show_methodology)) methodology_modal_ui()
      ))
    }

    # ── Onboarding ──
    if (page == "onboarding") {
      # Prefer (a) any name the user has just typed in this session,
      # (b) what's on their profile, (c) what they signed up with.
      effective_name <- if (nchar(rv$ob_name %||% "") > 0) rv$ob_name
        else as.character(rv$profile$display_name %||% "")
      ob_values <- list(
        goal             = rv$ob_goal,
        difficulty       = rv$ob_difficulty,
        sessions_per_week = rv$ob_freq,
        split_style      = rv$ob_split,
        session_length_minutes = rv$ob_session_length,
        pullup_baseline  = rv$ob_pullup_baseline,
        equipment        = rv$ob_equipment,
        display_name     = effective_name
      )
      # Render the methodology modal here too — the early `return`
      # above used to skip the global modal mount, so the "How is
      # this built?" link on step 5 did nothing pre-auth.
      return(tagList(
        onboarding_page_ui(step = rv$ob_step, values = ob_values),
        if (isTRUE(rv$show_methodology)) methodology_modal_ui()
      ))
    }
    
    # ── Authenticated pages (with bottom nav) ──
    page_content <- switch(page,
                           
                           "dashboard" = div(class = "ct-content-with-nav",
                                             div(class = "ct-dash-header",
                                                 div(style = "display:flex; align-items:center; gap:10px;",
                                                     catrack_logo_svg("icon"),
                                                     div(
                                                       div(style = "font-size:16px; font-weight:700; color:#f0f0f0;
                           letter-spacing:-0.5px; line-height:1.2;", "CatRack"),
                                                       div(style = "font-size:11px; color:#555;",
                                                           rv$profile$display_name %||% rv$user_email)
                                                     )
                                                 ),
                                                 div(style = "text-align:right;",
                                                     div(style = "font-size:10px; color:#555; text-transform:uppercase;
                         letter-spacing:0.06em;", "Block"),
                                                     div(style = "font-size:13px; color:#5DCAA5; font-weight:600;",
                                                         if (!is.null(rv$program))
                                                           paste0(rv$program$block_number, " of 3")
                                                         else "—")
                                                 )
                                             ),
                                             # Streak badge (shown when streak > 0)
                                             streak_badge_ui(rv$streak),
                                             dashboard_page_ui(rv$program, rv$workouts)
                           ),
                           
                           "preview" = div(class = "ct-content-with-nav",
                                           if (!is.null(rv$preview_workout_id) && !is.null(rv$preview_workout)) {
                                             workout_preview_ui(
                                               workout   = rv$preview_workout,
                                               exercises = rv$preview_exercises
                                             )
                                           } else {
                                             div(style = "text-align:center; padding:40px; color:#555;",
                                                 "Loading preview...")
                                           }
                           ),

                           "workout" = div(class = "ct-content-with-nav",
                                           uiOutput("timer_js"),
                                           if (!is.null(rv$active_workout_id) && !is.null(rv$active_workout)) {
                                             tagList(
                                               workout_screen_ui(
                                                 workout       = rv$active_workout,
                                                 exercises     = rv$active_exercises,
                                                 last_perf_map = rv$last_perf_map,
                                                 set_logs_rv   = rv$set_logs,
                                                 timer_active  = FALSE,
                                                 history_map   = rv$exercise_history
                                               ),
                                               if (!is.null(rv$swap_we_id))
                                                 swap_modal_ui(rv$swap_we_id, rv$swap_ex_id, rv$swap_suggestions)
                                             )
                                           } else {
                                             # No workout open — show prompt to pick one from the calendar
                                             div(style = "text-align:center; padding:60px 20px; color:#555;",
                                                 div(style="font-size:40px; margin-bottom:16px;", "💪"),
                                                 div(style="font-size:16px; color:#ddd; margin-bottom:8px;",
                                                     "No session open"),
                                                 div(style="font-size:13px; margin-bottom:20px;",
                                                     "Go to the Program tab and tap a session card to start logging."),
                                                 tags$button("← Go to Program", class="ct-btn-secondary",
                                                             onclick="Shiny.setInputValue('nav_tab','dashboard',{priority:'event'})")
                                             )
                                           }
                           ),
                           
                           "progress" = div(class = "ct-content-with-nav",
                                            progress_screen_ui(
                                              logs              = rv$all_logs,
                                              prs               = rv$prs,
                                              program           = rv$program,
                                              workouts          = rv$workouts,
                                              selected_exercise = rv$selected_exercise,
                                              metric            = rv$progress_metric %||% "e1rm"
                                            )
                           ),
                           
                           "friends" = div(class = "ct-content-with-nav",
                                           friends_screen_ui(
                                             profile             = rv$profile,
                                             group_members       = rv$group_members,
                                             leaderboard_data    = rv$leaderboard,
                                             invite_code         = rv$invite_code,
                                             member_streaks      = rv$member_streaks,
                                             member_1rm          = rv$member_1rm,
                                             hidden_exercises    = rv$hidden_exercises,
                                             user_exercise_names = tryCatch(
                                               sort(unique(na.omit(sapply(
                                                 seq_len(nrow(rv$all_logs %||% data.frame())),
                                                 \(i) get_ex_name(rv$all_logs, i))))),
                                               error = \(e) character(0)),
                                             adjusted_my_volume  = if (length(rv$hidden_exercises) > 0)
                                               compute_adjusted_volume(rv$all_logs, rv$hidden_exercises)
                                             else NULL,
                                             activity_feed       = rv$activity_feed
                                           )
                           ),
                           
                           "programs" = div(class = "ct-content-with-nav",
                                            programs_page_ui(
                                              active_program      = if (!is.null(rv$all_programs) && nrow(rv$all_programs) > 0) {
                                                ap <- rv$all_programs[as.logical(rv$all_programs$is_active) == TRUE &
                                                                        !is.na(as.logical(rv$all_programs$is_active)), ]
                                                if (nrow(ap) > 0) ap[1, ] else NULL
                                              } else NULL,
                                              all_programs        = rv$all_programs,
                                              rename_program_id   = rv$rename_program_id,
                                              rename_current_name = rv$rename_current_name,
                                              delete_program_id   = rv$delete_program_id,
                                              delete_program_name = rv$delete_program_name
                                            ),
                                            if (!is.null(rv$skip_workout_id))
                                              skip_modal_ui(rv$skip_workout_id, rv$skip_session_label %||% "Session")
                           ),
                           
                           "summary" = div(class = "ct-content-with-nav",
                                           if (!is.null(rv$summary_data))
                                             workout_summary_ui(rv$summary_data, rv$program)
                                           else
                                             div(style = "padding:40px; text-align:center; color:#555;",
                                                 "No summary available.")
                           ),

                           "password_reset" = div(style = "padding:20px;",
                                                  password_reset_ui(error_msg = rv$pw_reset_error)
                           ),
                           
                           "profile" = div(class = "ct-content-with-nav",
                                           profile_page_ui(
                                             profile      = rv$profile,
                                             user_email   = rv$user_email,
                                             program      = rv$program,
                                             expanded     = isTRUE(rv$profile_expanded),
                                             edit_gear    = isTRUE(rv$profile_edit_gear),
                                             edit_freq    = isTRUE(rv$profile_edit_freq),
                                             ob_equipment = if (isTRUE(rv$profile_edit_gear)) rv$ob_equipment else NULL,
                                             save_msg     = rv$profile_save_msg
                                           )
                           ),
                           
                           # Default
                           div("Loading...")
    )
    
    tagList(page_content,
            bottom_nav_ui(active = rv$nav_tab),
            if (isTRUE(rv$show_methodology)) methodology_modal_ui(),
            # Render the rename + delete modals at the global level so
            # the dashboard's edit pencil works without routing the user
            # to the Programs tab first.
            if (!is.null(rv$rename_program_id))
              rename_modal_ui(rv$rename_program_id,
                              rv$rename_current_name %||% ""),
            if (!is.null(rv$delete_program_id))
              delete_program_modal_ui(rv$delete_program_id,
                                      rv$delete_program_name %||% "Program"))
  })
  
  # ── Workout screen setup ────────────────────────────────────
  setup_workout_server(input, output, session, rv)
  setup_progress_server(input, output, session, rv)
  setup_program_server(input, output, session, rv)
  setup_profile_server(input, output, session, rv)
  setup_summary_server(input, output, session, rv)
  setup_methodology_server(input, output, session, rv)
  
  # ── Timer JS handler output ──────────────────────────────────
  output$timer_js <- renderUI({
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('start_rest_timer', function(msg) {
         startRestTimer(msg.seconds);
       });
       Shiny.addCustomMessageHandler('reset_session_timer', function(msg) {
         window.catrackWsStart = Date.now();
       });"
    ))
  })
  
  # Holds the last-page hint sent by the browser before login completes.
  # Applied AFTER load_user_data() so we route to the user's prior view.
  pending_last_view <- reactiveVal(NULL)

  observeEvent(input$restore_last_view, {
    parsed <- tryCatch(jsonlite::fromJSON(input$restore_last_view),
                       error = \(e) NULL)
    if (is.null(parsed)) return()
    pending_last_view(list(
      page       = as.character(parsed$page       %||% ""),
      workout_id = as.character(parsed$workout_id %||% "")
    ))
  })

  # ── Session restore from localStorage refresh token ────────
  observeEvent(input$restore_session_refresh, {
    req(!is.null(input$restore_session_refresh), nchar(input$restore_session_refresh) > 0)
    if (!is.null(rv$token)) return()  # already logged in

    result <- sb_refresh(input$restore_session_refresh)
    if (result$status %in% c(200, 201)) {
      new_token <- tryCatch(result$body$access_token, error = \(e) NULL)
      if (!is.null(new_token) && nchar(new_token) > 0) {
        rv$token         <- new_token
        rv$refresh_token <- tryCatch(result$body$refresh_token, error = \(e) NULL)
        rv$user_id       <- tryCatch(result$body$user$id,    error = \(e) NULL)
        rv$user_email    <- tryCatch(result$body$user$email, error = \(e) NULL)
        # Save rotated refresh token
        session$sendCustomMessage("save_auth_session", list(
          refresh_token = rv$refresh_token %||% input$restore_session_refresh,
          email         = rv$user_email %||% ""
        ))
        load_user_data()

        # Route back to the last-active view if we have one. Workouts
        # take precedence — if the user was mid-session, drop them back
        # into it via the existing start_from_preview observer.
        view <- pending_last_view()
        if (!is.null(view)) {
          if (nchar(view$workout_id) > 0) {
            session$sendCustomMessage("trigger_input",
              list(name = "start_from_preview", value = view$workout_id))
          } else if (nchar(view$page) > 0 &&
                     view$page %in% c("dashboard","progress","friends","profile",
                                      "programs","preview","summary")) {
            rv$page    <- view$page
            rv$nav_tab <- if (view$page %in% c("dashboard","progress","friends","profile"))
                            view$page else "dashboard"
          }
          pending_last_view(NULL)
        }
      }
    }
    # If refresh fails, stay on login page — localStorage cleared by user on next explicit login
  })

  # Persist current page + active workout id so a disconnect during a
  # workout returns the user to that session, not the dashboard.
  observe({
    page <- rv$page
    wid  <- rv$active_workout_id
    # Don't persist pre-auth pages
    if (is.null(page) || page %in% c("login","onboarding","password_reset")) {
      session$sendCustomMessage("save_last_view",
        list(page = "", workout_id = ""))
    } else {
      session$sendCustomMessage("save_last_view",
        list(page = page, workout_id = wid %||% ""))
    }
  })

  # ── Logout ─────────────────────────────────────────────────
  observeEvent(input$logout, {
    session$sendCustomMessage("clear_auth_session", list())
    rv$token        <- NULL
    rv$refresh_token <- NULL
    rv$user_id      <- NULL
    rv$user_email   <- NULL
    rv$profile      <- NULL
    rv$program      <- NULL
    rv$workouts     <- NULL
    rv$all_logs     <- NULL
    rv$prs          <- NULL
    rv$leaderboard      <- NULL
    rv$group_members    <- NULL
    rv$invite_code      <- NULL
    rv$member_streaks   <- NULL
    rv$member_1rm       <- NULL
    rv$hidden_exercises <- character(0)
    rv$activity_feed    <- NULL
    rv$all_programs     <- NULL
    rv$delete_program_id   <- NULL
    rv$delete_program_name <- NULL
    rv$streak           <- NULL
    rv$profile_edit   <- list()
    rv$recovery_token <- NULL
    rv$pw_reset_error <- NULL
    rv$set_logs       <- list()
    rv$exercise_history <- list()
    rv$activity_feed    <- NULL
    rv$auth_mode    <- "login"
    rv$auth_error   <- NULL
    rv$page         <- "login"
    rv$nav_tab      <- "dashboard"
  })
  
  # ── Profile tab nav ─────────────────────────────────────────
  observeEvent(input$nav_tab, {
    rv$nav_tab <- input$nav_tab
    rv$page    <- input$nav_tab
    # Sync equipment from profile when opening profile page
    if (input$nav_tab == "profile" && !is.null(rv$profile)) {
      rv$ob_equipment <- tryCatch(
        rv$profile$equipment_available[[1]],
        error = \(e) rv$ob_equipment)
      rv$profile_edit <- list()  # reset edits
    }
  })
  
  # ── Auto-refresh workouts every 2 min when on dashboard ─────
  # Was 30s — refetched the whole workouts table 120 times per
  # browser-hour for a list that rarely changes. 2 min keeps
  # cross-device sync (finish a session on your phone, see it on
  # your laptop) without hammering Supabase.
  autoInvalidate <- reactiveTimer(120000)
  observe({
    autoInvalidate()
    if (!is.null(rv$token) && rv$page == "dashboard" && !is.null(rv$program)) {
      refresh_workouts()
    }
  })
}