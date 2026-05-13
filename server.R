# ============================================================
# server.R — CaTrack
# ============================================================

server <- function(input, output, session) {
  
  # ── Reactive state ─────────────────────────────────────────
  rv <- reactiveValues(
    # Auth
    token        = NULL,
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
    ob_equipment = c(
      "barbell","dumbbells","squat_rack","bench","pullup_bar",
      "cable_machine","lat_pulldown_machine","leg_press_machine"
    ),
    ob_name      = "",
    ob_generating = FALSE,
    
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
    exercise_gifs      = list(),  # exercise_id -> gif_url, session cache
    exercise_history   = list()   # exercise_id -> data.frame of recent sessions
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
      rv$token      <- token_val
      rv$user_id    <- tryCatch(result$body$user$id,    error = \(e) NULL)
      rv$user_email <- tryCatch(result$body$user$email, error = \(e) NULL)
      
      if (identical(input$auth_action, "signup")) {
        rv$ob_name <- trimws(input$signup_name %||% "")
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
      rv$ob_name      <- trimws(input$display_name)
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
            equipment_available = I(rv$ob_equipment)
          )
          sb_upsert("user_profiles", profile_data, token = rv$token)
          
          # 2. Generate program (uses service key for bulk writes)
          setProgress(0.4, detail = "Selecting exercises...")
          program_id <- generate_program(
            user_id           = rv$user_id,
            goal              = rv$ob_goal,
            difficulty        = rv$ob_difficulty,
            sessions_per_week = as.integer(rv$ob_freq),
            split_style       = rv$ob_split,
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
          showNotification(
            paste("Error generating program:", conditionMessage(e)),
            type = "error", duration = 8)
        })
      })
    }
  })
  
  # ── NAVIGATION ──────────────────────────────────────────────
  observeEvent(input$nav_tab, {
    rv$nav_tab <- input$nav_tab
    rv$page    <- input$nav_tab
  })
  
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
      return(login_page_ui(mode = rv$auth_mode))
    }
    
    # ── Onboarding ──
    if (page == "onboarding") {
      ob_values <- list(
        goal             = rv$ob_goal,
        difficulty       = rv$ob_difficulty,
        sessions_per_week = rv$ob_freq,
        split_style      = rv$ob_split,
        equipment        = rv$ob_equipment,
        display_name     = rv$ob_name
      )
      return(onboarding_page_ui(step = rv$ob_step, values = ob_values))
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
                                                 gif_map       = rv$exercise_gifs,
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
                                              selected_exercise = rv$selected_exercise
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
                                              active_program      = if (!is.null(rv$all_programs) && nrow(rv$all_programs) > 0)
                                                rv$all_programs[rv$all_programs$is_active == TRUE, ][1, ] else NULL,
                                              all_programs        = rv$all_programs,
                                              rename_program_id   = rv$rename_program_id,
                                              rename_current_name = rv$rename_current_name,
                                              delete_program_id   = rv$delete_program_id,
                                              delete_program_name = rv$delete_program_name
                                            ),
                                            if (!is.null(rv$skip_workout_id))
                                              skip_modal_ui(rv$skip_workout_id, rv$skip_session_label %||% "Session")
                           ),
                           
                           "password_reset" = div(style = "padding:20px;",
                                                  password_reset_ui(error_msg = rv$pw_reset_error)
                           ),
                           
                           "profile" = div(class = "ct-content-with-nav",
                                           profile_page_ui(
                                             profile    = rv$profile,
                                             user_email = rv$user_email,
                                             program    = rv$program
                                           )
                           ),
                           
                           # Default
                           div("Loading...")
    )
    
    tagList(page_content, bottom_nav_ui(active = rv$nav_tab))
  })
  
  # ── Workout screen setup ────────────────────────────────────
  setup_workout_server(input, output, session, rv)
  setup_progress_server(input, output, session, rv)
  setup_program_server(input, output, session, rv)
  setup_profile_server(input, output, session, rv)
  
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
  
  # ── Logout ─────────────────────────────────────────────────
  observeEvent(input$logout, {
    rv$token        <- NULL
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
    rv$exercise_gifs    <- list()
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
  
  # ── Auto-refresh workouts every 30s when on dashboard ──────
  autoInvalidate <- reactiveTimer(30000)
  observe({
    autoInvalidate()
    if (!is.null(rv$token) && rv$page == "dashboard" && !is.null(rv$program)) {
      refresh_workouts()
    }
  })
}