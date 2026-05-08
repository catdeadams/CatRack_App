# ============================================================
# global.R — CaTrack
# Loaded once at app startup. Shared across all sessions.
# ============================================================

library(shiny)
library(bslib)
library(httr2)
library(jsonlite)
library(dplyr)
library(lubridate)
library(plotly)

for (f in c("program_generation.R", "workout_screen.R", "progress_screen.R", "program_screen.R", "profile_screen.R")) {
  tryCatch(
    source(f),
    error = function(e) stop("Error sourcing ", f, ": ", conditionMessage(e))
  )
}

# ── CREDENTIALS ─────────────────────────────────────────────
# All secrets come exclusively from environment variables.
# Locally:  .Renviron file in the project root (never commit this)
# Deployed: Posit Connect → Settings → Environment Variables
#
# Required variables:
#   SUPABASE_URL
#   SUPABASE_ANON_KEY
#   SUPABASE_SERVICE_KEY
#   ANTHROPIC_API_KEY
#   EXERCISEDB_API_KEY   (RapidAPI key for ExerciseDB — optional, enables GIF demos)

SUPABASE_URL         <- Sys.getenv("SUPABASE_URL")
SUPABASE_ANON_KEY    <- Sys.getenv("SUPABASE_ANON_KEY")
SUPABASE_SERVICE_KEY <- Sys.getenv("SUPABASE_SERVICE_KEY")
ANTHROPIC_API_KEY    <- Sys.getenv("ANTHROPIC_API_KEY")
EXERCISEDB_API_KEY   <- Sys.getenv("EXERCISEDB_API_KEY")

# Warn loudly at startup if any required key is missing
missing_keys <- c("SUPABASE_URL","SUPABASE_ANON_KEY","SUPABASE_SERVICE_KEY")[
  c(SUPABASE_URL, SUPABASE_ANON_KEY, SUPABASE_SERVICE_KEY) == ""
]
if (length(missing_keys) > 0)
  warning("Missing environment variables: ", paste(missing_keys, collapse=", "),
          "\nCreate a .Renviron file or set them in Posit Connect.")

# ── NULL coalescing — defined here so ui.R + server.R can use it ──
`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.list(a) && !is.data.frame(a)) return(if (length(a) > 0) a else b)
  if (length(a) == 0) return(b)
  if (is.na(a[[1]])) return(b)
  a
}

for (f in c("program_generation.R", "workout_screen.R", "progress_screen.R", "program_screen.R", "profile_screen.R")) {
  tryCatch(
    source(f),
    error = function(e) stop("Error sourcing ", f, ": ", conditionMessage(e))
  )
}

# ── SUPABASE AUTH HELPERS ────────────────────────────────────

sb_login <- function(email, password) {
  resp <- request(paste0(SUPABASE_URL, "/auth/v1/token?grant_type=password")) |>
    req_headers(
      "apikey"       = SUPABASE_ANON_KEY,
      "Content-Type" = "application/json"
    ) |>
    req_body_raw(toJSON(list(email = email, password = password), auto_unbox = TRUE)) |>
    req_method("POST") |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  list(status = resp$status_code, body = fromJSON(resp_body_string(resp)))
}

sb_signup <- function(email, password) {
  resp <- request(paste0(SUPABASE_URL, "/auth/v1/signup")) |>
    req_headers(
      "apikey"       = SUPABASE_ANON_KEY,
      "Content-Type" = "application/json"
    ) |>
    req_body_raw(toJSON(list(email = email, password = password), auto_unbox = TRUE)) |>
    req_method("POST") |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  list(status = resp$status_code, body = fromJSON(resp_body_string(resp)))
}

# ── SUPABASE DATA HELPERS ────────────────────────────────────
# All user-facing calls pass the user's JWT so RLS applies.

.sb_req <- function(path, token = NULL) {
  key <- if (!is.null(token)) token else SUPABASE_ANON_KEY
  request(paste0(SUPABASE_URL, "/rest/v1/", path)) |>
    req_headers(
      "apikey"        = SUPABASE_ANON_KEY,
      "Authorization" = paste("Bearer", key),
      "Content-Type"  = "application/json"
    ) |>
    req_error(is_error = \(r) FALSE)
}

sb_select <- function(table, params = "", token = NULL) {
  resp <- .sb_req(paste0(table, params), token) |> req_perform()
  if (resp$status_code != 200) return(NULL)
  result <- tryCatch(
    fromJSON(resp_body_string(resp), simplifyDataFrame = TRUE),
    error = \(e) NULL
  )
  # fromJSON returns list() for empty arrays; normalise so callers
  # can safely use: !is.null(x) && nrow(x) > 0
  if (is.null(result)) return(NULL)
  if (is.list(result) && !is.data.frame(result) && length(result) == 0) return(NULL)
  if (is.data.frame(result) && nrow(result) == 0) return(NULL)
  result
}

sb_insert <- function(table, data, token = NULL) {
  .sb_req(table, token) |>
    req_headers("Prefer" = "return=representation") |>
    req_body_raw(toJSON(if (is.data.frame(data)) data else list(data),
                        auto_unbox = TRUE, na = "null")) |>
    req_method("POST") |>
    req_perform()
}

sb_upsert <- function(table, data, token = NULL) {
  .sb_req(table, token) |>
    req_headers("Prefer" = "resolution=merge-duplicates,return=representation") |>
    req_body_raw(toJSON(if (is.data.frame(data)) data else list(data),
                        auto_unbox = TRUE, na = "null")) |>
    req_method("POST") |>
    req_perform()
}

sb_update <- function(table, filter_params, data, token = NULL) {
  .sb_req(paste0(table, filter_params), token) |>
    req_headers("Prefer" = "return=representation") |>
    req_body_raw(toJSON(data, auto_unbox = TRUE, na = "null")) |>
    req_method("PATCH") |>
    req_perform()
}

# ── CONSTANTS ────────────────────────────────────────────────

GOALS <- list(
  hypertrophy = list(
    label = "Hypertrophy",
    desc  = "Build muscle size. Moderate loads, 8–15 reps, high volume.",
    icon  = "◈"
  ),
  strength = list(
    label = "Strength",
    desc  = "Get stronger. Heavy loads, 3–6 reps, focused on big lifts.",
    icon  = "▲"
  ),
  fat_loss = list(
    label = "Fat Loss",
    desc  = "Lean out while preserving muscle. Moderate loads, shorter rest.",
    icon  = "◇"
  ),
  pull_up = list(
    label = "Pull-up Focus",
    desc  = "Build pulling strength. Upper back emphasis, weighted progressions.",
    icon  = "↑"
  ),
  running_support = list(
    label = "Running Support",
    desc  = "Complement your running. Single-leg, posterior chain, calves.",
    icon  = "→"
  ),
  functional = list(
    label = "Functional",
    desc  = "Move athletically. Compound lifts across multiple planes.",
    icon  = "○"
  )
)

EQUIPMENT_CATEGORIES <- list(
  "Free Weights" = list(
    barbell        = "Barbell",
    dumbbells      = "Dumbbells",
    ez_bar         = "EZ Bar",
    trap_bar       = "Trap Bar",
    squat_rack     = "Squat Rack / Power Rack",
    bench          = "Adjustable Bench",
    pullup_bar     = "Pull-up Bar",
    dip_bars       = "Dip Bars",
    resistance_bands = "Resistance Bands"
  ),
  "Machines" = list(
    cable_machine           = "Cable Machine",
    lat_pulldown_machine    = "Lat Pulldown Machine",
    leg_press_machine       = "Leg Press Machine",
    hack_squat_machine      = "Hack Squat Machine",
    leg_extension_machine   = "Leg Extension Machine",
    seated_leg_curl_machine = "Seated Leg Curl Machine",
    lying_leg_curl_machine  = "Lying Leg Curl Machine",
    hip_thrust_machine      = "Hip Thrust Machine",
    hip_abduction_machine   = "Hip Abduction Machine",
    calf_raise_machine      = "Calf Raise Machine",
    seated_calf_raise_machine = "Seated Calf Raise Machine",
    pec_dec_machine         = "Pec Dec Machine",
    chest_press_machine     = "Chest Press Machine",
    incline_press_machine   = "Incline Press Machine",
    shoulder_press_machine  = "Shoulder Press Machine",
    lateral_raise_machine   = "Lateral Raise Machine",
    ab_machine              = "Ab Crunch Machine",
    row_machine             = "Row Machine"
  ),
  "Specialty" = list(
    sled                    = "Sled",
    hyperextension_bench    = "45° Hyperextension Bench",
    preacher_bench          = "Preacher Curl Bench",
    pendulum_squat_machine  = "Pendulum Squat Machine",
    belt_squat_machine      = "Belt Squat Machine",
    t_bar_row               = "T-Bar Row"
  )
)

# All equipment as flat named vector (id = label)
ALL_EQUIPMENT <- unlist(EQUIPMENT_CATEGORIES, use.names = FALSE)
names(ALL_EQUIPMENT) <- unlist(lapply(EQUIPMENT_CATEGORIES, names))

SPLIT_OPTIONS <- c(
  "Full Body"       = "full_body",
  "Push / Pull / Legs" = "push_pull_legs",
  "Upper / Lower"   = "upper_lower"
)

FREQUENCY_OPTIONS <- c("2x per week" = 2, "3x per week" = 3)

DIFFICULTY_OPTIONS <- c(
  "Beginner — new to lifting or returning after a long break" = "beginner",
  "Intermediate — consistently training for 6+ months"       = "intermediate",
  "Advanced — 2+ years of structured training"               = "advanced"
)

# Muscle group display names for volume tracker
MUSCLE_DISPLAY <- c(
  quads = "Quads", hamstrings = "Hamstrings", glutes = "Glutes",
  chest = "Chest", lats = "Lats", mid_back = "Mid Back",
  front_delts = "Front Delts", mid_delts = "Side Delts",
  rear_delts = "Rear Delts", biceps = "Biceps",
  triceps = "Triceps", calves = "Calves", core = "Core"
)

# ── THEME ────────────────────────────────────────────────────
# Use system fonts only — font_google() makes outbound HTTP calls
# at startup which are blocked in Posit Connect's build sandbox.
catrack_theme <- bs_theme(
  version   = 5,
  bg        = "#0f0f0f",
  fg        = "#f0f0f0",
  primary   = "#1D9E75",
  secondary = "#1e1e1e",
  success   = "#4ade80",
  danger    = "#f87171",
  warning   = "#fbbf24",
  info      = "#60a5fa",
  font_scale = 0.9,
  `border-radius` = "12px",
  `btn-border-radius` = "8px"
)

# ── SVG LOGO ─────────────────────────────────────────────────
catrack_logo_svg <- function(size = "full", color = "#1D9E75") {
  # size: "full" = mark + wordmark, "icon" = mark only
  light  <- "#5DCAA5"
  deep   <- "#0F6E56"
  darker <- "#085041"
  if (size == "icon") {
    HTML(sprintf('<svg width="48" height="52" viewBox="0 0 148 158" xmlns="http://www.w3.org/2000/svg">
      <rect x="0"   y="34" width="14" height="108" rx="4" fill="%s"/>
      <rect x="134" y="34" width="14" height="108" rx="4" fill="%s"/>
      <polygon points="0,34 14,34 0,8"     fill="%s"/>
      <polygon points="134,34 148,34 148,8" fill="%s"/>
      <polygon points="3,30 11,30 3,14"    fill="%s" opacity="0.55"/>
      <polygon points="137,30 145,30 145,14" fill="%s" opacity="0.55"/>
      <rect x="-14" y="70" width="176" height="9" rx="3" fill="%s"/>
      <rect x="-24" y="61" width="10"  height="27" rx="3" fill="%s"/>
      <rect x="162" y="61" width="10"  height="27" rx="3" fill="%s"/>
      <rect x="14"  y="70" width="10"  height="6"  rx="1.5" fill="%s"/>
      <rect x="124" y="70" width="10"  height="6"  rx="1.5" fill="%s"/>
      <circle cx="52" cy="52" r="5" fill="%s"/>
      <circle cx="96" cy="52" r="5" fill="%s"/>
      <rect x="-10" y="142" width="168" height="10" rx="3" fill="%s"/>
      <rect x="-18" y="148" width="22"  height="6"  rx="2" fill="%s"/>
      <rect x="144" y="148" width="22"  height="6"  rx="2" fill="%s"/>
    </svg>',
                 color,color,color,color,light,light,color,darker,darker,light,light,light,light,deep,color,color))
  } else {
    HTML(sprintf('<div style="display:flex;flex-direction:column;align-items:center;gap:0;">
      <svg width="96" height="82" viewBox="-40 0 228 158" xmlns="http://www.w3.org/2000/svg">
        <rect x="0"   y="34" width="14" height="108" rx="4" fill="%s"/>
        <rect x="134" y="34" width="14" height="108" rx="4" fill="%s"/>
        <polygon points="0,34 14,34 0,8"     fill="%s"/>
        <polygon points="134,34 148,34 148,8" fill="%s"/>
        <polygon points="3,30 11,30 3,14"    fill="%s" opacity="0.55"/>
        <polygon points="137,30 145,30 145,14" fill="%s" opacity="0.55"/>
        <rect x="-14" y="70" width="176" height="9" rx="3" fill="%s"/>
        <rect x="-24" y="61" width="10"  height="27" rx="3" fill="%s"/>
        <rect x="162" y="61" width="10"  height="27" rx="3" fill="%s"/>
        <rect x="14"  y="70" width="10"  height="6"  rx="1.5" fill="%s"/>
        <rect x="124" y="70" width="10"  height="6"  rx="1.5" fill="%s"/>
        <circle cx="52" cy="52" r="5" fill="%s"/>
        <circle cx="96" cy="52" r="5" fill="%s"/>
        <line x1="30" y1="58" x2="64" y2="60" stroke="%s" stroke-width="1.5" stroke-linecap="round" opacity="0.6"/>
        <line x1="84" y1="60" x2="118" y2="58" stroke="%s" stroke-width="1.5" stroke-linecap="round" opacity="0.6"/>
        <rect x="-10" y="142" width="168" height="10" rx="3" fill="%s"/>
        <rect x="-18" y="148" width="22"  height="6"  rx="2" fill="%s"/>
        <rect x="144" y="148" width="22"  height="6"  rx="2" fill="%s"/>
      </svg>
      <div style="font-size:26px;font-weight:700;color:#f0f0f0;letter-spacing:-1px;margin-top:-4px;font-family:system-ui,sans-serif;">CatRack</div>
    </div>',
                 color,color,color,color,light,light,color,darker,darker,light,light,light,light,light,light,deep,color,color))
  }
}

# ── PAGE UI HELPER FUNCTIONS ─────────────────────────────────
# Defined here in global.R so they are available to both
# ui.R and server.R (split-file Shiny shares global.R only).

login_page_ui <- function(mode = "login") {
  div(class = "ct-onboard-step",
      # Catch Supabase password recovery token from URL hash on page load
      tags$script(HTML('
      (function() {
        var h = window.location.hash + window.location.search;
        if (h.indexOf("type=recovery") !== -1) {
          document.addEventListener("DOMContentLoaded", function() {
            setTimeout(function() {
              Shiny.setInputValue("url_recovery_token", h, {priority:"event"});
            }, 800);
          });
        }
      })();
    ')),
      div(class = "ct-logo-wrap", catrack_logo_svg("full")),
      div(class = "ct-tagline", "Science-based training. Built around you."),
      
      div(class = "ct-auth-card",
          h5(if (mode == "login") "Welcome back" else "Create your account",
             style = "font-weight:700; margin-bottom:20px;"),
          
          if (mode == "signup")
            div(textInput("signup_name", "Display Name",
                          placeholder = "How you'll appear to friends")),
          
          textInput("auth_email", "Email", placeholder = "you@example.com"),
          passwordInput("auth_password", "Password", placeholder = "••••••••"),
          
          if (mode == "signup")
            passwordInput("auth_password2", "Confirm Password", placeholder = "••••••••"),
          
          uiOutput("auth_error"),
          
          if (mode == "login")
            tags$button("Log in", class = "ct-btn-primary",
                        onclick = "Shiny.setInputValue('auth_action', 'login', {priority:'event'})")
          else
            tags$button("Create Account", class = "ct-btn-primary",
                        onclick = "Shiny.setInputValue('auth_action', 'signup', {priority:'event'})"),
          
          hr(class = "ct-divider"),
          
          if (mode == "login")
            div(style = "text-align:center; font-size:13px; color:#666;",
                "New here? ",
                tags$a("Create an account", href = "#",
                       onclick = "Shiny.setInputValue('switch_auth_mode', 'signup', {priority:'event'})"))
          else
            div(style = "text-align:center; font-size:13px; color:#666;",
                "Already have an account? ",
                tags$a("Log in", href = "#",
                       onclick = "Shiny.setInputValue('switch_auth_mode', 'login', {priority:'event'})"))
      )
  )
}

onboarding_page_ui <- function(step, values = list()) {
  n_steps <- 5
  dots <- lapply(1:n_steps, function(i)
    div(class = paste("ct-step-dot",
                      if (i < step) "done" else if (i == step) "active" else "")))
  
  step_content <- switch(as.character(step),
                         "1" = tagList(
                           div(class = "ct-step-title", "What's your goal?"),
                           div(class = "ct-step-sub",
                               "This shapes your rep ranges, exercises, and volume for the whole 12-week block."),
                           div(class = "ct-goal-grid",
                               lapply(names(GOALS), function(g) {
                                 info   <- GOALS[[g]]
                                 is_sel <- isTRUE(values$goal == g)
                                 div(class = paste("ct-goal-card", if (is_sel) "selected"),
                                     onclick = sprintf("Shiny.setInputValue('select_goal','%s',{priority:'event'})", g),
                                     div(class = "ct-goal-icon",  info$icon),
                                     div(class = "ct-goal-label", gsub("^.+ ", "", info$label)),
                                     div(class = "ct-goal-desc",  info$desc))
                               })
                           )
                         ),
                         "2" = tagList(
                           div(class = "ct-step-title", "How experienced are you?"),
                           div(class = "ct-step-sub", "This adjusts volume, loading, and progression speed."),
                           div(style = "display:flex; flex-direction:column; gap:8px;",
                               lapply(names(DIFFICULTY_OPTIONS), function(label) {
                                 val    <- DIFFICULTY_OPTIONS[[label]]
                                 is_sel <- isTRUE(values$difficulty == val)
                                 div(class = paste("ct-session-card", if (is_sel) "today" else "future"),
                                     style = "cursor:pointer;",
                                     onclick = sprintf(
                                       "Shiny.setInputValue('select_difficulty','%s',{priority:'event'})", val),
                                     div(class = "ct-sess-type", style = "font-size:14px;",
                                         switch(val, beginner="🌱 Beginner",
                                                intermediate="🔥 Intermediate", advanced="⚡ Advanced")),
                                     div(class = "ct-sess-date", style = "margin-top:4px;", label))
                               })
                           )
                         ),
                         "3" = tagList(
                           div(class = "ct-step-title", "How often and how?"),
                           div(class = "ct-step-sub", "Choose your weekly lifting frequency and session structure."),
                           div(class = "ct-section-title", "Sessions per week"),
                           div(style = "display:flex; gap:8px; margin-bottom:16px;",
                               lapply(names(FREQUENCY_OPTIONS), function(label) {
                                 val    <- as.integer(FREQUENCY_OPTIONS[[label]])
                                 is_sel <- isTRUE(as.integer(values$sessions_per_week) == val)
                                 div(class = paste("ct-session-card", if (is_sel) "today" else "future"),
                                     style = "cursor:pointer; text-align:center; padding:14px;",
                                     onclick = sprintf(
                                       "Shiny.setInputValue('select_frequency',%d,{priority:'event'})", val),
                                     div(class = "ct-sess-type", style = "font-size:22px;", val),
                                     div(class = "ct-sess-date", "days/week"))
                               })
                           ),
                           div(class = "ct-section-title", "Split style"),
                           div(style = "display:flex; flex-direction:column; gap:8px;",
                               lapply(names(SPLIT_OPTIONS), function(label) {
                                 val    <- SPLIT_OPTIONS[[label]]
                                 is_sel <- isTRUE(values$split_style == val)
                                 div(class = paste("ct-session-card", if (is_sel) "today" else "future"),
                                     style = "cursor:pointer;",
                                     onclick = sprintf(
                                       "Shiny.setInputValue('select_split','%s',{priority:'event'})", val),
                                     div(class = "ct-sess-type", label),
                                     div(class = "ct-sess-date", switch(val,
                                                                        full_body      = "Every session hits all major muscle groups",
                                                                        push_pull_legs = "Separate pushing, pulling, and leg days",
                                                                        upper_lower    = "Alternate upper and lower body days")))
                               })
                           )
                         ),
                         "4" = tagList(
                           div(class = "ct-step-title", "What equipment do you have?"),
                           div(class = "ct-step-sub",
                               "Only exercises you can actually do will be prescribed."),
                           div(style = "text-align:right; margin-bottom:8px;",
                               tags$a("Select all", href="#",
                                      onclick="Shiny.setInputValue('equip_select_all',Math.random(),{priority:'event'})"),
                               " · ",
                               tags$a("Clear", href="#",
                                      onclick="Shiny.setInputValue('equip_clear_all',Math.random(),{priority:'event'})")
                           ),
                           lapply(names(EQUIPMENT_CATEGORIES), function(cat_name) {
                             cat_items <- EQUIPMENT_CATEGORIES[[cat_name]]
                             tagList(
                               div(class = "ct-equip-category", cat_name),
                               div(class = "ct-equip-grid",
                                   lapply(names(cat_items), function(equip_id) {
                                     is_sel <- equip_id %in% (values$equipment %||% character(0))
                                     div(class = paste("ct-equip-item", if (is_sel) "selected"),
                                         onclick = sprintf(
                                           "Shiny.setInputValue('toggle_equip','%s',{priority:'event'})", equip_id),
                                         div(class = "ct-equip-check", if (is_sel) "✓" else ""),
                                         cat_items[[equip_id]])
                                   })
                               )
                             )
                           })
                         ),
                         "5" = tagList(
                           div(class = "ct-step-title", "You're all set!"),
                           div(class = "ct-step-sub", "Here's your plan. We'll generate your 12-week program now."),
                           textInput("display_name", "Your display name (shown to friends)",
                                     value = values$display_name %||% "", placeholder = "e.g. Cat"),
                           div(class = "ct-session-card future", style = "margin:12px 0;",
                               div(style = "display:grid; grid-template-columns:1fr 1fr; gap:12px;",
                                   div(div(class="ct-sess-label","GOAL"),
                                       div(class="ct-sess-type", GOALS[[values$goal %||% "hypertrophy"]]$label)),
                                   div(div(class="ct-sess-label","LEVEL"),
                                       div(class="ct-sess-type",
                                           tools::toTitleCase(values$difficulty %||% "intermediate"))),
                                   div(div(class="ct-sess-label","FREQUENCY"),
                                       div(class="ct-sess-type", paste0(values$sessions_per_week %||% 3,"x / week"))),
                                   div(div(class="ct-sess-label","SPLIT"),
                                       div(class="ct-sess-type",
                                           names(SPLIT_OPTIONS)[
                                             SPLIT_OPTIONS == (values$split_style %||% "full_body")])),
                                   div(div(class="ct-sess-label","EQUIPMENT"),
                                       div(class="ct-sess-type",
                                           paste0(length(values$equipment %||% character(0)), " items"))),
                                   div(div(class="ct-sess-label","BLOCK"),
                                       div(class="ct-sess-type","12 weeks · Block 1"))
                               )
                           ),
                           uiOutput("onboard_generate_msg")
                         )
  )
  
  back_btn <- if (step > 1)
    tags$button("← Back", class = "ct-btn-secondary ct-btn-sm",
                onclick = "Shiny.setInputValue('onboard_back',Math.random(),{priority:'event'})")
  else div()
  
  next_label <- if (step < n_steps) "Continue →" else "Generate My Program 🚀"
  next_btn   <- tags$button(next_label, class = "ct-btn-primary",
                            onclick = "Shiny.setInputValue('onboard_next',Math.random(),{priority:'event'})")
  
  div(class = "ct-onboard-step",
      div(class = "ct-step-indicator", dots),
      step_content,
      br(),
      div(style = "display:flex; gap:8px; align-items:center;",
          back_btn, div(style="flex:1;", next_btn))
  )
}

dashboard_page_ui <- function(program, workouts, current_date = Sys.Date()) {
  if (is.null(program)) {
    return(div(style = "text-align:center; padding:60px 20px; color:#555;",
               div(style="font-size:40px;", "🏋️"),
               div(style="font-size:16px; margin-top:12px;", "No active program found."),
               tags$button("Create Program", class="ct-btn-primary",
                           onclick="Shiny.setInputValue('go_onboarding',1,{priority:'event'})")))
  }
  
  n_weeks      <- program$total_weeks
  completed    <- if (!is.null(workouts) && "completed_at" %in% names(workouts)) {
    sum(sapply(workouts$completed_at, function(x) {
      isTRUE(!is.null(x) && !is.na(x) && nchar(as.character(x)) > 5)
    }))
  } else 0L
  total_wo     <- if (!is.null(workouts)) nrow(workouts) else 0L
  pct          <- if (total_wo > 0) round(100 * completed / total_wo) else 0L
  start        <- as.Date(program$start_date)
  current_week <- max(1L, min(n_weeks,
                              as.integer(floor(as.numeric(current_date - start) / 7)) + 1L))
  
  tagList(
    div(class = "ct-block-header",
        div(
          div(class = "ct-block-label",
              paste("Block", program$block_number, "·",
                    tools::toTitleCase(program$goal), "·",
                    tools::toTitleCase(program$difficulty))),
          div(class = "ct-block-title", program$name)
        ),
        div(style = "text-align:right; font-size:12px; color:#888;",
            div(style="font-size:20px; font-weight:700; color:#1D9E75;", paste0(pct, "%")),
            "complete")
    ),
    div(class = "ct-progress-bar",
        div(class = "ct-progress-fill", style = sprintf("width:%d%%", pct))),
    div(style = "display:flex; justify-content:space-between;
                 font-size:11px; color:#555; margin-bottom:20px;",
        span(paste(completed, "sessions done")),
        span(paste0("Week ", current_week, " of ", n_weeks))
    ),
    lapply(1:n_weeks, function(w) {
      week_workouts <- if (!is.null(workouts)) workouts[workouts$week_number == w, ] else NULL
      block_char    <- c("A","B","C")[ceiling(w/4)]
      is_current    <- isTRUE(w == current_week)
      
      div(class = "ct-week-row",
          div(class = paste("ct-week-label", if (is_current) "current"),
              sprintf("WEEK %d — BLOCK %s", w, block_char)),
          div(class = "ct-sessions-row",
              if (!is.null(week_workouts) && nrow(week_workouts) > 0) {
                lapply(seq_len(nrow(week_workouts)), function(s) {
                  wo      <- week_workouts[s, ]
                  wo_date <- tryCatch(as.Date(wo$scheduled_date), error = \(e) NA)
                  # completed_at from Supabase is a string or NA
                  # Must handle: NA, "NA", NULL, "", and real timestamps
                  completed_val <- tryCatch(wo$completed_at, error = \(e) NA)
                  is_done <- isTRUE(
                    !is.null(completed_val) &&
                      length(completed_val) > 0 &&
                      !is.na(completed_val) &&
                      nchar(as.character(completed_val)) > 5
                  )
                  is_today <- isTRUE(!is.na(wo_date) && wo_date == current_date)
                  
                  card_class <- paste("ct-session-card",
                                      if (is_done) "completed" else if (is_today) "today" else "future")
                  
                  div(class = card_class,
                      onclick = sprintf(
                        "Shiny.setInputValue('open_workout','%s',{priority:'event'})",
                        wo$id),
                      div(style="display:flex;justify-content:space-between;align-items:flex-start;",
                          div(div(class="ct-sess-label", paste0("DAY ", wo$session_number)),
                              div(class="ct-sess-type",  wo$session_label)),
                          if (is_done)  div(class="ct-sess-check", "✓")
                          else if (is_today) div(style="color:#1D9E75;font-size:12px;font-weight:700;", "TODAY")
                          else if (!is_done && !is.na(wo_date) && wo_date < Sys.Date())
                            div(style="color:#555;font-size:10px;", "MISSED")
                          else NULL
                      ),
                      div(class="ct-sess-date",
                          if (!is.na(wo_date)) format(wo_date, "%b %d") else ""),
                      if (!is_done && !is.na(wo_date) && wo_date >= Sys.Date())
                        div(style="margin-top:5px;",
                            tags$button("Skip",
                                        style="font-size:10px;color:#555;background:none;border:none;
                             cursor:pointer;padding:0;text-decoration:underline;",
                                        onclick=sprintf(
                                          "Shiny.setInputValue('skip_workout_prompt','%s|%s',{priority:'event'});event.stopPropagation();",
                                          wo$id, wo$session_label)))
                  )
                })
              } else {
                div(style="color:#333;font-size:12px;padding:10px;", "No sessions")
              }
          )
      )
    })
  )
}

bottom_nav_ui <- function(active = "dashboard") {
  nav_item <- function(id, icon, label) {
    is_active <- identical(as.character(active %||% ""), id)
    tags$button(
      class   = trimws(paste("ct-nav-btn", if (is_active) "active" else "")),
      onclick = sprintf("Shiny.setInputValue('nav_tab','%s',{priority:'event'})", id),
      div(class = "nav-icon", icon),
      label
    )
  }
  div(class = "ct-bottom-nav",
      nav_item("dashboard", "📅", "Program"),
      nav_item("progress",  "📊", "Progress"),
      nav_item("friends",   "🏆", "Friends")
  )
}

# ── REDESIGN: Replace bottom_nav_ui with SVG icons ───────────
# Overrides the function defined above
nav_icon_program  <- '<svg style="width:22px;height:22px;" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="4" width="18" height="18" rx="2"/><line x1="16" y1="2" x2="16" y2="6"/><line x1="8" y1="2" x2="8" y2="6"/><line x1="3" y1="10" x2="21" y2="10"/><rect x="7" y="14" width="3" height="3" rx="0.5"/></svg>'
nav_icon_progress <- '<svg style="width:22px;height:22px;" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><polyline points="22 12 18 12 15 21 9 3 6 12 2 12"/></svg>'
nav_icon_friends  <- '<svg style="width:22px;height:22px;" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><circle cx="9" cy="7" r="4"/><path d="M3 21v-2a4 4 0 0 1 4-4h4a4 4 0 0 1 4 4v2"/><path d="M16 3.13a4 4 0 0 1 0 7.75"/><path d="M21 21v-2a4 4 0 0 0-3-3.87"/></svg>'
nav_icon_profile  <- '<svg style="width:22px;height:22px;" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><circle cx="12" cy="8" r="4"/><path d="M4 20c0-4 3.6-7 8-7s8 3 8 7"/></svg>'

bottom_nav_ui <- function(active = "dashboard") {
  nav_item <- function(id, icon_svg, label) {
    is_active <- identical(as.character(active %||% ""), id)
    tags$button(
      class   = trimws(paste("ct-nav-btn", if (is_active) "active" else "")),
      onclick = sprintf("Shiny.setInputValue('nav_tab','%s',{priority:'event'})", id),
      HTML(icon_svg),
      label
    )
  }
  div(class = "ct-bottom-nav",
      nav_item("dashboard", nav_icon_program,  "Program"),
      nav_item("progress",  nav_icon_progress, "Progress"),
      nav_item("friends",   nav_icon_friends,  "Friends"),
      nav_item("profile",   nav_icon_profile,  "Profile")
  )
}