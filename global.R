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

SUPABASE_URL         <- Sys.getenv("SUPABASE_URL")
SUPABASE_ANON_KEY    <- Sys.getenv("SUPABASE_ANON_KEY")
SUPABASE_SERVICE_KEY <- Sys.getenv("SUPABASE_SERVICE_KEY")
ANTHROPIC_API_KEY    <- Sys.getenv("ANTHROPIC_API_KEY")

# ── KIOSK AUTO-LOGIN ────────────────────────────────────────
# This is a single-user app: on startup it signs in automatically as the
# owner using these env vars, so there is no login screen. The email defaults
# to the owner's; set CATRACK_PASSWORD (Posit Connect → Environment Variables,
# and .Renviron locally). RLS still applies — the app holds a real user JWT.
# If the password is unset or wrong, the app falls back to the login form.
CATRACK_EMAIL    <- Sys.getenv("CATRACK_EMAIL", "catadamsm@gmail.com")
CATRACK_PASSWORD <- Sys.getenv("CATRACK_PASSWORD")

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

for (f in c("methodology.R", "program_generation.R", "workout_screen.R", "progress_screen.R",
            "program_screen.R", "profile_screen.R", "workout_summary.R")) {
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

sb_delete <- function(table, filter_params, token = NULL) {
  .sb_req(paste0(table, filter_params), token) |>
    req_method("DELETE") |>
    req_perform()
}

sb_refresh <- function(refresh_token) {
  resp <- request(paste0(SUPABASE_URL, "/auth/v1/token?grant_type=refresh_token")) |>
    req_headers(
      "apikey"       = SUPABASE_ANON_KEY,
      "Content-Type" = "application/json"
    ) |>
    req_body_raw(toJSON(list(refresh_token = refresh_token), auto_unbox = TRUE)) |>
    req_method("POST") |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()
  list(status = resp$status_code, body = tryCatch(fromJSON(resp_body_string(resp)), error = \(e) list()))
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
    kettlebell     = "Kettlebell",
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

# Session length budget. Picked at program creation; locked for the 12 weeks.
# Generator fills slots until the budget is met, then trims accessories.
SESSION_LENGTH_OPTIONS <- c(
  "30 min — quick" = 30,
  "45 min — standard" = 45,
  "60 min — long"     = 60
)

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

# Global runtime JS — session restore, keep-alive heartbeat, custom-message
# handlers, offline/disconnect self-heal, back-button guard, and password-
# recovery hash detection. Mounted ONCE in ui.R so it loads on every page.
# (It used to live inside login_page_ui, which no longer renders in normal
# kiosk operation, so these reliability features would otherwise never load.)
catrack_runtime_js <- function() {
      tags$script(HTML('
      (function() {
        // ── localStorage session restore ──────────────────────────
        // Saves/restores the Supabase refresh token so the user
        // stays logged in after screen timeout or browser reload.
        // Also restores the current page + active workout so a
        // disconnect mid-session returns you to your session, not
        // back to the dashboard.
        function tryRestore() {
          if (!window.Shiny) { setTimeout(tryRestore, 150); return; }
          var rt = localStorage.getItem("catrack_refresh_token");
          if (rt && rt.length > 10) {
            Shiny.setInputValue("restore_session_refresh", rt, {priority:"event"});
          }
          var lastPage = localStorage.getItem("catrack_last_page");
          var lastWo   = localStorage.getItem("catrack_last_workout_id");
          if (lastPage || lastWo) {
            Shiny.setInputValue("restore_last_view",
              JSON.stringify({page: lastPage || "", workout_id: lastWo || ""}),
              {priority:"event"});
          }
        }
        tryRestore();

        // ── Keep-alive heartbeat ──────────────────────────────────
        // Posit Connect drops idle websocket connections. Mid-workout a
        // user can rest 2-3 min between sets with no client->server
        // traffic, so the socket goes idle and disconnects — bouncing
        // them out of the session. A tiny periodic input keeps the
        // connection busy. Window-scoped so re-renders do not stack
        // multiple intervals. (Mobile browsers pause timers when the
        // screen locks, which is fine — that is not an active workout.)
        if (!window._catrackHeartbeat) {
          window._catrackHeartbeat = setInterval(function() {
            if (window.Shiny && Shiny.setInputValue) {
              Shiny.setInputValue("client_heartbeat", Date.now(), {priority:"event"});
            }
          }, 30000);
        }

        if (!window._catrackHandlersRegistered) {
          window._catrackHandlersRegistered = true;

          Shiny.addCustomMessageHandler("save_auth_session", function(msg) {
            if (msg.refresh_token) localStorage.setItem("catrack_refresh_token", msg.refresh_token);
            if (msg.email)         localStorage.setItem("catrack_email",         msg.email);
          });
          Shiny.addCustomMessageHandler("clear_auth_session", function(msg) {
            localStorage.removeItem("catrack_refresh_token");
            localStorage.removeItem("catrack_email");
            localStorage.removeItem("catrack_last_page");
            localStorage.removeItem("catrack_last_workout_id");
          });
          Shiny.addCustomMessageHandler("save_last_view", function(msg) {
            if (msg.page)        localStorage.setItem("catrack_last_page",        msg.page);
            else                 localStorage.removeItem("catrack_last_page");
            if (msg.workout_id)  localStorage.setItem("catrack_last_workout_id",  msg.workout_id);
            else                 localStorage.removeItem("catrack_last_workout_id");
          });
          // Server-driven input trigger — used to deep-link the user
          // back into an in-progress workout after a session restore.
          Shiny.addCustomMessageHandler("trigger_input", function(msg) {
            if (!msg || !msg.name) return;
            Shiny.setInputValue(msg.name, msg.value, {priority:"event"});
          });
        }

        // ── Offline / disconnect banner ──────────────────────────
        // Shown when the browser loses network OR Shiny disconnects
        // from the R server. Without this the app silently freezes
        // and the user has no signal that anything is wrong.
        if (!document.getElementById("catrack-offline-banner")) {
          var banner = document.createElement("div");
          banner.id = "catrack-offline-banner";
          banner.style.cssText = "position:fixed;top:0;left:0;right:0;" +
            "background:#854F0B;color:#ffe9c0;padding:8px 12px;" +
            "font-size:12px;text-align:center;z-index:9999;" +
            "display:none;font-family:system-ui,sans-serif;" +
            "box-shadow:0 2px 8px rgba(0,0,0,0.4);";
          banner.innerText = "⚠  You are offline — changes will sync when reconnected.";
          document.body && document.body.appendChild(banner);

          function showBanner(msg) {
            if (!banner) return;
            if (msg) banner.innerText = msg;
            banner.style.display = "block";
          }
          function hideBanner() {
            if (banner) banner.style.display = "none";
          }
          window.addEventListener("online",  hideBanner);
          window.addEventListener("offline", function() {
            showBanner("⚠  You are offline — changes will sync when reconnected.");
          });
          // Disconnect self-heal: reconnect briefly, then reload + restore.
          // The default Shiny disconnect overlay dims and freezes the screen
          // with no way back once Posit has dropped the session (the top
          // usability complaint). We suppress it and drive our own recovery:
          // show a clear Reconnecting screen, give Shiny auto-reconnect a
          // short grace period, then reload — which restores the session AND
          // the open workout from localStorage, so no manual re-login.
          if (!document.getElementById("catrack-hide-shiny-overlay")) {
            var _st = document.createElement("style");
            _st.id = "catrack-hide-shiny-overlay";
            _st.textContent = "#shiny-disconnected-overlay{display:none !important;}" +
              "@keyframes ctspin{to{transform:rotate(360deg)}}";
            document.head.appendChild(_st);
          }
          var _catrackReconnectTimer = null;
          function catrackShowReconnect() {
            var ov = document.getElementById("catrack-reconnect-overlay");
            if (!ov) {
              ov = document.createElement("div");
              ov.id = "catrack-reconnect-overlay";
              ov.style.cssText = "position:fixed;inset:0;z-index:10000;" +
                "background:rgba(12,12,12,0.94);display:flex;flex-direction:column;" +
                "align-items:center;justify-content:center;gap:14px;color:#f0f0f0;" +
                "font-family:system-ui,sans-serif;text-align:center;padding:24px;";
              var sp = document.createElement("div");
              sp.style.cssText = "width:34px;height:34px;border:3px solid #333;" +
                "border-top-color:#1D9E75;border-radius:50%;" +
                "animation:ctspin 0.9s linear infinite;";
              var t1 = document.createElement("div");
              t1.style.cssText = "font-size:15px;font-weight:700;";
              t1.textContent = "Reconnecting…";
              var t2 = document.createElement("div");
              t2.style.cssText = "font-size:12px;color:#aaa;max-width:270px;line-height:1.4;";
              t2.textContent = "Your session and workout are saved — this restores automatically.";
              var btn = document.createElement("button");
              btn.textContent = "Resume now";
              btn.style.cssText = "margin-top:6px;background:#1D9E75;color:#fff;border:none;" +
                "border-radius:10px;padding:11px 20px;font-size:14px;font-weight:700;cursor:pointer;";
              btn.addEventListener("click", function(){ location.reload(); });
              ov.appendChild(sp); ov.appendChild(t1); ov.appendChild(t2); ov.appendChild(btn);
              document.body.appendChild(ov);
            }
            ov.style.display = "flex";
          }
          function catrackHideReconnect() {
            var ov = document.getElementById("catrack-reconnect-overlay");
            if (ov) ov.style.display = "none";
          }
          document.addEventListener("shiny:disconnected", function() {
            showBanner("⟳  Reconnecting…");
            catrackShowReconnect();
            if (_catrackReconnectTimer) clearTimeout(_catrackReconnectTimer);
            _catrackReconnectTimer = setTimeout(function(){ location.reload(); }, 6000);
          });
          document.addEventListener("shiny:connected", function() {
            if (_catrackReconnectTimer) {
              clearTimeout(_catrackReconnectTimer); _catrackReconnectTimer = null;
            }
            catrackHideReconnect();
            hideBanner();
          });
          if (!navigator.onLine) showBanner();
        }

        // ── Back-button / swipe-back guard ────────────────────────
        // Without this the OS back gesture navigates the browser away and
        // closes the PWA (very easy to trigger by accident, especially on
        // the dim reconnect screen). We trap back navigation and route it
        // INSIDE the app instead: a sub-page (workout, progress, profile,
        // etc.) goes back to the dashboard; the dashboard stays put so the
        // app never closes from an accidental swipe.
        if (!window._catrackBackGuard) {
          window._catrackBackGuard = true;
          try { history.pushState({catrack: true}, ""); } catch (e) {}
          window.addEventListener("popstate", function() {
            // Re-arm immediately so there is always a state to pop.
            try { history.pushState({catrack: true}, ""); } catch (e) {}
            var page = localStorage.getItem("catrack_last_page") || "";
            var sub = ["workout","preview","summary","progress",
                       "profile","programs"];
            if (sub.indexOf(page) !== -1 && window.Shiny && Shiny.setInputValue) {
              Shiny.setInputValue("nav_tab", "dashboard", {priority: "event"});
            }
            // On dashboard / login / onboarding: do nothing — stay in app.
          });
        }

        // ── Supabase password recovery from URL hash ──────────────
        var h = window.location.hash + window.location.search;
        if (h.indexOf("type=recovery") !== -1) {
          setTimeout(function() {
            Shiny.setInputValue("url_recovery_token", h, {priority:"event"});
          }, 800);
        }
      })();
    '))
}

# Brief splash while the kiosk auto-login runs at startup.
loading_page_ui <- function() {
  div(class = "ct-onboard-step",
      style = "min-height:70vh; display:flex; flex-direction:column;
               align-items:center; justify-content:center; gap:16px;",
      div(class = "ct-logo-wrap", catrack_logo_svg("full")),
      tags$div(style = paste0(
        "width:30px; height:30px; border:3px solid #222; border-top-color:#1D9E75;",
        "border-radius:50%; animation:ctspin 0.9s linear infinite;")),
      div(style = "font-size:12px; color:#555;", "Loading your training…"),
      tags$style(HTML("@keyframes ctspin{to{transform:rotate(360deg)}}"))
  )
}

# Fallback login form. In normal kiosk operation the app auto-logs-in from the
# CATRACK_EMAIL / CATRACK_PASSWORD env vars, so this is never shown; it appears
# only if those are unset or the auto-login fails, so the app never bricks.
login_page_ui <- function(mode = "login") {
  div(class = "ct-onboard-step",
      div(class = "ct-logo-wrap", catrack_logo_svg("full")),
      div(class = "ct-tagline", "Science-based training. Built around you."),
      div(class = "ct-auth-card",
          h5("Sign in", style = "font-weight:700; margin-bottom:20px;"),
          textInput("auth_email", "Email", placeholder = "you@example.com"),
          passwordInput("auth_password", "Password", placeholder = "••••••••"),
          uiOutput("auth_error"),
          tags$button("Log in", class = "ct-btn-primary",
                      onclick = "Shiny.setInputValue('auth_action', 'login', {priority:'event'})")
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
                           ),
                           # ── Conditional: pull-up baseline (only for pull_up goal) ──
                           if (isTRUE(values$goal == "pull_up")) {
                             pu_baseline <- as.integer(values$pullup_baseline %||% 0L)
                             pu_buckets <- list(
                               list(val = 0L, label = "Zero",     sub = "Can't do one yet"),
                               list(val = 1L, label = "1",        sub = "Working on it"),
                               list(val = 3L, label = "2–3",      sub = "Getting there"),
                               list(val = 6L, label = "4–7",      sub = "Solid"),
                               list(val = 10L, label = "8+",      sub = "Advanced")
                             )
                             tagList(
                               div(class = "ct-section-title", style = "margin-top:18px;",
                                   "How many strict pull-ups can you do?"),
                               div(style = "font-size:11px; color:#555; margin-bottom:8px;",
                                   "Block A will start with band-assisted or eccentric work if you're under 3."),
                               div(style = "display:grid; grid-template-columns:1fr 1fr; gap:6px;",
                                   lapply(pu_buckets, function(b) {
                                     is_sel <- isTRUE(pu_baseline == b$val)
                                     div(class = paste("ct-goal-card", if (is_sel) "selected"),
                                         style = "padding:10px 8px;",
                                         onclick = sprintf(
                                           "Shiny.setInputValue('select_pullup_baseline',%d,{priority:'event'})", b$val),
                                         div(class = "ct-goal-label", b$label),
                                         div(class = "ct-goal-desc",  b$sub))
                                   })
                               )
                             )
                           }
                         ),
                         "3" = tagList(
                           div(class = "ct-step-title", "How often and how?"),
                           div(class = "ct-step-sub", "Choose your weekly frequency, session length, and split."),
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
                           div(class = "ct-section-title", "Session length"),
                           div(style = "display:flex; gap:8px; margin-bottom:16px;",
                               lapply(names(SESSION_LENGTH_OPTIONS), function(label) {
                                 val    <- as.integer(SESSION_LENGTH_OPTIONS[[label]])
                                 is_sel <- isTRUE(as.integer(values$session_length_minutes %||% 45L) == val)
                                 sub    <- strsplit(label, " — ")[[1]][2]
                                 div(class = paste("ct-session-card", if (is_sel) "today" else "future"),
                                     style = "cursor:pointer; text-align:center; padding:14px;",
                                     onclick = sprintf(
                                       "Shiny.setInputValue('select_session_length',%d,{priority:'event'})", val),
                                     div(class = "ct-sess-type", style = "font-size:18px;", paste0(val, " min")),
                                     div(class = "ct-sess-date", sub %||% ""))
                               })
                           ),
                           div(class = "ct-section-title", "Split style"),
                           # Running support is full-body only — the split choice
                           # is fixed (concurrent-training evidence), so show a note
                           # instead of the selector for that goal.
                           if (isTRUE(values$goal == "running_support"))
                             div(class = "ct-session-card today", style = "cursor:default;",
                                 div(class = "ct-sess-type", "Full Body (fixed)"),
                                 div(class = "ct-sess-date",
                                     "Running support runs a low-volume, legs-biased full-body plan every session — no split to choose."))
                           else
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
                           textInput("display_name", "Your name (used in your program names)",
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
                                   div(div(class="ct-sess-label","SESSION LENGTH"),
                                       div(class="ct-sess-type",
                                           paste0(values$session_length_minutes %||% 45L, " min"))),
                                   div(div(class="ct-sess-label","SPLIT"),
                                       div(class="ct-sess-type",
                                           names(SPLIT_OPTIONS)[
                                             SPLIT_OPTIONS == (values$split_style %||% "full_body")])),
                                   div(div(class="ct-sess-label","EQUIPMENT"),
                                       div(class="ct-sess-type",
                                           paste0(length(values$equipment %||% character(0)), " items"))),
                                   if (isTRUE(values$goal == "pull_up"))
                                     div(div(class="ct-sess-label","PULL-UP BASELINE"),
                                         div(class="ct-sess-type",
                                             paste0(as.integer(values$pullup_baseline %||% 0L), " strict"))),
                                   div(div(class="ct-sess-label","BLOCK"),
                                       div(class="ct-sess-type","12 weeks · Block 1"))
                               )
                           ),
                           div(style = "text-align:center; margin-top:10px;",
                               methodology_info_link("How is this built? →")),
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
          div(style = "display:flex; align-items:center; gap:8px;",
              div(class = "ct-block-title", program$name),
              tags$button(
                HTML("&#9998;"),
                title = "Rename program",
                style = paste0(
                  "background:#1e1e1e; border:1px solid #2a2a2a; border-radius:50%;",
                  "width:22px; height:22px; padding:0; color:#5DCAA5;",
                  "font-size:11px; cursor:pointer; line-height:1;",
                  "display:inline-flex; align-items:center; justify-content:center;"),
                onclick = sprintf(
                  "Shiny.setInputValue('rename_program','%s',{priority:'event'})",
                  program$id)),
              methodology_info_btn("sm"))
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
      block_char    <- c("A","B","C")[min(3L, ceiling(w/4))]
      is_current    <- isTRUE(w == current_week)
      
      div(class = "ct-week-row",
          div(class = paste("ct-week-label", if (is_current) "current"),
              sprintf("WEEK %d — BLOCK %s", w, block_char)),
          div(class = "ct-sessions-row",
              if (!is.null(week_workouts) && nrow(week_workouts) > 0) {
                lapply(seq_len(nrow(week_workouts)), function(s) {
                  wo      <- week_workouts[s, ]
                  # Week-based (not day-based): sessions stay fully
                  # actionable — do or skip — any time, even if they bleed
                  # into a later week. No per-day date labels or "missed"
                  # gating that used to block skipping a late session.
                  completed_val <- tryCatch(wo$completed_at, error = \(e) NA)
                  is_done <- isTRUE(
                    !is.null(completed_val) &&
                      length(completed_val) > 0 &&
                      !is.na(completed_val) &&
                      nchar(as.character(completed_val)) > 5
                  )
                  is_skipped <- isTRUE(tryCatch(
                    as.logical(wo$is_skipped), error = \(e) FALSE))

                  card_class <- paste("ct-session-card",
                                      if (is_done) "completed"
                                      else if (!is_skipped && is_current) "today"
                                      else "future")

                  div(class = card_class,
                      onclick = if (!is_done) sprintf(
                        "Shiny.setInputValue('open_preview','%s',{priority:'event'})",
                        wo$id) else NULL,
                      div(style="display:flex;justify-content:space-between;align-items:flex-start;",
                          div(div(class="ct-sess-label", paste0("SESSION ", wo$session_number)),
                              div(class="ct-sess-type",  wo$session_label)),
                          if (is_done) div(class="ct-sess-check", "✓")
                          else if (is_skipped)
                            div(style="color:#777;font-size:10px;font-weight:700;", "SKIPPED")
                          else NULL
                      ),
                      if (is_done)
                        tags$button("View Summary",
                          style=paste0("margin-top:6px;font-size:11px;color:#1D9E75;",
                                       "background:none;border:none;cursor:pointer;padding:0;",
                                       "text-decoration:underline;font-weight:600;"),
                          onclick=sprintf(
                            "Shiny.setInputValue('view_summary','%s',{priority:'event'});event.stopPropagation();",
                            wo$id))
                      else if (!is_skipped)
                        # Skip is available for ANY incomplete session now,
                        # regardless of how many days have passed.
                        div(style="margin-top:5px;",
                            tags$button("Skip",
                                        style="font-size:10px;color:#555;background:none;border:none;
                             cursor:pointer;padding:0;text-decoration:underline;",
                                        onclick=sprintf(
                                          "Shiny.setInputValue('skip_workout_prompt','%s|%s',{priority:'event'});event.stopPropagation();",
                                          wo$id, wo$session_label)))
                      else NULL
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

# ── Bottom navigation (SVG icons) ────────────────────────────
nav_icon_program  <- '<svg style="width:22px;height:22px;" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="4" width="18" height="18" rx="2"/><line x1="16" y1="2" x2="16" y2="6"/><line x1="8" y1="2" x2="8" y2="6"/><line x1="3" y1="10" x2="21" y2="10"/><rect x="7" y="14" width="3" height="3" rx="0.5"/></svg>'
nav_icon_progress <- '<svg style="width:22px;height:22px;" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"><polyline points="22 12 18 12 15 21 9 3 6 12 2 12"/></svg>'
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
      nav_item("profile",   nav_icon_profile,  "Profile")
  )
}