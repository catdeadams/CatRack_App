# ============================================================
# app.R — DIAGNOSTIC VERSION
# Replace your real app.R with this temporarily.
# Deploy this to Posit Connect to find exactly what's crashing.
# ============================================================

library(shiny)

# Test each library one at a time
tryCatch(library(bslib),      error = function(e) message("FAIL bslib: ",     e$message))
tryCatch(library(httr2),      error = function(e) message("FAIL httr2: ",     e$message))
tryCatch(library(jsonlite),   error = function(e) message("FAIL jsonlite: ",  e$message))
tryCatch(library(dplyr),      error = function(e) message("FAIL dplyr: ",     e$message))
tryCatch(library(lubridate),  error = function(e) message("FAIL lubridate: ", e$message))
tryCatch(library(plotly),     error = function(e) message("FAIL plotly: ",    e$message))

message("All libraries loaded OK")

# Test environment variables
message("SUPABASE_URL set: ",         nchar(Sys.getenv("SUPABASE_URL")) > 0)
message("SUPABASE_ANON_KEY set: ",    nchar(Sys.getenv("SUPABASE_ANON_KEY")) > 0)
message("SUPABASE_SERVICE_KEY set: ", nchar(Sys.getenv("SUPABASE_SERVICE_KEY")) > 0)

# Test bslib theme without font_google
tryCatch({
  theme <- bslib::bs_theme(version = 5, bg = "#0f0f0f", fg = "#f0f0f0")
  message("Theme OK")
}, error = function(e) message("FAIL theme: ", e$message))

# Minimal working UI and server
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bg = "#0f0f0f", fg = "#f0f0f0"),
  h2("CaTrack diagnostic — if you see this, the base app works",
     style = "color:white; padding:40px;")
)

server <- function(input, output, session) {
  message("Server started OK")
}

shinyApp(ui = ui, server = server)