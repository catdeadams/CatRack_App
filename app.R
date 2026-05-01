# ============================================================
# app.R — CaTrack
# Entry point for Posit Connect deployment.
# global.R, ui.R, server.R are loaded automatically by Shiny
# when app.R calls shinyApp() in a directory containing them.
# ============================================================

source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)