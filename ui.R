# ============================================================
# ui.R — CaTrack
# Keep this file minimal. All helper functions live in global.R
# so they are accessible to both ui.R and server.R.
# The ui object MUST be the last expression in this file.
# ============================================================

ct_css <- "
  body { background: #0f0f0f; color: #f0f0f0; }
  .ct-app { max-width: 480px; margin: 0 auto; padding: 16px; min-height: 100vh; }
  .ct-logo    { font-size: 28px; font-weight: 700; color: #e8ff47; letter-spacing: -1px; margin-bottom: 4px; }
  .ct-tagline { font-size: 13px; color: #666; margin-bottom: 32px; }
  .ct-auth-card { background: #1a1a1a; border-radius: 16px; padding: 28px; margin-top: 40px; }
  .ct-onboard-step { animation: fadeIn 0.25s ease; }
  @keyframes fadeIn { from { opacity:0; transform:translateY(8px); } to { opacity:1; transform:translateY(0); } }
  .ct-step-indicator { display:flex; gap:6px; margin-bottom:24px; }
  .ct-step-dot { flex:1; height:4px; border-radius:2px; background:#2a2a2a; }
  .ct-step-dot.active { background:#e8ff47; }
  .ct-step-dot.done   { background:#4ade80; }
  .ct-step-title { font-size:20px; font-weight:700; margin-bottom:6px; }
  .ct-step-sub   { font-size:13px; color:#888; margin-bottom:20px; }
  .ct-goal-grid { display:grid; grid-template-columns:1fr 1fr; gap:10px; }
  .ct-goal-card { background:#1e1e1e; border:2px solid #2a2a2a; border-radius:12px; padding:14px 12px; cursor:pointer; transition:all 0.15s; text-align:center; }
  .ct-goal-card:hover    { border-color:#444; }
  .ct-goal-card.selected { border-color:#e8ff47; background:#1e2200; }
  .ct-goal-icon  { font-size:26px; margin-bottom:6px; }
  .ct-goal-label { font-size:12px; font-weight:600; color:#ddd; }
  .ct-goal-desc  { font-size:11px; color:#666; margin-top:4px; line-height:1.3; }
  .ct-equip-category { font-size:11px; font-weight:700; color:#888; letter-spacing:0.08em; text-transform:uppercase; margin:16px 0 8px; }
  .ct-equip-grid { display:grid; grid-template-columns:1fr 1fr; gap:6px; }
  .ct-equip-item { background:#1e1e1e; border:1.5px solid #2a2a2a; border-radius:8px; padding:8px 10px; cursor:pointer; font-size:12px; color:#ccc; transition:all 0.12s; display:flex; align-items:center; gap:6px; }
  .ct-equip-item:hover    { border-color:#444; }
  .ct-equip-item.selected { border-color:#e8ff47; background:#1e2200; color:#fff; }
  .ct-equip-check { width:14px; height:14px; border-radius:3px; border:1.5px solid #444; flex-shrink:0; display:flex; align-items:center; justify-content:center; font-size:10px; font-weight:700; }
  .ct-equip-item.selected .ct-equip-check { background:#e8ff47; border-color:#e8ff47; color:#000; }
  .ct-bottom-nav { position:fixed; bottom:0; left:50%; transform:translateX(-50%); width:100%; max-width:480px; background:#1a1a1a; border-top:1px solid #2a2a2a; display:flex; z-index:100; }
  .ct-nav-btn { flex:1; padding:10px 0; text-align:center; cursor:pointer; color:#555; font-size:10px; font-weight:600; border:none; background:none; transition:color 0.15s; display:flex; flex-direction:column; align-items:center; gap:2px; }
  .ct-nav-btn .nav-icon { font-size:20px; }
  .ct-nav-btn.active { color:#e8ff47; }
  .ct-content-with-nav { padding-bottom:70px; }
  .ct-block-header { display:flex; justify-content:space-between; align-items:center; margin-bottom:16px; }
  .ct-block-label  { font-size:13px; color:#888; }
  .ct-block-title  { font-size:18px; font-weight:700; }
  .ct-week-row     { margin-bottom:8px; }
  .ct-week-label   { font-size:10px; color:#555; font-weight:600; text-transform:uppercase; letter-spacing:0.06em; margin-bottom:4px; }
  .ct-week-label.current { color:#e8ff47; }
  .ct-sessions-row  { display:flex; gap:6px; }
  .ct-session-card  { flex:1; background:#1a1a1a; border:1.5px solid #2a2a2a; border-radius:10px; padding:10px 8px; cursor:pointer; transition:all 0.15s; min-height:70px; }
  .ct-session-card:hover     { border-color:#444; transform:translateY(-1px); }
  .ct-session-card.completed { border-color:#4ade80; background:#0a1f0a; }
  .ct-session-card.today     { border-color:#e8ff47; background:#1e2200; }
  .ct-session-card.future    { opacity:0.65; }
  .ct-sess-label { font-size:10px; font-weight:700; color:#888; text-transform:uppercase; letter-spacing:0.05em; }
  .ct-sess-type  { font-size:12px; font-weight:600; color:#ddd; margin:3px 0; }
  .ct-sess-date  { font-size:10px; color:#555; }
  .ct-sess-check { color:#4ade80; font-size:14px; }
  .ct-progress-bar  { background:#2a2a2a; border-radius:4px; height:6px; margin:12px 0; overflow:hidden; }
  .ct-progress-fill { height:100%; border-radius:4px; background:linear-gradient(90deg,#e8ff47,#4ade80); transition:width 0.4s ease; }
  .ct-btn-primary   { background:#e8ff47; color:#000; font-weight:700; border:none; border-radius:10px; padding:14px; width:100%; font-size:15px; cursor:pointer; transition:opacity 0.15s; margin-top:8px; }
  .ct-btn-primary:hover { opacity:0.88; }
  .ct-btn-secondary { background:#2a2a2a; color:#ddd; font-weight:600; border:none; border-radius:10px; padding:12px; width:100%; font-size:14px; cursor:pointer; }
  .ct-btn-sm { padding:8px 16px; font-size:13px; width:auto; border-radius:8px; }
  .form-control { background:#1e1e1e !important; border:1.5px solid #2a2a2a !important; color:#f0f0f0 !important; border-radius:10px !important; }
  .form-control:focus { border-color:#e8ff47 !important; box-shadow:0 0 0 3px rgba(232,255,71,0.15) !important; }
  .form-label  { color:#aaa; font-size:13px; font-weight:600; }
  .form-select { background:#1e1e1e !important; border:1.5px solid #2a2a2a !important; color:#f0f0f0 !important; border-radius:10px !important; }
  .ct-alert         { padding:12px 14px; border-radius:8px; font-size:13px; margin:8px 0; }
  .ct-alert-error   { background:#2d0f0f; border:1px solid #f87171; color:#f87171; }
  .ct-alert-success { background:#0a1f0a; border:1px solid #4ade80; color:#4ade80; }
  .ct-divider       { border:none; border-top:1px solid #2a2a2a; margin:16px 0; }
  .text-muted       { color:#666 !important; }
  .ct-section-title { font-size:11px; font-weight:700; color:#666; text-transform:uppercase; letter-spacing:0.08em; margin-bottom:10px; }
"

# ── UI object — MUST be the last expression in this file ─────
ui <- page_fluid(
  theme = catrack_theme,
  tags$head(tags$style(HTML(ct_css))),
  div(class = "ct-app",
      uiOutput("main_ui")
  )
)