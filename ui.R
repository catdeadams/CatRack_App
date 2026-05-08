# ============================================================
# ui.R — CaTrack  (Session 8 redesign)
# CSS only — all helper functions live in global.R
# ui object MUST be the last expression in this file
# ============================================================

ct_css <- "
  body { background:#0f0f0f; color:#f0f0f0; }
  .ct-app { max-width:480px; margin:0 auto; padding:16px; min-height:100vh; }

  /* ── Logo ── */
  .ct-logo-wrap  { text-align:center; margin-bottom:8px; }
  .ct-tagline    { font-size:13px; color:#555; margin-bottom:28px; text-align:center; letter-spacing:0.04em; }

  /* ── Auth card ── */
  .ct-auth-card { background:#161616; border:1px solid #222; border-radius:16px; padding:28px; margin-top:32px; }

  /* ── Onboarding ── */
  .ct-onboard-step { animation:fadeIn 0.2s ease; }
  @keyframes fadeIn { from{opacity:0;transform:translateY(6px);}to{opacity:1;transform:translateY(0);} }
  .ct-step-indicator { display:flex; gap:5px; margin-bottom:24px; }
  .ct-step-dot  { flex:1; height:3px; border-radius:2px; background:#222; transition:background 0.2s; }
  .ct-step-dot.active { background:#1D9E75; }
  .ct-step-dot.done   { background:#0F6E56; }
  .ct-step-title { font-size:20px; font-weight:700; margin-bottom:5px; color:#f0f0f0; }
  .ct-step-sub   { font-size:13px; color:#666; margin-bottom:20px; line-height:1.5; }

  /* ── Goal cards ── */
  .ct-goal-grid  { display:grid; grid-template-columns:1fr 1fr; gap:8px; }
  .ct-goal-card  { background:#161616; border:1.5px solid #262626; border-radius:12px; padding:14px 12px;
                   cursor:pointer; transition:all 0.15s; text-align:center; }
  .ct-goal-card:hover    { border-color:#333; }
  .ct-goal-card.selected { border-color:#1D9E75; background:#061a12; }
  .ct-goal-icon  { font-size:22px; margin-bottom:5px; }
  .ct-goal-label { font-size:12px; font-weight:600; color:#ccc; }
  .ct-goal-desc  { font-size:10px; color:#555; margin-top:3px; line-height:1.35; }

  /* ── Equipment ── */
  .ct-equip-category { font-size:10px; font-weight:700; color:#555; letter-spacing:0.1em;
                       text-transform:uppercase; margin:14px 0 6px; }
  .ct-equip-grid { display:grid; grid-template-columns:1fr 1fr; gap:5px; }
  .ct-equip-item { background:#161616; border:1.5px solid #262626; border-radius:8px; padding:8px 10px;
                   cursor:pointer; font-size:12px; color:#aaa; transition:all 0.12s;
                   display:flex; align-items:center; gap:6px; }
  .ct-equip-item:hover    { border-color:#333; color:#ddd; }
  .ct-equip-item.selected { border-color:#1D9E75; background:#061a12; color:#f0f0f0; }
  .ct-equip-check { width:14px; height:14px; border-radius:3px; border:1.5px solid #333; flex-shrink:0;
                    display:flex; align-items:center; justify-content:center; font-size:9px; font-weight:700; }
  .ct-equip-item.selected .ct-equip-check { background:#1D9E75; border-color:#1D9E75; color:#fff; }

  /* ── Bottom nav ── */
  .ct-bottom-nav { position:fixed; bottom:0; left:50%; transform:translateX(-50%);
                   width:100%; max-width:480px; background:#111; border-top:1px solid #1e1e1e;
                   display:flex; z-index:100; }
  .ct-nav-btn    { flex:1; padding:10px 0; cursor:pointer; color:#444; font-size:10px; font-weight:600;
                   border:none; background:none; transition:color 0.15s;
                   display:flex; flex-direction:column; align-items:center; gap:3px; letter-spacing:0.04em; }
  .ct-nav-icon   { width:22px; height:22px; }
  .ct-nav-btn.active         { color:#1D9E75; }
  .ct-nav-btn.active svg path,
  .ct-nav-btn.active svg rect,
  .ct-nav-btn.active svg circle,
  .ct-nav-btn.active svg polyline { stroke:#1D9E75; }
  .ct-content-with-nav { padding-bottom:72px; }

  /* ── Dashboard header ── */
  .ct-dash-header { display:flex; justify-content:space-between; align-items:center; margin-bottom:16px; }
  .ct-block-label { font-size:11px; color:#555; letter-spacing:0.06em; text-transform:uppercase; }
  .ct-block-title { font-size:17px; font-weight:700; color:#f0f0f0; margin-top:2px; }

  /* ── Progress bar ── */
  .ct-progress-bar  { background:#1e1e1e; border-radius:4px; height:5px; margin:10px 0; overflow:hidden; }
  .ct-progress-fill { height:100%; border-radius:4px; background:#1D9E75; transition:width 0.5s ease; }

  /* ── Week / session calendar ── */
  .ct-week-row   { margin-bottom:7px; }
  .ct-week-label { font-size:9px; color:#444; font-weight:700; text-transform:uppercase;
                   letter-spacing:0.08em; margin-bottom:4px; }
  .ct-week-label.current { color:#5DCAA5; }
  .ct-sessions-row { display:flex; gap:5px; }
  .ct-session-card { flex:1; background:#161616; border:1px solid #222; border-radius:10px;
                     padding:10px 8px; cursor:pointer; transition:all 0.15s; min-height:68px; }
  .ct-session-card:hover     { border-color:#2a2a2a; transform:translateY(-1px); }
  .ct-session-card.completed { border-color:#0F6E56; background:#061a12; }
  .ct-session-card.today     { border-color:#1D9E75; background:#061a12; }
  .ct-session-card.future    { opacity:0.55; }
  .ct-sess-label { font-size:9px; font-weight:700; color:#555; text-transform:uppercase; letter-spacing:0.06em; }
  .ct-sess-type  { font-size:12px; font-weight:600; color:#ddd; margin:3px 0 1px; }
  .ct-sess-date  { font-size:10px; color:#444; }
  .ct-sess-check { color:#4ade80; font-size:13px; }

  /* ── Workout screen ── */
  .ct-workout-card { background:#161616; border-radius:12px; padding:14px; margin-bottom:10px;
                     border:1px solid #222; }
  .ct-set-input:focus { border-color:#1D9E75 !important; outline:none;
                        box-shadow:0 0 0 2px rgba(29,158,117,0.18) !important; }

  /* ── Buttons ── */
  .ct-btn-primary  { background:#1D9E75; color:#fff; font-weight:700; border:none;
                     border-radius:10px; padding:14px; width:100%; font-size:15px;
                     cursor:pointer; transition:background 0.15s; margin-top:8px; letter-spacing:0.01em; }
  .ct-btn-primary:hover { background:#17886a; }
  .ct-btn-secondary { background:#1e1e1e; color:#aaa; font-weight:600; border:1px solid #2a2a2a;
                      border-radius:10px; padding:12px; width:100%; font-size:14px;
                      cursor:pointer; transition:background 0.15s; }
  .ct-btn-secondary:hover { background:#252525; color:#ddd; }
  .ct-btn-sm { padding:7px 14px !important; font-size:12px !important;
               width:auto !important; border-radius:8px !important; }
  .ct-btn-danger { background:#2d0f0f; color:#f87171; border:1px solid #3d1515;
                   border-radius:10px; padding:12px; width:100%; font-size:14px;
                   font-weight:600; cursor:pointer; }

  /* ── Form inputs ── */
  .form-control { background:#161616 !important; border:1.5px solid #262626 !important;
                  color:#f0f0f0 !important; border-radius:10px !important; }
  .form-control:focus { border-color:#1D9E75 !important;
                        box-shadow:0 0 0 3px rgba(29,158,117,0.18) !important; }
  .form-label  { color:#888; font-size:13px; font-weight:600; }
  .form-select { background:#161616 !important; border:1.5px solid #262626 !important;
                 color:#f0f0f0 !important; border-radius:10px !important; }

  /* ── Alerts ── */
  .ct-alert         { padding:11px 14px; border-radius:8px; font-size:13px; margin:8px 0; line-height:1.5; }
  .ct-alert-error   { background:#1a0a0a; border:1px solid #3d1515; color:#f87171; }
  .ct-alert-success { background:#061a12; border:1px solid #0F6E56; color:#5DCAA5; }

  /* ── Misc ── */
  .ct-divider     { border:none; border-top:1px solid #1e1e1e; margin:16px 0; }
  .text-muted     { color:#555 !important; }
  .ct-section-title { font-size:10px; font-weight:700; color:#555; text-transform:uppercase;
                      letter-spacing:0.1em; margin-bottom:8px; }
  .ct-coaching-tip { background:#061a12; border-left:2px solid #1D9E75; border-radius:0 6px 6px 0;
                     padding:7px 10px; font-size:11px; color:#5DCAA5; margin-bottom:10px; line-height:1.45; }
  a { color:#5DCAA5; text-decoration:none; }
  a:hover { color:#1D9E75; text-decoration:underline; }
"

# ── UI object — MUST be the last expression in this file ─────
ui <- page_fluid(
  theme = catrack_theme,
  tags$head(
    tags$style(HTML(ct_css)),

    # ── PWA ─────────────────────────────────────────────────
    tags$link(rel = "manifest", href = "manifest.json"),
    tags$meta(name = "theme-color",                          content = "#1D9E75"),
    tags$meta(name = "mobile-web-app-capable",               content = "yes"),
    tags$meta(name = "apple-mobile-web-app-capable",         content = "yes"),
    tags$meta(name = "apple-mobile-web-app-status-bar-style",content = "black-translucent"),
    tags$meta(name = "apple-mobile-web-app-title",           content = "CatRack"),
    tags$link(rel = "apple-touch-icon",        href = "icons/icon-192.png"),
    tags$link(rel = "icon", type = "image/svg+xml", href = "icons/icon.svg"),

    tags$script(HTML("
      if ('serviceWorker' in navigator) {
        window.addEventListener('load', function() {
          navigator.serviceWorker.register('sw.js').catch(function() {});
        });
      }
    "))
  ),
  div(class = "ct-app",
      uiOutput("main_ui")
  )
)