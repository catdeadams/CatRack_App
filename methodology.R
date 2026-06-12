# ============================================================
# methodology.R — CaTrack
# Single source of truth for the programming methodology.
# Surfaced in-app via the "Programming Method" info modal.
# ============================================================

# ── VOLUME TARGETS ───────────────────────────────────────────
# Weekly working sets per muscle (Nippard / RP framework).
# Hypertrophy + intermediate is the anchor; goal & experience
# modifiers in program_generation.R scale these.
#
# MV  = Maintenance Volume      (keep what you've got)
# MEV = Minimum Effective Vol.  (smallest dose that grows)
# MAV = Maximum Adaptive Vol.   (sweet spot — programs target here)
# MRV = Maximum Recoverable Vol.(ceiling, only Block C flirts with it)

VOLUME_TARGETS_HYPERTROPHY_INTERMEDIATE <- list(
  quads       = list(MV = 4, MEV = 8,  MAV_low = 12, MAV_high = 18, MRV = 20),
  hamstrings  = list(MV = 3, MEV = 6,  MAV_low = 10, MAV_high = 16, MRV = 20),
  glutes      = list(MV = 0, MEV = 0,  MAV_low = 8,  MAV_high = 12, MRV = 16),
  chest       = list(MV = 6, MEV = 8,  MAV_low = 12, MAV_high = 20, MRV = 22),
  lats        = list(MV = 6, MEV = 10, MAV_low = 14, MAV_high = 22, MRV = 25),
  mid_back    = list(MV = 4, MEV = 8,  MAV_low = 12, MAV_high = 18, MRV = 20),
  front_delts = list(MV = 0, MEV = 0,  MAV_low = 6,  MAV_high = 8,  MRV = 12),
  mid_delts   = list(MV = 4, MEV = 8,  MAV_low = 12, MAV_high = 20, MRV = 26),
  rear_delts  = list(MV = 4, MEV = 8,  MAV_low = 12, MAV_high = 20, MRV = 26),
  biceps      = list(MV = 5, MEV = 8,  MAV_low = 14, MAV_high = 20, MRV = 26),
  triceps     = list(MV = 4, MEV = 6,  MAV_low = 10, MAV_high = 14, MRV = 18),
  calves      = list(MV = 6, MEV = 8,  MAV_low = 12, MAV_high = 16, MRV = 20),
  core        = list(MV = 0, MEV = 0,  MAV_low = 8,  MAV_high = 12, MRV = 16)
)

# ── GOAL MULTIPLIERS ─────────────────────────────────────────
# Applied to MAV_low / MAV_high of the hypertrophy base.
# Floors at MV — never schedule below maintenance for trained muscles.

GOAL_VOLUME_MULTIPLIERS <- list(
  hypertrophy = list(
    quads = 1.0, hamstrings = 1.0, glutes = 1.0, chest = 1.0, lats = 1.0,
    mid_back = 1.0, front_delts = 1.0, mid_delts = 1.0, rear_delts = 1.0,
    biceps = 1.0, triceps = 1.0, calves = 1.0, core = 1.0
  ),
  strength = list(
    # Heavy compound focus — fewer total sets, more 3-6 rep work
    quads = 0.7, hamstrings = 0.7, glutes = 0.7, chest = 0.7, lats = 0.7,
    mid_back = 0.6, front_delts = 0.5, mid_delts = 0.5, rear_delts = 0.5,
    biceps = 0.5, triceps = 0.5, calves = 0.5, core = 0.6
  ),
  pull_up = list(
    # Pulling emphasis, but lower body still gets maintenance work —
    # entirely cutting legs caused the "no lower body" complaint and
    # leaves real strength on the table for full-body splits.
    quads = 0.35, hamstrings = 0.35, glutes = 0.35, chest = 0.4,
    lats = 1.4, mid_back = 1.3, front_delts = 0.0,
    mid_delts = 0.3, rear_delts = 1.2, biceps = 1.2, triceps = 0.3,
    calves = 0.3, core = 0.8
  ),
  running_support = list(
    # Lower body + posterior chain + core; upper body maintenance
    quads = 1.1, hamstrings = 1.2, glutes = 1.2, chest = 0.0, lats = 0.0,
    mid_back = 0.5, front_delts = 0.0, mid_delts = 0.0, rear_delts = 0.0,
    biceps = 0.0, triceps = 0.0, calves = 1.5, core = 1.5
  ),
  functional = list(
    # Balanced base, core/calves bumped, single-leg emphasis
    quads = 1.0, hamstrings = 1.0, glutes = 1.0, chest = 0.9, lats = 0.9,
    mid_back = 1.0, front_delts = 0.7, mid_delts = 0.9, rear_delts = 1.0,
    biceps = 0.7, triceps = 0.7, calves = 1.3, core = 1.5
  )
)

# ── EXPERIENCE MODIFIERS ─────────────────────────────────────
# Where in the MAV range we target by experience.
EXPERIENCE_MAV_POSITION <- list(
  beginner     = 0.25,  # near MAV_low — less recovery debt while patterns groove
  intermediate = 0.50,  # mid-MAV
  advanced     = 0.85   # upper MAV (block C pushes toward MRV)
)

# ── BLOCK INTENSITY (A/B/C across 12 weeks) ──────────────────
# Each block is 4 weeks. Within a block, week 1-3 progress and week 4 deloads.
BLOCK_PROFILES <- list(
  A = list(
    label = "Foundation",
    description = "Groove the patterns, establish working weights.",
    rir_target = 3.0,        # 3 reps in reserve = RPE ~7
    volume_mult = 0.85       # below MAV target — accumulation
  ),
  B = list(
    label = "Intensification",
    description = "Push working weight harder. Sets near MAV.",
    rir_target = 1.5,        # RPE ~8.5
    volume_mult = 1.0
  ),
  C = list(
    label = "Realization",
    description = "Push toward MRV. Test rep PRs / approach 1RM.",
    rir_target = 0.5,        # RPE ~9.5
    volume_mult = 1.15       # creep toward MRV
  )
)

# ── WEEK-IN-BLOCK MODIFIERS ──────────────────────────────────
# Each 4-week block: Base → Build → Peak → Deload
WEEK_IN_BLOCK_PROFILES <- list(
  `1` = list(label = "Base",   volume_mult = 0.85, rir_mod =  0.5),
  `2` = list(label = "Build",  volume_mult = 1.00, rir_mod =  0.0),
  `3` = list(label = "Peak",   volume_mult = 1.05, rir_mod = -0.5),
  `4` = list(label = "Deload", volume_mult = 0.60, rir_mod =  2.0)
)

# ── SESSION TIME BUDGET ──────────────────────────────────────
# Minutes per slot type (warmups, working sets, rest baked in).
SLOT_TIME_COST <- list(
  heavy_compound = 12,  # ~3 working sets + 2 warmups, 3 min rest
  compound       = 10,  # 3 working sets, 2-3 min rest
  isolation      = 5,   # 2 sets, 60-90s rest
  superset_block = 8    # paired isolation, ~1.5x a solo isolation
)

# Targets — generator fills slots until budget is met, trims from bottom.
SESSION_LENGTH_BUDGETS <- list(
  `30` = 30L,
  `45` = 45L,
  `60` = 60L
)

# ── GOAL DEFINITIONS ─────────────────────────────────────────
GOAL_DEFINITIONS <- list(
  hypertrophy = list(
    label    = "Hypertrophy",
    one_line = "Build muscle size.",
    detail   = paste(
      "Targets MAV across all major muscle groups using moderate loads (8-15 reps)",
      "and high working volume. Aims for 2+ sessions per muscle per week where the",
      "split allows. Default rep ranges: 8-12 for compounds, 12-15 for isolation."
    )
  ),
  strength = list(
    label    = "Strength",
    one_line = "Maximize the weight on the bar.",
    detail   = paste(
      "Lower total volume, higher intensity. Big lifts (squat, bench, deadlift,",
      "overhead press) get 3-5 rep working sets. Accessories drop to maintenance",
      "to free recovery for the main lifts. RPE bias is +0.5 across the board."
    )
  ),
  pull_up = list(
    label    = "Pull-up Focus",
    one_line = "Build to consecutive strict pull-ups.",
    detail   = paste(
      "Pulling emphasis: lats, mid-back, biceps, and rear delts get 1.2-1.4x",
      "hypertrophy volume. Other muscles drop to maintenance.\n\n",
      "If onboarding pull-up baseline is < 3 reps, Block A prescribes band-assisted",
      "and eccentric-only pull-ups. The generator auto-progresses to strict",
      "pull-ups in Block B/C once you log enough quality reps."
    )
  ),
  running_support = list(
    label    = "Running Support",
    one_line = "Strength training that complements your running.",
    detail   = paste(
      "Lower body and posterior chain bias: quads, hamstrings, glutes, calves, and",
      "core all get bumped. Single-leg work is heavily featured for runners.",
      "Upper body work drops to maintenance — just enough to not regress."
    )
  ),
  functional = list(
    label    = "Functional",
    one_line = "Strength and movement quality for everyday life.",
    detail   = paste(
      "Balanced hypertrophy base, with single-leg, multi-planar, and loaded-carry",
      "work featured. Core and calf volume bumped. Less direct arm isolation than",
      "pure hypertrophy. Good fit if your goal isn't a specific lift or sport."
    )
  )
)

# ── METHODOLOGY MODAL UI ─────────────────────────────────────
# Full-screen modal showing all of the above in human-readable form.

methodology_modal_ui <- function() {

  vol_row <- function(muscle, label) {
    v <- VOLUME_TARGETS_HYPERTROPHY_INTERMEDIATE[[muscle]]
    div(style = "display:grid; grid-template-columns:1.4fr 0.5fr 0.5fr 1fr 0.5fr;
                 gap:6px; padding:6px 8px; font-size:11px;
                 border-bottom:1px solid #1a1a1a;",
        div(style = "color:#ddd;", label),
        div(style = "color:#666; text-align:center;",      v$MV),
        div(style = "color:#888; text-align:center;",      v$MEV),
        div(style = "color:#5DCAA5; text-align:center; font-weight:700;",
            paste0(v$MAV_low, "-", v$MAV_high)),
        div(style = "color:#FF9800; text-align:center;",   v$MRV)
    )
  }

  goal_block <- function(g) {
    info <- GOAL_DEFINITIONS[[g]]
    if (is.null(info)) return(NULL)
    div(style = "background:#161616; border:1px solid #222; border-radius:10px;
                 padding:12px 14px; margin-bottom:8px;",
        div(style = "font-size:13px; font-weight:700; color:#f0f0f0; margin-bottom:2px;",
            info$label),
        div(style = "font-size:11px; color:#5DCAA5; font-style:italic; margin-bottom:6px;",
            info$one_line),
        div(style = "font-size:11px; color:#888; line-height:1.5;",
            info$detail)
    )
  }

  block_row <- function(k) {
    b <- BLOCK_PROFILES[[k]]
    div(style = "padding:8px 10px; background:#161616; border-radius:8px;
                 border:1px solid #1e1e1e; margin-bottom:6px;",
        div(style = "display:flex; justify-content:space-between; align-items:center;",
            div(
              div(style = "font-size:11px; color:#5DCAA5; font-weight:700;
                           letter-spacing:0.06em; text-transform:uppercase;",
                  paste0("Block ", k, " — ", b$label)),
              div(style = "font-size:11px; color:#888; margin-top:2px;",
                  b$description)
            ),
            div(style = "text-align:right; font-size:10px; color:#666;",
                paste0("RIR ", b$rir_target), br(),
                paste0(round(100 * b$volume_mult), "% vol"))
        )
    )
  }

  div(style = "position:fixed; top:0; left:0; right:0; bottom:0;
               background:rgba(0,0,0,0.92); z-index:300;
               display:flex; align-items:flex-start; justify-content:center;
               overflow-y:auto;",
    div(style = "background:#0f0f0f; border:1px solid #1e1e1e;
                 width:100%; max-width:480px; min-height:100vh;
                 padding:20px 20px 40px;",

      # ── Header
      div(style = "display:flex; justify-content:space-between; align-items:center;
                   margin-bottom:16px;",
        div(style = "font-size:17px; font-weight:700; color:#f0f0f0;",
            "Programming Method"),
        tags$button("✕",
          style = paste0("background:#1e1e1e; border:none; border-radius:8px;",
                         "width:32px; height:32px; color:#aaa; font-size:16px;",
                         "cursor:pointer;"),
          onclick = "Shiny.setInputValue('close_methodology', Math.random(), {priority:'event'})")
      ),
      div(style = "font-size:12px; color:#555; margin-bottom:20px; line-height:1.5;",
          "How CatRack builds your programs — the science, the numbers, and the why."),

      # ── Volume terms
      div(class = "ct-section-title", "VOLUME TERMINOLOGY"),
      div(style = "background:#161616; border:1px solid #222; border-radius:10px;
                   padding:14px; margin-bottom:14px;",
        div(style = "font-size:12px; color:#aaa; line-height:1.6;",
            tags$b(style = "color:#5DCAA5;", "MV — Maintenance Volume."),
            " Minimum weekly sets to keep what you've got.", br(), br(),
            tags$b(style = "color:#5DCAA5;", "MEV — Minimum Effective Volume."),
            " Smallest dose that drives growth.", br(), br(),
            tags$b(style = "color:#5DCAA5;", "MAV — Maximum Adaptive Volume."),
            " Sweet spot. Most growth per unit of fatigue. ",
            tags$em("Programs target here."), br(), br(),
            tags$b(style = "color:#5DCAA5;", "MRV — Maximum Recoverable Volume."),
            " The ceiling. Past this you crash. Block C flirts with it briefly."
        )
      ),

      # ── Volume table
      div(class = "ct-section-title", "WEEKLY VOLUME TARGETS — HYPERTROPHY"),
      div(style = "font-size:11px; color:#555; margin-bottom:8px;",
          "Working sets per muscle per week. Intermediate baseline; beginner sits at the low end, advanced at the high end."),
      div(style = "background:#161616; border:1px solid #222; border-radius:10px;
                   padding:8px 4px 12px; margin-bottom:14px;",
        # Header row
        div(style = "display:grid; grid-template-columns:1.4fr 0.5fr 0.5fr 1fr 0.5fr;
                     gap:6px; padding:6px 8px; font-size:10px; color:#666;
                     font-weight:700; text-transform:uppercase;
                     border-bottom:1px solid #2a2a2a;",
            div("Muscle"), div(style = "text-align:center;", "MV"),
            div(style = "text-align:center;", "MEV"),
            div(style = "text-align:center;", "MAV"),
            div(style = "text-align:center;", "MRV")
        ),
        vol_row("quads",       "Quads"),
        vol_row("hamstrings",  "Hamstrings"),
        vol_row("glutes",      "Glutes"),
        vol_row("chest",       "Chest"),
        vol_row("lats",        "Lats"),
        vol_row("mid_back",    "Mid Back"),
        vol_row("front_delts", "Front Delts"),
        vol_row("mid_delts",   "Side Delts"),
        vol_row("rear_delts",  "Rear Delts"),
        vol_row("biceps",      "Biceps"),
        vol_row("triceps",     "Triceps"),
        vol_row("calves",      "Calves"),
        vol_row("core",        "Core")
      ),

      # ── 12-week structure
      div(class = "ct-section-title", "12-WEEK STRUCTURE"),
      div(style = "font-size:11px; color:#555; margin-bottom:8px;",
          "Three 4-week blocks. Each block has its own intensity character; same exercises repeat for 4 weeks for clean progressive overload tracking, then swap at the block boundary."),
      block_row("A"),
      block_row("B"),
      block_row("C"),

      # ── Week-in-block
      div(class = "ct-section-title", style = "margin-top:14px;",
          "WEEK-IN-BLOCK PROGRESSION"),
      div(style = "background:#161616; border:1px solid #222; border-radius:10px;
                   padding:12px 14px; margin-bottom:14px; font-size:11px; color:#aaa; line-height:1.6;",
          tags$b(style = "color:#5DCAA5;", "Week 1 — Base."),
          " 85% of target volume, RIR +0.5. Establish working weights.", br(), br(),
          tags$b(style = "color:#5DCAA5;", "Week 2 — Build."),
          " Full target volume, prescribed RIR.", br(), br(),
          tags$b(style = "color:#5DCAA5;", "Week 3 — Peak."),
          " 105% volume, RIR -0.5. The hardest week.", br(), br(),
          tags$b(style = "color:#5DCAA5;", "Week 4 — Deload."),
          " 60% volume, RIR +2. Reduce weight ~40%, same exercises, same reps. Adaptation happens here."
      ),

      # ── Goals
      div(class = "ct-section-title", "GOALS"),
      div(style = "font-size:11px; color:#555; margin-bottom:8px;",
          "Each goal scales the volume table differently. Same framework, different priorities."),
      lapply(names(GOAL_DEFINITIONS), goal_block),

      # ── Experience modifier
      div(class = "ct-section-title", style = "margin-top:6px;",
          "EXPERIENCE LEVEL"),
      div(style = "background:#161616; border:1px solid #222; border-radius:10px;
                   padding:12px 14px; margin-bottom:14px; font-size:11px; color:#aaa; line-height:1.6;",
          tags$b(style = "color:#5DCAA5;", "Beginner."),
          " Sits at the low end of MAV. Less recovery debt while patterns groove.",
          br(), br(),
          tags$b(style = "color:#5DCAA5;", "Intermediate."),
          " Mid-MAV. The default.",
          br(), br(),
          tags$b(style = "color:#5DCAA5;", "Advanced."),
          " Upper MAV in Blocks A/B, pushes toward MRV in Block C."
      ),

      # ── Weight philosophy
      div(class = "ct-section-title", "WEIGHT SUGGESTIONS"),
      div(style = "background:#161616; border:1px solid #222; border-radius:10px;
                   padding:12px 14px; margin-bottom:14px; font-size:11px; color:#aaa; line-height:1.6;",
          "The app ",
          tags$b(style = "color:#5DCAA5;", "suggests"),
          " a starting weight based on your last logged performance — it doesn't prescribe one. ",
          "Suggestions come from your estimated 1RM (Epley/Brzycki) scaled to the block's RIR target. ",
          "You always pick the weight. If a suggestion feels too easy or too hard, override it; the next session's suggestion will adapt."
      ),

      # ── Sources
      div(class = "ct-section-title", "SOURCES"),
      div(style = "background:#161616; border:1px solid #222; border-radius:10px;
                   padding:12px 14px; margin-bottom:20px; font-size:11px; color:#888; line-height:1.6;",
          "• Jeff Nippard — Scientific Principles of Hypertrophy / Strength Training", br(),
          "• Mike Israetel / Renaissance Periodization — MV/MEV/MAV/MRV framework", br(),
          "• Brad Schoenfeld — meta-analyses on frequency, volume, and rep ranges", br(),
          "• Greg Nuckols / Stronger By Science — load prescription, RPE/RIR research"
      ),

      tags$button("Close",
        style = paste0("width:100%; background:#1D9E75; color:#fff; border:none;",
                       "border-radius:12px; padding:14px; font-size:14px;",
                       "font-weight:700; cursor:pointer;"),
        onclick = "Shiny.setInputValue('close_methodology', Math.random(), {priority:'event'})")
    )
  )
}

# ── METHODOLOGY SERVER LOGIC ─────────────────────────────────
setup_methodology_server <- function(input, output, session, rv) {

  observeEvent(input$open_methodology, {
    rv$show_methodology <- TRUE
  })

  observeEvent(input$close_methodology, {
    rv$show_methodology <- FALSE
  })
}

# ── INFO BUTTON HELPER ───────────────────────────────────────
# Small circular (i) button that opens the methodology modal.
methodology_info_btn <- function(size = "sm") {
  s <- if (size == "lg") "32px" else "22px"
  fs <- if (size == "lg") "14px" else "11px"
  tags$button(
    "i",
    style = paste0(
      "background:#1e1e1e; border:1px solid #2a2a2a; border-radius:50%;",
      "width:", s, "; height:", s, "; color:#5DCAA5;",
      "font-style:italic; font-family:Georgia,serif; font-weight:700;",
      "font-size:", fs, "; cursor:pointer; padding:0; line-height:1;",
      "display:inline-flex; align-items:center; justify-content:center;"),
    onclick = "Shiny.setInputValue('open_methodology', Math.random(), {priority:'event'})",
    title = "Programming method"
  )
}

# Text link variant for places where a button looks out of place
methodology_info_link <- function(label = "How this is built →") {
  tags$a(label, href = "#",
    style = "color:#5DCAA5; font-size:12px; text-decoration:none;",
    onclick = "Shiny.setInputValue('open_methodology', Math.random(), {priority:'event'}); return false;")
}
