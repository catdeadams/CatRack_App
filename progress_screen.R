# ============================================================
# progress_screen.R — CaTrack  (Session 9)
# ============================================================

epley_1rm <- function(weight, reps) {
  if (is.na(weight) || is.na(reps) || reps <= 0 || weight <= 0) return(NA_real_)
  if (reps == 1) return(as.numeric(weight))
  round(as.numeric(weight) * (1 + as.numeric(reps) / 30), 1)
}

brzycki_1rm <- function(weight, reps) {
  if (is.na(weight) || is.na(reps) || reps <= 0 || weight <= 0) return(NA_real_)
  if (reps == 1) return(as.numeric(weight))
  denom <- 1.0278 - (0.0278 * as.numeric(reps))
  if (denom <= 0) return(NA_real_)
  round(as.numeric(weight) / denom, 1)
}

estimate_1rm <- function(weight, reps) {
  if (is.na(weight) || is.na(reps) || reps <= 0) return(NA_real_)
  e <- epley_1rm(weight, reps)
  b <- brzycki_1rm(weight, reps)
  if (is.na(e) && is.na(b)) return(NA_real_)
  if (is.na(e)) return(b)
  if (is.na(b)) return(e)
  if (as.numeric(reps) <= 10) round(0.6*b + 0.4*e, 1) else round((e+b)/2, 1)
}

fetch_all_logs <- function(user_id, token) {
  # Fetch newest-first so the row cap drops the OLDEST history, not the most
  # recent (an asc order + cap silently hid new sessions once the cap was
  # hit). Downstream code assumes ascending order (last row = most recent),
  # so re-sort ascending before returning.
  logs <- sb_select("workout_set_logs",
            sprintf(paste0("?user_id=eq.%s&is_warmup=eq.false",
                           "&select=*,workout_exercises(exercise_id,exercises(name,category,primary_muscles))",
                           "&order=logged_at.desc&limit=5000"), user_id),
            token = token)
  if (is.null(logs) || nrow(logs) == 0) return(logs)
  logs[order(as.POSIXct(logs$logged_at, tz = "UTC")), , drop = FALSE]
}

fetch_prs <- function(user_id, token) {
  sb_select("personal_records",
            sprintf("?user_id=eq.%s&order=last_logged_at.desc", user_id),
            token = token)
}

# Extract the exercise name for log row i from the embedded join.
# PostgREST's to-one embed comes back (via jsonlite simplifyDataFrame) as a
# nested DATA.FRAME: logs$workout_exercises has columns (exercise_id, exercises),
# and logs$workout_exercises$exercises$name is a plain vector indexed by row.
# The old code used [[i]] which indexes the nested COLUMN, not the row, so it
# returned NA for everything. Handle both the nested-df and list-column shapes.
get_ex_name <- function(logs, i) {
  tryCatch({
    we <- logs$workout_exercises
    if (is.null(we)) return(NA_character_)

    # Shape A — fully simplified nested data.frames (the normal case)
    if (is.data.frame(we)) {
      ex <- we$exercises
      if (is.data.frame(ex) && "name" %in% names(ex)) return(as.character(ex$name[i]))
      if (is.list(ex)) {
        nm <- tryCatch(ex[[i]]$name, error = \(e) NULL)
        if (!is.null(nm)) return(as.character(nm)[1])
      }
      return(NA_character_)
    }

    # Shape B — list column, one entry per row (defensive fallback)
    if (is.list(we)) {
      wi <- we[[i]]
      if (is.null(wi)) return(NA_character_)
      ex <- tryCatch(wi$exercises, error = \(e) NULL)
      if (is.data.frame(ex) && "name" %in% names(ex)) return(as.character(ex$name[1]))
      nm <- tryCatch(ex$name, error = \(e) NULL)
      if (!is.null(nm)) return(as.character(nm)[1])
    }
    NA_character_
  }, error = \(e) NA_character_)
}


# Collapse a single exercise's set-logs into one row per session (date).
# Captures the best set (by est-1RM), heaviest weight, max reps, RPE,
# session volume, and any notes — everything the progress views need.
compute_exercise_sessions <- function(logs_df, program = NULL) {
  if (is.null(logs_df) || nrow(logs_df) == 0) return(NULL)
  d <- logs_df
  d$date     <- as.Date(as.POSIXct(d$logged_at))
  d$weight_n <- suppressWarnings(as.numeric(d$weight_lbs))
  d$reps_n   <- suppressWarnings(as.integer(d$reps_completed))
  d$rpe_n    <- suppressWarnings(as.numeric(d$rpe_actual))
  d$e1rm     <- mapply(estimate_1rm, d$weight_n, d$reps_n)
  notes_raw  <- if ("notes" %in% names(d)) d$notes else rep(NA, nrow(d))
  d$note_txt <- ifelse(is.na(notes_raw), "", as.character(notes_raw))
  d <- d[!is.na(d$date), ]
  if (nrow(d) == 0) return(NULL)

  has_weight <- any(!is.na(d$weight_n) & d$weight_n > 0)
  start      <- tryCatch(as.Date(program$start_date), error = \(e) NA)

  rows <- lapply(split(d, d$date), function(g) {
    # Rank sets to find the "best" one: prefer est-1RM, then weight, then reps
    score <- g$e1rm
    if (all(is.na(score))) score <- g$weight_n
    if (all(is.na(score))) score <- as.numeric(g$reps_n)
    bi <- which.max(replace(score, is.na(score), -Inf))
    wi <- if (all(is.na(g$weight_n))) bi
          else which.max(replace(g$weight_n, is.na(g$weight_n), -Inf))
    notes_all <- unique(g$note_txt[nchar(g$note_txt) > 0])
    data.frame(
      date        = g$date[1],
      best_e1rm   = if (all(is.na(g$e1rm)))     NA_real_    else max(g$e1rm, na.rm = TRUE),
      best_weight = g$weight_n[bi],
      best_reps   = g$reps_n[bi],
      best_rpe    = g$rpe_n[bi],
      top_weight  = if (all(is.na(g$weight_n))) NA_real_    else max(g$weight_n, na.rm = TRUE),
      max_reps    = if (all(is.na(g$reps_n)))   NA_integer_ else max(g$reps_n, na.rm = TRUE),
      n_sets      = nrow(g),
      volume      = sum(g$weight_n * g$reps_n, na.rm = TRUE),
      notes       = paste(notes_all, collapse = " · "),
      stringsAsFactors = FALSE
    )
  })
  sessions <- do.call(rbind, rows)
  sessions <- sessions[order(sessions$date), ]
  rownames(sessions) <- NULL

  sessions$week_lbl <- if (!is.na(start))
    paste0("Week ", pmax(1L, as.integer(floor(as.numeric(sessions$date - start) / 7)) + 1L))
  else format(sessions$date, "%b %d")
  sessions$e1rm_delta <- c(NA_real_, diff(sessions$best_e1rm))
  sessions$has_weight <- has_weight
  sessions
}

# One clean trend line. metric: "e1rm" | "weight" (bodyweight forces "reps").
plot_exercise_progress <- function(sessions, exercise_name, metric = "e1rm") {
  if (is.null(sessions) || nrow(sessions) == 0) return(NULL)
  has_weight <- isTRUE(sessions$has_weight[1])
  eff <- if (!has_weight) "reps" else metric

  yvals <- switch(eff,
                  e1rm   = sessions$best_e1rm,
                  weight = sessions$top_weight,
                  reps   = as.numeric(sessions$max_reps))
  yttl  <- switch(eff, e1rm = "Est. 1RM (lbs)", weight = "Weight (lbs)", reps = "Reps")
  lname <- switch(eff, e1rm = "Est. 1RM", weight = "Heaviest weight", reps = "Max reps")

  keep <- !is.na(yvals)
  if (!any(keep)) return(NULL)
  s  <- sessions[keep, ]
  yv <- yvals[keep]

  hover <- paste0(
    s$week_lbl, " · ", format(s$date, "%b %d"), "<br>",
    ifelse(!is.na(s$best_weight),
           paste0("Top set: ", s$best_weight, " lbs × ", s$best_reps,
                  ifelse(!is.na(s$best_rpe), paste0(" @ RPE ", s$best_rpe), "")),
           paste0(s$max_reps, " reps")),
    ifelse(!is.na(s$best_e1rm), paste0("<br>Est. 1RM: ", round(s$best_e1rm), " lbs"), ""),
    ifelse(nchar(s$notes) > 0, paste0("<br>📝 ", s$notes), "")
  )

  plotly::plot_ly(x = s$date, y = yv, type = "scatter", mode = "lines+markers",
                  name = lname,
                  line   = list(color = "#1D9E75", width = 2.5),
                  marker = list(color = "#1D9E75", size = 8,
                                line = list(color = "#0f0f0f", width = 1)),
                  text = hover, hoverinfo = "text") |>
    plotly::layout(
      paper_bgcolor = "#161616", plot_bgcolor = "#161616",
      font  = list(color = "#cfcfcf", size = 11, family = "system-ui,sans-serif"),
      xaxis = list(gridcolor = "#242424", zeroline = FALSE, tickformat = "%b %d",
                   title = "", color = "#aaa"),
      yaxis = list(gridcolor = "#242424", zeroline = FALSE, title = yttl,
                   titlefont = list(color = "#aaa", size = 11), color = "#aaa"),
      showlegend = FALSE,
      hoverlabel = list(bgcolor = "#1e1e1e", bordercolor = "#0F6E56", align = "left",
                        font = list(color = "#f0f0f0", size = 12)),
      margin = list(t = 14, b = 40, l = 48, r = 16)
    ) |> plotly::config(displayModeBar = FALSE)
}

# Headline stat: first → latest value for the active metric + delta/percent.
exercise_summary_ui <- function(sessions, metric = "e1rm") {
  if (is.null(sessions) || nrow(sessions) == 0) return(NULL)
  has_weight <- isTRUE(sessions$has_weight[1])
  eff <- if (!has_weight) "reps" else metric
  vals <- switch(eff, e1rm = sessions$best_e1rm, weight = sessions$top_weight,
                 reps = as.numeric(sessions$max_reps))
  ok <- which(!is.na(vals))
  if (length(ok) == 0) return(NULL)
  fv <- vals[ok[1]]; lv <- vals[ok[length(ok)]]
  delta <- lv - fv
  pct   <- if (fv > 0) round(100 * delta / fv) else 0
  unit  <- if (eff == "reps") "reps" else "lbs"
  label <- switch(eff, e1rm = "Est. 1RM", weight = "Heaviest weight", reps = "Max reps")
  arrow <- if (delta > 0.01) "▲" else if (delta < -0.01) "▼" else "—"
  color <- if (delta > 0.01) "#1D9E75" else if (delta < -0.01) "#f87171" else "#888"
  n_sess <- nrow(sessions)

  div(style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:10px;",
      div(
        div(style = "font-size:10px;color:#555;text-transform:uppercase;letter-spacing:0.06em;", label),
        div(style = "font-size:20px;font-weight:700;color:#f0f0f0;",
            paste0(round(lv), " ", unit))
      ),
      div(style = "text-align:right;",
          div(style = paste0("font-size:14px;font-weight:700;color:", color, ";"),
              paste0(arrow, " ", ifelse(delta >= 0, "+", ""), round(delta), " ", unit,
                     if (fv > 0) paste0(" (", ifelse(pct >= 0, "+", ""), pct, "%)") else "")),
          div(style = "font-size:10px;color:#555;",
              paste0("over ", n_sess, " session", ifelse(n_sess == 1, "", "s")))
      )
  )
}

# Flatten a single exercise's logs to one row per set (date, set#, wt, reps,
# rpe, note) so the detail list can show every set, not just the top one.
build_exercise_set_rows <- function(logs_df) {
  if (is.null(logs_df) || nrow(logs_df) == 0) return(NULL)
  d <- logs_df
  d$date <- as.Date(as.POSIXct(d$logged_at))
  d$wt   <- suppressWarnings(as.numeric(d$weight_lbs))
  d$reps <- suppressWarnings(as.integer(d$reps_completed))
  d$rpe  <- suppressWarnings(as.numeric(d$rpe_actual))
  d$sn   <- suppressWarnings(as.integer(d$set_number))
  raw_notes <- if ("notes" %in% names(d)) d$notes else rep(NA, nrow(d))
  d$note <- vapply(seq_len(nrow(d)), function(k) {
    n <- trimws(as.character(raw_notes[k] %||% ""))
    if (n %in% c("", "NA", "NULL", "{}", "[]", "null")) "" else n
  }, character(1))
  d <- d[!is.na(d$date), c("date", "sn", "wt", "reps", "rpe", "note")]
  if (nrow(d) == 0) return(NULL)
  d[order(d$date, replace(d$sn, is.na(d$sn), .Machine$integer.max)), ]
}

# Scrollable session-by-session breakdown. When set_rows is supplied, each
# session expands to show every set's weight × reps × RPE next to its note.
exercise_detail_list_ui <- function(sessions, set_rows = NULL) {
  if (is.null(sessions) || nrow(sessions) == 0) return(NULL)
  s <- sessions[order(sessions$date, decreasing = TRUE), ]
  div(style = "margin-top:12px;border-top:1px solid #222;padding-top:10px;
               max-height:340px;overflow-y:auto;",
    lapply(seq_len(nrow(s)), function(i) {
      r    <- s[i, ]
      d_e1 <- r$e1rm_delta
      delta_badge <- if (!is.na(d_e1) && abs(d_e1) >= 0.5) {
        up <- d_e1 > 0
        span(style = paste0("font-size:10px;font-weight:700;margin-left:6px;color:",
                            if (up) "#1D9E75" else "#f87171", ";"),
             paste0(if (up) "▲+" else "▼", round(abs(d_e1)), " 1RM"))
      } else NULL
      topset <- if (!is.na(r$best_weight))
        paste0(r$best_weight, " lbs × ", r$best_reps,
               if (!is.na(r$best_rpe)) paste0(" @ RPE ", r$best_rpe) else "")
      else paste0(r$max_reps, " reps (BW)")
      day_sets <- if (!is.null(set_rows)) set_rows[set_rows$date == r$date, ] else NULL
      div(style = "padding:8px 0;border-bottom:1px solid #1a1a1a;",
          div(style = "display:flex;justify-content:space-between;align-items:baseline;gap:8px;",
              div(style = "font-size:12px;color:#ddd;font-weight:600;white-space:nowrap;",
                  paste0(r$week_lbl, " · ", format(r$date, "%b %d"))),
              div(style = "font-size:12px;color:#f0f0f0;text-align:right;", topset, delta_badge)),
          if (!is.na(r$best_e1rm))
            div(style = "font-size:10px;color:#555;margin-top:1px;",
                paste0("Est. 1RM ", round(r$best_e1rm), " lbs · ", r$n_sets,
                       " set", ifelse(r$n_sets == 1, "", "s"),
                       " · vol ", format(round(r$volume), big.mark = ","), " lbs")),
          # Per-set breakdown — each set's numbers next to its own note
          if (!is.null(day_sets) && nrow(day_sets) > 0)
            div(style = "margin-top:4px;display:flex;flex-direction:column;gap:1px;",
                lapply(seq_len(nrow(day_sets)), function(k) {
                  st <- day_sets[k, ]
                  div(
                    div(style = "display:flex;justify-content:space-between;gap:8px;font-size:11px;",
                        span(style = "color:#666;",
                             if (!is.na(st$sn)) paste0("Set ", st$sn) else "Set"),
                        span(style = "color:#aaa;",
                             paste0(if (!is.na(st$wt)) paste0(st$wt, " lbs") else "BW",
                                    " × ", if (!is.na(st$reps)) st$reps else "—", " reps",
                                    if (!is.na(st$rpe)) paste0("  ·  RPE ", st$rpe) else ""))),
                    if (nchar(st$note) > 0)
                      div(style = "font-size:10px;color:#5DCAA5;font-style:italic;margin:0 0 2px 0;",
                          paste0("\U0001F4DD ", st$note))
                  )
                }))
          else if (nchar(r$notes) > 0)
            div(style = "font-size:11px;color:#5DCAA5;margin-top:3px;font-style:italic;",
                paste0("\U0001F4DD ", r$notes))
      )
    })
  )
}

plot_weekly_volume <- function(logs_df) {
  if (is.null(logs_df) || nrow(logs_df) == 0) return(NULL)
  logs_df$logged_at <- as.POSIXct(logs_df$logged_at)
  logs_df$week      <- as.Date(cut(as.Date(logs_df$logged_at), "week"))
  logs_df$volume    <- as.numeric(logs_df$weight_lbs) * as.numeric(logs_df$reps_completed)
  weekly <- logs_df |> group_by(week) |>
    summarise(total_volume = sum(volume, na.rm=TRUE), .groups="drop") |> tail(12)
  
  plotly::plot_ly(weekly, x=~week, y=~total_volume, type="bar",
                  text=~paste0(format(week,"%b %d"),"<br>",format(round(total_volume),big.mark=",")," lbs"),
                  hoverinfo="text",
                  marker=list(color="rgba(29,158,117,0.8)", line=list(color="#5DCAA5",width=1))) |>
    plotly::layout(
      paper_bgcolor="#161616", plot_bgcolor="#161616",
      font=list(color="#cfcfcf",size=11,family="system-ui,sans-serif"),
      xaxis=list(gridcolor="#242424",zeroline=FALSE,tickformat="%b %d",title="",
                 color="#aaa"),
      yaxis=list(gridcolor="#242424",zeroline=FALSE,title="Volume (lbs)",
                 titlefont=list(color="#aaa",size=11), color="#aaa"),
      hoverlabel=list(bgcolor="#1e1e1e",bordercolor="#0F6E56",font=list(color="#f0f0f0",size=12)),
      margin=list(t=10,b=40,l=55,r=15)
    ) |> plotly::config(displayModeBar=FALSE)
}

deload_banner_ui <- function(program, workouts) {
  if (is.null(program)) return(NULL)
  start <- tryCatch(as.Date(program$start_date), error=\(e) NULL)
  if (is.null(start)) return(NULL)
  current_week  <- as.integer(floor(as.numeric(Sys.Date()-start)/7))+1L
  week_in_block <- ((current_week-1L) %% 4L)+1L
  if (week_in_block != 4L) return(NULL)
  div(style="background:#1a120a;border:1px solid #854F0B;border-radius:10px;
             padding:12px 16px;margin-bottom:14px;display:flex;align-items:flex-start;gap:12px;",
      div(style="font-size:20px;flex-shrink:0;", "🔄"),
      div(
        div(style="font-size:13px;font-weight:700;color:#fbbf24;margin-bottom:3px;",
            paste0("Deload Week — Week ",current_week," of ",program$total_weeks)),
        div(style="font-size:12px;color:#888;line-height:1.5;",
            "Reduce weight ~40%, keep reps the same. Deloads are when adaptation happens — your next block will be stronger for it.")
      )
  )
}

overload_reminder_ui <- function(logs) {
  if (is.null(logs) || nrow(logs) < 6) return(NULL)
  tryCatch({
    logs$weight_n <- as.numeric(logs$weight_lbs)
    logs$date     <- as.Date(as.POSIXct(logs$logged_at))
    logs$ex_name  <- sapply(seq_len(nrow(logs)), \(i) get_ex_name(logs, i))
    stale <- logs |>
      filter(!is.na(ex_name), !is.na(weight_n)) |>
      group_by(ex_name) |> arrange(date) |>
      summarise(n_sess=n_distinct(date), wts=list(weight_n[order(date)]), .groups="drop") |>
      filter(n_sess >= 3) |>
      rowwise() |>
      mutate(no_prog = (max(tail(unlist(wts),3)) - min(tail(unlist(wts),3))) < 2.5) |>
      filter(no_prog) |> head(2)
    if (nrow(stale) == 0) return(NULL)
    div(style="background:#0a0f1a;border:1px solid #185FA5;border-radius:10px;
               padding:12px 16px;margin-bottom:14px;display:flex;align-items:flex-start;gap:12px;",
        div(style="font-size:20px;flex-shrink:0;","📈"),
        div(
          div(style="font-size:13px;font-weight:700;color:#60a5fa;margin-bottom:3px;",
              "Progressive Overload Reminder"),
          div(style="font-size:12px;color:#888;line-height:1.5;",
              paste0(paste(stale$ex_name,collapse=" and "),
                     " weight hasn't changed in 3 sessions. Try adding 2.5–5 lbs or one more rep."))
        )
    )
  }, error=\(e) NULL)
}

progress_screen_ui <- function(logs, prs, program=NULL, workouts=NULL,
                               selected_exercise=NULL, metric="e1rm") {
  ex_names <- character(0)
  if (!is.null(logs) && nrow(logs) > 0) {
    ex_names <- tryCatch(
      sort(unique(na.omit(sapply(seq_len(nrow(logs)), \(i) get_ex_name(logs, i))))),
      error=\(e) character(0))
  }
  # Resolve the effective selection using the SAME fallback the server-side
  # exercise_sessions() reactive uses (most-recently-logged exercise), so the
  # dropdown and the chart always agree — even on first open when
  # selected_exercise is still NULL.
  sel <- selected_exercise %||%
    tryCatch(get_ex_name(logs, nrow(logs)), error = \(e) NULL)
  if (!is.null(sel) && !(sel %in% ex_names)) sel <- NULL
  tagList(
    div(style="margin-bottom:16px;",
        div(style="font-size:18px;font-weight:700;color:#f0f0f0;","Progress"),
        div(style="font-size:12px;color:#555;","History, records, and estimated 1RM")),
    deload_banner_ui(program, workouts),
    overload_reminder_ui(logs),
    div(style="background:#161616;border-radius:12px;padding:16px;margin-bottom:12px;border:1px solid #222;",
        div(class="ct-section-title","WEEKLY VOLUME — LAST 12 WEEKS"),
        if (!is.null(logs) && nrow(logs) > 0)
          plotly::plotlyOutput("plot_weekly_volume", height="170px")
        else div(style="text-align:center;color:#444;padding:30px;font-size:13px;",
                 "Complete a workout to see your volume trend.")),
    div(style="background:#161616;border-radius:12px;padding:16px;margin-bottom:12px;border:1px solid #222;",
        div(class="ct-section-title","EXERCISE PROGRESS"),
        if (length(ex_names) > 0) {
          tagList(
            div(style="margin-bottom:10px;",
                tags$select(id="selected_exercise",
                            style="background:#1e1e1e;border:1.5px solid #262626;color:#f0f0f0;border-radius:8px;padding:8px 10px;font-size:13px;width:100%;",
                            onchange="Shiny.setInputValue('select_progress_exercise',this.value,{priority:'event'})",
                            lapply(ex_names, \(nm) tags$option(value=nm, selected=identical(nm, sel), nm)))),
            # Metric toggle rendered as its own output so clicking it updates
            # the highlight without re-rendering (and resetting) the dropdown.
            uiOutput("progress_metric_toggle"),
            uiOutput("exercise_progress_summary"),
            plotly::plotlyOutput("plot_exercise_progress", height="240px"),
            div(style="font-size:11px;color:#444;margin-top:8px;",
                "Hover a point for your top set, RPE, and notes."),
            uiOutput("exercise_detail_list"))
        } else div(style="text-align:center;color:#444;padding:20px;font-size:13px;",
                   "Log workouts to track progress.")),
    div(style="background:#161616;border-radius:12px;padding:16px;margin-bottom:12px;border:1px solid #222;",
        div(class="ct-section-title","PERSONAL RECORDS"),
        if (!is.null(prs) && nrow(prs) > 0) {
          div(style="display:flex;flex-direction:column;gap:5px;",
              lapply(seq_len(min(15, nrow(prs))), \(i) {
                pr  <- prs[i,]
                wt  <- tryCatch(as.numeric(pr$max_weight_lbs), error=\(e) NA)
                rps <- tryCatch(as.integer(pr$reps_at_max_weight), error=\(e) NA)
                e1  <- estimate_1rm(wt, rps)
                div(style="display:flex;justify-content:space-between;align-items:center;padding:10px 12px;background:#1e1e1e;border-radius:8px;border:1px solid #222;",
                    div(style="flex:1;min-width:0;",
                        div(style="font-size:13px;font-weight:600;color:#f0f0f0;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",pr$exercise_name),
                        div(style="font-size:11px;color:#555;margin-top:1px;",
                            tryCatch(format(as.Date(pr$last_logged_at),"%b %d, %Y"),error=\(e) ""))),
                    div(style="text-align:right;flex-shrink:0;margin-left:12px;",
                        div(style="font-size:15px;font-weight:700;color:#1D9E75;",
                            if(!is.na(wt)) paste0(wt," lbs") else "BW"),
                        div(style="font-size:11px;color:#555;",
                            if(!is.na(rps)) paste0("x ",rps," reps") else ""),
                        if(!is.na(e1)) div(style="font-size:10px;color:#5DCAA5;margin-top:1px;",
                                           paste0("~",round(e1)," lbs 1RM"))))
              }))
        } else div(style="text-align:center;color:#444;padding:20px;font-size:13px;",
                   "Complete workouts to set records."))
  )
}


setup_progress_server <- function(input, output, session, rv) {
  observe({
    req(rv$token, rv$user_id)
    if (rv$page != "progress") return()
    if (is.null(rv$all_logs))
      rv$all_logs <- tryCatch(fetch_all_logs(rv$user_id, rv$token), error=\(e) NULL)
    if (is.null(rv$prs))
      rv$prs <- tryCatch(fetch_prs(rv$user_id, rv$token), error=\(e) NULL)
    # Default the exercise selector to the most-recently-logged exercise so
    # the dropdown and the chart agree on first open (they used to disagree).
    if (is.null(rv$selected_exercise) && !is.null(rv$all_logs) && nrow(rv$all_logs) > 0)
      rv$selected_exercise <- tryCatch(
        get_ex_name(rv$all_logs, nrow(rv$all_logs)), error = \(e) NULL)
  })
  
  observeEvent(input$select_progress_exercise, { rv$selected_exercise <- input$select_progress_exercise })
  observeEvent(input$progress_metric, {
    if (input$progress_metric %in% c("e1rm", "weight")) rv$progress_metric <- input$progress_metric
  })

  # Metric toggle (Est. 1RM / Heaviest). Its own output so a click updates
  # only this control + the chart — not the whole page — which is what used
  # to rebuild the exercise dropdown and lose the user's selection.
  output$progress_metric_toggle <- renderUI({
    metric <- rv$progress_metric %||% "e1rm"
    metric_btn <- function(m, lbl) tags$button(
      lbl,
      style = paste0(
        "flex:1;padding:7px;border-radius:7px;font-size:12px;cursor:pointer;",
        if (identical(metric, m))
          "background:#0a1f16;border:1px solid #1D9E75;color:#5DCAA5;font-weight:700;"
        else
          "background:#1e1e1e;border:1px solid #262626;color:#888;font-weight:400;"),
      onclick = sprintf("Shiny.setInputValue('progress_metric','%s',{priority:'event'})", m))
    div(style = "display:flex;gap:6px;margin-bottom:12px;",
        metric_btn("e1rm",   "Est. 1RM"),
        metric_btn("weight", "Heaviest"))
  })

  output$plot_weekly_volume <- plotly::renderPlotly({
    req(rv$all_logs); plot_weekly_volume(rv$all_logs)
  })

  # Cached per-session summary for the selected exercise. Recomputes only
  # when the logs or the selected exercise change — not on every reactive tick.
  # Returns both the per-session summary (for the chart/summary) and the raw
  # per-set logs (for the per-set detail list), from a single filter pass.
  exercise_sessions <- reactive({
    req(rv$all_logs)
    ex_name <- rv$selected_exercise %||%
      tryCatch(get_ex_name(rv$all_logs, nrow(rv$all_logs)), error = \(e) NULL)
    if (is.null(ex_name) || is.na(ex_name)) return(NULL)
    keep <- vapply(seq_len(nrow(rv$all_logs)),
                   \(i) isTRUE(identical(get_ex_name(rv$all_logs, i), ex_name)),
                   logical(1))
    ex_logs <- rv$all_logs[keep, ]
    if (nrow(ex_logs) == 0) return(NULL)
    list(summary = compute_exercise_sessions(ex_logs, rv$program),
         raw     = ex_logs)
  })

  output$plot_exercise_progress <- plotly::renderPlotly({
    s <- exercise_sessions(); req(!is.null(s), !is.null(s$summary))
    plot_exercise_progress(s$summary, rv$selected_exercise, rv$progress_metric %||% "e1rm")
  })
  output$exercise_progress_summary <- renderUI({
    s <- exercise_sessions(); if (is.null(s) || is.null(s$summary)) return(NULL)
    exercise_summary_ui(s$summary, rv$progress_metric %||% "e1rm")
  })
  output$exercise_detail_list <- renderUI({
    s <- exercise_sessions(); if (is.null(s) || is.null(s$summary)) return(NULL)
    exercise_detail_list_ui(s$summary, build_exercise_set_rows(s$raw))
  })
}