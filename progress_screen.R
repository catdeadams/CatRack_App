# ============================================================
# progress_screen.R — CaTrack
# Progress charts, personal records, and friends leaderboard.
# Source this from global.R
# ============================================================

# ── HELPERS ──────────────────────────────────────────────────

# Fetch all set logs for a user with exercise info
fetch_all_logs <- function(user_id, token) {
  sb_select("workout_set_logs",
    sprintf(paste0(
      "?user_id=eq.%s",
      "&is_warmup=eq.false",
      "&select=*,workout_exercises(exercise_id,exercises(name,category,primary_muscles))",
      "&order=logged_at.desc",
      "&limit=2000"
    ), user_id),
    token = token)
}

# Fetch personal records from the view
fetch_prs <- function(user_id, token) {
  sb_select("personal_records",
    sprintf("?user_id=eq.%s&order=last_logged_at.desc", user_id),
    token = token)
}

# Fetch leaderboard for a friend group
fetch_leaderboard <- function(group_id, token) {
  sb_select("leaderboard_weekly_volume",
    sprintf("?friend_group_id=eq.%s&order=week_start.desc,total_volume_lbs.desc",
            group_id),
    token = token)
}

# Fetch friend group members with display names
fetch_group_members <- function(group_id, token) {
  sb_select("friend_group_members",
    sprintf(paste0("?group_id=eq.%s",
                   "&select=user_id,joined_at,user_profiles(display_name)"),
            group_id),
    token = token)
}

# ── PLOT HELPERS ─────────────────────────────────────────────

# Build a Plotly volume-over-time chart for one exercise
plot_exercise_volume <- function(logs_df, exercise_name) {
  if (is.null(logs_df) || nrow(logs_df) == 0) return(NULL)

  logs_df$logged_at <- as.POSIXct(logs_df$logged_at)
  logs_df$date      <- as.Date(logs_df$logged_at)
  logs_df$volume    <- as.numeric(logs_df$weight_lbs) *
                       as.numeric(logs_df$reps_completed)

  daily <- logs_df |>
    group_by(date) |>
    summarise(
      total_volume = sum(volume, na.rm = TRUE),
      max_weight   = max(as.numeric(weight_lbs), na.rm = TRUE),
      total_reps   = sum(as.numeric(reps_completed), na.rm = TRUE),
      .groups = "drop"
    )

  plotly::plot_ly(daily, x = ~date) |>
    plotly::add_lines(y = ~max_weight, name = "Max Weight (lbs)",
                      line = list(color = "#e8ff47", width = 2)) |>
    plotly::add_bars(y = ~total_volume, name = "Total Volume",
                     marker = list(color = "rgba(96,165,250,0.3)"),
                     yaxis = "y2") |>
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font          = list(color = "#888", size = 11),
      title         = list(text = exercise_name,
                           font = list(color = "#f0f0f0", size = 14)),
      xaxis = list(gridcolor = "#2a2a2a", zeroline = FALSE,
                   tickformat = "%b %d"),
      yaxis  = list(gridcolor = "#2a2a2a", zeroline = FALSE,
                    title = "Weight (lbs)", titlefont = list(color="#e8ff47")),
      yaxis2 = list(overlaying = "y", side = "right", showgrid = FALSE,
                    title = "Volume", titlefont = list(color="#60a5fa")),
      legend = list(orientation = "h", y = -0.2,
                    font = list(color = "#888")),
      margin = list(t = 40, b = 60, l = 50, r = 50)
    ) |>
    plotly::config(displayModeBar = FALSE)
}

# Weekly volume bar chart across all exercises
plot_weekly_volume <- function(logs_df) {
  if (is.null(logs_df) || nrow(logs_df) == 0) return(NULL)

  logs_df$logged_at <- as.POSIXct(logs_df$logged_at)
  logs_df$week      <- as.Date(cut(as.Date(logs_df$logged_at), "week"))
  logs_df$volume    <- as.numeric(logs_df$weight_lbs) *
                       as.numeric(logs_df$reps_completed)

  weekly <- logs_df |>
    group_by(week) |>
    summarise(total_volume = sum(volume, na.rm = TRUE), .groups = "drop") |>
    tail(12)

  plotly::plot_ly(weekly, x = ~week, y = ~total_volume,
                  type = "bar",
                  marker = list(
                    color = "rgba(232,255,71,0.7)",
                    line  = list(color = "#e8ff47", width = 1)
                  )) |>
    plotly::layout(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      font          = list(color = "#888", size = 11),
      xaxis = list(gridcolor = "#2a2a2a", zeroline = FALSE,
                   tickformat = "%b %d", title = ""),
      yaxis = list(gridcolor = "#2a2a2a", zeroline = FALSE,
                   title = "Total Volume (lbs × reps)"),
      margin = list(t = 10, b = 50, l = 60, r = 20)
    ) |>
    plotly::config(displayModeBar = FALSE)
}

# ── PROGRESS SCREEN UI ───────────────────────────────────────
progress_screen_ui <- function(logs, prs, selected_exercise = NULL) {

  # Get unique exercise names from logs
  ex_names <- character(0)
  if (!is.null(logs) && nrow(logs) > 0) {
    ex_names <- tryCatch({
      names_raw <- sapply(seq_len(nrow(logs)), function(i) {
        tryCatch(logs$workout_exercises[[i]]$exercises$name, error = \(e) NA)
      })
      sort(unique(na.omit(names_raw)))
    }, error = \(e) character(0))
  }

  tagList(
    div(style = "margin-bottom:16px;",
      div(style="font-size:18px; font-weight:700; margin-bottom:4px;", "Progress"),
      div(style="font-size:12px; color:#666;", "Your training history and personal records")
    ),

    # ── Weekly volume overview ────────────────────────────────
    div(style = "background:#1a1a1a; border-radius:12px; padding:16px; margin-bottom:12px;",
      div(class = "ct-section-title", "WEEKLY VOLUME — LAST 12 WEEKS"),
      if (!is.null(logs) && nrow(logs) > 0)
        plotly::plotlyOutput("plot_weekly_volume", height = "180px")
      else
        div(style="text-align:center;color:#444;padding:30px;font-size:13px;",
            "No sessions logged yet. Complete a workout to see your progress.")
    ),

    # ── Exercise selector + chart ─────────────────────────────
    div(style = "background:#1a1a1a; border-radius:12px; padding:16px; margin-bottom:12px;",
      div(class = "ct-section-title", "EXERCISE PROGRESS"),
      if (length(ex_names) > 0) {
        tagList(
          div(style = "margin-bottom:12px;",
            tags$select(
              id    = "selected_exercise",
              style = paste0("background:#2a2a2a; border:1.5px solid #333;",
                             "color:#f0f0f0; border-radius:8px; padding:8px 10px;",
                             "font-size:13px; width:100%;"),
              onchange = "Shiny.setInputValue('select_progress_exercise',
                           this.value, {priority:'event'})",
              lapply(ex_names, function(nm) {
                tags$option(value = nm,
                            selected = identical(nm, selected_exercise),
                            nm)
              })
            )
          ),
          plotly::plotlyOutput("plot_exercise_progress", height = "200px")
        )
      } else {
        div(style="text-align:center;color:#444;padding:20px;font-size:13px;",
            "Log some workouts to track exercise progress.")
      }
    ),

    # ── Personal Records ──────────────────────────────────────
    div(style = "background:#1a1a1a; border-radius:12px; padding:16px; margin-bottom:12px;",
      div(class = "ct-section-title", "PERSONAL RECORDS"),
      if (!is.null(prs) && nrow(prs) > 0) {
        div(style = "display:flex; flex-direction:column; gap:6px;",
          lapply(seq_len(min(10, nrow(prs))), function(i) {
            pr <- prs[i, ]
            div(style = "display:flex; justify-content:space-between;
                         align-items:center; padding:10px 12px;
                         background:#2a2a2a; border-radius:8px;",
              div(
                div(style="font-size:13px; font-weight:600; color:#f0f0f0;",
                    pr$exercise_name),
                div(style="font-size:11px; color:#555; margin-top:2px;",
                    format(as.Date(pr$last_logged_at), "%b %d, %Y"))
              ),
              div(style="text-align:right;",
                div(style="font-size:16px; font-weight:700; color:#e8ff47;",
                    if (!is.na(pr$max_weight_lbs))
                      paste0(pr$max_weight_lbs, " lbs")
                    else "BW"),
                div(style="font-size:11px; color:#555;",
                    if (!is.na(pr$reps_at_max_weight))
                      paste0("× ", pr$reps_at_max_weight, " reps")
                    else "")
              )
            )
          })
        )
      } else {
        div(style="text-align:center;color:#444;padding:20px;font-size:13px;",
            "Complete workouts to set personal records.")
      }
    )
  )
}

# ── FRIENDS / LEADERBOARD SCREEN UI ──────────────────────────
friends_screen_ui <- function(profile, group_members, leaderboard_data,
                               invite_code = NULL) {

  has_group <- !is.null(profile) && !is.null(profile$friend_group_id) &&
               !is.na(profile$friend_group_id)

  tagList(
    div(style = "margin-bottom:16px;",
      div(style="font-size:18px; font-weight:700; margin-bottom:4px;", "Friends 🏆"),
      div(style="font-size:12px; color:#666;",
          "Friendly competition. No weights, no measurements. Just volume and consistency.")
    ),

    if (!has_group) {
      # ── No group yet ─────────────────────────────────────────
      div(
        div(style="background:#1a1a1a; border-radius:12px; padding:20px;
                   text-align:center; margin-bottom:12px;",
          div(style="font-size:32px; margin-bottom:12px;", "👥"),
          div(style="font-size:15px; font-weight:600; margin-bottom:6px;", "Join a friend group"),
          div(style="font-size:13px; color:#666; margin-bottom:16px;",
              "Enter an invite code from a friend, or create a new group."),

          div(style="display:flex; gap:8px; margin-bottom:12px;",
            tags$input(
              type = "text", id = "invite_code_input",
              placeholder = "Enter 8-character invite code",
              style = paste0("flex:1; background:#2a2a2a; border:1.5px solid #333;",
                             "color:#f0f0f0; border-radius:8px; padding:10px;",
                             "font-size:13px;")),
            tags$button("Join",
              style = "background:#e8ff47; color:#000; border:none; border-radius:8px;
                       padding:10px 16px; font-weight:700; cursor:pointer;",
              onclick = "Shiny.setInputValue('join_group',
                          document.getElementById('invite_code_input').value,
                          {priority:'event'})")
          ),

          tags$button("Create a new group",
            class = "ct-btn-secondary",
            onclick = "Shiny.setInputValue('create_group', Math.random(), {priority:'event'})")
        ),

        uiOutput("group_action_msg")
      )

    } else {
      # ── Has a group ──────────────────────────────────────────
      tagList(

        # Invite code card
        if (!is.null(invite_code))
          div(style="background:#1e2200; border:1px solid #e8ff47; border-radius:10px;
                     padding:12px 16px; margin-bottom:12px;
                     display:flex; justify-content:space-between; align-items:center;",
            div(
              div(style="font-size:10px; color:#888; text-transform:uppercase;
                         letter-spacing:0.06em;", "INVITE CODE"),
              div(style="font-size:20px; font-weight:700; color:#e8ff47;
                         letter-spacing:0.1em;", invite_code)
            ),
            div(style="font-size:12px; color:#666;", "Share with friends")
          ),

        # This week's leaderboard
        div(style="background:#1a1a1a; border-radius:12px; padding:16px; margin-bottom:12px;",
          div(class="ct-section-title", "THIS WEEK — TOTAL VOLUME"),
          if (!is.null(leaderboard_data) && nrow(leaderboard_data) > 0) {
            # Get this week's data
            this_week <- leaderboard_data[
              as.Date(leaderboard_data$week_start) ==
                as.Date(cut(Sys.Date(), "week")), ]

            if (nrow(this_week) == 0) this_week <- leaderboard_data[
              leaderboard_data$week_start == max(leaderboard_data$week_start), ]

            this_week <- this_week[order(-this_week$total_volume_lbs), ]

            lapply(seq_len(nrow(this_week)), function(i) {
              row      <- this_week[i, ]
              is_me    <- isTRUE(row$user_id == profile$id)
              max_vol  <- max(this_week$total_volume_lbs)
              pct      <- if (max_vol > 0) round(100 * row$total_volume_lbs / max_vol) else 0L
              medal    <- c("🥇","🥈","🥉","")[min(i, 4)]

              div(style = paste0(
                "padding:10px 12px; border-radius:8px; margin-bottom:6px;",
                if (is_me) "background:#1e2200; border:1px solid #e8ff47;"
                else "background:#2a2a2a;"),
                div(style="display:flex; justify-content:space-between;
                           align-items:center; margin-bottom:6px;",
                  div(style="display:flex; align-items:center; gap:8px;",
                    div(style="font-size:18px;", medal),
                    div(style=paste0("font-size:13px; font-weight:",
                                     if(is_me) "700" else "500", ";"),
                        row$display_name,
                        if (is_me) span(style="color:#e8ff47;font-size:11px;
                                               margin-left:4px;", " (you)"))
                  ),
                  div(style="font-size:13px; font-weight:700; color:#e8ff47;",
                      format(round(row$total_volume_lbs), big.mark=","),
                      span(style="font-size:10px;color:#555;margin-left:3px;","lbs"))
                ),
                # Volume bar
                div(style="background:#1a1a1a; border-radius:3px; height:4px;",
                  div(style=sprintf("width:%d%%;height:100%%;border-radius:3px;
                                    background:%s;",
                                    pct,
                                    if(is_me)"#e8ff47" else "#4ade80")))
              )
            })
          } else {
            div(style="text-align:center;color:#444;padding:20px;font-size:13px;",
                "No data yet this week. Complete a workout to appear on the board!")
          }
        ),

        # Member list
        div(style="background:#1a1a1a; border-radius:12px; padding:16px;",
          div(class="ct-section-title", "GROUP MEMBERS"),
          if (!is.null(group_members) && nrow(group_members) > 0) {
            lapply(seq_len(nrow(group_members)), function(i) {
              m    <- group_members[i, ]
              name <- tryCatch(m$user_profiles$display_name, error=\(e) "Unknown")
              div(style="display:flex; align-items:center; gap:10px; padding:8px 0;
                         border-bottom:1px solid #222;",
                div(style="width:32px; height:32px; border-radius:50%;
                           background:#2a2a2a; display:flex; align-items:center;
                           justify-content:center; font-size:14px;",
                    substr(name, 1, 1)),
                div(style="font-size:13px;", name)
              )
            })
          } else {
            div(style="color:#444;font-size:13px;", "No members found.")
          }
        )
      )
    }
  )
}

# ── PROGRESS + FRIENDS SERVER LOGIC ──────────────────────────
setup_progress_server <- function(input, output, session, rv) {

  # ── Lazy-load progress data when tab opens ──────────────────
  observe({
    req(rv$token, rv$user_id)
    if (!rv$page %in% c("progress", "friends")) return()

    if (is.null(rv$all_logs)) {
      rv$all_logs <- tryCatch(
        fetch_all_logs(rv$user_id, rv$token),
        error = \(e) NULL)
    }
    if (is.null(rv$prs)) {
      rv$prs <- tryCatch(
        fetch_prs(rv$user_id, rv$token),
        error = \(e) NULL)
    }

    # Load friend group data
    if (rv$page == "friends" && !is.null(rv$profile) &&
        !is.null(rv$profile$friend_group_id) &&
        !is.na(rv$profile$friend_group_id)) {
      gid <- rv$profile$friend_group_id
      if (is.null(rv$leaderboard)) {
        rv$leaderboard    <- tryCatch(fetch_leaderboard(gid, rv$token), error=\(e) NULL)
        rv$group_members  <- tryCatch(fetch_group_members(gid, rv$token), error=\(e) NULL)
        rv$invite_code    <- tryCatch({
          g <- sb_select("friend_groups", sprintf("?id=eq.%s&select=invite_code", gid),
                         token = rv$token)
          if (!is.null(g)) g$invite_code[1] else NULL
        }, error=\(e) NULL)
      }
    }
  })

  # ── Exercise selector ───────────────────────────────────────
  observeEvent(input$select_progress_exercise, {
    rv$selected_exercise <- input$select_progress_exercise
  })

  # ── Weekly volume plot ──────────────────────────────────────
  output$plot_weekly_volume <- plotly::renderPlotly({
    req(rv$all_logs)
    plot_weekly_volume(rv$all_logs)
  })

  # ── Exercise progress plot ──────────────────────────────────
  output$plot_exercise_progress <- plotly::renderPlotly({
    req(rv$all_logs)
    ex_name <- rv$selected_exercise %||% {
      # Default to first available exercise
      tryCatch(
        rv$all_logs$workout_exercises[[1]]$exercises$name,
        error = \(e) NULL)
    }
    if (is.null(ex_name)) return(NULL)

    # Filter logs for this exercise
    ex_logs <- rv$all_logs[sapply(seq_len(nrow(rv$all_logs)), function(i) {
      tryCatch(
        identical(rv$all_logs$workout_exercises[[i]]$exercises$name, ex_name),
        error = \(e) FALSE)
    }), ]

    plot_exercise_volume(ex_logs, ex_name)
  })

  # ── Create friend group ─────────────────────────────────────
  observeEvent(input$create_group, {
    req(rv$token, rv$user_id, rv$profile)

    group_name <- paste0(rv$profile$display_name, "'s Group")
    resp <- sb_insert("friend_groups",
      list(name = group_name, created_by = rv$user_id),
      token = rv$token)

    if (resp$status_code %in% c(200, 201)) {
      new_group <- tryCatch(fromJSON(resp_body_string(resp), simplifyDataFrame=TRUE),
                            error=\(e) NULL)
      if (!is.null(new_group)) {
        group_id <- if (is.data.frame(new_group)) new_group$id[1] else new_group[[1]]$id

        # Add self as member
        sb_insert("friend_group_members",
          list(group_id = group_id, user_id = rv$user_id),
          token = rv$token)

        # Update profile with group_id
        sb_update("user_profiles",
          sprintf("?id=eq.%s", rv$user_id),
          list(friend_group_id = group_id),
          token = rv$token)

        # Refresh profile
        profile <- sb_select("user_profiles",
          sprintf("?id=eq.%s", rv$user_id), token = rv$token)
        if (!is.null(profile)) rv$profile <- profile[1, ]

        # Clear cached group data so it reloads
        rv$leaderboard   <- NULL
        rv$group_members <- NULL
        rv$invite_code   <- NULL

        showNotification("Group created! Share your invite code with friends.",
                         type="message", duration=4)
      }
    } else {
      showNotification("Error creating group. Try again.", type="error")
    }
  })

  # ── Join friend group ───────────────────────────────────────
  observeEvent(input$join_group, {
    req(rv$token, rv$user_id)
    code <- trimws(input$join_group %||% "")
    if (nchar(code) == 0) {
      showNotification("Please enter an invite code.", type="warning"); return()
    }

    # Look up group by invite code
    group <- sb_select("friend_groups",
      sprintf("?invite_code=eq.%s&select=id,name", code),
      token = rv$token)

    if (is.null(group)) {
      showNotification("Invite code not found. Check with your friend.",
                       type="error"); return()
    }

    group_id <- group$id[1]

    # Add as member
    sb_insert("friend_group_members",
      list(group_id = group_id, user_id = rv$user_id),
      token = rv$token)

    # Update profile
    sb_update("user_profiles",
      sprintf("?id=eq.%s", rv$user_id),
      list(friend_group_id = group_id),
      token = rv$token)

    profile <- sb_select("user_profiles",
      sprintf("?id=eq.%s", rv$user_id), token = rv$token)
    if (!is.null(profile)) rv$profile <- profile[1, ]

    rv$leaderboard   <- NULL
    rv$group_members <- NULL
    rv$invite_code   <- NULL

    showNotification(paste0("Joined ", group$name[1], "! 🎉"),
                     type="message", duration=4)
  })

  output$group_action_msg <- renderUI({ NULL })
}
