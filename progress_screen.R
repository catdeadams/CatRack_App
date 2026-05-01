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
  sb_select("workout_set_logs",
            sprintf(paste0("?user_id=eq.%s&is_warmup=eq.false",
                           "&select=*,workout_exercises(exercise_id,exercises(name,category,primary_muscles))",
                           "&order=logged_at.asc&limit=3000"), user_id),
            token = token)
}

fetch_prs <- function(user_id, token) {
  sb_select("personal_records",
            sprintf("?user_id=eq.%s&order=last_logged_at.desc", user_id),
            token = token)
}

fetch_leaderboard <- function(group_id, token) {
  sb_select("leaderboard_weekly_volume",
            sprintf("?friend_group_id=eq.%s&order=week_start.desc,total_volume_lbs.desc",
                    group_id), token = token)
}

fetch_group_members <- function(group_id, token) {
  sb_select("friend_group_members",
            sprintf("?group_id=eq.%s&select=user_id,joined_at,user_profiles(display_name,id)",
                    group_id), token = token)
}

get_ex_name <- function(logs, i) {
  tryCatch({
    we <- logs$workout_exercises[[i]]
    if (is.list(we) && !is.null(we$exercises)) we$exercises$name
    else if (is.data.frame(we) && "exercises" %in% names(we)) we$exercises$name[[1]]
    else NA_character_
  }, error = \(e) NA_character_)
}


plot_exercise_progress <- function(logs_df, exercise_name) {
  if (is.null(logs_df) || nrow(logs_df) == 0) return(NULL)
  logs_df$logged_at <- as.POSIXct(logs_df$logged_at)
  logs_df$date      <- as.Date(logs_df$logged_at)
  logs_df$weight_n  <- as.numeric(logs_df$weight_lbs)
  logs_df$reps_n    <- as.integer(logs_df$reps_completed)
  logs_df$rpe_n     <- as.numeric(logs_df$rpe_actual)
  logs_df$e1rm      <- mapply(estimate_1rm, logs_df$weight_n, logs_df$reps_n)
  
  sess <- logs_df |>
    group_by(date) |>
    summarise(
      max_weight = max(weight_n,  na.rm = TRUE),
      best_e1rm  = max(e1rm,      na.rm = TRUE),
      best_reps  = reps_n[which.max(replace(e1rm, is.na(e1rm), -Inf))],
      best_rpe   = rpe_n[which.max(replace(e1rm, is.na(e1rm), -Inf))],
      .groups = "drop"
    ) |> filter(is.finite(max_weight))
  
  if (nrow(sess) == 0) return(NULL)
  
  plotly::plot_ly(sess, x = ~date) |>
    plotly::add_lines(y = ~max_weight, name = "Max Weight",
                      line = list(color = "#1D9E75", width = 2.5),
                      text = ~paste0(format(date,"%b %d"), "<br>",
                                     max_weight," lbs x ",best_reps," reps",
                                     ifelse(!is.na(best_rpe), paste0(" @ RPE ",best_rpe), "")),
                      hoverinfo = "text") |>
    plotly::add_lines(y = ~best_e1rm, name = "Est. 1RM",
                      line = list(color = "#5DCAA5", width = 2, dash = "dot"),
                      text = ~paste0(format(date,"%b %d"),"<br>Est. 1RM: ",round(best_e1rm)," lbs"),
                      hoverinfo = "text") |>
    plotly::add_markers(y = ~max_weight, showlegend = FALSE,
                        marker = list(color="#1D9E75", size=6), hoverinfo="skip") |>
    plotly::add_markers(y = ~best_e1rm, showlegend = FALSE,
                        marker = list(color="#5DCAA5", size=5, symbol="circle-open"), hoverinfo="skip") |>
    plotly::layout(
      paper_bgcolor = "#161616", plot_bgcolor = "#161616",
      font  = list(color="#888", size=11, family="system-ui,sans-serif"),
      xaxis = list(gridcolor="#1e1e1e", zeroline=FALSE, tickformat="%b %d",
                   title="", color="#555"),
      yaxis = list(gridcolor="#1e1e1e", zeroline=FALSE, title="lbs",
                   titlefont=list(color="#555",size=11)),
      legend = list(orientation="h", x=0, y=1.18,
                    font=list(color="#888",size=11), bgcolor="rgba(0,0,0,0)"),
      hoverlabel = list(bgcolor="#1e1e1e", bordercolor="#0F6E56",
                        font=list(color="#f0f0f0",size=12)),
      margin = list(t=32,b=40,l=45,r=15)
    ) |> plotly::config(displayModeBar=FALSE)
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
                  marker=list(color="rgba(29,158,117,0.55)", line=list(color="#1D9E75",width=1))) |>
    plotly::layout(
      paper_bgcolor="#161616", plot_bgcolor="#161616",
      font=list(color="#888",size=11,family="system-ui,sans-serif"),
      xaxis=list(gridcolor="#1e1e1e",zeroline=FALSE,tickformat="%b %d",title="",
                 color="#555"),
      yaxis=list(gridcolor="#1e1e1e",zeroline=FALSE,title="Volume (lbs)",
                 titlefont=list(color="#555",size=11), color="#555"),
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

progress_screen_ui <- function(logs, prs, program=NULL, workouts=NULL, selected_exercise=NULL) {
  ex_names <- character(0)
  if (!is.null(logs) && nrow(logs) > 0) {
    ex_names <- tryCatch(
      sort(unique(na.omit(sapply(seq_len(nrow(logs)), \(i) get_ex_name(logs, i))))),
      error=\(e) character(0))
  }
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
                            lapply(ex_names, \(nm) tags$option(value=nm, selected=identical(nm,selected_exercise), nm)))),
            div(style="font-size:11px;color:#444;margin-bottom:8px;",
                "Solid = max weight  ·  Dashed = estimated 1RM  ·  Hover for details"),
            plotly::plotlyOutput("plot_exercise_progress", height="220px"))
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

friends_screen_ui <- function(profile, group_members, leaderboard_data,
                              invite_code=NULL, user_prs=NULL) {
  has_group <- !is.null(profile) && !is.null(profile$friend_group_id) &&
    !is.na(profile$friend_group_id) &&
    nchar(as.character(profile$friend_group_id)) > 5
  tagList(
    div(style="margin-bottom:16px;",
        div(style="font-size:18px;font-weight:700;color:#f0f0f0;","Friends"),
        div(style="font-size:12px;color:#555;","Volume, streaks, and 1RM")),
    if (!has_group) {
      div(
        div(style="background:#161616;border:1px solid #222;border-radius:12px;padding:24px;text-align:center;margin-bottom:12px;",
            div(style="font-size:32px;margin-bottom:12px;","👥"),
            div(style="font-size:15px;font-weight:600;color:#f0f0f0;margin-bottom:6px;","Join a friend group"),
            div(style="font-size:13px;color:#555;margin-bottom:16px;","Enter an invite code from a friend, or create a new group."),
            div(style="display:flex;gap:8px;margin-bottom:10px;",
                tags$input(type="text",id="invite_code_input",placeholder="8-character invite code",
                           style="flex:1;background:#1e1e1e;border:1.5px solid #262626;color:#f0f0f0;border-radius:8px;padding:10px;font-size:13px;"),
                tags$button("Join",
                            style="background:#1D9E75;color:#fff;border:none;border-radius:8px;padding:10px 16px;font-weight:700;cursor:pointer;font-size:13px;",
                            onclick="Shiny.setInputValue('join_group',document.getElementById('invite_code_input').value,{priority:'event'})")),
            tags$button("Create a new group",class="ct-btn-secondary",
                        onclick="Shiny.setInputValue('create_group',Math.random(),{priority:'event'})")),
        uiOutput("group_action_msg"))
    } else {
      tagList(
        if(!is.null(invite_code))
          div(style="background:#061a12;border:1px solid #0F6E56;border-radius:10px;padding:12px 16px;margin-bottom:12px;display:flex;justify-content:space-between;align-items:center;",
              div(div(style="font-size:10px;color:#555;text-transform:uppercase;letter-spacing:0.08em;","Invite Code"),
                  div(style="font-size:22px;font-weight:700;color:#1D9E75;letter-spacing:0.12em;",invite_code)),
              div(style="font-size:12px;color:#555;","Share with friends")),
        div(style="background:#161616;border:1px solid #222;border-radius:12px;padding:16px;margin-bottom:12px;",
            div(class="ct-section-title","THIS WEEK — TOTAL VOLUME"),
            if(!is.null(leaderboard_data) && nrow(leaderboard_data)>0) {
              tw <- leaderboard_data[as.Date(leaderboard_data$week_start)==as.Date(cut(Sys.Date(),"week")),]
              if(nrow(tw)==0) tw <- leaderboard_data[leaderboard_data$week_start==max(leaderboard_data$week_start),]
              tw <- tw[order(-tw$total_volume_lbs),]
              max_v <- max(tw$total_volume_lbs, na.rm=TRUE)
              lapply(seq_len(nrow(tw)), \(i) {
                row   <- tw[i,]
                is_me <- isTRUE(row$user_id==profile$id)
                pct   <- if(max_v>0) round(100*row$total_volume_lbs/max_v) else 0L
                medal <- c("🥇","🥈","🥉","")[min(i,4)]
                div(style=paste0("padding:10px 12px;border-radius:8px;margin-bottom:5px;",
                                 if(is_me)"background:#061a12;border:1px solid #0F6E56;" else "background:#1e1e1e;border:1px solid #222;"),
                    div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:5px;",
                        div(style="display:flex;align-items:center;gap:8px;",
                            div(style="font-size:16px;",medal),
                            div(style=paste0("font-size:13px;font-weight:",if(is_me)"700"else"500",";color:",if(is_me)"#f0f0f0"else"#aaa",";"),
                                row$display_name,
                                if(is_me) span(style="color:#5DCAA5;font-size:10px;font-weight:400;margin-left:4px;"," you"))),
                        div(style="font-size:14px;font-weight:700;color:#1D9E75;",
                            format(round(row$total_volume_lbs),big.mark=","),
                            span(style="font-size:10px;color:#555;margin-left:2px;","lbs"))),
                    div(style="background:#111;border-radius:3px;height:3px;",
                        div(style=sprintf("width:%d%%;height:100%%;border-radius:3px;background:%s;",
                                          pct,if(is_me)"#1D9E75"else"#0F6E56"))))
              })
            } else div(style="text-align:center;color:#444;padding:20px;font-size:13px;","No data yet this week.")),
        div(style="background:#161616;border:1px solid #222;border-radius:12px;padding:16px;",
            div(class="ct-section-title","GROUP MEMBERS"),
            if(!is.null(group_members) && nrow(group_members)>0) {
              lapply(seq_len(nrow(group_members)), \(i) {
                m    <- group_members[i,]
                name <- tryCatch(m$user_profiles$display_name, error=\(e) "Unknown")
                is_me <- isTRUE(m$user_id==profile$id)
                div(style="display:flex;align-items:center;gap:10px;padding:10px 0;border-bottom:1px solid #1e1e1e;",
                    div(style="width:34px;height:34px;border-radius:50%;flex-shrink:0;display:flex;align-items:center;justify-content:center;font-size:13px;font-weight:700;background:#0F6E56;color:#E1F5EE;",
                        toupper(substr(name,1,1))),
                    div(div(style="font-size:13px;font-weight:600;color:#f0f0f0;",name,
                            if(is_me) span(style="color:#555;font-size:11px;font-weight:400;margin-left:4px;","(you)")),
                        div(style="font-size:11px;color:#555;","Member")))
              })
            } else div(style="color:#444;font-size:13px;padding:10px 0;","No members found."))
      )
    }
  )
}

setup_progress_server <- function(input, output, session, rv) {
  observe({
    req(rv$token, rv$user_id)
    if (!rv$page %in% c("progress","friends")) return()
    if (is.null(rv$all_logs))
      rv$all_logs <- tryCatch(fetch_all_logs(rv$user_id,rv$token), error=\(e) NULL)
    if (is.null(rv$prs))
      rv$prs <- tryCatch(fetch_prs(rv$user_id,rv$token), error=\(e) NULL)
    if (rv$page=="friends" && !is.null(rv$profile) &&
        !is.null(rv$profile$friend_group_id) && !is.na(rv$profile$friend_group_id) &&
        nchar(as.character(rv$profile$friend_group_id))>5) {
      gid <- rv$profile$friend_group_id
      if (is.null(rv$leaderboard)) {
        rv$leaderboard   <- tryCatch(fetch_leaderboard(gid,rv$token), error=\(e) NULL)
        rv$group_members <- tryCatch(fetch_group_members(gid,rv$token), error=\(e) NULL)
        rv$invite_code   <- tryCatch({
          g <- sb_select("friend_groups",sprintf("?id=eq.%s&select=invite_code",gid),token=rv$token)
          if(!is.null(g)) g$invite_code[1] else NULL
        }, error=\(e) NULL)
      }
    }
  })
  
  observeEvent(input$select_progress_exercise, { rv$selected_exercise <- input$select_progress_exercise })
  
  output$plot_weekly_volume <- plotly::renderPlotly({
    req(rv$all_logs); plot_weekly_volume(rv$all_logs)
  })
  
  output$plot_exercise_progress <- plotly::renderPlotly({
    req(rv$all_logs)
    ex_name <- rv$selected_exercise %||% tryCatch(get_ex_name(rv$all_logs,1), error=\(e) NULL)
    if (is.null(ex_name)||is.na(ex_name)) return(NULL)
    ex_logs <- rv$all_logs[sapply(seq_len(nrow(rv$all_logs)),
                                  \(i) isTRUE(identical(get_ex_name(rv$all_logs,i), ex_name))),]
    plot_exercise_progress(ex_logs, ex_name)
  })
  
  observeEvent(input$create_group, {
    req(rv$token, rv$user_id, rv$profile)
    tryCatch({
      group_name <- paste0(rv$profile$display_name, "'s Group")
      
      # Use service key for group creation to avoid RLS cross-table issues
      svc_token <- if (nchar(SUPABASE_SERVICE_KEY) > 0) SUPABASE_SERVICE_KEY else rv$token
      
      resp <- sb_insert("friend_groups",
                        list(name = group_name, created_by = rv$user_id),
                        token = svc_token)
      
      if (!resp$status_code %in% c(200, 201)) {
        msg <- tryCatch(fromJSON(resp_body_string(resp))$message, error=\(e) "Unknown error")
        showNotification(paste("Could not create group:", msg), type="error")
        return()
      }
      
      ng  <- tryCatch(fromJSON(resp_body_string(resp), simplifyDataFrame=TRUE), error=\(e) NULL)
      gid <- tryCatch({
        if (is.data.frame(ng)) ng$id[1] else ng[[1]]$id
      }, error=\(e) NULL)
      
      if (is.null(gid)) {
        showNotification("Group created but could not read ID. Refresh and try again.",
                         type="warning"); return()
      }
      
      sb_insert("friend_group_members",
                list(group_id = gid, user_id = rv$user_id), token = svc_token)
      
      sb_update("user_profiles",
                sprintf("?id=eq.%s", rv$user_id),
                list(friend_group_id = gid), token = rv$token)
      
      pf <- sb_select("user_profiles", sprintf("?id=eq.%s", rv$user_id), token = rv$token)
      if (!is.null(pf)) rv$profile <- pf[1, ]
      
      rv$leaderboard <- NULL; rv$group_members <- NULL; rv$invite_code <- NULL
      showNotification(paste0("Group \"", group_name, "\" created! Share your invite code."),
                       type = "message", duration = 5)
      
    }, error = function(e) {
      showNotification(paste("Error creating group:", conditionMessage(e)), type="error")
    })
  })
  
  observeEvent(input$join_group, {
    req(rv$token,rv$user_id)
    code <- trimws(input$join_group %||% "")
    if (nchar(code)==0) { showNotification("Please enter an invite code.",type="warning"); return() }
    group <- sb_select("friend_groups",sprintf("?invite_code=eq.%s&select=id,name",code),token=rv$token)
    if (is.null(group)) { showNotification("Invite code not found.",type="error"); return() }
    gid <- group$id[1]
    sb_insert("friend_group_members",list(group_id=gid,user_id=rv$user_id),token=rv$token)
    sb_update("user_profiles",sprintf("?id=eq.%s",rv$user_id),list(friend_group_id=gid),token=rv$token)
    pf <- sb_select("user_profiles",sprintf("?id=eq.%s",rv$user_id),token=rv$token)
    if(!is.null(pf)) rv$profile <- pf[1,]
    rv$leaderboard<-NULL; rv$group_members<-NULL; rv$invite_code<-NULL
    showNotification(paste0("Joined ",group$name[1],"! 🎉"),type="message",duration=4)
  })
  
  output$group_action_msg <- renderUI({ NULL })
}