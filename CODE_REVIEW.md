# CatRack — Code Review & Fix Log

_Review date: 2026-07-21_

## 1. What the app is

CatRack is a mobile-first Progressive Web App (installable, offline-aware shell)
for guided strength training. A 5-step onboarding collects goal, difficulty,
training split, frequency, session length, pull-up baseline, and available
equipment, then generates a **12-week periodized program**. Users log each
session set-by-set with auto-suggested weights, rest timers, exercise history,
and swapping; they track progress with estimated-1RM charts, and compare weekly
volume / streaks / 1RMs against friends.

**Stack:** R Shiny (`global.R` / `ui.R` / `server.R` / `app.R` split) · Supabase
(PostgreSQL + RLS + PostgREST) via `httr2` · bslib Bootstrap 5 dark theme ·
plotly charts · PWA service worker + manifest. Deployed via GitHub →
Posit Connect auto-deploy (never the rsconnect CLI).

## 2. Methodology (the "inspiration")

Renaissance Periodization / Jeff Nippard volume-and-periodization framework:

- **Volume landmarks** per muscle (MV / MEV / MAV / MRV) for 13 muscle groups,
  scaled by goal multipliers and experience level (`methodology.R`).
- **12 weeks = 3 blocks of 4 weeks.** Block A Foundation (RIR 3.0, 0.85× volume),
  Block B Intensification (RIR 1.5, 1.0×), Block C Realization (RIR 0.5, 1.15×).
  Week 4 of each block is a deload.
- **Exercise selection** is scored on compound preference, current muscle-volume
  deficit, block-preferred equipment (barbell→A, machine→B, DB/BW→C), and
  pull-up baseline (`program_generation.R`).
- **Weight suggestions** use an Epley/Brzycki e1RM blend × block RIR factor,
  rounded to 2.5 lb.
- **Goals:** Hypertrophy, Strength, Pull-Up, Running Support (running-specific
  full-body with plyometric + isometric slots), Functional.

## 3. Findings & resolutions

Legend: ✅ fixed · ⏭️ intentionally deferred/left as-is

### Bugs

| ID | File | Issue | Resolution |
|----|------|-------|-----------|
| B1 | `global.R` | `bottom_nav_ui` defined twice; the first (emoji) definition was dead code, overridden by the SVG version. | ✅ Removed the dead definition. |
| B2 | `workout_summary.R` | "Sets Done" overcounted: `set_logs` is a sparse list keyed by set number, so `length()` counted `NULL` holes when earlier sets were skipped. | ✅ Count only non-null entries; also filter NULL holes in the per-exercise breakdown. |
| B4 | `profile_screen.R` | 111-line `input$save_profile` observer had no UI trigger (replaced by the Edit Gear / Edit Frequency modals) — dead code. | ✅ Removed (confirmed intentional). |
| B5 | `server.R` / `workout_screen.R` | PR detection compared against `rv$prs`, only loaded when the Progress tab was opened. If never opened, `rv$prs` was NULL → every set flagged as a PR and could overwrite real records with lower values. | ✅ Pre-load PRs in `load_user_data()`. |
| B6 | `www/sw.js` | Service worker precached absolute paths (`/`, `/www/...`) that 404 under Posit Connect's content path, so `addAll()` rejected atomically (precached nothing), while risking a wrong-shell cache on a root deploy. | ✅ Dropped the static precache; rely on the scope-correct runtime cache. Bumped `catrack-v3`→`v4`. |
| B8 | `progress_screen.R` | `fetch_all_logs()` used `order=logged_at.asc&limit=3000` — once the cap is hit the **newest** sessions are silently dropped from all charts. | ✅ Fetch `desc&limit=5000`, then re-sort ascending in R (downstream code assumes ascending order). |
| B9 | `program_screen.R` | CSV export made one `workout_exercises` request per session (N+1 — 36+ round-trips for a 12-week/3-day program). | ✅ Single batched `workout_id=in.(...)` query, joined client-side. |
| B10 | `workout_screen.R` | `fetch_exercise_history()` and `fetch_last_performance()` fetched all `workout_exercise` IDs for an exercise with no limit (URL-length risk via the `in.(...)` list). | ✅ Added `limit=1000` safety cap. |

### Inefficiencies

| ID | File | Issue | Resolution |
|----|------|-------|-----------|
| I1 | `server.R` | `reactiveTimer(120000)` refetched the whole workouts table every 2 min on the dashboard, even when idle — only useful for simultaneous multi-device use. | ✅ Removed (single-device confirmed; refresh is already event-driven). |
| I2 | `server.R` | Concern that a newly generated program wouldn't appear on the Programs tab. | ⏭️ Already handled — onboarding calls `load_user_data()`, which re-fetches `all_programs`. |

### Improvements

| ID | File | Issue | Resolution |
|----|------|-------|-----------|
| U1 | `workout_screen.R` | After finishing a session, `rv$prs` was set to NULL, so the Progress tab and next session's PR baseline lagged until reload. | ✅ Refresh `rv$prs` from the DB on finish instead of nulling. |
| U3 | `workout_summary.R` | Set-notes filter missed the `"NULL"` token and untrimmed whitespace. | ✅ `trimws()` the value and added `"NULL"` to the ignore list. |
| U2 | `profile_screen.R` | Considered removing the `current_streak > 0` gate on the "at risk" warning. | ⏭️ Left as-is — `streak_badge_ui` already hides the badge when `current == 0`, so the change would be a no-op. |

### Deferred by design

| ID | Issue | Why left |
|----|-------|----------|
| B7 | Edit Frequency modal only offers 2 or 3 days/week. | Intentional per product owner. |
| — | Changing the Progress exercise/metric re-renders the whole `main_ui` (minor flicker) because the router reads `rv$selected_exercise` / `rv$progress_metric`. | Cosmetic; a proper fix means decoupling those selectors into child outputs — larger refactor, not done in this pass. |

## 3b. Follow-up fixes (user-reported)

### Progress tab: selected exercise wouldn't stay selected

**Cause:** `output$main_ui` read `rv$selected_exercise` and `rv$progress_metric`,
so every dropdown pick (and the now-removed 2-min poll) re-rendered the *entire*
progress page, rebuilding the native `<select>` and losing the visible choice.

**Fix:**
- `server.R` — `isolate()` the `selected_exercise` / `metric` reads in the
  progress branch, so changing them updates only the child outputs (chart,
  summary, detail, toggle) instead of the whole page. The native dropdown now
  keeps its own value.
- `progress_screen.R` — the dropdown now resolves its selected option with the
  same "most-recently-logged" fallback the chart reactive uses, so they agree on
  first open; the metric toggle moved into its own `renderUI`
  (`progress_metric_toggle`) so clicking it re-highlights without touching the
  dropdown.

### Server times out mid-workout, forcing a restart

Two compounding causes:
- **No keep-alive during rest.** Posit Connect drops idle websocket
  connections; a 2-3 min rest between sets sends no client→server traffic.
  `global.R` now sends a tiny `client_heartbeat` input every 30 s to keep the
  socket busy. (Removing the old 2-min dashboard poll had left *no* keep-alive
  at all during a workout.)
- **Restore-routing race.** On reload, `restore_last_view` and
  `restore_session_refresh` fire in the same tick with no guaranteed order; if
  the refresh ran first, `pending_last_view` was still empty and the user landed
  on the dashboard. `server.R` now shares a `route_to_last_view()` helper that
  routes from whichever observer wins (immediately if already authenticated,
  else stashed for post-auth). `start_from_preview` already reloads logged sets
  from the DB, so resuming loses no data.

### History showed only the last comment per set

`fetch_exercise_history()` (`workout_screen.R`) kept only the best set's note per
session. It now collects **every** set's note for the day, in set order,
labelled `Set N:` when there's more than one; the history render splits and shows
each on its own line. Also switched the session cap from `[seq_len()]` to
`head()` so users with little history don't get NA-padded rows.

## 4. Verification

All seven edited R files pass `parse()` with R 4.3.3. No functional/runtime
testing was performed (requires a live Supabase session). Recommend a manual
smoke test of: finish-session PR detection, Progress charts, CSV export, and
PWA reinstall (service worker cache bumped to v4).
