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

---

# 2026-09 Refactor — single-user, no social, better programming

_Date: 2026-09-01_

Owner-approved sweep: remove social, auto-login (no sign-in screen), fix bugs,
and overhaul the programming engine. All existing tracked data preserved.

## Social / friends — removed
Deleted the Friends tab, leaderboard, groups, activity feed, and exercise-privacy
(`friends_screen_ui` + all group/leaderboard/streak/1RM/feed helpers in
`progress_screen.R`; the friends route, social reactive state, and resets in
`server.R`; the nav item + icon + back-guard entry in `global.R`). Kept
`display_name` (it names programs; reworded "shown to friends" copy). The DB
tables `friend_groups` / `friend_group_members` / `leaderboard_weekly_volume` and
the `user_profiles.friend_group_id` / `hidden_from_leaderboard` columns are left
in place (non-destructive) — the code just stops reading them.

## Auth — kiosk auto-login (RLS kept)
`server.R` auto-signs-in as the owner at startup from `CATRACK_EMAIL` /
`CATRACK_PASSWORD` env vars (email defaults to the owner's), so there is no login
screen. A real user JWT is obtained, so RLS still applies. The reliability JS
(heartbeat, reconnect self-heal, session/last-view restore, back-guard, offline
banner) was extracted from `login_page_ui` into `catrack_runtime_js()` and mounted
once in `ui.R` so it loads on every page. Falls back to a login-only form if the
creds are unset/rejected (never bricks). **Set `CATRACK_PASSWORD` in Posit Connect
env vars (and `.Renviron` locally) to activate auto-login.**

## Bugs fixed
- **PR detection** (`workout_screen.R`): an `rbind` column mismatch aborted PR
  detection after the first PR of a session for any returning user — normalised
  the frame and compare against the running best (no more regressions).
- **Deload load** (`suggest_weight`): now suggests ~40% lighter in week 4 to match
  the deload banner (was ~5%).
- **Completion counts** (`workout_screen.R`): count non-NULL sparse-list entries so
  logging the last set first no longer marks an exercise 100% done.
- **CSV export** (`program_screen.R`): use `rep_range_low/high` (was `rep_low/high`
  → always blank).
- **Zero-set summary** crash guard (`workout_summary.R`).
- **Delete cascade** (`program_screen.R`): chunk `in.()` id lists + check responses
  so a large program's children can't be orphaned.
- **Session duration**: clear the persisted start on close/finish so reopening a
  workout doesn't inflate the recorded time.

## Programming engine (`program_generation.R` + `methodology.R`)
- **Variety**: anti-repeat penalty vs the previous block + within-week soft
  penalty, and a rotation among tied top picks (replaces the alphabetical
  tie-break). Blocks A/B/C now actually rotate exercises even on limited gear.
- **Progression**: Block C adds a set to heavy/compound work; peak week adds a
  set to the main lift; deload halves sets (block_mult now reaches the real
  prescription, not just the scoring target).
- **Trim** is priority-based: protects heavy lifts, plyometrics, isometric holds,
  compounds/carries, and calf/core over generic accessories, so 30-min running /
  functional sessions no longer collapse to two lifts.
- Plyometric/isometric slots no longer count toward hypertrophy MAV.
- Pull-up assist (assisted/pulldown bias for sub-3 users) now applies in every
  block, not just Block A. Dead no-op pull-pattern branch removed. `full_body`
  missing-key fallback hardened.

## DB migration (`db_migrate_2026_09.R`, idempotent — already run)
- Renamed **"Kettlebell Swing" → "Dumbbell Swing"** (it required only dumbbells —
  the source of the "kettlebell prescribed" complaint; labels are never stored).
- Fixed **Nordic Ham Curl** equipment (`lat_pulldown_machine` → `bodyweight`).
- Added the **plyometric + isometric-hold** library (`seed_running_exercises.R`
  had never been run on this DB, so running-support was silently dropping every
  jump/hold) + bodyweight/cable **hip-abduction** fallbacks.

## Data preservation
The owner's active program was regenerated from the first un-logged week only
(`from_week` computed to keep every week with a completed/logged session).
Verified before/after: `workout_set_logs` 350→350, `workouts` 72→72, PRs
untouched. Verified via parse (all files, R 4.3.3), full app load, generator
dry-run, live regeneration, and an app boot (which caught + fixed a reactive-
context crash in the kiosk-login call). `manifest.json` regenerated (R 4.4.0).
