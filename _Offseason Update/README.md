# DANTE offseason update: the runbook

Everything needed to roll VRGL (men's) and BTRC (women's) into a new season, in order.

- **Steps 0–2** are short and hands-on: setting the year, team data, and the scrapes.
- **Steps 3–8** are scripted. `_Offseason Update/Run Offseason Update.R` runs them in order, logs each one, and stops at the first failure.

The offseason scripts run from RStudio (project open or not) or with `Rscript` from the repo root. They find the DANTE folder themselves, from the script's own location in RStudio or the working directory otherwise, so the working directory doesn't matter. The daily scripts still run from the repo root, as GitHub Actions runs them.

## The year lives in one place
`_Helper Files/season.R`:
```r
dante_season    <- 2027             # the coming season (2026-27 = 2027)
dante_completed <- dante_season - 1 # the season that just finished
```
Every daily, tournament, offseason and backtest script reads it. **Change it once, right after both NCAA tournaments are over.** Nothing else in the code holds a year.

## The steps

| Step | What | Script(s) | Run by | Time |
|---|---|---|---|---|
| 0 | Set the coming season | `_Helper Files/season.R` | you | 1 min |
| 1 | Team data and conferences | `_Helper Files/Team Data/_Get CBB Conferences.R`; `VRGL/.../Recruiting and Priors/0. Update 247 Teams.R`; as needed: `_Fix Team Database.R`, `_Get Missing Venues.R` (and `- WBB`) | you | 30 min |
| 2 | Men's recruiting, transfers, draft entrants | `VRGL/.../Recruiting and Priors/1. Recruiting and Transfer Scrape - MBB.R`, `2. Draft Entrant Scrape - MBB.R` (paste that year's Hoops Rumors URL), `3. Clean Recruiting - MBB.R` | you | 1–2 h |
| 3 | Full-season ratings for the finished season | `VRGL/.../Full Season Ratings/Full-Season Ratings - All.R`, `BTRC/.../Full-Season Ratings WBB - All.R` | runner | 10–20 min |
| 4 | Shot models | per league: `_Create Shot File for Models`, then `_Model Maker - All Models (Preseason)` | runner | 30–60 min |
| 5 | Re-test and re-pick the prior models | `_Offseason Update/5. Refresh Prior Models.R` | runner | 5–10 min |
| 6 | Priors for the coming season | `VRGL/.../Recruiting and Priors/4. Create Priors - MBB.R`, `BTRC/.../_Create Priors (Simple) - WBB.R` | runner | 5 min |
| 7 | Backtest and rebuild the sim calibration | `_Offseason Update/7. Rebuild Calibration.R` | runner | 1–1.5 h |
| 8 | Check everything is in place | `_Offseason Update/8. Check Season Ready.R` | runner | 1 min |
| 9 | Commit and push; turn the daily run back on once games start | `.github/workflows/vrgl-pipeline.yml` | you | 5 min |

To run steps 3–8: open `Run Offseason Update.R`, set `run_steps` (default `c(3, 4, 5, 6, 7, 8)`), and source it. If a step fails, fix it and rerun from that step, e.g. `run_steps <- c(6, 7, 8)`.

**When each step can happen:**
- Step 0: right after the tournaments.
- Step 3: any time after that.
- Step 2: needs the draft withdrawal deadline (June) and most of the portal and signing period. In practice, September–October.
- Steps 5–7: need steps 2 and 3.
- Step 6 also needs the conference file from step 1. Without it, teams new to D1 get no prior; the check in step 8 flags it.

## What each scripted step does

**3. Full-season ratings.**
- Fits the finished season's ratings with the current weights (nearest weight row).
- Saves them to `Team Ratings/Full Season/` (what the site shows) and to `Team Ratings/Full Season - Current Weights/` (what the priors train on).
- If you ever change the in-season weights (`single_season_*_wgt.rds`), refit every past season into `Full Season - Current Weights`, so the priors stay on the same scale as the in-season ratings. The quick way: `Rscript _Backtest/priors/fullseason_fast.R MBB "<weights folder>" "<Current Weights folder>"`, and the same for WBB.

**4. Shot models.**
- The shot file pulls the last four finished seasons of play-by-play through hoopR/wehoop and saves `pbp_raw_current.rds` (not committed).
- The preseason model maker trains on it and saves `xfg {season}.model`, `xfg noloc {season}.model` and `ft {season}.rds`.
- It also saves the `.json` copies that script 1 loads, reloads them, and stops if they don't predict exactly what the fitted model does.
- It prints the xgboost version. The GitHub workflow needs the same major version (3.x) to read these models.
- `_Helper Files/Function Scripts/Update xgb models to json.R` is only needed to convert older `.model` files by hand.

**5. Prior models.**
- Re-runs the prior test (`_Backtest/priors/_Test Priors - All Stats.R`) through the finished season. It is a rolling-origin test, same design as the EuroLeagues one.
- It prints old vs new chosen model per stat × side.
- It saves `prior_models_mbb.rds` / `prior_models_wbb.rds` in the Weights folders. The old ones go to `_Offseason Update/backup/<date>/`.
- Set `SAVE <- FALSE` to just look.

**6. Priors.**
- Builds `priors_{season}.rds` / `priors_wbb_{season}.rds` and the in-season starter files from the prior models.
- It prints the top 25 and the teams new to D1.
- No cbbdata login: player stats come from the cleaned recruiting files and the ESPN box scores.

**7. Calibration.**
- Rebuilds honest priors for every past season. Each is fit only on earlier seasons, the same way step 6 does it.
- Walk-forward backtests production as configured, with and without priors.
- Refits `calibration_mbb.rds` / `calibration_wbb.rds` and the `_no_prior` versions. The old ones go to `_Offseason Update/backup/<date>/`.
- It prints each month's scale/spread/totals correction and a season-by-season check.
- Set `RUN_NO_PRIOR <- FALSE` to skip the no-prior half.

**8. Check.**
- Verifies, for both leagues:
  - the conference file and team database;
  - last season's data, ratings, rosters and recruiting files;
  - the shot models: they exist, are `.json`, and their features match what script 1 builds;
  - the weights;
  - the prior models: tested through the finished season;
  - the priors: every D1 team, no NAs, a sane pace intercept;
  - the calibration: includes the finished season;
  - the Results folder;
  - that no daily script still has its own year.
- It prints PASS / WARN / FAIL and errors if anything fails.

## Start of the season
1. **Run step 8.** Fix any FAIL.
2. **Commit and push.** The GitHub Action only sees what's in the repo.
3. **Turn the daily run back on after the first games are played.** Uncomment the `schedule:` block in `.github/workflows/vrgl-pipeline.yml`, then commit and push. Running it before any games exist just fails.

## Every few years (optional)
These re-test the modeling choices rather than refresh data. They live in `_Backtest/`; see its README.
- **In-season weights** (shrinkage and recency decay per component), one change at a time: `Rscript _Backtest/run_tuning.R <MBB|WBB> <pace|ast|oreb|to|rtg>`, then `compare_variants.R`. If the weight files change:
  1. refit `Full Season - Current Weights` (see step 3);
  2. then run steps 5–7.
- **Prior blending** (how fast the prior gives way to this season's games): `_Backtest/compare_priors.R` with the variants in `_Backtest/variants.R`. The blend weights are `with_priors_*_wgt.rds`; both leagues use the VRGL ones.
- **Women's preseason rosters.** The "returning minutes" prior models (R1) helped WBB in the backtest, but they need preseason rosters, which DANTE doesn't pull yet.

## If something goes wrong
- **Logs:** `_Offseason Update/logs/<date-time>/`, one file per step.
- **Backups:** previous prior models and calibration files are in `_Offseason Update/backup/<date>/`. Everything else is in git.
- **A step fails partway:** rerun just that step and the ones after it.
