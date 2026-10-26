# Calibrated game engine: what changed in the pipeline (2026-10-11)

## Summary
The daily and tournament sims now use a **calibrated engine** in place of the old ×1.3 margin stretch and single `disp_Rtg`. The ratings themselves (`2. In-Season Ratings`) are **unchanged**.

| Before | After |
|---|---|
| margin stretched ×1.32 all season | margin scale `k` by phase (Nov / Dec / Jan–Feb / Mar–Apr), fitted on past seasons |
| one noise σ (`disp_Rtg = 13`, independent per team) | separate margin σ and total σ by phase; the two teams' noise is correlated, so totals get their real spread (about 35% wider than before) |
| home-court effect leaked through the component stats | `hfa` correction added for non-neutral home teams |
| ties → OT points added on top | ties → OT winner drawn from the expected margin; totals already include OT on average |

Backtest effect, 2020–2026 (men's) and 2023–2026 (women's), walk-forward:
- **Margin RMSE:** 11.98 → 11.86 (men's), 13.26 → 13.14 (women's).
- **Win-probability log loss:** improves for men's; about unchanged for women's.
- **Totals:** stated uncertainty now matches reality (it was 26–36% too small).
- See `RESULTS.md`.

## Files added
| File | Purpose |
|---|---|
| `_Helper Files/Function Scripts/process_games_wk_calibrated.R` | Source of the engine. Running it re-creates the two `.rds` files below |
| `_Helper Files/Simulation Functions/process_games_wk_calibrated.rds` | Daily/weekly sims (`process_games_wk_{sim_method}.rds`) |
| `_Helper Files/Simulation Functions/process_games_calibrated.rds` | Tournament sims (`process_games_{sim_method}.rds`), same function |
| `VRGL/Stats/Team and Player Stats/Power Ratings/Weights/calibration_mbb.rds` | MBB calibration (fit on 2024–2026, the seasons with priors) |
| `VRGL/…/Weights/calibration_mbb_no_prior.rds` | For the no-prior daily sims (fit on 2019–2023) |
| `BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Weights/calibration_wbb.rds` | WBB (fit on 2025–2026) |
| `BTRC/…/Weights/calibration_wbb_no_prior.rds` | WBB no-prior (fit on 2022–2024) |

## Files edited (small, reversible)
- `VRGL/4. Run Simulation - MBB.R`:
  - `sim_method <- "calibrated"`;
  - `calibration = readRDS(...)` added to both ratings lists;
  - `disp_Rtg` set back to 13, the regular-season value (only used if you revert).
- `BTRC/4. Run Simulation - WBB.R`: same, with the WBB files.
- `VRGL/_March Madness Simulation.R` and `BTRC/_March Madness Simulation - WBB.R`: `sim_method <- "calibrated"`, plus `calibration` in the ratings list.

**To revert:** set `sim_method` back to `"to_oreb_ast"` (daily) or `"to_reb_ast"` (tournament). Nothing else needs undoing.

## Calibration values (2026-10-11 build)
MBB, with priors (home-court correction +0.43 pts):

| phase | k | sd margin | total bias | sd total |
|---|---|---|---|---|
| Nov | 1.70 | 13.3 | +1.9 | 18.3 |
| Dec | 1.52 | 12.0 | +1.8 | 16.8 |
| Jan–Feb | 1.23 | 11.3 | +1.4 | 16.8 |
| Mar–Apr | 1.23 | 11.2 | +1.7 | 16.6 |

WBB, with priors (home-court correction +0.52 pts):

| phase | k | sd margin | total bias | sd total |
|---|---|---|---|---|
| Nov | 1.52 | 15.6 | −0.3 | 17.0 |
| Dec | 1.42 | 13.2 | +0.7 | 15.9 |
| Jan–Feb | 1.21 | 11.9 | +0.5 | 15.9 |
| Mar–Apr | 1.19 | 11.8 | +1.2 | 15.6 |

## Before the first run
- **Commit and push** the new `.rds` and `.R` files. The GitHub Actions workflow runs from the repo, so it only sees committed files.
- **Each offseason:**
  1. add the finished season to the backtest (`Rscript _Backtest/run_variant.R C1_lam050 MBB` and `… WBB`; V0_raw before change 2);
  2. rebuild the calibration (`Rscript _Backtest/build_calibration.R`);
  3. commit the new calibration files.

## Tested
- **Against the backtest's own predictions:** engine output matches the analytic calibrated predictions to within 0.15–0.27 pts (simulation noise and rounding), with the target margin and total sds.
- **End to end:** ran through the production `simulate_ncaa_wk` / `simulate_round_wk` functions on a full 2026 game day.
- **Tournament path:** checked with tournament-style inputs (no `is_neutral` or `game_date` columns); output columns are unchanged.
- **Not tested here:** the hoopR schedule download and the odds pull (no network to those services from my workspace). Those parts of the scripts are untouched.

## Change 2 (2026-10-24): less shrinkage on the ratings (C1)
Tested in `RESULTS_round3.md`: better margins, win probability and totals on the held-out seasons in both leagues.

**Weight files.** In the `single_season_*_wgt.rds` files for pace, ast, oreb, to and rtg, `lambda_adj` is now −0.5 in every season row, for both leagues. That means λ = CV λ.min × 0.5. The decays (`wgt`) are unchanged, and `efg_adj` is untouched.

| | Before (2024 / 2025 rows) | After |
|---|---|---|
| MBB rtg | 0.75 / 1.00 | −0.5 |
| MBB to | 0.05 / 0.05 | −0.5 |
| MBB oreb | 0 / 0.1 | −0.5 |
| MBB ast, pace | 0 / 0 | −0.5 |
| WBB rtg | 0.50 / 0.35 | −0.5 |
| WBB to | 0 / 0.1 | −0.5 |
| WBB oreb, ast | 0 / 0.05 | −0.5 |
| WBB pace | 0.1 / 0 | −0.5 |

**Calibration files.** All four `calibration_*.rds` files were rebuilt from the C1 predictions:
```
BT_CAL_VARIANT=C1_lam050 Rscript _Backtest/build_calibration.R
```
MBB Jan–Feb margin scale k went from 1.23 to 1.09: the ratings are less compressed, so there is less to stretch.

**Things to know:**
- **Offseason scripts.** `Full-Season Ratings - All.R` (both leagues) reads the same weight files, so a rerun will also use ×0.5. That wasn't backtested.
- **Weight-tuning scripts.** The `_Testing/Power Rating Weights` scripts rewrite these files. If you rerun them, they will put back RMSE-tuned values.
- **To revert:** copies of all the previous files are in `_Backtest/output/prod_before_C1/`, and git has them too.
- **Commit and push.** The GitHub Actions run only sees committed files.

**Offseason refresh.** The steps above still apply, but with C1: run `run_variant.R C1_lam050 MBB`/`WBB`, then `build_calibration.R`. Its default is now C1_lam050.

## Change 3 (2026-10-24): new priors and the 2027 rollover
Tested in `RESULTS_round4.md`.

**Season rollover:**
- **Season year.** Every daily and tournament script (1–4 plus March Madness, both leagues) and both preseason table scripts now use 2027.
- **First run of a season.** Script 1 starts from nothing when no `pbp_{year}.rds` exists yet.
- **Results folders.** `Results/2027/` was added for the sims.
- **Shot models.** The refresh scripts now use 2026 for the shot file, and 2027 for the preseason models and json conversion. They still have to be run (see the bottom of this section).
- **Full-season ratings scripts:**
  - they pick the nearest weight row (they used to stop when no row matched the season exactly);
  - they now use 2026;
  - each run also saves to `Full Season - Current Weights`.
- **2026 full-season ratings** are saved, fitted with the production function and the C1 weights.
- **Conference script.** It now skips the Pac-12 only for 2025–26, so the new Pac-12 is included for 2027.

**Priors:**
- **New scripts, same names.**
  - `4. Create Priors - MBB.R` and `_Create Priors (Simple) - WBB.R` now use `_Helper Files/Function Scripts/prior_functions.R`.
  - They read the models from `prior_models_mbb.rds` / `prior_models_wbb.rds` (Weights folders).
  - They train on `Full Season - Current Weights`. That folder holds every season refit with the current weights, so the scale is consistent. The `Full Season` folder still holds each year's published ratings for the site.
  - They need no cbbdata login.
  - The old scripts are in git and in `_Backtest/output/prod_before_priors/`.
- **2027 priors** (`priors_2027.rds`, `priors_wbb_2027.rds`) and the in-season starters are built. They are provisional: they use the 2026 team list.
- **Calibration.** `calibration_mbb.rds` / `calibration_wbb.rds` were rebuilt from the new-prior backtest:
  ```
  BT_CAL_VARIANT=P_C1_pch BT_CAL_PRIOR_FROM="MBB=2022,WBB=2023" build_calibration.R
  ```
  Nov MBB k went from 1.46 to 1.43, and the total correction from +1.8 to +0.1. The no-prior files are unchanged. The previous files are in `_Backtest/output/prod_before_priors/`.

**Still to run on your machine:**
1. **Shot models, both leagues:** `_Create Shot File for Models`, then `_Model Maker - All Models (Preseason)`, then `Update xgb models to json.R`.
2. **Conference files:** `_Get CBB Conferences.R` with `season <- 2027`. Then rerun both prior scripts so teams new to D1 get a prior.
3. **At season start:** re-enable the schedule in `vrgl-pipeline.yml`, then commit and push.

**Proposed, not applied:** efficiency prior_wgt 0.97 → 0.90 in `with_priors_rtg_wgt.rds` (shared by both leagues), together with the calibration in `output/candidate_Q_rtg90_pch/`.
