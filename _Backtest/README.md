# DANTE `_Backtest/`: walk-forward test bench for the power ratings

This folder is a self-contained experiment area. It reads the existing VRGL and BTRC data but **never writes outside `_Backtest/`**, so production scripts, ratings and sims are untouched.

## What it does
For every game date in a season, the harness fits ratings on games **strictly before** that date and predicts that date's games. That matches what the daily pipeline knows at 6 am. For each game it records:

- predicted margin, home and away points, total, and pace;
- win probability, from the model's stated margin uncertainty;
- the stated uncertainty of the margin and of the total.

These are scored against outcomes:
- margin MAE / RMSE / bias / slope;
- log loss and Brier;
- total and team-points error;
- calibration of the stated uncertainty (`z_*_sd` should be about 1).

## Files
| File | Purpose |
|---|---|
| `R/00_config.R` | Paths (same layout as the repo), seasons, production specs |
| `R/01_data.R` | Loads `poss_stats_with_types_*`, priors, and weight files exactly as `2. In-Season Ratings` does |
| `R/02_prod_replica.R` | Line-for-line copy of `create_ratings()` plus the prior blend. **Reproduces the saved `inseason_ratings_all_2026.rds` exactly (max diff 0.0000), MBB and WBB** |
| `R/03_fast_engine.R` | Same model on a sparse matrix (about 50× faster; predictions within 0.04 pts/100 of the dense replica). Experimental switches: `standardize`, `fold_by_game`, `lambda_mult`, prior offsets |
| `R/04_models.R` | Model specs: `model_components()` (production pathway and its fixes), `model_direct()` (direct efficiency model), `model_xshoot()` (expected rating + team shooting over expected) |
| `R/05_harness.R` | Walk-forward loop (`run_season`, `run_model`) |
| `R/06_metrics.R` | Scoring, paired date-block bootstrap comparisons, calibration tables |
| `R/07_saved.R` | Loads the saved daily projections (`all_results_*`) and Vegas lines (`all_bets_*`) |
| `R/08_report.R` | Cross-season walk-forward calibration and helpers |
| `R/09_postcal.R` | Calibration layer: `fit_calibration()` (phase scale/sd, total bias/sd, home-court) and walk-forward `calibrate_layers()` |
| `R/10_tuning.R` | One-change-at-a-time tuning engine (shares non-target component fits across configs) |
| `build_calibration.R` | Builds the production `calibration_*.rds` files from `pred_V0_raw_*` (or another variant via `BT_CAL_VARIANT`; `BT_CAL_OUT_DIR` writes elsewhere) |
| `run_tuning.R` | `Rscript _Backtest/run_tuning.R <MBB/WBB> <pace/ast/oreb/to/rtg>` → `output/tune_eval_*.csv` |
| `compare_xshoot.R` | Expected-rating + shooting models vs production |
| `compare_combo.R` | Combined tuning winners (C1–C4) vs production, held-out vs tuning seasons, plus Vegas 2026 |
| `PIPELINE_CHANGES.md` | What changed in the production sims and how to revert |
| `RESULTS_round2.md` | Calibration, tuning and expected-rating results |
| `RESULTS_round3.md` | Combined tuning winners on the held-out seasons, and the recommended production change |
| `RESULTS_round4.md` | Priors: how they're created (per-stat test) and how they're blended (game level) |
| `priors/` | Prior test bench: `_Test Priors - All Stats.R` (EuroLeagues-style rolling-origin test), `_Build Priors.R` (priors for any season from the chosen models), `prior_data.R`, `fullseason_fast.R` |
| `compare_priors.R` | Game-level comparison of prior methods and blending variants (round 4) |
| `variants.R` | The experiment definitions |
| `run_variant.R` | `Rscript _Backtest/run_variant.R <variant> <MBB/WBB>` → `output/pred_<variant>_<league>.rds` |
| `compare_variants.R` | `Rscript _Backtest/compare_variants.R` → comparison tables and CSVs in `output/` |

## Yearly use
The routine offseason refresh (prior models, priors, calibration) is scripted in `_Offseason Update/`; see its README. The year comes from `_Helper Files/season.R`, and `bt_seasons` runs through `dante_completed`. This folder is for re-testing modeling choices.

## Running it (from the DANTE repo root, as with the production scripts)
```r
# one-time: install.packages(c("glmnet", "fastDummies", "dplyr", "tidyr", "purrr", "stringr", "glue", "Matrix"))
# in a terminal (PowerShell or the RStudio terminal), from the DANTE folder:
Rscript _Backtest/run_variant.R V0_prod MBB
Rscript _Backtest/run_variant.R V0_prod WBB
Rscript _Backtest/compare_variants.R
```
On Windows the seasons run one after another (no forking). Expect roughly 10–20 minutes per variant per league.

## Ground rules baked in
- **No look-ahead.** Training data is always `game_date < test date`. Situational inputs (rest, travel, home/neutral) are known before tip.
- **Same games for every model.** Comparisons use only games that every variant predicted.
- **Honest calibration.** In "calibrated" results, each season's margin scale, margin sd, total bias and total sd are estimated **only from earlier seasons**. The first season of each league is used for history only.
- **Uncertainty.** Paired differences come with 95% intervals from a bootstrap that resamples whole game dates.
- **Priors.** `priors = "prod"` uses the saved production priors where they exist (MBB 2024–2026, WBB 2025–2026). Earlier seasons run without priors.
- **Lambda caching.** Lambda is re-chosen by CV every 7 days, identically for every model. Daily CV changes predictions by under 0.05 pts.
- **In-sample caveat.** The production decay and lambda weights were tuned on 2024 and 2025, so those seasons flatter the baseline slightly.
