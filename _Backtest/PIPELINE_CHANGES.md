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
  1. add the finished season to the backtest (`Rscript _Backtest/run_variant.R V0_raw MBB` and `… WBB`);
  2. rebuild the calibration (`Rscript _Backtest/build_calibration.R`);
  3. commit the new calibration files.

## Tested
- **Against the backtest's own predictions:** engine output matches the analytic calibrated predictions to within 0.15–0.27 pts (simulation noise and rounding), with the target margin and total sds.
- **End to end:** ran through the production `simulate_ncaa_wk` / `simulate_round_wk` functions on a full 2026 game day.
- **Tournament path:** checked with tournament-style inputs (no `is_neutral` or `game_date` columns); output columns are unchanged.
- **Not tested here:** the hoopR schedule download and the odds pull (no network to those services from my workspace). Those parts of the scripts are untouched.
