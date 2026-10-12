# Backtest results: round 2 (2026-10-11)
Covers: (1) the calibrated engine now in the pipeline, (2) one-change-at-a-time tuning so far, (3) the expected-rating + team-shooting model.

## 1. Calibration layer: now in the pipeline
See `PIPELINE_CHANGES.md` for files, values and how to revert.
- **Fitted on the right seasons.** Calibration fitted only on prior-era seasons (MBB 2024–26, WBB 2025–26) validated better than fitting on all seasons:
  - MBB 2025–26: margin MSE −3.38, log loss −0.0034;
  - WBB 2026: margin MSE −2.66.
- **No-prior calibration.** The no-prior daily sims get their own calibration, fitted on the seasons that had no priors.
- **Engine checked against the backtest.** Simulated output matches the analytic predictions to within 0.15–0.27 pts. It also runs end to end through the production `simulate_ncaa_wk` functions and the tournament code path.

## 2. One-change-at-a-time tuning (`run_tuning.R`)
**Setup:**
- Tuned on MBB 2019–2023 and WBB 2022–2024; the later seasons are held out for validation.
- Each config changes **one** setting of **one** component; everything else stays at production.
- Configs are scored after the walk-forward calibration layer, paired against production on identical games. 95% CIs come from a date-block bootstrap. Negative = better.

**Efficiency model (`rtg`), the one that matters most for margins:**

| Change | MBB margin MSE | MBB log loss | MBB total MSE | WBB margin MSE | WBB log loss | WBB total MSE |
|---|---|---|---|---|---|---|
| λ ×0.5 | **−1.25** [−1.89, −0.61] | −0.0004 (n.s.) | +6.46 | **−5.71** [−7.56, −3.97] | **−0.0044** | +3.64 |
| λ ×1.0 (CV λ.min) | **−0.84** [−1.15, −0.50] | **−0.0007** | +2.45 | **−2.52** [−3.29, −1.79] | **−0.0022** | +1.20 |
| λ ×2 / ×3 | worse | worse | better (−0.4 / −1.3) | worse | worse | better |
| decay 0.92 / 0.95 | worse | worse | ~0 | worse | worse | ~0 |
| decay 0.99 / 1.00 | ~0 | ~0 | slightly worse | ~0 | ~0 | slightly worse |
| CV folds by game | ~0 | ~0 | ~0 | −0.33 (small, real) | −0.0002 | −0.08 |

(Production: MBB decay 0.98 with λ ×1.75; WBB decay 0.98 with λ ×1.35.)

**What this shows:**
- **The efficiency ratings are over-shrunk for predicting margins and win probability, in both leagues.** The current λ was tuned on team-game efficiency RMSE, which mixes margin and total accuracy.
- **Shrinkage pulls margins and totals in opposite directions.** The fix is to shrink a team's **net rating** (offense − defense → margins) and its **scoring level** (offense + defense → totals) separately. That's the next experiment.
- **The decay is already about right.**

**Pace (affects totals only):**

| Change | MBB total MSE | WBB total MSE |
|---|---|---|
| λ ×0.5 | **−1.07** [−1.54, −0.68] | **−1.13** [−2.02, −0.23] |
| slower decay (+0.01) | ~0 | **−0.29** |
| more shrinkage | much worse | much worse |

**Turnovers, offensive rebounds, assists** (margin MSE / log loss ×1000; totals ~unchanged for all three):

| Change | MBB TO | MBB OREB | MBB AST | WBB TO | WBB OREB | WBB AST |
|---|---|---|---|---|---|---|
| λ ×0.5 | **−0.23 / −0.48** | **−0.40 / −0.67** | **−0.13 / −0.18** | **−0.71 / −0.83** | **−0.78 / −1.07** | **−0.20 / −0.25** |
| more shrinkage (×1.5–3) | worse, steadily | worse | worse | worse | worse | worse |
| faster decay | slightly worse | slightly worse | slightly worse | ~0 | ~0 margin; WBB OREB 0.96 improves totals (−0.15) | ~0 |
| slower decay | ~0 | ~0 | ~0 | ~0 | slightly worse | ~0 |
| CV folds by game | ~0 | ~0 | ~0 | ~0 | ~0 | ~0 |

All of the λ ×0.5 gains have 95% CIs that exclude zero.

(Production: MBB TO 0.97 with λ ×1.05, OREB 0.98 ×1, AST 0.98 ×1; WBB TO 0.95 ×1, OREB 0.99 ×1, AST 0.98 ×1.)

**What the full grid shows, one change at a time:**
1. **Every component is over-shrunk for margins and win probability.** Each one improves at λ ×0.5, and the efficiency model improves the most. λ ×0.5 is the edge of the grid, so the optimum may be lower still.
2. **Totals respond to shrinkage only through efficiency and pace,** and in opposite directions: efficiency wants more shrinkage for totals, pace wants less.
3. **Production recency decays are already about right everywhere.**
4. **Grouping CV folds by game makes no practical difference.**

**Not yet done:** combining the winners and validating them on the held-out seasons (MBB 2024–26, WBB 2025–26), so **no production weights have been changed yet**.

**Proposed combined test:**
- all components at λ ×0.5;
- also try ×0.25, since ×0.5 is the edge of the grid;
- separate net/level shrinkage for efficiency to protect totals;
- pace at λ ×0.5;
- re-run the calibration layer on top.

## 3. Expected rating + team shooting (`model_xshoot`)
**Model:**
- The component model is unchanged, but the efficiency part is fit to shot-quality expected rating (`x_rtg`).
- Added to that: a team-specific **shooting-over-expected** number (offense and defense), the difference (actual − expected) points per 100. It's a separate ridge with its own recency decay and shrinkage, so it updates through the season as teams shoot above or below expectation.
- Both sides use the same calibration layer, so differences come from the model alone.

| Variant | Shooting decay / λ | Rows without `x_rtg` | MBB margin MSE (all) | MBB log loss | WBB margin MSE (all) | WBB log loss |
|---|---|---|---|---|---|---|
| X1 | 0.99 / ×2 | dropped | +2.89 | +0.0042 | +1.47 | ±0 |
| X2 | 0.97 / ×2 | dropped | +2.96 | +0.0043 | +1.52 | ±0 |
| X3 | 0.99 / ×4 (heavy) | dropped | +5.12 | +0.0089 | +4.16 | +0.0034 |
| X4 | 0.99 / ×2 | kept | +1.10 | +0.0012 (n.s.) | +1.19 (n.s.) | −0.0008 (n.s.) |
| **X5** | **0.99 / ×1** | **kept** | +0.61 [+0.02, +1.18] | +0.0004 (n.s.) | **−0.52** (n.s.) | **−0.0021** [−0.0036, −0.0006] |
| X6 | 1.00 / ×0.5 | kept | +1.66 | +0.0024 | −0.84 (n.s.) | ±0, totals worse |

**What this shows:**
- **Missing expected-rating data was most of X1's early deficit.** 10–14% of 2018–23 games have no `x_rtg`. Dropping those games cost teams real data. Keeping them (actual rating stands in for those games) fixes most of it.
- **Shooting over expected is persistent.** Heavy shrinkage (X3) hurts, and λ around CV λ.min works best (X5).
- **Women's (X5):**
  - log loss: better overall, and clearly better in seasons with priors (−0.0035);
  - margins: equal to production;
  - totals: equal overall, but worse with priors (+3.5).
- **Men's (X5):** no gain. Margins slightly worse, log loss equal.
- **Not yet promoted.** Two things are confounded in this round: the expected-rating part still uses production's over-shrunk λ, and its prior is borrowed from the actual-rating prior.

**Next for this model:**
1. Re-test X5 with the efficiency λ lowered (per section 2) and with net/level shrinkage.
2. Split shooting over expected by shot type (3pt vs 2pt vs FT), where team shooting skill is most persistent.
3. Build an `x_rtg` / shooting prior once priors are back in scope.
