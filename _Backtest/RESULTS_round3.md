# Backtest results: round 3 (2026-10-23): combined tuning winners

The round 2 single-change winners were combined and tested on the **held-out seasons** (MBB 2024–26, WBB 2025–26). None of those seasons were used to choose the settings.

**Scoring:**
- Every model gets the same walk-forward calibration layer.
- Comparisons are paired against production (V0_raw + calibration) on identical games.
- 95% CIs come from a date-block bootstrap.
- Negative = better.

Script: `compare_combo.R`. Tables: `output/combo_metrics.csv`, `output/combo_paired.csv`.

## Variants
| Variant | Shrinkage (λ = CV λ.min × …) on all five components | Net/level split on efficiency |
|---|---|---|
| V0_raw (production) | 1 + `lambda_adj` from the weight files; varies by season row. 2026 rows: MBB rtg ×2.0, OREB ×1.1, TO ×1.05, pace/AST ×1; WBB rtg ×1.35, TO ×1.1, AST/OREB ×1.05, pace ×1 | no |
| **C1_lam050** | **×0.5** | no |
| C3_lam050_nl4 | ×0.5 | scoring level shrunk 4× harder than net rating |
| C2_lam025 | ×0.25 | no |
| C4_lam025_nl8 | ×0.25 | level 8× harder |

Recency decays are unchanged from production: round 2 showed they are already about right.

## Held-out seasons (the honest test)
| | MBB margin MSE | MBB log loss | MBB total MSE | WBB margin MSE | WBB log loss | WBB total MSE |
|---|---|---|---|---|---|---|
| **C1 ×0.5** | **−1.49** [−2.43, −0.57] | **−0.0033** [−0.0044, −0.0022] | **−2.24** [−2.94, −1.57] | **−3.61** [−5.36, −1.99] | **−0.0040** | **−1.22** |
| C3 ×0.5 + net/level | −0.65 (n.s.) | −0.0026 | −2.05 | −3.24 | −0.0039 | −1.21 |
| C2 ×0.25 | −0.49 (n.s.) | −0.0031 | −2.75 | −3.17 | **−0.0048** | −1.32 |
| C4 ×0.25 + net/level | +0.44 (n.s.) | −0.0021 | −2.75 | −2.60 | −0.0046 | −1.31 |

Levels for C1 vs production:

| | MBB | WBB |
|---|---|---|
| Margin MAE | 9.36 → 9.30 | 10.29 → 10.17 |
| Margin RMSE | 11.90 → 11.84 | 13.06 → 12.92 |
| Log loss | 0.5450 → 0.5417 | 0.4787 → 0.4748 |
| Total MAE | 13.48 → 13.42 | 12.78 → 12.75 |

## Tuning seasons (context only: these chose the settings, and they have no priors)
| | MBB margin MSE | MBB total MSE | WBB margin MSE | WBB total MSE |
|---|---|---|---|---|
| C1 ×0.5 | −2.29 | **+6.05** | −7.56 | **+2.83** |
| C3 ×0.5 + net/level | −1.50 | +0.27 (n.s.) | −7.00 | −1.19 |
| C2 ×0.25 | −3.01 | +12.06 | −10.75 | +6.93 |
| C4 ×0.25 + net/level | −2.23 | +2.69 | −10.25 | +0.92 |

## What this shows
1. **C1 is the only variant that wins on every held-out metric in both leagues.**
   - It improves margins, win probability and totals, all with CIs that exclude zero.
   - Going further to ×0.25 helps win probability a little in WBB, but costs MBB margins.
2. **The totals penalty from less shrinkage only appears in seasons without priors.**
   - In the tuning seasons (no priors), C1 makes totals worse.
   - In the held-out seasons (priors on), C1 makes totals better.
   - The prior anchors each team's scoring level, so the net/level split (C3) has nothing left to protect once priors are on. Production always runs with priors.
3. **The calibration layer now has less to correct.**
   - MBB Jan–Feb margin scale k: 1.23 → 1.09. Nov: 1.70 → 1.46.
   - Production ratings were compressed by over-shrinkage, and the calibration was stretching them back out. C1 removes most of that at the source.
4. **2026 vs Vegas (MBB morning consensus, 4,933 spreads / 4,003 totals):**
   - C1 spread MAE 9.41 vs production 9.46 vs Vegas 9.02.
   - C1 total MAE 14.19 vs 14.30 vs Vegas 13.63.
   - Neither model carries information the line doesn't already have:
     - the correlation of (model − line) with (result − line) is 0.01–0.03, with CIs spanning 0;
     - at a 3+ point edge, sides and totals hit 52.6–53.1% for both models. The break-even at −110 is 52.4%.
   - Better ratings alone won't make the over/under picks profitable. That needs the totals-specific model.

## Recommendation: applied 2026-10-24 (see `PIPELINE_CHANGES.md`, change 2)
1. **Move both leagues to C1.** In each `single_season_*_wgt.rds` weight file (`VRGL/.../Weights`, `BTRC/.../Weights`), set `lambda_adj = -0.5` for all five stats (pace, ast, oreb, to, rtg) in every season row: the backtest used ×0.5 in every season. Keep the decays (`wgt`) as they are.
2. **Swap in the matching calibration.** It must come from the same model's predictions. The four candidate files are built and waiting in `output/candidate_C1/`; copy them over the current `calibration_*.rds` files in the same two Weights folders.
3. **To revert:** restore the old weight files and the old calibration files. Git has both.

`build_calibration.R` now takes `BT_CAL_VARIANT` and `BT_CAL_OUT_DIR`, so any variant's calibration can be built without touching production.

## Next candidates
- **X5 (expected rating + shooting over expected) on top of C1.** Round 2 X5 ran with production's over-shrunk λ, so it hasn't had a fair test.
- **A totals-specific model,** the one route to better over/unders.
- **Priors work.** They clearly matter for totals.
