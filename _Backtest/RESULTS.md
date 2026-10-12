# Backtest results: round 1 (2026-10-11)

**Sample:** walk-forward on every game, men's 2019–2026 and women's 2022–2026, about 68k rated games. Each game is predicted using only data from before its date. Priors are the saved production priors where they exist (MBB 2024–26, WBB 2025–26); earlier seasons have none.

**"Calibrated"** means each season's margin scale, margin σ, total bias and total σ are estimated **from earlier seasons only**. This puts every model on equal footing. The first season of each league is used for history only.

**Paired differences** are on identical games, with 95% intervals from a bootstrap that resamples whole game dates. Negative = better.

## 1. The harness reproduces production
- The replica of `create_ratings()` and the prior blend reproduces the saved `inseason_ratings_all_2026.rds` **exactly** (max diff 0.0000, every component, MBB and WBB).
- The walk-forward 2026 baseline against the saved daily `all_results_*` files: margin correlation 0.997, win-probability correlation 0.995, totals within about 0.5 pts from December on. The 0.5 is the overtime points the sim adds.
- **The November 2026 as-run totals ran about 9 pts lower than today's code produces from the same data.** That's most of the November totals miss. It came from settings that changed after mid-November.

## 2. What the baseline gets wrong (V0 = production, all seasons)
| | MBB | WBB |
|---|---|---|
| Margin MAE / RMSE (as run) | 9.41 / 11.98 | 10.33 / 13.20 |
| Stated **margin** uncertainty, actual / stated (should be 1) | 0.96 | 1.04 |
| Stated **total** uncertainty, actual / stated | **1.36** | **1.29** |
| Home team beat its projection by (non-neutral, calibrated) | **+0.99 pts** | **+0.98 pts** |
| Actual-vs-projected margin slope, Nov → Jan–Feb | 1.22 → 0.87 | 1.15 → 0.86 |

- **Totals:** the sim treats each team's efficiency noise as independent and pace as fixed, so a total's spread is about 30–35% too narrow. That is why the over/under probabilities were overconfident.
- **Margin scaling:** the fixed ×1.32 stretch is too small in November, when priors and ridge shrinkage stack, and too large by January.

## 3. Model experiments (calibrated, paired vs V0)
| Variant | What changed | MBB margin MSE Δ [95% CI] | WBB margin MSE Δ | Verdict |
|---|---|---|---|---|
| V1_sitcomp | home/neutral/rest/travel added to the pace, ast, oreb and TO models | +0.91 [+0.63, +1.19] | +1.31 [+0.86, +1.76] | Removes the home bias, but is **less** accurate overall |
| D0_direct | direct efficiency model (team O/D + pace + situational) | +1.01 [+0.52, +1.52] | −0.25 [−1.51, +0.95] | No better; log loss worse in both leagues |
| V2 / D1 | unstandardized ridge, λ.min, folds by game, no stretch | unstable | unstable | Numerically unstable in my implementation, so **no conclusion** about the idea. With no blow-ups (WBB Dec–Feb) they were still worse: less shrinkage hurts |

**So the component architecture isn't the problem.** Multicollinearity is negligible (game-level VIFs about 1.0–1.1). The direct model doesn't beat the component pathway; the stat-specific decay and shrinkage appear to add a little.

## 4. Calibration layer on production's own predictions (no refit)
Phase-specific margin scale and σ, separate total σ, and a home-court correction, all estimated from earlier seasons:

| | MBB | WBB |
|---|---|---|
| Margin RMSE | 11.98 → **11.86** | 13.26 → **13.14** |
| Margin MAE | 9.41 → **9.33** | 10.36 → **10.28** |
| Margin MSE Δ [95% CI] | **−2.77 [−3.57, −1.93]** | **−3.28 [−4.77, −1.69]** |
| Log loss Δ | −0.0024 [−0.0034, −0.0013] | +0.0000 (n.s.) |
| Total uncertainty, actual / stated | 1.36 → **1.00** | 1.28 → **0.99** |
| Total MSE Δ | −1.68 [−2.08, −1.27] | −0.25 (n.s.) |

This is the clearest win available now. It replaces the hard-coded ×1.32 stretch and σ = 13 with values fitted from history.

## 5. Against Vegas (MBB 2026, morning consensus lines)
- **Spreads** (4,933 games): model MAE 9.57 vs Vegas 9.02.
- **Totals** (4,003 games): model MAE 14.28 vs Vegas 13.63.
- Where the model disagrees with the line, the disagreement has essentially no predictive value (correlation 0.01 for spreads, 0.03 for totals).
- **Over/under probabilities are still wrong after calibration.** The calibrated model says its picks hit 59.5% of the time; they actually hit 51.1%. Calibration fixes the model's uncertainty about the *final score*, but the market total is more accurate than the model's, so the probability of beating the *line* is close to 50%. **The model shouldn't be used for totals betting** until the total means themselves improve.

## 6. Next steps (proposed)
1. **Production-ready calibration layer:**
   - port `R/09_postcal.R` logic into the daily pipeline;
   - replace the stretch and `disp_Rtg` with phase-specific scale and σ, plus a separate σ for totals;
   - add a home-court correction;
   - add the expected-overtime allowance.
2. **Single-change ablations, tuned on 2019–2023 and validated on 2024–2026** to avoid in-sample flattering: λ multiplier grid, folds by game alone, decay grid per stat.
3. **A totals-specific scoring model:** pace with situational terms and its own variance, correlated scoring noise between the two teams, team-level points rather than efficiency × fixed pace.
4. **Re-attempt shrinkage toward the prior** (prior as the ridge offset) once priors are back in scope.
