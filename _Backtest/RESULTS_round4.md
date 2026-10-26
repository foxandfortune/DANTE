# Backtest results: round 4 (2026-10-24): priors

Two questions:
1. How should the preseason priors be created?
2. How should they be blended into the in-season ratings?

Everything is tested for both leagues on seasons the models never saw. All runs use the C1 ratings model, now in production.

## Step 1: creating the priors (`priors/_Test Priors - All Stats.R`)

**Design** (the same as the EuroLeagues test):
- **Rolling origin.** For each season S, fit on seasons before S, predict each team's full-season rating in S, and score the RMSE.
- **Scope.** Seven stats × offense/defense. Test seasons: MBB 2021–26, WBB 2023–26.
- **Baselines:** B0 (league average) and B1 (last season unchanged). P0 is the current method: last season plus a team fixed effect.
- **Parsimony.** The simplest model within 1% of the best RMSE wins.
- **Consistent targets.** Every season's full-season ratings were refit with the current (C1) weights, so the targets are on the same scale as the in-season ratings.

**Features** (all known before tip-off):
- last season and two seasons ago, same side;
- the other side's rating;
- the conference-mates' average;
- for MBB:
  - departures (draft entrants, transfers out, seniors) as a share of minutes and minutes-weighted OBPM/DBPM. Players on those lists who came back are removed.
  - incoming transfers (minutes, BPM, 247 rating);
  - recruits (sum of 247 percentiles).

**Missing data handled with flags,** not special cases:
- **`new_d1`.** Teams with no last-season rating get 0 and a flag, and the model learns how far below average they start.
- **`no_247`.** The 21 schools with no 247 page get 0 for commits and incoming transfers and a flag.

These replace the old 0.8× / 0.2× patches.

**Prior RMSE, current method → chosen model** (lower is better; gain = share of error removed):

| Stat | MBB off | MBB def | WBB off | WBB def |
|---|---|---|---|---|
| Efficiency (rtg) | 4.42 → **4.04** (9%) | 3.97 → **3.64** (8%) | 5.06 → **4.51** (11%) | 4.01 → **3.51** (12%) |
| Raw rating (site) | 5.56 → **4.90** (12%) | 5.15 → **4.58** (11%) | 6.97 → **6.20** (11%) | 5.93 → **5.14** (13%) |
| Pace | 2.33 → **2.09** (10%) | 2.32 → **2.09** (10%) | 2.44 → **2.13** (13%) | 2.44 → **2.12** (13%) |
| Turnovers | 0.0187 → **0.0164** | 0.0194 → **0.0173** | 0.0259 → **0.0232** | 0.0284 → **0.0251** |
| Off. rebounds | 0.0373 → **0.0338** | 0.0271 → **0.0249** | 0.0397 → **0.0357** | 0.0318 → **0.0287** |
| Assists | 0.0443 → **0.0407** | 0.0423 → **0.0389** | 0.0481 → **0.0430** | 0.0413 → **0.0367** |

**What this shows:**
- **Every stat and side improves, by 7–13%.**
- **The team fixed effect is the problem.** With 3–8 seasons per team, `factor(team_id)` overfits. For WBB, the current method is no better than carrying last season forward unchanged.
- **Recruiting data matters for efficiency, not much else.** For MBB efficiency and raw rating, the winners use departures, transfers and recruits. Pace, turnovers, rebounding and assists are best predicted from the ratings alone (two seasons plus conference).
- **WBB** (no recruiting data) is best served by two seasons plus the conference or the other side.
- **Preseason rosters.** "Returning minutes" (`ret_min_box`, models R1/R2) helps WBB a little and MBB hardly at all. It needs preseason rosters in production. In the backtest the season's box scores stand in, so it's slightly optimistic.

The models are saved in `prior_models_mbb.rds` / `prior_models_wbb.rds` (Weights folders).

## Step 2: game level (`compare_priors.R`)

**Setup:**
- Walk-forward, with each run's own calibration fitted on earlier seasons.
- Scored seasons: MBB 2022–26 (27–28k games), WBB 2024–26 (16k games).
- Negative = better; 95% CIs from a date-block bootstrap.

| | MBB margin MSE | MBB log loss | MBB total MSE | WBB margin MSE | WBB log loss | WBB total MSE |
|---|---|---|---|---|---|---|
| New priors vs current method (rebuilt for the same seasons) | **−3.7** | **−0.0074** | **−1.8** | **−26.9** | **−0.028** | **−2.0** |
| New priors vs the priors production actually used (MBB 2024–26, WBB 2025–26) | **−6.8** | **−0.0088** | **−6.7** | **−10.4** | **−0.0094** | **−3.2** |
| No priors vs new priors | +8.5 | +0.011 | +8.8 | +12.9 | +0.004 (n.s.) | +11.6 |
| New + preseason-roster models vs new | ~0 | ~0 | ~0 | **−3.6** | **−0.0063** | **−0.4** |

**The gain is concentrated early in the season:**

| | November margin RMSE | December margin RMSE |
|---|---|---|
| MBB, production priors → new | 13.08 → 12.43 | 12.09 → 11.59 |
| WBB, production priors → new | 15.25 → 14.24 | 13.25 → 12.79 |

**Bug found in the production 2026 MBB prior.** Its pace intercept was 59.2 possessions; it should have been about 69.7. It is in both `priors_2026.rds` and `priors_2026 (original).rds`. With the new priors, the November totals correction the calibration has to apply falls from +1.8 to +0.1 points.

### Blending (all with the new priors; vs the production blend curve)

| Change | MBB margin MSE | MBB log loss | MBB total MSE | WBB margin MSE | WBB log loss | WBB total MSE |
|---|---|---|---|---|---|---|
| Shrink toward the prior inside the ridge (×0.5 / ×1 / ×2 / ×4 λ) | +2.6 / +1.3 / +0.4 / +0.6 | worse or ~0 | +10.2 / +4.8 / +1.9 / +3.2 | −2.0 / −2.9 / −2.7 / −0.7 | better | +10.6 / +5.2 / +1.9 / +2.2 |
| All components slower (prior_wgt 0.99) | +2.0 | +0.0040 | +8.0 | +7.2 | +0.0108 | +6.5 |
| All components prior floor 0.30 | +1.3 | +0.0030 | +2.9 | +4.3 | +0.0073 | +1.8 |
| All components prior floor 0 | −0.15 | −0.0005 | +0.35 | −1.0 | −0.0021 | +0.75 |
| **Efficiency only: prior_wgt 0.97 → 0.90** | **−0.32** | **−0.0009** | +0.25 (n.s.) | **−2.1** | **−0.0046** | +0.67 |
| Efficiency only: prior_wgt 0.85 | +0.06 | −0.0004 | +0.83 | −2.3 | −0.0057 | +1.5 |
| Efficiency only: floor 0.15 → 0 | ~0 | ~0 | +0.32 | −0.6 | −0.0015 | +0.58 |

**What this shows:**
- **Keep the production blending curve.** Shrinking toward the prior inside the ridge costs totals in both leagues, so there's no case for the extra complexity.
- **Most curves are already about right.** The one worthwhile change is a faster efficiency curve: prior_wgt 0.90 instead of 0.97.
  - It is a clear win for WBB, at a small totals cost.
  - It is a small win for MBB.
  - WBB uses the men's prior-blend weights, so one change to `with_priors_rtg_wgt.rds` covers both leagues.

## What is in production now (see `PIPELINE_CHANGES.md`, change 3)

**New prior scripts, same names:**
- `4. Create Priors - MBB.R`;
- `_Create Priors (Simple) - WBB.R`;
- shared functions in `_Helper Files/Function Scripts/prior_functions.R`.

They need no cbbdata login: player stats come from the recruiting files and the ESPN box scores.

**2027 priors** (`priors_2027.rds`, `priors_wbb_2027.rds`) and the in-season starter files. They are **provisional**: they use the 2026 team list until the 2027 conference files exist.

**Calibration** rebuilt from the new-prior backtest (MBB 2022–26, WBB 2023–26). The no-prior calibration is unchanged.

## Proposed, not applied
**Efficiency prior_wgt 0.97 → 0.90** in `with_priors_rtg_wgt.rds`. It comes with its own calibration (`output/candidate_Q_rtg90_pch/`), and the two must be applied together.

## Next
- **2027 conference files.** Run `_Get CBB Conferences.R` with `season <- 2027`, then rerun both prior scripts. That brings in teams new to D1 and the new Pac-12.
- **2026 transfer file.** `transfers_clean_2026.rds` has no `percentile` / `class` columns, and unrated transfers' ratings are still NA. The prior functions rebuild the percentile. Script 3's transfer section is worth a look.
- **WBB preseason rosters.** Pulling them from ESPN would let the roster models (R1) run in production.
