# Experiment definitions (one change at a time where possible)
variants <- list(
  # production as run (component pathway, glmnet defaults, lambda x(1+adj), x1.32 stretch)
  V0_prod     = model_components(label = "V0_prod"),
  # + home/neutral/rest/travel in the pace/ast/oreb/to models (HFA leak fix)
  V1_sitcomp  = model_components(situational_components = TRUE, label = "V1_sitcomp"),
  # + ridge fixes: no column standardization, CV folds by game, lambda.min, no stretch
  V2_ridgefix = model_components(situational_components = TRUE, standardize = FALSE,
                                 fold_by_game = TRUE, lambda_mult = 1, stretch = 1,
                                 label = "V2_ridgefix"),
  # direct efficiency model with production ridge settings and production-like stretch
  D0_direct   = model_direct(stretch = 1.3, label = "D0_direct"),
  # direct model + ridge fixes, situational pace, no stretch
  D1_directfix = model_direct(standardize = FALSE, fold_by_game = TRUE, lambda_mult = 1,
                              situational_pace = TRUE, stretch = 1, label = "D1_directfix")
)
# production fits with no margin stretch: base for the calibration layer
variants$V0_raw <- model_components(stretch = 1, label = "V0_raw")
# expected rating + team shooting-over-expected (soe decay / shrinkage variants)
variants$X1_xshoot   <- model_xshoot(soe_wgt = 0.99, soe_lambda_mult = 2, label = "X1_xshoot")
variants$X2_xshoot_fast <- model_xshoot(soe_wgt = 0.97, soe_lambda_mult = 2, label = "X2_xshoot_fast")
variants$X3_xshoot_heavy <- model_xshoot(soe_wgt = 0.99, soe_lambda_mult = 4, label = "X3_xshoot_heavy")
variants$X4_xshoot_impute <- model_xshoot(soe_wgt = 0.99, soe_lambda_mult = 2, impute_missing = TRUE, label = "X4_xshoot_impute")
variants$X5_impute_light <- model_xshoot(soe_wgt = 0.99, soe_lambda_mult = 1, impute_missing = TRUE, label = "X5_impute_light")
variants$X6_impute_vlight <- model_xshoot(soe_wgt = 1.00, soe_lambda_mult = 0.5, impute_missing = TRUE, label = "X6_impute_vlight")
# Combined tuning winners (round 3): less shrinkage on every component; optional
# net/level split for the efficiency model (level = scoring level, shrunk harder)
variants$C1_lam050     <- model_components(stretch = 1, lambda_mult = 0.5, label = "C1_lam050")
variants$C2_lam025     <- model_components(stretch = 1, lambda_mult = 0.25, label = "C2_lam025")
variants$C3_lam050_nl4 <- model_components(stretch = 1, lambda_mult = 0.5, netlevel_ratio = 4, label = "C3_lam050_nl4")
variants$C4_lam025_nl8 <- model_components(stretch = 1, lambda_mult = 0.25, netlevel_ratio = 8, label = "C4_lam025_nl8")

# Round 4: priors. All on top of C1 (production since 2026-10-24). Which priors are
# used is set by BT_PRIORS_DIR (see priors/_Build Priors.R); BT_TAG names the output.
variants$P_C1 <- model_components(stretch = 1, lambda_mult = 0.5, label = "P_C1")
# Prior enters the ridge as the center of shrinkage instead of a separate blend
variants$Q_off05 <- model_components(stretch = 1, lambda_mult = 0.5, prior_mode = "offset", prior_lambda_mult = 0.5, label = "Q_off05")
variants$Q_off1  <- model_components(stretch = 1, lambda_mult = 0.5, prior_mode = "offset", prior_lambda_mult = 1, label = "Q_off1")
variants$Q_off2  <- model_components(stretch = 1, lambda_mult = 0.5, prior_mode = "offset", prior_lambda_mult = 2, label = "Q_off2")
variants$Q_off4  <- model_components(stretch = 1, lambda_mult = 0.5, prior_mode = "offset", prior_lambda_mult = 4, label = "Q_off4")
# Production blend with different curves (all five components)
bo <- function(...) { x <- list(...); setNames(rep(list(x), 5), names(prod_stats)) }
variants$Q_fast  <- model_components(stretch = 1, lambda_mult = 0.5, blend_override = bo(prior_wgt = 0.85), label = "Q_fast")
variants$Q_slow  <- model_components(stretch = 1, lambda_mult = 0.5, blend_override = bo(prior_wgt = 0.99), label = "Q_slow")
variants$Q_min0  <- model_components(stretch = 1, lambda_mult = 0.5, blend_override = bo(prior_min = 0), label = "Q_min0")
variants$Q_min30 <- model_components(stretch = 1, lambda_mult = 0.5, blend_override = bo(prior_min = 0.3), label = "Q_min30")
# One component at a time: only the efficiency (rtg) blend changes. Production rtg:
# prior_wgt 0.97, prior_min 0.15, max_games 32. (Q_fast/Q_slow above also moved
# pace, whose production prior_wgt is 0.65.)
variants$Q_rtg90   <- model_components(stretch = 1, lambda_mult = 0.5, blend_override = list(rtg = list(prior_wgt = 0.90)), label = "Q_rtg90")
variants$Q_rtg85   <- model_components(stretch = 1, lambda_mult = 0.5, blend_override = list(rtg = list(prior_wgt = 0.85)), label = "Q_rtg85")
variants$Q_rtgmin0 <- model_components(stretch = 1, lambda_mult = 0.5, blend_override = list(rtg = list(prior_min = 0)), label = "Q_rtgmin0")
