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
