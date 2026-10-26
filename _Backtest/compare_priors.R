# Round 4: priors and prior blending, game level ----------------------------------------
# Usage (from the DANTE repo root): Rscript _Backtest/compare_priors.R
# All runs use the C1 ratings model. Every run gets the same walk-forward
# calibration layer (fitted on earlier seasons only), so the first season of each
# run (MBB 2021, WBB 2023) is calibration history and scoring starts the season after.
#   P_C1_pp0    priors rebuilt with the current method (team fixed effects)
#   P_C1_pch    priors from the models the prior test chose
#   P_C1_pnone  no priors
#   P_C1_pcr    chosen + roster models (needs preseason rosters in production)
#   Q_*_pch     chosen priors with a different blend (see variants.R)
#   C1_lam050   the ACTUAL production priors (MBB 2024-26, WBB 2025-26 only)
bt_dir <- if (dir.exists("_Backtest")) "_Backtest" else "/home/claude/_Backtest"
for (f in list.files(file.path(bt_dir, "R"), full.names = TRUE)) source(f)
options(width = 220, pillar.sigfig = 4)
runs <- c("P_C1_pp0", "P_C1_pch", "P_C1_pnone", "P_C1_pcr", "Q_off05_pch", "Q_off1_pch", "Q_off2_pch",
          "Q_off4_pch", "Q_fast_pch", "Q_slow_pch", "Q_min0_pch", "Q_min30_pch")
if (nzchar(Sys.getenv("BT_RUNS"))) runs <- strsplit(Sys.getenv("BT_RUNS"), ",")[[1]]
first <- c(MBB = 2021, WBB = 2023)
out <- list()
for (lg in c("MBB", "WBB")) {
  raw <- Filter(Negate(is.null), setNames(lapply(runs, load_variant, lg = lg), runs))
  raw <- lapply(raw, function(d) filter(d, season >= first[[lg]]))
  raw <- common_games(lapply(raw, add_scores))
  cal <- lapply(raw, calibrate_layers)
  cat("\n=================", lg, "| seasons", paste(range(cal[[1]]$season), collapse = "-"), "| games", nrow(cal[[1]]), "=================\n")
  m <- bind_rows(lapply(names(cal), function(v) summarise_metrics(cal[[v]]) %>% mutate(model = v)))
  print(m %>% select(model, n, margin_mae, margin_rmse, logloss, brier, total_mae, total_rmse, total_bias))
  for (base in intersect(c("P_C1_pp0", "P_C1_pch"), names(cal))) {
    cat("\n-- paired vs", base, "(negative = better) --\n")
    pc <- bind_rows(lapply(setdiff(names(cal), base), function(v)
      bind_rows(lapply(c("sq_margin", "logloss", "sq_total"), function(k) compare_models(cal[[base]], cal[[v]], k, B = 1000))) %>%
        mutate(model = v)))
    print(pc %>% select(model, metric, diff, lo95, hi95) %>% as.data.frame(), row.names = FALSE)
    out[[paste(lg, base)]] <- pc %>% mutate(league = lg, base = base)
  }
  cat("\n-- by phase: margin RMSE / log loss / total RMSE --\n")
  ph <- bind_rows(lapply(names(cal), function(v) summarise_metrics(cal[[v]], phase) %>% mutate(model = v)))
  print(ph %>% select(model, phase, n, margin_rmse, logloss, total_rmse, total_bias) %>%
          tidyr::pivot_wider(names_from = phase, values_from = c(margin_rmse, logloss, total_rmse), id_cols = model) %>% as.data.frame(), row.names = FALSE)
  out[[paste(lg, "metrics")]] <- m %>% mutate(league = lg)
  out[[paste(lg, "phase")]] <- ph %>% mutate(league = lg)

  # Against the priors production actually used (where they exist)
  prod <- load_variant("C1_lam050", lg)
  if (!is.null(prod)) {
    ps <- if (lg == "MBB") 2024:2026 else 2025:2026
    two <- common_games(list(prod = add_scores(prod), new = add_scores(load_variant("P_C1_pch", lg))))
    two <- lapply(two, function(d) calibrate_layers(filter(d, season >= min(ps) - 1)) %>% filter(season %in% ps))
    cat("\n-- chosen priors vs the actual production priors,", paste(range(ps), collapse = "-"), "--\n")
    print(bind_rows(lapply(names(two), function(v) summarise_metrics(two[[v]], phase) %>% mutate(model = v))) %>%
            select(model, phase, n, margin_rmse, logloss, total_rmse, total_bias) %>% as.data.frame(), row.names = FALSE)
    print(bind_rows(lapply(c("sq_margin", "logloss", "sq_total"), function(k) compare_models(two$prod, two$new, k, B = 1000))) %>%
            select(metric, diff, lo95, hi95) %>% as.data.frame(), row.names = FALSE)
  }
}
saveRDS(out, file.path(bt_dir, "output", "priors_round4.rds"))
