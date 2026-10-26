# Full-season ratings for every backtest season, with the current weights ------------
# Same specs as "Full-Season Ratings - All.R" (all seven stats, nearest-season
# weight row), fitted with the backtest's sparse ridge engine (predictions within
# ~0.04 pts/100 of the production dense fit) so all seasons take minutes, not hours.
# Usage: Rscript fullseason_fast.R <MBB|WBB> <weights_dir> <out_dir>
args <- commandArgs(TRUE); lg <- args[1]; wdir <- args[2]; out_dir <- args[3]
bt_dir <- "/home/claude/_Backtest"
for (f in list.files(file.path(bt_dir, "R"), full.names = TRUE)) source(f)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
sit <- c("is_home", "neutral_site", "days_rest", "travel")
spec <- list(pace = list("poss_per_40", c("game_date", "team_id", "opp_id")),
             ast  = list("ast_rt", c("game_date", "team_id", "opp_id")),
             oreb = list("oreb_rt", c("game_date", "team_id", "opp_id")),
             to   = list("to_rt", c("game_date", "team_id", "opp_id")),
             rtg  = list("rtg", c("game_date", "team_id", "opp_id", "poss_per_40", "ast_rt", "oreb_rt", "to_rt", sit)),
             efg  = list("efg_adj", c("game_date", "team_id", "opp_id")),
             raw_rating = list("rtg", c("game_date", "team_id", "opp_id", "poss_per_40", sit)))
wfile <- function(stat) file.path(wdir, if (lg == "MBB") sprintf("single_season_%s_wgt.rds", stat) else sprintf("single_season_wbb_%s_wgt.rds", stat))
for (s in bt_seasons_all <- if (lg == "MBB") 2018:2026 else 2021:2026) {
  df <- load_summary(lg, s)
  res <- lapply(spec, function(sp) {
    w <- readRDS(wfile(sp[[1]])) %>% mutate(d = abs(season - !!s)) %>% filter(d == min(d)) %>% slice(1)
    as.data.frame(fit_ridge(df, sp[[1]], sp[[2]], w$wgt, lambda_mult = 1 + w$lambda_adj, seed = 421))
  })
  saveRDS(res, file.path(out_dir, if (lg == "MBB") sprintf("ratings_all_%d.rds", s) else sprintf("ratings_all_wbb_%d.rds", s)))
  cat(lg, s, "done\n")
}
