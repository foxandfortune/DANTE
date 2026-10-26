# ──────────────────────────────────────────────────────────────────────────────
# _Build Priors.R: build a season's priors from the models the test chose
#
# Usage (from the DANTE repo root):
#   Rscript "_Backtest/priors/_Build Priors.R" <MBB|WBB> <season> [method] [fs_dir] [out_dir]
#     method: "chosen" (default; per stat x side from prior_model_tests_<lg>_lm.rds),
#             "chosen_roster" (allows the roster models; needs this season's rosters),
#             or a model id for every stat (e.g. "P0" = current method, "M03")
#     fs_dir: folder of ratings_all_*.rds (default: production Full Season folder)
#     out_dir: where priors_<season>.rds / priors_wbb_<season>.rds goes (default:
#              _Backtest/priors/output/priors_<method>)
# Every model is fit only on seasons before <season>, so building past seasons
# gives honest priors for the walk-forward backtest.
#
# Output format is exactly the production priors list: pace, ast, oreb, to, rtg,
# efg, raw_rating, each a data frame of name/value with the intercept and
# covariate rows followed by team_id_<id> (offense) and opp_id_<id> (defense).
# ──────────────────────────────────────────────────────────────────────────────
args <- commandArgs(TRUE)
LEAGUE <- args[1]; S <- as.integer(args[2])
METHOD <- if (length(args) >= 3) args[3] else "chosen"
FS_DIR <- if (length(args) >= 4 && nzchar(args[4])) args[4] else NULL
bt_dir <- if (dir.exists("_Backtest")) "_Backtest" else "/home/claude/_Backtest"
OUT_DIR <- if (length(args) >= 5) args[5] else file.path(bt_dir, "priors", "output", paste0("priors_", METHOD))
source(file.path(bt_dir, "priors", "prior_data.R"))

first_season <- if (LEAGUE == "MBB") 2018 else 2021
seasons <- first_season:(S - 1)
fs <- load_fullseason(LEAGUE, seasons, FS_DIR)
if (!(S - 1) %in% fs$teams$season) stop("No full-season ratings for ", S - 1, ": run Full-Season Ratings first")

# Teams in season S: the conference file for S, else last season's teams
cf <- fill_path(lg_paths(LEAGUE)$conf, S)
extra <- if (file.exists(cf)) readRDS(cf) %>% transmute(season = S, team_id = as.integer(team_id)) else {
  warning("No conference file for ", S, "; using last season's teams (new D1 teams will be missing)")
  fs$teams %>% filter(season == S - 1) %>% distinct(team_id) %>% mutate(season = S)
}
setup <- prior_data_setup(LEAGUE, first_season:S, fs, extra = extra)

# Which model for each stat x side
STATS <- c("pace", "ast", "oreb", "to", "rtg", "efg", "raw_rating")
tests <- file.path(bt_dir, "priors", "output", glue::glue("prior_model_tests_{LEAGUE}_lm.rds"))
plan <- if (METHOD %in% c("chosen", "chosen_roster")) {
  readRDS(tests)[[METHOD]] %>% select(stat, side, model, formula)
} else {
  cand <- readRDS(tests)$candidates
  tidyr::crossing(stat = STATS, side = c("off", "def")) %>% mutate(model = METHOD) %>%
    left_join(cand %>% select(model, formula), by = "model")
}

preds <- purrr::pmap_dfr(plan, function(stat, side, model, formula) {
  d <- setup$build_model_data(stat, side)
  train <- d %>% filter(season < S, season >= first_season + 1, !is.na(rtg))
  test <- d %>% filter(season == S)
  est <- if (model == "B1") test$rtg_prior else if (model == "B0") rep(0, nrow(test)) else
    prior_fit_predict(formula, train, test, "lm")$est
  tibble(stat = stat, side = side, team_id = as.character(test$team_id), value = est, model = model)
})

# Intercept and covariates: recency-weighted average of past full-season fits, as
# 4. Create Priors does (pace starts the season ~3 possessions faster than it ends)
fac <- fs$factors %>% filter(season >= S - 7) %>%
  mutate(w = 0.97 ^ (S - season)) %>%
  group_by(stat, name) %>% summarise(value = weighted.mean(value, w), .groups = "drop") %>%
  mutate(value = if_else(stat == "pace" & name == "(Intercept)", value + 3, value))

priors <- purrr::map(setNames(STATS, STATS), function(st) {
  bind_rows(fac %>% filter(stat == st) %>% select(name, value),
            preds %>% filter(stat == st) %>%
              transmute(name = paste0(if_else(side == "off", "team_id_", "opp_id_"), team_id), value)) %>%
    as.data.frame()
})
attr(priors, "built") <- list(league = LEAGUE, season = S, method = METHOD, models = plan, date = Sys.Date())

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(OUT_DIR, basename(fill_path(lg_paths(LEAGUE)$priors, S)))
saveRDS(priors, out_file)
cat(LEAGUE, S, METHOD, ": ", n_distinct(preds$team_id), " teams -> ", out_file, "\n", sep = "")
