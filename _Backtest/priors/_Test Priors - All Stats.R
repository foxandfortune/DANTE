# ──────────────────────────────────────────────────────────────────────────────
# _Test Priors - All Stats.R   (DANTE: VRGL men's / BTRC women's)
#
# Which preseason model gives the best PRIOR for each power rating?
#
#   next season's rating ~ last season's rating + roster / recruiting features
#
# Same design as the EuroLeagues test. Every candidate is scored the same way:
# fit on seasons before S, predict season S, compare with the full-season rating
# S actually ended with. Repeated for each S in TEST_SEASONS ("rolling origin":
# the model never sees the season it predicts, exactly like real preseason use).
#
# Usage (from the DANTE repo root):
#   Rscript "_Backtest/priors/_Test Priors - All Stats.R" MBB [lm|stan] [fs_dir]
#   fs_dir: folder of ratings_all_*.rds to use as ratings/targets (default: the
#           production Full Season folder). The backtest uses ratings refit with
#           the current (C1) weights so targets match the in-season scale.
# Output: _Backtest/priors/output/prior_model_tests_<league>.rds with
#   leaderboard, chosen, predictions, candidates, features, config.
#
# ── HOW THE DATA ARE SET UP ──────────────────────────────────────────────────
# One row = one team in one season (the season being predicted). Per stat x side:
#   rtg          full-season rating that season (the target)
#   rtg_prior    last season's rating, same side; 0 for teams new to D1
#   new_d1       1 if the team has no rating last season
#   rtg_prior2   two seasons ago (falls back to rtg_prior if missing)
#   other_prior  minus last season's rating on the OTHER side (+ = points the
#                same way as the target)
#   conf_prior   average rtg_prior of the team's conference-mates this season
# Roster (both leagues, from ESPN box scores):
#   ret_min_box  share of last season's minutes played by players who also play
#                for the team this season. NEEDS THE PRESEASON ROSTER in
#                production; in the backtest this season's box scores stand in.
# Departures and arrivals (men's only, 247 / Hoops Rumors / Torvik files):
#   min_lost     share of last season's minutes leaving (draft, transfer, senior),
#                minus players on those lists who came back
#   bpm_lost     minutes-weighted BPM leaving (OBPM for off, DBPM for def)
#   tfr_min_z / tfr_bpm_z / tfr_rtg_z   incoming transfers: minutes, BPM, 247
#                rating, z-scored within season (the portal grew ~50x since 2018)
#   cmt_z        incoming high-school talent (sum of 247 percentiles), z in season
#   no_247       1 if the school has no 247 team page (commits and incoming
#                transfers unknown, so they are 0 and the flag carries the offset)
# ──────────────────────────────────────────────────────────────────────────────

args <- commandArgs(TRUE)
LEAGUE  <- if (length(args) >= 1) args[1] else "MBB"
ENGINE  <- if (length(args) >= 2) args[2] else "lm"     # "lm" to screen, "stan" to confirm
FS_DIR  <- if (length(args) >= 3) args[3] else NULL
bt_dir  <- if (dir.exists("_Backtest")) "_Backtest" else "/home/claude/_Backtest"
source(file.path(bt_dir, "priors", "prior_data.R"))

# ── CONFIGURATION ──────────────────────────────────────────────────────────────
STATS <- c("pace", "ast", "oreb", "to", "rtg", "raw_rating", "efg")
SIDES <- c("off", "def")
SEASONS      <- if (LEAGUE == "MBB") 2018:2026 else 2021:2026
TEST_SEASONS <- if (LEAGUE == "MBB") 2021:2026 else 2023:2026
PARSIMONY_TOL <- 0.01   # simplest model within 1% of the best RMSE wins
CHAINS <- 4; ITER <- 1000
options(mc.cores = parallel::detectCores(), width = 220)

# ── CANDIDATE MODELS ───────────────────────────────────────────────────────────
mbb_only <- c("M04", "M05", "M06", "M07", "M08", "M09", "M10", "M11", "R2")
candidates <- tibble::tribble(
  ~model, ~label,                                   ~formula,
  "B0",   "baseline: league average (0)",           NA,
  "B1",   "baseline: last season unchanged",        NA,
  "M00",  "prior only",                             "rtg ~ rtg_prior + new_d1",
  "M01",  "two seasons",                            "rtg ~ rtg_prior + rtg_prior2 + new_d1",
  "M02",  "two seasons + other side",               "rtg ~ rtg_prior + rtg_prior2 + other_prior + new_d1",
  "M03",  "two seasons + conference",               "rtg ~ rtg_prior + rtg_prior2 + conf_prior + new_d1",
  "P0",   "current method: prior + team effect",    "rtg ~ rtg_prior + factor(team_id)",
  "M04",  "two seasons + departures",               "rtg ~ rtg_prior + rtg_prior2 + new_d1 + min_lost + bpm_lost",
  "M05",  "+ transfers in",                         "rtg ~ rtg_prior + rtg_prior2 + new_d1 + min_lost + bpm_lost + tfr_min_z + tfr_bpm_z + no_247",
  "M06",  "+ recruits",                             "rtg ~ rtg_prior + rtg_prior2 + new_d1 + min_lost + bpm_lost + tfr_min_z + tfr_bpm_z + tfr_rtg_z + cmt_z + no_247",
  "M07",  "M06 + persistence x continuity",         "rtg ~ rtg_prior * min_lost + rtg_prior2 + new_d1 + bpm_lost + tfr_min_z + tfr_bpm_z + tfr_rtg_z + cmt_z + no_247",
  "M08",  "M06 + conference",                       "rtg ~ rtg_prior + rtg_prior2 + conf_prior + new_d1 + min_lost + bpm_lost + tfr_min_z + tfr_bpm_z + tfr_rtg_z + cmt_z + no_247",
  "M09",  "M06 + other side",                       "rtg ~ rtg_prior + rtg_prior2 + other_prior + new_d1 + min_lost + bpm_lost + tfr_min_z + tfr_bpm_z + tfr_rtg_z + cmt_z + no_247",
  "M10",  "M06 + program effect",                   "rtg ~ rtg_prior + rtg_prior2 + new_d1 + min_lost + bpm_lost + tfr_min_z + tfr_bpm_z + tfr_rtg_z + cmt_z + no_247 + (1 | team_id)",
  "M11",  "kitchen sink",                           "rtg ~ rtg_prior * min_lost + rtg_prior2 + other_prior + conf_prior + new_d1 + bpm_lost + tfr_min_z + tfr_bpm_z + tfr_rtg_z + cmt_z + no_247",
  # R models need this season's roster before tip-off (see ret_min_box)
  "R1",   "two seasons x returning minutes [roster]", "rtg ~ rtg_prior * ret_min_box + rtg_prior2 + conf_prior + new_d1",
  "R2",   "M08 x returning minutes [roster]",       "rtg ~ rtg_prior * ret_min_box + rtg_prior2 + conf_prior + new_d1 + bpm_lost + tfr_min_z + tfr_bpm_z + tfr_rtg_z + cmt_z + no_247"
)
if (LEAGUE == "WBB") candidates <- filter(candidates, !model %in% mbb_only)

# ── LOAD ───────────────────────────────────────────────────────────────────────
fs <- load_fullseason(LEAGUE, SEASONS, FS_DIR)
setup <- prior_data_setup(LEAGUE, SEASONS, fs)
ratings_wide <- setup$ratings_wide; frame <- setup$frame; build_model_data <- setup$build_model_data

# ── FIT / PREDICT ──────────────────────────────────────────────────────────────
fit_predict <- function(formula_str, train, test) prior_fit_predict(formula_str, train, test, ENGINE, CHAINS, ITER)

# ── RUN ────────────────────────────────────────────────────────────────────────
jobs <- tidyr::crossing(stat = STATS, side = SIDES, test_season = TEST_SEASONS)
cat(glue::glue("{LEAGUE}: {nrow(jobs)} stat x side x season jobs, {sum(!is.na(candidates$formula))} fitted models each, engine = {ENGINE}"), "\n")
data_cache <- list(); preds <- vector("list", nrow(jobs)); t0 <- Sys.time()

for (j in seq_len(nrow(jobs))) {
  jb <- jobs[j, ]; key <- paste(jb$stat, jb$side)
  if (is.null(data_cache[[key]])) data_cache[[key]] <- build_model_data(jb$stat, jb$side)
  d <- data_cache[[key]] %>% filter(!is.na(rtg))
  train <- d %>% filter(season < jb$test_season, season >= min(SEASONS) + 1)
  test  <- d %>% filter(season == jb$test_season)
  if (nrow(test) == 0 || n_distinct(train$season) < 2) next
  out <- map_dfr(seq_len(nrow(candidates)), function(k) {
    cand <- candidates[k, ]
    p <- switch(cand$model,
      B0 = tibble(est = 0, lo = NA_real_, hi = NA_real_),
      B1 = tibble(est = test$rtg_prior, lo = NA_real_, hi = NA_real_),
      tryCatch(fit_predict(cand$formula, train, test),
               error = function(e) { warning(glue::glue("{key} {jb$test_season} {cand$model}: {conditionMessage(e)}"))
                                     tibble(est = rep(NA_real_, nrow(test)), lo = NA_real_, hi = NA_real_) }))
    test %>% select(season, team_id, new_d1, any_of("no_247"), rtg, rtg_prior) %>% bind_cols(p) %>% mutate(model = cand$model)
  })
  preds[[j]] <- out %>% mutate(stat = jb$stat, side = jb$side, n_train = nrow(train))
  cat(sprintf("[%d/%d] %-10s %-3s %d  (%.1f min)\n", j, nrow(jobs), jb$stat, jb$side, jb$test_season,
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
predictions <- bind_rows(preds) %>% mutate(err = est - rtg)

# ── LEADERBOARD ────────────────────────────────────────────────────────────────
n_terms <- function(f) {
  if (is.na(f)) return(0L)
  f2 <- str_remove(f, "\\s*\\+\\s*\\(1 \\| team_id\\)")
  length(attr(terms(as.formula(f2)), "term.labels")) + as.integer(str_detect(f, "\\||factor"))
}
candidates <- candidates %>% mutate(n_terms = map_int(formula, n_terms))

leaderboard <- predictions %>% filter(!is.na(est)) %>%
  group_by(stat, side, model) %>%
  summarise(n = n(), rmse = sqrt(mean(err^2)), mae = mean(abs(err)),
            corr = if (sd(est) > 0) cor(est, rtg) else NA_real_,
            cover80 = mean(rtg >= lo & rtg <= hi),
            rmse_new = sqrt(mean(err[new_d1 == 1]^2)), n_new = sum(new_d1 == 1), .groups = "drop") %>%
  group_by(stat, side) %>%
  mutate(skill_vs_avg = 1 - rmse / rmse[model == "B0"], rank = rank(rmse, ties.method = "first")) %>%
  ungroup() %>%
  left_join(candidates %>% select(model, label, n_terms, formula), by = "model") %>%
  arrange(stat, side, rank)

# Chosen: simplest fitted model within PARSIMONY_TOL of the best. Roster (R) models
# are chosen separately, since they need data production doesn't have yet.
pick <- function(lb) lb %>% filter(!str_starts(model, "B")) %>% group_by(stat, side) %>%
  mutate(best_rmse = min(rmse)) %>% filter(rmse <= best_rmse * (1 + PARSIMONY_TOL)) %>%
  arrange(n_terms, rmse, .by_group = TRUE) %>% slice(1) %>% ungroup()
chosen <- pick(leaderboard %>% filter(!str_starts(model, "R"))) %>%
  left_join(leaderboard %>% filter(model == "P0") %>% select(stat, side, rmse_current = rmse), by = c("stat", "side")) %>%
  select(stat, side, model, label, formula, n_terms, rmse, best_rmse, rmse_current, skill_vs_avg, corr, cover80, rmse_new)
chosen_roster <- pick(leaderboard) %>% select(stat, side, model, label, formula, rmse, skill_vs_avg)

# ── REPORT ─────────────────────────────────────────────────────────────────────
r4 <- function(d) as.data.frame(d %>% mutate(across(where(is.numeric), ~ round(.x, 4))))
cat("\n", strrep("=", 78), "\n", LEAGUE, ": CHOSEN MODEL PER STAT x SIDE (no preseason rosters)\n", strrep("=", 78), "\n", sep = "")
print(r4(chosen %>% select(stat, side, model, label, rmse, rmse_current, skill_vs_avg, corr, cover80, rmse_new)), row.names = FALSE)
cat("\nWith preseason rosters available:\n")
print(r4(chosen_roster %>% select(stat, side, model, label, rmse, skill_vs_avg)), row.names = FALSE)
cat("\nAverage skill vs league-average baseline, by model (higher = better):\n")
print(leaderboard %>% group_by(model, label) %>%
        summarise(mean_skill = round(mean(skill_vs_avg), 3), mean_rank = round(mean(rank), 1),
                  rmse_new = round(mean(rmse_new, na.rm = TRUE), 3), .groups = "drop") %>%
        arrange(mean_rank) %>% as.data.frame(), row.names = FALSE)
cat(sprintf("\nTeams new to D1 in test predictions: %d\n",
            n_distinct(predictions %>% filter(new_d1 == 1) %>% select(season, team_id))))

# ── SAVE ───────────────────────────────────────────────────────────────────────
res_dir <- file.path(bt_dir, "priors", "output"); dir.create(res_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(list(leaderboard = leaderboard, chosen = chosen, chosen_roster = chosen_roster,
             predictions = predictions, candidates = candidates, features = frame,
             config = list(LEAGUE = LEAGUE, STATS = STATS, SIDES = SIDES, TEST_SEASONS = TEST_SEASONS,
                           ENGINE = ENGINE, PARSIMONY_TOL = PARSIMONY_TOL, FS_DIR = FS_DIR)),
        file.path(res_dir, glue::glue("prior_model_tests_{LEAGUE}_{ENGINE}.rds")))
cat(glue::glue("\nSaved {res_dir}/prior_model_tests_{LEAGUE}_{ENGINE}.rds"), "\n")
