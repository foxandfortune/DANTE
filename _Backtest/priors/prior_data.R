# Data for the prior tests: full-season ratings, roster continuity, recruiting -----------
# Everything is keyed on the season being PREDICTED (S, spring-year convention:
# 2025-26 = 2026). Features for S use only information known before S tips off:
# ratings through S-1, the S-1 box scores, and the recruiting/departure files
# whose `season` column is S-1 (the class of 2025 plays in the 2026 season).
suppressMessages({library(dplyr); library(tidyr); library(purrr); library(stringr); library(tibble)})

PRIOR_STATS <- c("pace", "ast", "oreb", "to", "rtg", "efg", "raw_rating")

lg_paths <- function(lg) {
  if (lg == "MBB") list(
    fs = "VRGL/Stats/Team and Player Stats/Power Ratings/Team Ratings/Full Season/ratings_all_{season}.rds",
    roster = "VRGL/Stats/Team and Player Stats/Rosters/roster_{season}.rds",
    cruit = "VRGL/Stats/Offseason Updates - MBB/Recruiting and Priors/Recruiting - MBB",
    conf = "_Helper Files/Team Data/Conferences/mbb_conf_{season}.rds",
    teams = "_Helper Files/Team Data/team_database.rds",
    teams247 = "_Helper Files/Team Data/teams247.rds",
    priors = "VRGL/Stats/Team and Player Stats/Power Ratings/Team Ratings/Priors/priors_{season}.rds")
  else list(
    fs = "BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Team Ratings/Full Season/ratings_all_wbb_{season}.rds",
    roster = "BTRC/Stats/Team and Player Stats - WBB/Rosters/roster_wbb_{season}.rds",
    cruit = NA_character_,
    conf = "_Helper Files/Team Data/Conferences/wbb_conf_{season}.rds",
    teams = "_Helper Files/Team Data/team_database_wbb.rds",
    teams247 = NA_character_,
    priors = "BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Team Ratings/Priors/priors_wbb_{season}.rds")
}
fill_path <- function(tmpl, season) glue::glue_data(list(season = season), tmpl)

# Full-season ratings -> long: season, stat, team_id, off, def -------------------------
# fs_dir overrides the folder (the test uses ratings refit with the current weights).
load_fullseason <- function(lg, seasons, fs_dir = NULL) {
  tmpl <- lg_paths(lg)$fs
  if (!is.null(fs_dir)) tmpl <- file.path(fs_dir, basename(tmpl))
  out <- map_dfr(seasons, function(s) {
    f <- fill_path(tmpl, s)
    if (!file.exists(f)) return(NULL)
    x <- readRDS(f)
    imap_dfr(x[intersect(names(x), PRIOR_STATS)], ~ as_tibble(.x) %>% mutate(stat = .y, season = s))
  })
  list(
    teams = out %>% filter(str_detect(name, "^(team|opp)_id_")) %>%
      mutate(side = if_else(str_starts(name, "team_id_"), "off", "def"),
             team_id = as.integer(str_extract(name, "\\d+$"))) %>%
      select(season, stat, team_id, side, value) %>%
      pivot_wider(names_from = side, values_from = value),
    factors = out %>% filter(!str_detect(name, "^(team|opp)_id_")))
}

# Player names: one normaliser for box scores and the 247/Torvik files ------------------
norm_name <- function(x) {
  x <- stringi::stri_trans_general(x, "Latin-ASCII") %>% str_to_lower()
  x <- str_replace_all(x, "[^a-z ]", " ")
  x <- str_remove_all(x, "\\b(jr|sr|ii|iii|iv|v)\\b")
  str_squish(x)
}

# Box-score minutes by player and team for one season ----------------------------------
box_minutes <- function(lg, season) {
  f <- fill_path(lg_paths(lg)$roster, season)
  if (!file.exists(f)) return(NULL)
  readRDS(f) %>%
    filter(!did_not_play %in% TRUE, !is.na(minutes)) %>%
    group_by(team_id = as.integer(team_id), athlete_id, name = norm_name(athlete_display_name)) %>%
    summarise(min = sum(as.numeric(minutes)), g = n_distinct(game_id), .groups = "drop")
}

# Roster continuity for season S --------------------------------------------------------
#  ret_min_box: share of the team's S-1 minutes played by players who also play for
#               the team in S. Uses S box scores, so in production it needs the
#               PRESEASON roster (known before tip-off); in the backtest the S box is
#               a stand-in. Flagged as such in the candidates.
#  min_lost / obpm_lost / dbpm_lost: departures announced before the season (draft
#               entrants, transfers out, seniors), weighted by minutes. Players on the
#               list who still play for the team in S (5th-year seniors, withdrawn
#               draft entrants) are taken off it.
continuity_features <- function(lg, S) {
  prev <- box_minutes(lg, S - 1); cur <- box_minutes(lg, S)
  if (is.null(prev)) return(NULL)
  team_min <- prev %>% group_by(team_id) %>% summarise(team_min = sum(min), .groups = "drop")
  out <- team_min
  if (!is.null(cur)) {
    back <- prev %>% semi_join(cur, by = c("team_id", "athlete_id"))
    out <- out %>% left_join(back %>% group_by(team_id) %>% summarise(ret_min = sum(min), .groups = "drop"),
                             by = "team_id") %>%
      mutate(ret_min_box = coalesce(ret_min, 0) / team_min) %>% select(-ret_min)
  }
  if (lg == "MBB") {
    d <- lg_paths(lg)$cruit; yr <- S - 1
    rd <- function(p) { f <- file.path(d, sprintf("%s_%d.rds", p, yr)); if (file.exists(f)) readRDS(f) else NULL }
    leave <- bind_rows(
      rd("draft_entrants_clean") %>% transmute(team_id, name = norm_name(match_name), total_min, obpm, dbpm, why = "draft"),
      rd("graduates_clean") %>% transmute(team_id, name = norm_name(match_name), total_min, obpm, dbpm, why = "senior"),
      rd("transfers_clean") %>% transmute(team_id = transfromID, name = norm_name(match_name), total_min, obpm, dbpm, why = "transfer")) %>%
      filter(!is.na(team_id)) %>% mutate(team_id = as.integer(team_id)) %>%
      distinct(team_id, name, .keep_all = TRUE)
    if (!is.null(cur)) leave <- leave %>% anti_join(cur %>% distinct(team_id, name), by = c("team_id", "name"))
    lost <- leave %>% group_by(team_id) %>%
      summarise(n_lost = n(), min_lost = sum(total_min, na.rm = TRUE),
                obpm_lost = sum(total_min * obpm, na.rm = TRUE), dbpm_lost = sum(total_min * dbpm, na.rm = TRUE),
                .groups = "drop")
    out <- out %>% left_join(lost, by = "team_id") %>%
      mutate(across(c(n_lost, min_lost, obpm_lost, dbpm_lost), ~ coalesce(.x, 0)),
             min_lost = pmin(min_lost / team_min, 1),
             obpm_lost = obpm_lost / team_min, dbpm_lost = dbpm_lost / team_min)
  }
  out %>% mutate(season = S)
}

# Incoming talent for season S (MBB only: 247 commits and transfers in) -----------------
# Teams without a 247 page have no commits or incoming transfers recorded; they get
# zeros plus no_247 = 1, so the model learns their offset instead of treating them as
# teams that signed nobody.
incoming_features <- function(S) {
  p <- lg_paths("MBB"); d <- p$cruit; yr <- S - 1
  cm <- file.path(d, sprintf("commits_clean_%d.rds", yr)); tf <- file.path(d, sprintf("transfers_clean_%d.rds", yr))
  if (!file.exists(cm)) return(NULL)
  t247 <- readRDS(p$teams247); tdb <- readRDS(p$teams)
  no_page <- tdb %>% left_join(t247, by = "Team") %>%
    transmute(team_id = as.integer(team_id), no_247 = as.integer(is.na(URL) | URL == "https://247sports.com/"))
  cmt <- readRDS(cm) %>% group_by(team_id = as.integer(team_id)) %>%
    summarise(n_cmt = n(), cmt_talent = sum(percentile, na.rm = TRUE),
              cmt_top100 = sum(rank_overall <= 100, na.rm = TRUE), .groups = "drop")
  # Some years' cleaned transfer files have no percentile column (the 2026 run of
  # script 3); rebuild it the same way: rank among rated transfers, unrated = NA
  add_pct <- function(x) {
    if (!"percentile" %in% names(x)) {
      r <- x$rating
      x$percentile <- NA_real_
      ok <- !is.na(r)
      x$percentile[ok] <- dplyr::cume_dist(r[ok])
    }
    x
  }
  tfr <- if (file.exists(tf)) readRDS(tf) %>% add_pct() %>% filter(!is.na(transtoID)) %>%
    distinct(transtoID, match_name, .keep_all = TRUE) %>%
    group_by(team_id = as.integer(transtoID)) %>%
    summarise(n_tfr = n(), tfr_min = sum(total_min, na.rm = TRUE),
              tfr_obpm = sum(total_min * obpm, na.rm = TRUE), tfr_dbpm = sum(total_min * dbpm, na.rm = TRUE),
              tfr_rating = sum(percentile, na.rm = TRUE), .groups = "drop") else NULL
  no_page %>% left_join(cmt, by = "team_id") %>% left_join(tfr, by = "team_id") %>%
    mutate(across(-c(team_id, no_247), ~ coalesce(.x, 0)), season = S)
}

# Conference membership (files exist from 2021) -----------------------------------------
# Seasons without a file use the nearest one (membership before 2021 ~ 2021).
load_conf <- function(lg, seasons) {
  have <- seasons[file.exists(fill_path(lg_paths(lg)$conf, seasons))]
  if (!length(have)) have <- 2021:2026
  have <- have[file.exists(fill_path(lg_paths(lg)$conf, have))]
  map_dfr(seasons, function(s) {
    src <- have[which.min(abs(have - s))]
    readRDS(fill_path(lg_paths(lg)$conf, src)) %>% transmute(season = s, team_id = as.integer(team_id), conf_id)
  })
}

# Team-season frame + per-stat model data ------------------------------------------------
# fs: output of load_fullseason(). extra: optional data frame (season, team_id) of a
# season with no ratings yet (the season being built), so it gets features too.
prior_data_setup <- function(lg, seasons, fs, extra = NULL) {
  ratings_wide <- fs$teams
  frame <- ratings_wide %>% distinct(season, team_id) %>% filter(season >= min(seasons) + 1)
  if (!is.null(extra)) frame <- bind_rows(frame, extra %>% mutate(team_id = as.integer(team_id))) %>% distinct(season, team_id)
  conf <- load_conf(lg, sort(unique(frame$season)))
  cont <- map_dfr(sort(unique(frame$season)), ~ continuity_features(lg, .x))
  frame <- frame %>% left_join(cont, by = c("season", "team_id"))
  if (lg == "MBB") {
    inc <- map_dfr(sort(unique(frame$season)), incoming_features)
    frame <- frame %>% left_join(inc, by = c("season", "team_id")) %>% mutate(tfr_min = tfr_min / team_min)
  }
  z <- function(x) { s <- sd(x, na.rm = TRUE); out <- if (is.na(s) || s == 0) x * 0 else (x - mean(x, na.rm = TRUE)) / s; replace_na(out, 0) }
  frame <- frame %>% group_by(season) %>%
    mutate(across(any_of(c("ret_min_box", "min_lost", "obpm_lost", "dbpm_lost")), ~ replace_na(.x, 0))) %>%
    { if (lg == "MBB") mutate(., tfr_min_z = z(tfr_min), tfr_rtg_z = z(tfr_rating), cmt_z = z(cmt_talent),
                              tfr_obpm_z = z(tfr_obpm / team_min), tfr_dbpm_z = z(tfr_dbpm / team_min),
                              no_247 = replace_na(no_247, 1L)) else . } %>%
    ungroup()
  build_model_data <- function(stat_name, side) {
    other <- setdiff(c("off", "def"), side)
    r <- ratings_wide %>% filter(stat == stat_name) %>% select(season, team_id, rtg = all_of(side), oth = all_of(other))
    d <- frame %>%
      left_join(r %>% select(season, team_id, rtg), by = c("season", "team_id")) %>%
      left_join(r %>% transmute(season = season + 1L, team_id, rtg_prior = rtg, other_prior = -oth), by = c("season", "team_id")) %>%
      left_join(r %>% transmute(season = season + 2L, team_id, rtg_prior2 = rtg), by = c("season", "team_id")) %>%
      mutate(new_d1 = as.integer(is.na(rtg_prior)),
             rtg_prior = replace_na(rtg_prior, 0), other_prior = replace_na(other_prior, 0),
             rtg_prior2 = coalesce(rtg_prior2, rtg_prior)) %>%
      left_join(conf, by = c("season", "team_id")) %>%
      group_by(season, conf_id) %>%
      mutate(conf_prior = if_else(is.na(conf_id) | n() < 2, 0, (sum(rtg_prior) - rtg_prior) / (n() - 1))) %>%
      ungroup()
    if (lg == "MBB") d <- d %>% mutate(bpm_lost = if (side == "off") obpm_lost else dbpm_lost,
                                       tfr_bpm_z = if (side == "off") tfr_obpm_z else tfr_dbpm_z)
    d %>% mutate(team_id = factor(team_id))
  }
  list(ratings_wide = ratings_wide, frame = frame, conf = conf, build_model_data = build_model_data)
}

# Fit one candidate on train, predict test (lm / lmer screen; rstanarm confirm) -----------
prior_fit_predict <- function(formula_str, train, test, ENGINE = "lm", CHAINS = 4, ITER = 1000) {
  f <- as.formula(formula_str)
  is_hier <- str_detect(formula_str, "\\|")
  is_fe <- str_detect(formula_str, "factor\\(team_id\\)")
  if (ENGINE == "lm" || is_fe) {
    if (is_fe) {
      # Current method: team fixed effects. Teams the model has never seen fall back
      # to 0.8 x last season + 0.2 x two seasons ago, as 4. Create Priors does.
      m <- lm(f, data = train %>% mutate(team_id = droplevels(team_id)))
      seen <- test$team_id %in% unique(train$team_id)
      est <- 0.8 * test$rtg_prior + 0.2 * test$rtg_prior2
      if (any(seen)) est[seen] <- predict(m, newdata = test[seen, ] %>% mutate(team_id = factor(as.character(team_id), levels = levels(droplevels(train$team_id)))))
      return(tibble(est = est, lo = NA_real_, hi = NA_real_))
    }
    if (is_hier) {
      m <- lme4::lmer(f, data = train)
      est <- predict(m, newdata = test, allow.new.levels = TRUE)
      s <- sqrt(sigma(m)^2 + as.numeric(lme4::VarCorr(m)$team_id))
      return(tibble(est = est, lo = est - 1.2816 * s, hi = est + 1.2816 * s))
    }
    m <- lm(f, data = train)
    p <- predict(m, newdata = test, interval = "prediction", level = 0.8)
    return(tibble(est = p[, "fit"], lo = p[, "lwr"], hi = p[, "upr"]))
  }
  m <- if (is_hier) rstanarm::stan_glmer(f, data = train, chains = CHAINS, iter = ITER, seed = 421, refresh = 0)
       else rstanarm::stan_glm(f, data = train, chains = CHAINS, iter = ITER, seed = 421, refresh = 0)
  pp <- rstanarm::posterior_predict(m, newdata = test)
  tibble(est = colMeans(pp), lo = apply(pp, 2, quantile, 0.10), hi = apply(pp, 2, quantile, 0.90))
}

