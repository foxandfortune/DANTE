# Saved production projections (all_results_*.rds) for as-run comparison --------
load_saved_projections <- function(league, season, dir = NULL) {
  if (is.null(dir)) dir <- bt_path(league, "results", season = season)
  fs <- list.files(dir, pattern = "^all_results_.*\\.rds$", full.names = TRUE)
  bind_rows(lapply(fs, function(f) {
    x <- readRDS(f); x$file <- sub("all_results_(.*)\\.rds", "\\1", basename(f))
    x %>% mutate(across(any_of(c("game_id", "team_id", "opp_id")), as.integer))
  })) %>%
    arrange(file) %>% group_by(game_id, team_id) %>% slice_tail(n = 1) %>% ungroup()
}

# Vegas spreads / totals recoverable from all_bets_*.rds (MBB only)
load_saved_lines <- function(league, season, dir = NULL) {
  if (is.null(dir)) dir <- bt_path(league, "results", season = season)
  fs <- list.files(dir, pattern = "^all_bets_.*\\.rds$", full.names = TRUE)
  if (!length(fs)) return(NULL)
  b <- lapply(fs, readRDS)
  sp <- bind_rows(lapply(b, function(x) x$spread %>% select(game_id, team_id, spread))) %>%
    filter(!is.na(game_id)) %>% mutate(across(c(game_id, team_id), as.integer)) %>%
    group_by(game_id, team_id) %>% slice_tail(n = 1) %>% ungroup()
  tot <- bind_rows(lapply(b, function(x) bind_rows(
           x$game_over %>% select(game_id, total), x$game_under %>% select(game_id, total)))) %>%
    filter(!is.na(game_id)) %>% mutate(game_id = as.integer(game_id)) %>%
    group_by(game_id) %>% slice_tail(n = 1) %>% ungroup() %>% rename(vegas_total = total)
  list(spread = sp, total = tot)
}
