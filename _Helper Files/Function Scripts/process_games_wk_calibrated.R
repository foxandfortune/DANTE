process_games <- function(teams, games, ratings, day_no, ...) {
  # Calibrated daily game engine ------------------------------------------------
  # Same ratings and expected-efficiency formula as process_games_wk_to_oreb_ast,
  # but the fixed x1.3 margin stretch and single disp_Rtg are replaced by a
  # calibration table (ratings$calibration, built by _Backtest/build_calibration.R):
  #   * margin  = k[phase] * raw margin + hfa (non-neutral home team)
  #   * total   = raw total + total_bias[phase]
  #   * scoring noise drawn so the margin sd and total sd each match history
  #     (the two teams' noise is correlated; total sd > margin sd)
  # Self-contained on purpose: it is saved as an .rds closure.

  cal <- ratings$calibration
  if (is.null(cal)) stop("ratings$calibration is missing; load calibration_*.rds into the ratings list")

  ratings_pace <- ratings$pace
  ratings_rtg  <- ratings$rtg
  ratings_oreb <- ratings$oreb
  ratings_to   <- ratings$to
  ratings_ast  <- ratings$ast

  base_of <- function(r) r$value[r$name == "(Intercept)"]
  team_tbl <- function(r) {
    off <- r %>% filter(str_detect(name, "team_id_")) %>%
      mutate(team_id = as.integer(str_remove_all(name, "team_id_")),
             OffRtg = value + base_of(r)) %>% select(team_id, OffRtg)
    def <- r %>% filter(str_detect(name, "opp_id_")) %>%
      mutate(team_id = as.integer(str_remove_all(name, "opp_id_")),
             DefRtg = value) %>% select(team_id, DefRtg)
    left_join(off, def, by = "team_id") %>% filter(team_id %in% teams$team_id)
  }
  tm_pace <- team_tbl(ratings_pace); tm_oreb <- team_tbl(ratings_oreb)
  tm_to   <- team_tbl(ratings_to);   tm_ast  <- team_tbl(ratings_ast)
  tm_rtg  <- team_tbl(ratings_rtg)

  coefs <- ratings_rtg %>% filter(!str_detect(name, "team_") & !str_detect(name, "opp_"))
  cf <- function(n) { v <- coefs$value[coefs$name == n]; if (length(v)) v else 0 }

  phase_of <- function(d) {
    m <- as.integer(format(as.Date(d), "%m"))
    dplyr::case_when(m %in% c(10, 11) ~ "1 Nov", m == 12 ~ "2 Dec",
                     m %in% 1:2 ~ "3 Jan-Feb", TRUE ~ "4 Mar-Apr")
  }

  played_games   <- games %>% filter(!is.na(result))
  unplayed_games <- games %>% filter(is.na(result))
  if (nrow(unplayed_games) == 0) {
    return(list(teams = as_tibble(teams), games = as_tibble(games)))
  }

  pair <- function(df, tbl, a, b, prefix) {
    df %>%
      left_join(tbl %>% rename(!!paste0(prefix, "_a_off") := OffRtg, !!paste0(prefix, "_a_def") := DefRtg),
                by = setNames("team_id", a)) %>%
      left_join(tbl %>% rename(!!paste0(prefix, "_b_off") := OffRtg, !!paste0(prefix, "_b_def") := DefRtg),
                by = setNames("team_id", b))
  }

  has_date <- "game_date" %in% names(unplayed_games)
  # Tournament schedules have no is_neutral column: every game is neutral there
  if (!"is_neutral" %in% names(unplayed_games)) unplayed_games$is_neutral <- TRUE
  g <- unplayed_games %>%
    pair(tm_pace, "away_id", "home_id", "pace") %>%
    pair(tm_oreb, "away_id", "home_id", "oreb") %>%
    pair(tm_to,   "away_id", "home_id", "to") %>%
    pair(tm_ast,  "away_id", "home_id", "ast") %>%
    pair(tm_rtg,  "away_id", "home_id", "rtg") %>%
    mutate(
      .neutral = as.numeric(is_neutral),
      pace = ((pace_a_off + pace_b_def) + (pace_b_off + pace_a_def)) / 2,
      away_oreb = oreb_a_off + oreb_b_def, home_oreb = oreb_b_off + oreb_a_def,
      away_to   = to_a_off + to_b_def,     home_to   = to_b_off + to_a_def,
      away_ast  = ast_a_off + ast_b_def,   home_ast  = ast_b_off + ast_a_def,
      away_base = rtg_a_off + rtg_b_def + cf("poss_per_40") * pace +
        cf("days_rest") * away_rest + cf("travel") * away_dist + cf("neutral_site") * .neutral +
        cf("oreb_rt") * away_oreb + cf("to_rt") * away_to + cf("ast_rt") * away_ast,
      home_base = rtg_b_off + rtg_a_def + cf("poss_per_40") * pace +
        cf("days_rest") * home_rest + cf("travel") * home_dist + cf("neutral_site") * .neutral +
        cf("is_home") * (1 - .neutral) +
        cf("oreb_rt") * home_oreb + cf("to_rt") * home_to + cf("ast_rt") * home_ast,
      .phase = if (has_date) phase_of(game_date) else "4 Mar-Apr") %>%
    left_join(cal$phase_table %>% rename(.phase = phase), by = ".phase") %>%
    mutate(
      .raw_margin = (home_base - away_base) * pace / 100,
      .raw_total  = (home_base + away_base) * pace / 100,
      .mu_margin  = k * .raw_margin + cal$hfa * (1 - .neutral),
      .mu_total   = .raw_total + total_bias,
      .s   = sqrt((sd_margin^2 + sd_total^2) / 4),
      .rho = (sd_total^2 - sd_margin^2) / (sd_total^2 + sd_margin^2),
      .z1 = rnorm(n()), .z2 = rnorm(n()),
      .e_home = .s * .z1,
      .e_away = .s * (.rho * .z1 + sqrt(1 - .rho^2) * .z2),
      home_score = round((.mu_total + .mu_margin) / 2 + .e_home),
      away_score = round((.mu_total - .mu_margin) / 2 + .e_away),
      # Ties: a 5-minute overtime decides it (total points are already
      # calibrated to include overtime on average, so only the winner is drawn)
      .ot = home_score == away_score,
      .p_ot_home = pnorm(.mu_margin * (5 / 40) / (sd_margin * sqrt(5 / 40))),
      .ot_home = runif(n()) < .p_ot_home,
      home_score = ifelse(.ot & .ot_home, home_score + 1, home_score),
      away_score = ifelse(.ot & !.ot_home, away_score + 1, away_score),
      result = home_score - away_score)

  # same columns as the input (home_score / away_score / result now filled in)
  unplayed_games <- g[, union(names(games), c("home_score", "away_score", "result"))]

  games <- bind_rows(played_games, unplayed_games)
  list(teams = as_tibble(teams), games = as_tibble(games))
}

# Daily/weekly sims load process_games_wk_{sim_method}.rds and the tournament
# sims load process_games_{sim_method}.rds; the same function serves both.
saveRDS(process_games, "_Helper Files/Simulation Functions/process_games_wk_calibrated.rds")
saveRDS(process_games, "_Helper Files/Simulation Functions/process_games_calibrated.rds")
