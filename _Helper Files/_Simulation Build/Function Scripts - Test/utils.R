# progress report using rlang to avoid usethis dependency
report <- function(msg,
                   ...,
                   .cli_fct = cli::cli_alert_info,
                   .envir = parent.frame()) {
  .cli_fct(c(format(Sys.time(), '%H:%M:%S'), " | ", msg), ..., .envir = .envir)
}

saveRDS(report, 'Simulation Backup/Functions/report.rds')

# this makes it so there's two rows per game (one/team)
double_games <- function(g) {
  g1 <- g %>%
    select(sim, round_name, round_no, away_id, home_id, result, winner_to) %>%
    rename(team_id = away_id, opp_id = home_id) %>%
    mutate(result = -1 * result)
  g2 <- g %>%
    select(sim, round_name, round_no, away_id, home_id, result, winner_to) %>%
    rename(team_id = home_id, opp_id = away_id)
  g <- bind_rows(g1, g2) %>%
    mutate(outcome = case_when(
      result > 0 ~ 1,
      result < 0 ~ 0,
      TRUE ~ NA_real_
    ))
  return(g)
}

saveRDS(double_games, 'Simulation Backup/Functions/double_games.rds')

is_single_digit_numeric <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x)

saveRDS(is_single_digit_numeric, 'Simulation Backup/Functions/is_single_digit_numeric.rds')

# Identify sessions with sequential future resolving
is_sequential <- function() inherits(future::plan(), "sequential")

saveRDS(is_sequential, 'Simulation Backup/Functions/is_sequential.rds')

