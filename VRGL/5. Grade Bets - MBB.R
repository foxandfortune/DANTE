library(tidyverse)

setwd("~/GitHub/DANTE")
# Grades one day's VRGL bets (all_bets_{date}.rds) against final scores.
#
# Two ways of staking:
#   * One unit:      every bet risks 1 unit.
#   * Quarter Kelly: each bet risks 1/4 of the Kelly fraction of a bankroll of
#                    `bankroll` units (100 by default, so 1 unit = 1% of bankroll).
#                    All of a day's stakes come from the same starting bankroll.
#                    Bets the model gives no edge get no Kelly stake.
#
# Broken down by spread, moneyline, team totals and game totals.
# Run from the repo root (same as the daily scripts), after the day's games are final.

source("_Helper Files/season.R")

# Settings ------------------------------------------------------------------
grade_date  <- Sys.Date() - 1   # the slate to grade, e.g. as.Date("2026-11-03")
bankroll    <- 100              # units in the Kelly bankroll
kelly_mult  <- 0.25             # quarter Kelly
kelly_cap   <- 0.05             # most of the bankroll any one bet can risk (Inf = no cap)

# ESPN game IDs to leave out (bad odds etc.). 401929372 = Arkansas vs
# Arkansas-Pine Bluff on 2026-11-03, whose moneyline and team totals were posted
# on the wrong sides.
exclude_game_ids <- c(401929372)

save_graded <- TRUE

results_dir <- glue::glue("VRGL/Stats/Season Schedules/Results/{dante_season}")

# Load bets ------------------------------------------------------------------
bets <- readRDS(glue::glue("{results_dir}/all_bets_{grade_date}.rds"))

# Spread bets need a cover probability for Kelly. Bets files from 2026-11-04 on
# carry it (`cover`); for earlier files it's estimated from the projected margin
# and the calibrated margin sd for that part of the season.
sd_margin <- local({
  cal <- readRDS("VRGL/Stats/Team and Player Stats/Power Ratings/Weights/calibration_mbb.rds")
  m <- lubridate::month(grade_date)
  phase <- case_when(m %in% c(11)     ~ "1 Nov",
                     m %in% c(12)     ~ "2 Dec",
                     m %in% c(1, 2)   ~ "3 Jan-Feb",
                     TRUE             ~ "4 Mar-Apr")
  cal$phase_table$sd_margin[cal$phase_table$phase == phase]
})

spread_bets <- bets$spread
if (!"cover" %in% names(spread_bets)) {
  # result = projected points the team loses by; it covers if margin + spread > 0
  spread_bets <- spread_bets %>% mutate(cover = pnorm((spread - result) / sd_margin))
}

# One row per bet: what was bet, the line, payout per unit and model probability
all_bets <- bind_rows(
  bets$ml %>%
    transmute(game_id, category = "Moneyline", bet = "ML",
              team_id, team, line = NA_real_, odds = ml, payout, prob = win),
  spread_bets %>%
    transmute(game_id, category = "Spread", bet = "Spread",
              team_id, team, line = spread, odds = spread_line, payout, prob = cover),
  bets$tm_over %>%
    transmute(game_id, category = "Team total", bet = "Over",
              team_id, team, line = total, odds = tm_over, payout, prob = is_over),
  bets$tm_under %>%
    transmute(game_id, category = "Team total", bet = "Under",
              team_id, team, line = total, odds = tm_under, payout, prob = is_under),
  bets$game_over %>%
    transmute(game_id, category = "Game total", bet = "Over",
              team_id = NA_integer_, team = glue::glue("{away_tm} at {home_tm}"),
              line = total, odds = over, payout, prob = is_over),
  bets$game_under %>%
    transmute(game_id, category = "Game total", bet = "Under",
              team_id = NA_integer_, team = glue::glue("{away_tm} at {home_tm}"),
              line = total, odds = under, payout, prob = is_under)
) %>%
  mutate(game_id = as.character(game_id),
         team_id = as.integer(team_id),
         team = as.character(team))

excluded <- all_bets %>% filter(game_id %in% as.character(exclude_game_ids))
all_bets <- all_bets %>% filter(!game_id %in% as.character(exclude_game_ids))

# Final scores ------------------------------------------------------------------
# ESPN's scoreboard for the day (live), with the hoopR schedule as a backup for
# any game not on it.
from_scoreboard <- tryCatch(
  hoopR::espn_mbb_scoreboard(season = format(grade_date, "%Y%m%d")) %>%
    transmute(game_id    = as.character(game_id),
              home_id    = as.integer(home_team_id),
              away_id    = as.integer(away_team_id),
              home_score = as.numeric(home_score),
              away_score = as.numeric(away_score),
              final      = status_name == "STATUS_FINAL"),
  error = function(e) {
    message("Couldn't read the ESPN scoreboard: ", conditionMessage(e))
    NULL
  }
)

scores <- from_scoreboard
missing_ids <- setdiff(unique(all_bets$game_id), scores$game_id)

if (length(missing_ids) > 0) {
  from_schedule <- hoopR::load_mbb_schedule(seasons = dante_season) %>%
    filter(as.character(game_id) %in% missing_ids) %>%
    transmute(game_id    = as.character(game_id),
              home_id    = as.integer(home_id),
              away_id    = as.integer(away_id),
              home_score = as.numeric(home_score),
              away_score = as.numeric(away_score),
              final      = as.logical(status_type_completed))
  scores <- bind_rows(scores, from_schedule)
}

# Grade --------------------------------------------------------------------------
graded <- all_bets %>%
  left_join(scores, by = "game_id") %>%
  mutate(
    team_score = if_else(team_id == home_id, home_score, away_score),
    opp_score  = if_else(team_id == home_id, away_score, home_score),
    score = if_else(category == "Game total",
                    paste0(away_score, "-", home_score, " (", away_score + home_score, ")"),
                    paste0(team_score, "-", opp_score)),
    # > 0 wins, 0 pushes, < 0 loses
    edge = case_when(
      bet == "ML"                                ~ team_score - opp_score,
      bet == "Spread"                            ~ team_score - opp_score + line,
      category == "Team total" & bet == "Over"   ~ team_score - line,
      category == "Team total" & bet == "Under"  ~ line - team_score,
      category == "Game total" & bet == "Over"   ~ home_score + away_score - line,
      category == "Game total" & bet == "Under"  ~ line - (home_score + away_score)
    ),
    outcome = case_when(
      is.na(final) | !final ~ "Pending",
      edge > 0              ~ "Win",
      edge < 0              ~ "Loss",
      TRUE                  ~ "Push"
    ),
    # Profit per unit risked
    unit_profit = case_when(outcome == "Win"  ~ payout,
                            outcome == "Loss" ~ -1,
                            TRUE              ~ 0),
    # Kelly fraction: p - (1 - p) / b, with b the net payout per unit
    kelly_full  = prob - (1 - prob) / payout,
    kelly_stake = pmin(pmax(kelly_mult * kelly_full, 0), kelly_cap) * bankroll,
    kelly_profit = kelly_stake * unit_profit
  )

pending <- graded %>% filter(outcome == "Pending")
done    <- graded %>% filter(outcome != "Pending")

# Summary ------------------------------------------------------------------------
summarise_bets <- function(df) {
  df %>%
    summarise(
      bets         = n(),
      W            = sum(outcome == "Win"),
      L            = sum(outcome == "Loss"),
      P            = sum(outcome == "Push"),
      units        = round(sum(unit_profit), 2),
      roi          = round(sum(unit_profit) / n(), 3),
      kelly_bets   = sum(kelly_stake > 0),
      kelly_staked = round(sum(kelly_stake), 2),
      kelly_units  = round(sum(kelly_profit), 2),
      kelly_roi    = round(if_else(sum(kelly_stake) > 0,
                                   sum(kelly_profit) / sum(kelly_stake), NA_real_), 3),
      .groups = "drop"
    )
}

category_order <- c("Spread", "Moneyline", "Team total", "Game total")

summary_tbl <- bind_rows(
  done %>%
    mutate(category = factor(category, levels = category_order)) %>%
    group_by(category) %>%
    summarise_bets() %>%
    mutate(category = as.character(category)),
  done %>% summarise_bets() %>% mutate(category = "Total")
) %>%
  relocate(category)

# Output ---------------------------------------------------------------------------
cat(glue::glue("\nVRGL bets graded for {grade_date}"), "\n")
cat(glue::glue("One unit: 1 unit per bet. Quarter Kelly: units out of a {bankroll}-unit bankroll",
               "{if (is.finite(kelly_cap)) paste0(', max ', kelly_cap * bankroll, ' per bet') else ''}."),
    "\n\n")
print(as.data.frame(summary_tbl), row.names = FALSE)

if (nrow(excluded) > 0) {
  cat("\nLeft out (exclude_game_ids):", nrow(excluded), "bet(s) on game(s)",
      paste(unique(excluded$game_id), collapse = ", "), "\n")
}
if (nrow(pending) > 0) {
  cat("\nNot graded yet (game not final or not found):\n")
  print(as.data.frame(pending %>% select(game_id, category, bet, team, line, odds)), row.names = FALSE)
}

cat("\nBy bet:\n")
print(as.data.frame(
  done %>%
    arrange(factor(category, levels = category_order), desc(kelly_stake)) %>%
    transmute(category, bet, team, line, odds,
              prob = round(prob, 3), score,
              outcome, units = round(unit_profit, 2),
              kelly_stake = round(kelly_stake, 2), kelly_units = round(kelly_profit, 2))
), row.names = FALSE)

if (save_graded) {
  saveRDS(list(summary = summary_tbl, bets = graded),
          glue::glue("{results_dir}/graded_bets_{grade_date}.rds"))
}