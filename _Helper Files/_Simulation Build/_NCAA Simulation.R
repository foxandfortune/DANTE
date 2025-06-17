library(tidyverse)
library(gsubfn)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')

# Add NOT IN function:
`%!in%` = Negate(`%in%`)

# Set reference year
cur_yr <- 2025

# Sim method ------------------------
sim_method <- "to_oreb_ast"

# Load ratings ------------------
ratings <- readRDS(glue::glue("Stats/Power Ratings/Team Ratings/Inseason/inseason_ratings_all_{cur_yr}.rds"))
ratings_no_prior <- readRDS(glue::glue("Stats/Power Ratings/Team Ratings/Inseason/inseason_ratings_all_no_prior_{cur_yr}.rds"))

ratings <- list(
  pace = ratings$pace,
  ast = ratings$ast,
  oreb = ratings$oreb,
  to = ratings$to,
  rtg = ratings$rtg,
  
  disp_Rtg = 13
)

ratings_no_prior <- list(
  pace = ratings_no_prior$pace,
  ast = ratings_no_prior$ast,
  oreb = ratings_no_prior$oreb,
  to = ratings_no_prior$to,
  rtg = ratings_no_prior$rtg,
  
  disp_Rtg = 13
)

# Load teams -------------------
teams <- readRDS(glue::glue('Stats/Teams/team_database.rds')) %>% 
  mutate(team_id = as.integer(team_id))

## Reset the working directory so the simulation can work ---------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load functions ----------------------------------------------------------
double_games <- readRDS("Simulation Backup/Functions/double_games_wk.rds")
compute_league_ranks <- readRDS("Simulation Backup/Functions/compute_league_ranks.rds")
simulate_round <- readRDS("Simulation Backup/Functions/simulate_round_wk.rds")
simulate_ncaa <- readRDS("Simulation Backup/Functions/simulate_ncaa_wk.rds")
report <- readRDS("Simulation Backup/Functions/report.rds")
is_single_digit_numeric <- readRDS("Simulation Backup/Functions/is_single_digit_numeric.rds")
is_sequential <- readRDS("Simulation Backup/Functions/is_sequential.rds")

## Viz functions --------------------------------------
#table_theme <- readRDS('Simulation Backup/Functions/table_theme.rds')
#table_colors_positive <- readRDS('Simulation Backup/Functions/table_colors_positive.rds')
#summary.ncaa_simulation <- readRDS('Simulation Backup/Functions/summary.ncaa_simulation.rds')
#gt_fmt_pct_special <- readRDS('Simulation Backup/Functions/gt_fmt_pct_special.rds')
#fmt_pct_special <- readRDS('Simulation Backup/Functions/fmt_pct_special.rds')

# Load process_games and ratings based on sim method -------------------------------------------
process_games <- readRDS(glue::glue("Simulation Backup/Functions/process_games_wk_{sim_method}.rds"))

# Load schedule -----------------
schedule <- hoopR::load_mbb_schedule(seasons = {cur_yr}) %>% 
  filter(status_type_completed == FALSE,
         home_id %in% teams$team_id & away_id %in% teams$team_id) %>%
  filter(game_date >= Sys.Date(),
         game_date <= Sys.Date() + 0)

unique(schedule$game_date)

## Adjusted schedule -----------
adj_schedule <- readRDS(glue::glue('Season Simulation Backup/schedule_adj_{cur_yr}.rds'))

## Add travel/rest to schedule ---------
schedule <- schedule %>% 
  left_join(adj_schedule, by = c('game_date', 'game_id', 'home_id', 'away_id')) %>% 
  mutate(result = home_score - away_score) %>% 
  select(game_date, game_id,
         away_id, away_rest, away_dist, away_score,
         home_id, home_rest, home_dist, home_score,
         is_neutral = neutral_site,
         result)

# Check for errors
schedule %>% 
  filter(is.na(home_rest) | is.na(away_rest) | is.na(home_dist) | is.na(away_dist))

rating_ids <- ratings$rtg %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(team_id = str_remove_all(name, "team_id_")) %>% 
  pull(team_id)

schedule <- schedule %>% 
  filter(home_id %in% rating_ids & away_id %in% rating_ids)

##########################RUN SIMULATION #########################################################################
## Set seed ------------------
set.seed(214)

## Run it -------------------------------------
object <- simulate_ncaa(ncaa_season =  {cur_yr},
                     process_games = {process_games},
                     schedule = {schedule},
                     if_ended_today = FALSE,
                     fresh_season = TRUE,
                     ratings = {ratings},
                     simulations = 10000,
                     sim_include = "REG")

## Set seed ------------------
set.seed(214)

## Run again without priors -------------------------------------
object_no_prior <- simulate_ncaa(ncaa_season =  {cur_yr},
                                 process_games = {process_games},
                                 schedule = {schedule},
                                 if_ended_today = FALSE,
                                 fresh_season = TRUE,
                                 ratings = {ratings_no_prior},
                                 simulations = 10000,
                                 sim_include = "REG")

##########################GET ODDS############################################################################
library(jsonlite)

## Load odds functions ---------
get_odds <- readRDS('Simulation Backup/Functions/get_odds.rds')
double_odds <- readRDS('Simulation Backup/Functions/double_odds_wk.rds')
game_ids <- schedule %>% 
  select(game_id, game_date, team_id = home_id) %>% 
  bind_rows(schedule %>% 
              select(game_id, game_date, team_id = away_id))

## Get game dates --------
dates <- schedule %>% 
  select(game_date) %>% 
  distinct()

#i <- 1
#bk_name <- "Consensus"
#league_nm <- "ncaab"
#df <- dates

## Get odds ----------
odds <- get_odds(dates,
                 bk_name = "Consensus",
                 league_nm = 'ncaab')
 
# Team bets ---------------------------------------------
team_bets <- as.data.frame(object$games) %>% 
  double_odds() %>% 
  left_join(odds %>% 
              select(game_id, date,
                     espn_tm, team,
                     espn_opp, opponent,
                     ml, spread, spread_line, tm_total,
                     tm_over, tm_under) %>% 
              mutate(espn_tm = as.integer(espn_tm),
                     espn_opp = as.integer(espn_opp)), by = c("team_id" = "espn_tm",
                         "opp_id" = "espn_opp")) %>% 
  mutate(is_over = case_when(team_score > tm_total ~ 1,
                             TRUE ~ 0),
         is_under = case_when(team_score < tm_total ~ 1,
                              TRUE ~ 0),
         cover = case_when(result + spread > 0 ~ 1,
                           TRUE ~ 0)) %>% 
  with_groups(.groups = c(game_id, date,
                          team_id, team,
                          opp_id, opponent),
              summarise, 
              ml = mean(ml),
              win = mean(outcome),
              result = -median(result),
              spread = mean(spread),
              spread_line = mean(spread_line),
              cover = mean(cover),
              total = mean(tm_total, na.rm = TRUE),
              est_total = round(mean(team_score)/.5, 0) * 0.5,
              is_over = mean(is_over),
              is_under = mean(is_under),
              tm_over = mean(tm_over),
              tm_under = mean(tm_under)) %>% 
  mutate(ml_odds = odds.converter::odds.us2prob(ml),
         over_odds = odds.converter::odds.us2prob(tm_over),
         under_odds = odds.converter::odds.us2prob(tm_under),
         spread_odds = odds.converter::odds.us2prob(spread_line),
         ml_val = round(win - ml_odds, 3),
         over_val = round(is_over - over_odds, 3),
         under_val = round(is_under - under_odds, 3)) %>% 
  relocate(c(ml_odds, ml_val), .after = win) %>% 
  relocate(spread_odds, .after = spread_line) %>% 
  relocate(c(over_odds, over_val, tm_over), .after = is_over) %>% 
  relocate(c(under_odds, under_val), .after = is_under) %>% 
  filter(!is.na(team))

head(team_bets, 10)

## Moneyline bets ------------------------------------------
moneyline <- team_bets %>% 
  filter(ml_val >= 0) %>%
  mutate(payout = case_when(
    ml > 0 ~ ml / 100,
    ml < 0 ~ -100 / ml)) %>% 
  select(game_date = date, team_id, team,
         ml, payout, win, ml_odds,
         ml_val) %>% 
  left_join(game_ids, by = c("game_date", "team_id")) %>% 
  relocate(game_id)

print(moneyline %>% arrange(team))

moneyline %>% 
  filter(ml_val > .02, ml_val <=.11) %>% 
  arrange(team)

## Wrong team favored ----------------------
wtf <- team_bets %>% 
  filter(result < 0 & spread > 0) %>% 
  mutate(payout = case_when(
    ml > 0 ~ ml / 100,
    ml < 0 ~ -100 / ml)) %>% 
  select(game_date = date, team_id, team,
         result, spread, ml, payout) %>% 
  left_join(game_ids, by = c("game_date", "team_id")) %>% 
  relocate(game_id)

print(wtf)

## Spread bets --------------
spread <- team_bets %>% 
  mutate(spread_val = spread - result,
         payout = case_when(
           spread_line > 0 ~ spread_line / 100,
           spread_line < 0 ~ -100 / spread_line)) %>% 
  filter(spread_val >= 0.5) %>% 
  select(game_date = date, team_id, team,
         spread_line, payout, result, spread, spread_val) %>% 
  left_join(game_ids, by = c("game_date", "team_id")) %>% 
  relocate(game_id)

print(spread %>% arrange(team))
print(spread %>% filter(spread_val > 0.5) %>% arrange(game_date,
                                                      team))

## Team overs -----------------
tm_over <- team_bets %>%  
  filter(over_val > 0) %>% 
  mutate(payout = case_when(
    tm_over > 0 ~ tm_over / 100,
    tm_over < 0 ~ -100 / tm_over)) %>% 
  select(game_date = date, team_id, team,
         tm_over, payout, est_total, total,
         is_over, over_odds, over_val) %>% 
  left_join(game_ids, by = c("game_date", "team_id")) %>% 
  relocate(game_id)

## Team unders ---------
tm_under <- team_bets %>% 
  filter(under_val > 0) %>% 
  mutate(payout = case_when(
    tm_under > 0 ~ tm_under / 100,
    tm_under < 0 ~ -100 / tm_under)) %>% 
  select(game_date = date, team_id, team,
         tm_under, payout, est_total, total,
         is_under, under_odds, under_val) %>% 
  left_join(game_ids, by = c("game_date", "team_id")) %>% 
  relocate(game_id)

print(tm_over)
print(tm_under)

# Pull game bets ---------------------------------------
game_bets <- as.data.frame(object$games) %>% 
  select(-game_id) %>% 
  mutate(est_total = away_score + home_score) %>% 
  left_join(odds %>% 
              filter(type == "away") %>% 
              mutate(espn_tm = as.double(espn_tm),
                     espn_opp = as.double(espn_opp)) %>% 
              select(game_id, date, away_id = espn_tm, away_tm = team,
                     home_id = espn_opp, home_tm = opponent,
                     total, over, under), by = c("away_id", "home_id")) %>% 
  filter(!is.na(game_id)) %>% 
  mutate(is_over = case_when(est_total > total ~ TRUE,
                             TRUE ~  FALSE),
         is_under = case_when(est_total < total ~ TRUE,
                             TRUE ~  FALSE)) %>% 
  with_groups(.groups = c(game_id, date, away_id, away_tm, home_id, home_tm),
              summarise, 
              total = mean(total),
              est_total = round(mean(est_total)/.5, 0) * 0.5,
              is_over = mean(is_over),
              over = mean(over),
              is_under = mean(is_under),
              under = mean(under)) %>% 
  mutate(over_odds = odds.converter::odds.us2prob(over),
         under_odds = odds.converter::odds.us2prob(under),
         over_val = round(is_over - over_odds, 3),
         under_val = round(is_under - under_odds, 3)) %>% 
  relocate(c(over_odds, over_val), .before = over) %>% 
  relocate(c(under_odds, under_val), .before = under)
  
head(game_bets)

## Game overs ----------------------------
game_over <- game_bets %>% 
  mutate(total_val = abs(total - est_total)) %>% 
  filter(over_val > 0) %>% 
  mutate(payout = case_when(
    over > 0 ~ over / 100,
    over < 0 ~ -100 / over)) %>% 
  select(-game_id) %>% 
  left_join(schedule %>% 
              select(game_date, game_id, away_id, home_id),
            by = c("date" = "game_date", "away_id", "home_id")) %>% 
  select(game_id, game_date = date,
         away_id, away_tm, home_id, home_tm,
         over, payout, est_total, total, is_over, over_odds,
         total_val)

## Game unders -----------------------------
game_under <- game_bets %>% 
  mutate(total_val = abs(total - est_total)) %>% 
  filter(under_val > 0) %>% 
  mutate(payout = case_when(
    under > 0 ~ under / 100,
    under < 0 ~ -100 / under)) %>% 
  select(-game_id) %>% 
  left_join(schedule %>% 
              select(game_date, game_id, away_id, home_id),
            by = c("date" = "game_date", "away_id", "home_id")) %>% 
  select(game_id, game_date = date,
         away_id, away_tm, home_id, home_tm,
         under, payout, est_total, total, is_under, under_odds,
         total_val)

print(game_over)
print(game_under %>% arrange(away_tm))

# Combine all bets --------------------------------------
all_bets <- list(ml = {moneyline},
                 wtf = {wtf},
                 spread = {spread},
                 tm_over = {tm_over},
                 tm_under = {tm_under},
                 game_over = {game_over},
                 game_under = {game_under})

all_results <- as.data.frame(object$games) %>% 
  double_odds() %>% 
  left_join(schedule %>% 
              select(game_date, team_id = home_id, opp_id = away_id) %>% 
              bind_rows(schedule %>% 
                          select(game_date, team_id = away_id, opp_id = home_id)),
            by = c("team_id", "opp_id")) %>% 
  mutate(opp_score = team_score - result,
         game_total = team_score + opp_score) %>% 
  left_join(teams %>% 
              select(team_id, Team = team), by = "team_id") %>%
  left_join(teams %>% 
              select(opp_id = team_id, Opp = team), by = "opp_id") %>% 
  with_groups(.groups = c(team_id, Team,
                          opp_id, Opp, game_date),
              summarise, win = mean(outcome),
              result = -median(result),
              team_total = round(mean(team_score)/.5, 0) * 0.5,
              opp_total = round(mean(opp_score)/.5, 0) * 0.5,
              game_total = round(mean(game_total)/.5, 0) * 0.5) %>% 
  left_join(schedule %>% 
              select(game_date, team_id = home_id, game_id) %>% 
              bind_rows(schedule %>% 
                          select(game_date, team_id = away_id, game_id)),
            by = c("team_id", "game_date")) %>% 
  select(-game_date) %>% 
  left_join(hoopR::load_mbb_schedule() %>% 
              select(game_id, game_date = game_date_time), by = "game_id") %>% 
  relocate(c(game_date, game_date, game_id)) %>% 
  arrange(game_date, game_id)

all_results_no_prior <- as.data.frame(object_no_prior$games) %>% 
  double_odds() %>% 
  left_join(schedule %>% 
              select(game_date, team_id = home_id, opp_id = away_id) %>% 
              bind_rows(schedule %>% 
                          select(game_date, team_id = away_id, opp_id = home_id)),
            by = c("team_id", "opp_id")) %>% 
  mutate(opp_score = team_score - result,
         game_total = team_score + opp_score) %>% 
  left_join(teams %>% 
              select(team_id, Team = team), by = "team_id") %>%
  left_join(teams %>% 
              select(opp_id = team_id, Opp = team), by = "opp_id") %>% 
  with_groups(.groups = c(team_id, Team,
                          opp_id, Opp, game_date),
              summarise, win = mean(outcome),
              result = -median(result),
              team_total = round(mean(team_score)/.5, 0) * 0.5,
              opp_total = round(mean(opp_score)/.5, 0) * 0.5,
              game_total = round(mean(game_total)/.5, 0) * 0.5) %>% 
  left_join(schedule %>% 
              select(game_date, team_id = home_id, game_id) %>% 
              bind_rows(schedule %>% 
                          select(game_date, team_id = away_id, game_id)),
            by = c("team_id", "game_date")) %>% 
  select(-game_date) %>% 
  left_join(hoopR::load_mbb_schedule() %>% 
              select(game_id, game_date = game_date_time), by = "game_id") %>% 
  relocate(c(game_date, game_date, game_id)) %>% 
  arrange(game_date, game_id)

## Combine results with no prior
all_results <- all_results %>% 
  left_join(all_results_no_prior %>% 
              select(game_id, team_id,
                     win_noprior = win,
                     result_noprior = result), by = c("game_id", "team_id")) %>% 
  relocate(win_noprior, .after = win) %>% 
  relocate(result_noprior, .after = result)

## Save -----
saveRDS(all_bets,
        glue::glue('Season Simulation Backup/Bets/{cur_yr}/all_bets_{max(schedule$game_date)}.rds'))

saveRDS(all_results,
        glue::glue('Season Simulation Backup/Bets/{cur_yr}/all_results_{max(schedule$game_date)}.rds'))
