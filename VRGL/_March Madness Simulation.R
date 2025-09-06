library(tidyverse)
library(gsubfn)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')

options(chromote.headless. = 'new')

# Add NOT IN function:
`%!in%` = Negate(`%in%`)

# Set reference year
cur_yr <- 2025

# Sim method ------------------------
sim_method <- "to_reb_ast"

# Load ratings ------------------
ratings <- readRDS(glue::glue("VRGL/Stats/Team and Player Stats/Power Ratings/Team Ratings/Inseason/inseason_ratings_all_{cur_yr}.rds"))

ratings <- list(
  pace = ratings$pace,
  ast = ratings$ast,
  oreb = ratings$oreb,
  to = ratings$to,
  rtg = ratings$rtg,
  
  disp_Rtg = 11
)

# Load functions ----------------------------------------------------------
double_games <- readRDS("_Helper Files/Simulation Functions/double_games.rds")
compute_conference_seeds <- readRDS("_Helper Files/Simulation Functions/compute_conference_seeds.rds")
simulate_round <- readRDS("_Helper Files/Simulation Functions/simulate_round.rds")
simulate_ncaa <- readRDS("_Helper Files/Simulation Functions/simulate_ncaa.rds")
report <- readRDS("_Helper Files/Simulation Functions/report.rds")
is_single_digit_numeric <- readRDS("_Helper Files/Simulation Functions/is_single_digit_numeric.rds")
is_sequential <- readRDS("_Helper Files/Simulation Functions/is_sequential.rds")
load_completed_games <- readRDS("_Helper Files/Simulation Functions/load_completed_games.rds")

## Viz functions --------------------------------------
table_theme <- readRDS('_Helper Files/Simulation Functions/table_theme.rds')
table_colors_positive <- readRDS('_Helper Files/Simulation Functions/table_colors_positive.rds')
summary.ncaa_simulation <- readRDS('_Helper Files/Simulation Functions/summary.ncaa_simulation.rds')
gt_fmt_pct_special <- readRDS('_Helper Files/Simulation Functions/gt_fmt_pct_special.rds')
fmt_pct_special <- readRDS('_Helper Files/Simulation Functions/fmt_pct_special.rds')
gt_dante_title <- readRDS('_Helper Files/Simulation Functions/gt_dante_title.rds')

# Load process_games and ratings based on sim method -------------------------------------------
process_games <- readRDS(glue::glue("_Helper Files/Simulation Functions/process_games_{sim_method}.rds"))

# Load ratings and teams
teams <- readRDS(glue::glue('_Helper Files/MM - Tourney and First Fours/MBB/tourney_teams_{cur_yr}.rds'))

##########################RUN SIMULATION #########################################################################
# Set seed ----
set.seed(421)

# Run it----
object <- simulate_ncaa(ncaa_season =  {cur_yr},
                        process_games = {process_games},
                        playoff_seeds = 17,
                        left_bracket = c("West", "South"),
                        right_bracket = c("East", "Midwest"),
                        if_ended_today = FALSE,
                        fresh_tourney = TRUE,
                        ratings = {ratings},
                        simulations = 100,
                        mbb_wbb = 'MBB',
                        sim_include = "POST")

## Save viz ----
summary.ncaa_simulation(object,
                        type = 'mbb') %>% 
  gtExtras::gtsave_extra(glue::glue('March Madness Backup/Visualizations/pre_tourney_{cur_yr}.png'),
                         vwidth = 2350)


object$games %>% 
  filter(round_no >= 4)

## Save object
saveRDS({object}, glue::glue('March Madness Backup/Sims/pre_tourney_{cur_yr}.rds'))
saveRDS({object}, glue::glue('March Madness Backup/Sims/pre_tourney_{cur_yr}.rds'))

##########################GET BRACKETS############################################################################
# Reload if needed ----
object <- readRDS(glue::glue('March Madness Backup/Sims/pre_tourney_{cur_yr}.rds'))

## Get list of teams with "reasonable" chance of winning -------
champ_tms <- data.frame(object$overall) %>% 
  mutate(won_title = round(won_title * 100, 0)) %>% 
  filter(won_title >= 3) %>% 
  select(region, team_id, won_title) %>% 
  left_join(teams %>% 
              select(team_id, team_name), by = c("team_id"))  %>% 
  arrange(desc(won_title))

#Print
champ_tms

# Go through each team -----
champ <- champ_tms$team_id[6]

## Print
print(teams$team_name[teams$team_id == champ])

## Get sims where that team wins ---------------------
sims <- data.frame(object$teams) %>% 
  filter(exit == 7, team_id == champ) %>% 
  select(sim)

### Sample 2x number of "wins" for that team from list of sims ----------
sim_sample <- sample(sims$sim,
                     size = 1,
                       #2 * champ_tms$won_title[champ_tms$team_id == champ],
       replace = FALSE)

samp_no <- 1

### Print results from each round /
data.frame(object$games) %>% 
  filter(sim == 38898, round_no >= 1) %>% 
  mutate(winner = case_when(
    result > 0 ~ home_id,
    result < 0 ~ away_id)) %>% 
  select(region, round_name, round_no, winner) %>% 
  left_join(teams %>% 
              select(team_id, team_name), by = c("winner" = "team_id")) %>% 
  arrange(round_no, region) %>% 
  select(round_no, round_name, region, team_name)

data.frame(object$games) %>% 
  filter(sim == sim_sample[samp_no], round_no >= 5) %>% 
  mutate(winner = case_when(
    result > 0 ~ home_id,
    result < 0 ~ away_id)) %>% 
  select(region, round_name, round_no, winner) %>% 
  left_join(teams %>% 
              select(team_id, team_name), by = c("winner" = "team_id")) %>% 
  arrange(round_no) %>% 
  select(round_no, round_name, region, team_name)


data.frame(object$games) %>% 
  filter(sim == sim_sample[samp_no], round_no == 6) %>% 
  mutate(total = home_score + away_score) %>% 
  select(total)


data.frame(object$games) %>% 
  left_join(teams %>% 
              select(away_id = team_id, away_name = team_name),
            by = c("away_id")) %>% 
  left_join(teams %>% 
              select(home_id = team_id, home_name = team_name),
            by = c("home_id")) %>% 
  select(region, round_no, round_name,
         away_seed, away_name, away_score,
         home_seed, home_name, home_score,
         result) %>% 
  mutate(total = away_score + home_score) %>% 
  with_groups(.groups = c(region, round_no, round_name,
                          away_seed, away_name,
                          home_seed, home_name), summarise,
              line = -median(result),
              win = round(100 * mean(result > 0), 1), total = mean(total)) %>% 
  arrange(region, home_seed) %>% 
  filter(round_no == 1)

################################## GET RESULTS #############################################
pull_results <- readRDS('Simulation Backup/Functions/pull_results.rds')


tourney <- hoopR::load_mbb_schedule(seasons = {cur_yr}) %>% 
  filter(tournament_id == 22)

results <- pull_results(tourney)

# Get overall results as data frame -----
overall <- as.data.frame(object$teams)

res_by_sim <- overall %>% 
  mutate(t32_corr = case_when(
      exit >= 2 & team_id %in% results$second_rd ~ TRUE,
      exit >= 2 & team_id %!in% results$second_rd ~ FALSE),
    s16_corr = case_when(
      exit >= 3 & team_id %in% results$sweet_16 ~ TRUE,
      exit >= 3 & team_id %!in% results$sweet_16 ~ FALSE),
    e8_corr = case_when(
      exit >= 4 & team_id %in% results$elite_8 ~ TRUE,
      exit >= 4 & team_id %!in% results$elite_8 ~ FALSE),
    f4_corr = case_when(
      exit >= 5 & team_id %in% results$final_4 ~ TRUE,
      exit >= 5 & team_id %!in% results$final_4 ~ FALSE),
    f2_corr = case_when(
      exit >= 6 & team_id %in% results$finalist ~ TRUE,
      exit >= 6 & team_id %!in% results$finalist ~ FALSE),
    champ_corr = case_when(
      exit == 7 & team_id %in% results$champ ~ TRUE,
      exit == 7 & team_id %!in% results$champ ~ FALSE)) %>% 
  with_groups(.groups = sim, summarise,
              t32_tot = sum(t32_corr, na.rm = TRUE),
              s16_tot = sum(s16_corr, na.rm = TRUE),
              e8_tot = sum(e8_corr, na.rm = TRUE),
              f4_tot = sum(f4_corr, na.rm = TRUE),
              f2_tot = sum(f2_corr, na.rm = TRUE),
              champ_tot = sum(champ_corr, na.rm = TRUE))


res_by_sim %>% 
  arrange(desc(t32_tot), desc(s16_tot),
          desc(e8_tot), desc(f4_tot),
          desc(f2_tot), desc(champ_tot))


res_by_sim %>% 
  summarise(champ = mean(champ_tot),
            f2 = mean(f2_tot), f2_max = max(f2_tot),
            f4 = mean(f4_tot), f4_max = max(f4_tot),
            e8 = mean(e8_tot), e8_max = max(e8_tot),
            s16 = mean(s16_tot), s16_max = max(s16_tot),
            t32 = mean(t32_tot), t32_max = max(t32_tot))


res_by_sim %>% 
  ggplot(aes(x = s16_tot)) + geom_histogram(bins = 29)
