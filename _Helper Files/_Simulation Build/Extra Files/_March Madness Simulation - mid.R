library(tidyverse)
library(gsubfn)

# Set working directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Add NOT IN function:
`%!in%` = Negate(`%in%`)

# Set reference year
cur_yr <- 2023

# Sim method ------------------------
sim_method <- "rtg"

# Load functions ----------------------------------------------------------
double_games <- readRDS("Simulation Backup/Functions/double_games.rds")
#compute_conference_seeds <- readRDS("Simulation Backup/Functions/compute_conference_seeds.rds")
#simulate_round <- readRDS("Simulation Backup/Functions/simulate_round.rds")
#simulate_ncaa <- readRDS("Simulation Backup/Functions/simulate_ncaa.rds")
report <- readRDS("Simulation Backup/Functions/report.rds")
is_single_digit_numeric <- readRDS("Simulation Backup/Functions/is_single_digit_numeric.rds")
is_sequential <- readRDS("Simulation Backup/Functions/is_sequential.rds")
pull_results <- readRDS("Simulation Backup/Functions/pull_results.rds")

## Viz functions --------------------------------------
table_theme <- readRDS('Simulation Backup/Functions/table_theme.rds')
table_colors_positive <- readRDS('Simulation Backup/Functions/table_colors_positive.rds')
#summary.ncaa_simulation <- readRDS('Simulation Backup/Functions/summary.ncaa_simulation.rds')
gt_fmt_pct_special <- readRDS('Simulation Backup/Functions/gt_fmt_pct_special.rds')
fmt_pct_special <- readRDS('Simulation Backup/Functions/fmt_pct_special.rds')

# Load process_games and ratings based on sim method -------------------------------------------
if(sim_method == "rtg"){
  process_games <- readRDS("Simulation Backup/Functions/process_games_rtg.rds")
} else {
  process_games <- readRDS("Simulation Backup/Functions/process_games.rds")
}

if(sim_method == "rtg"){
  ratings <- readRDS(glue::glue("Simulation Backup/Power Ratings for Sim/madness_ratings_all_rtg_{cur_yr}.rds"))
} else {
  ratings <- readRDS(glue::glue("Simulation Backup/Power Ratings for Sim/madness_ratings_all_{cur_yr}.rds"))
}

# Load ratings and teams
teams <- readRDS(glue::glue('March Madness Backup/tourney_teams_{cur_yr}.rds'))


##########################RUN SIMULATION #########################################################################
object <- simulate_ncaa(ncaa_season =  {cur_yr},
                     process_games = {process_games},
                     playoff_seeds = 17,
                     left_bracket = c("South", "East"),
                     right_bracket = c("West", "Midwest"),
                     if_ended_today = FALSE,
                     fresh_tourney = TRUE,
                     ratings = {ratings},
                     simulations = 1000,
                     sim_include = "POST")


summary.ncaa_simulation(object) #%>% 
  gtExtras::gtsave_extra('test.png', vwidth = 2100)

# Get tournament results from hoopR schedule
res.actual <- hoopR::load_mbb_schedule(seasons = {cur_yr}) %>% 
  filter(tournament_id == 22) %>% 
  arrange(date)

res.actual <- pull_results(res.actual)
  
# Get overall results as data frame -----
overall <- as.data.frame(object$teams)

res_by_sim <- overall %>% 
  mutate(champ_corr = case_when(
    exit == 7 & team_id %in% res.actual$champ ~ TRUE,
    exit == 7 & team_id %!in% res.actual$champ ~ FALSE),
    f2_corr = case_when(
      exit >= 6 & team_id %in% res.actual$finalist ~ TRUE,
      exit >= 6 & team_id %!in% res.actual$finalist ~ FALSE),
    f4_corr = case_when(
      exit >= 5 & team_id %in% res.actual$final_4 ~ TRUE,
      exit >= 5 & team_id %!in% res.actual$final_4 ~ FALSE),
    e8_corr = case_when(
      exit >= 4 & team_id %in% res.actual$elite_8 ~ TRUE,
      exit >= 4 & team_id %!in% res.actual$elite_8 ~ FALSE),
    s16_corr = case_when(
      exit >= 3 & team_id %in% res.actual$sweet_16 ~ TRUE,
      exit >= 3 & team_id %!in% res.actual$sweet_16 ~ FALSE)) %>% 
  with_groups(.groups = sim, summarise,
              champ_tot = sum(champ_corr, na.rm = TRUE),
              f2_tot = sum(f2_corr, na.rm = TRUE),
              f4_tot = sum(f4_corr, na.rm = TRUE),
              e8_tot = sum(e8_corr, na.rm = TRUE),
              s16_tot = sum(s16_corr, na.rm = TRUE))


res_by_sim %>% 
  summarise(champ = mean(champ_tot),
            f2 = mean(f2_tot), f2_max = max(f2_tot),
            f4 = mean(f4_tot), f4_max = max(f4_tot),
            e8 = mean(e8_tot), e8_max = max(e8_tot),
            s16 = mean(s16_tot), s16_max = max(s16_tot))


res_by_sim %>% 
  ggplot(aes(x = s16_tot)) + geom_histogram()


as.data.frame(object$overall) %>% 
  left_join(teams %>% 
              select(team_id, seed, team_name), by = "team_id") %>% 
  arrange(region, desc(won_title)) %>% 
  relocate(c(team_name, seed), .after = team_id)

as.data.frame(object$games) %>% 
  filter(round_no == 3) %>% 
  with_groups(.groups = c(region, away_id, home_id), summarise,
              res = mean(result), res_mid = median(result),
              win = mean(result > 0)) %>% 
  left_join(teams %>% 
              select(team_id, away_seed = seed, away_name = team_name), by = c("away_id" = "team_id")) %>% 
  left_join(teams %>% 
              select(team_id, home_seed = seed, home_name = team_name), by = c("home_id" = "team_id")) %>% 
  select(region,
         away_seed, away_name,
         home_seed, home_name,
         res, res_mid, win) %>% 
  arrange(region, away_seed)
