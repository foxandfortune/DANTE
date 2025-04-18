library(cbbdata)
library(rstanarm)
library(bayesplot)
library(tidyverse)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')

# Negate in formula
`%!in%` = Negate(`%in%`)

# Set minimum year for now 
min_season <- 2020

# Log in to CBB Data ---------
cbbdata::cbd_login(username = 'foxandfortune',
                   password = 'Memphis#24')

# Load Teams 
teams <- readRDS('Teams/team_database.rds')
teams247 <- readRDS('Teams/teams247.rds')
teams_cbd <- as.data.frame(cbbdata::cbd_teams())

## Combine teams into simple table for joins
all_teams <- teams %>% 
  select(Team, team_id) %>% 
  left_join(teams_cbd %>% 
              select(espn_id, torvik_team), by = c("team_id" = "espn_id")) %>% 
  left_join(teams247, by = c("Team"))

# Create list of files in directory -----------------
files.ratings = unlist(map(list.files(path = 'Power Ratings/Team Ratings/Full Season',
                                      pattern = '.rds'),
                           ~glue::glue("Power Ratings/Team Ratings/Full Season/", ., sep = "")))

## Set up blank data frames --------
rating.pace <- data.frame()
rating.ast <- data.frame()
rating.oreb <- data.frame()
rating.to <- data.frame()
rating.rtg <- data.frame()
rating.efg <- data.frame()
rating.raw_rtg <- data.frame()

for(i in files.ratings){
  temp <- readRDS(i)
  
  yr <- as.numeric(str_remove(str_remove_all(i, '.*all_'), ".rds"))
  
  rating.pace <- bind_rows(rating.pace,
                           data.frame(temp$pace) %>% 
                             mutate(season = yr))
  
  rating.ast <- bind_rows(rating.ast,
                          data.frame(temp$ast) %>% 
                            mutate(season = yr))
  
  rating.oreb <- bind_rows(rating.oreb,
                           data.frame(temp$oreb) %>% 
                             mutate(season = yr))
  
  rating.to <- bind_rows(rating.to,
                         data.frame(temp$to) %>% 
                           mutate(season = yr))
  
  rating.rtg <- bind_rows(rating.rtg,
                          data.frame(temp$rtg) %>% 
                            mutate(season = yr))

  rating.efg <- bind_rows(rating.efg,
                          data.frame(temp$efg) %>% 
                            mutate(season = yr))
  
  rating.raw_rtg <- bind_rows(rating.raw_rtg,
                              data.frame(temp$raw_rating) %>%
                                mutate(season = yr))
  
  rm(temp, yr, i)
}

### Pace ---------
rating.pace <- rating.pace %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(name = str_remove_all(name, "team_id_")) %>% 
  rename(team_id = name,
         ortg = value) %>% 
  left_join(rating.pace %>% 
              filter(str_detect(name, "opp_id_")) %>% 
              mutate(name = str_remove_all(name, "opp_id_")) %>% 
              rename(team_id = name,
                     drtg = value),
            by = c("team_id", "season")) %>% 
  filter(season >= {min_season - 2})

### Rebounding ------------
rating.oreb <- rating.oreb %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(name = str_remove_all(name, "team_id_")) %>% 
  rename(team_id = name,
         ortg = value) %>% 
  left_join(rating.oreb %>% 
              filter(str_detect(name, "opp_id_")) %>% 
              mutate(name = str_remove_all(name, "opp_id_")) %>% 
              rename(team_id = name,
                     drtg = value),
            by = c("team_id", "season")) %>% 
  filter(season >= {min_season - 2})

### Assists --------------------------
rating.ast <- rating.ast %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(name = str_remove_all(name, "team_id_")) %>% 
  rename(team_id = name,
         ortg = value) %>% 
  left_join(rating.ast %>% 
              filter(str_detect(name, "opp_id_")) %>% 
              mutate(name = str_remove_all(name, "opp_id_")) %>% 
              rename(team_id = name,
                     drtg = value),
            by = c("team_id", "season")) %>% 
  filter(season >= {min_season - 2})

### Turnovers ---------------------
rating.to <- rating.to %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(name = str_remove_all(name, "team_id_")) %>% 
  rename(team_id = name,
         ortg = value) %>% 
  left_join(rating.to %>% 
              filter(str_detect(name, "opp_id_")) %>% 
              mutate(name = str_remove_all(name, "opp_id_")) %>% 
              rename(team_id = name,
                     drtg = value),
            by = c("team_id", "season")) %>% 
  filter(season >= {min_season - 2})

### Rating -------------------
rating.rtg <- rating.rtg %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(name = str_remove_all(name, "team_id_")) %>% 
  rename(team_id = name,
         ortg = value) %>% 
  left_join(rating.rtg %>% 
              filter(str_detect(name, "opp_id_")) %>% 
              mutate(name = str_remove_all(name, "opp_id_")) %>% 
              rename(team_id = name,
                     drtg = value),
            by = c("team_id", "season")) %>% 
  filter(season >= {min_season - 2})

### EFG -------------------
rating.efg <- rating.efg %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(name = str_remove_all(name, "team_id_")) %>% 
  rename(team_id = name,
         ortg = value) %>% 
  left_join(rating.efg %>% 
              filter(str_detect(name, "opp_id_")) %>% 
              mutate(name = str_remove_all(name, "opp_id_")) %>% 
              rename(team_id = name,
                     drtg = value),
            by = c("team_id", "season")) %>% 
  filter(season >= {min_season - 2})

### Raw Rating -------------------
rating.raw_rtg <- rating.raw_rtg %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(name = str_remove_all(name, "team_id_")) %>% 
  rename(team_id = name,
         ortg = value) %>% 
  left_join(rating.raw_rtg %>% 
              filter(str_detect(name, "opp_id_")) %>% 
              mutate(name = str_remove_all(name, "opp_id_")) %>% 
              rename(team_id = name,
                     drtg = value),
            by = c("team_id", "season")) %>% 
  filter(season >= {min_season - 2})

# Load Commits/Transfers/Draft Entrants -----------------
files.commits <- unlist(map(list.files(path = 'Recruiting', pattern = 'commits_clean'),
                            ~glue::glue("Recruiting/", ., sep = "")))

files.transfers <- unlist(map(list.files(path = 'Recruiting', pattern = 'transfers_clean'),
                              ~glue::glue("Recruiting/", ., sep = "")))

files.draftees <- unlist(map(list.files(path = 'Recruiting', pattern = 'draft_entrants_clean'),
                             ~glue::glue("Recruiting/", ., sep = "")))

files.graduates <- unlist(map(list.files(path = 'Recruiting', pattern = 'graduates_clean'),
                              ~glue::glue("Recruiting/", ., sep = "")))


commits <- map_df(files.commits, readRDS) %>% 
  filter(season >= {min_season})
transfers <- map_df(files.transfers, readRDS) %>% 
  filter(season >= {min_season})
draftees <- map_df(files.draftees, readRDS) %>% 
  filter(season >= {min_season})
graduates <- map_df(files.graduates, readRDS) %>% 
  filter(season >= {min_season})

## Clear files lists --------
rm(files.commits, files.draftees, files.graduates, files.ratings, files.transfers)

# Get player stats for seasons ----------------
seasons <- as.list(unique(commits$season))

player.stats <- pmap(seasons, cbbdata::cbd_torvik_player_season)

player.stats <- map_df(player.stats, bind_rows)

## Clean stats -----------------
player.stats <- player.stats %>%
  as.data.frame() %>% 
  rename(season = year) %>% 
  filter(season %in% seasons) %>% 
  mutate(match_name = nflreadr::clean_player_names(player),
         match_name = str_to_title(match_name),
         match_name = str_remove_all(match_name, "Jr "),
         match_name = str_remove_all(match_name, " Jr")) %>%
  left_join(all_teams, by = c("team" = "torvik_team"))

## Create team summary by year --------------
summary.team <- player.stats %>% 
  left_join(draftees %>%
              mutate(drafted = 1) %>% 
              select(season, match_name, team_id, drafted),
            by = c("season", "match_name", "team_id")) %>% 
  left_join(graduates %>% 
              mutate(grad = 1) %>% 
              select(season, match_name, team_id, grad),
            by = c("season", "match_name", "team_id")) %>% 
  left_join(transfers %>% 
              mutate(transfer = 1) %>% 
              select(season, match_name, team_id = transfromID, transfer),
            by = c("season", "match_name", "team_id")) %>% 
  mutate(left_school = case_when(
    drafted == 1 | grad == 1 | transfer ==1 ~ -1,
    TRUE ~ 0),
    total_min = g * mpg) %>%
  replace(is.na(.), 0) %>% 
  with_groups(.groups = c(season, team_id, Team, 
                          left_school),
              summarise,
              players = n_distinct(match_name),
              minutes = sum(total_min),
              oreb_rate = weighted.mean(oreb_rate, total_min),
              dreb_rate = weighted.mean(dreb_rate, total_min),
              ast = weighted.mean(ast, total_min),
              tov = weighted.mean(tov, total_min),
              stl = weighted.mean(stl, total_min),
              efg = weighted.mean(efg, total_min),
              obpm = weighted.mean(obpm, total_min),
              dbpm = weighted.mean(dbpm, total_min)) %>% 
  with_groups(.groups = c(season, team_id),
              mutate,
              pct_minutes = minutes / sum(minutes)) %>% 
  relocate(pct_minutes, .after = minutes) %>% 
  filter(team_id != 0)

# Create summary of Commits, Transfers, Drafted/Graduated ----------
## Commits ---------
summary.commit <- commits %>% 
  with_groups(.groups = c(season, team_id),
              summarise,
              commits = n_distinct(player),
              avg_rtg = mean(rating),
              star_5 = sum(class == 1),
              star_4 = sum(class == 2),
              star_3 = sum(class == 3),
              star_2 = sum(class == 4),
              star_1 = sum(class == 5),
              max_rtg = max(rating)) %>% 
  mutate(wtd_stars = (5 * star_5 + 4 * star_4 + 3 * star_3 +
           2 * star_2 + star_1) / commits) %>% 
  with_groups(.groups = season,
              mutate,
              min_ = min(avg_rtg), max_ = max(avg_rtg),
              rtg_scaled = (avg_rtg - min_) / (max_ - min_))

## Transfers ---------
summary.transfers <- transfers %>% 
  replace(is.na(.), 0) %>% 
  with_groups(.groups = c(season, transtoID),
              summarise,
              transfers = n_distinct(match_name),
              minutes = sum(total_min),
              avg_rtg = mean(rating),
              max_rtg = max(rating),
              star_5 = sum(class == 1),
              star_4 = sum(class == 2),
              star_3 = sum(class == 3),
              star_2 = sum(class == 4),
              star_1 = sum(class == 5),
              oreb_rate = weighted.mean(oreb_rate, total_min),
              dreb_rate = weighted.mean(dreb_rate, total_min),
              ast = weighted.mean(ast, total_min),
              tov = weighted.mean(tov, total_min),
              stl = weighted.mean(stl, total_min),
              efg = weighted.mean(efg, total_min),
              obpm = weighted.mean(obpm, total_min),
              dbpm = weighted.mean(dbpm, total_min)) %>%
  replace(is.na(.), 0) %>%
  mutate(wtd_stars = (5 * star_5 + 4 * star_4 + 3 * star_3 +
                        2 * star_2 + star_1) / transfers) %>% 
  with_groups(.groups = season,
              mutate,
              min_ = min(avg_rtg), max_ = max(avg_rtg),
              rtg_scaled = (avg_rtg - min_) / (max_ - min_))

## Create the data base for models ------------------
offseason_summary <- summary.team %>% 
  mutate(status = case_when(
    left_school == -1 ~ "left",
    left_school == 0 ~ "stay")) %>% 
  select(-left_school) %>%
  pivot_wider(names_from = c(status), values_from = c(players:dbpm)) %>% 
  mutate_at(vars(contains("_left")), ~ . * -1) %>%
  left_join(summary.commit,
            by = c("season", "team_id")) %>% 
  left_join(summary.transfers %>% 
              rename(team_id = transtoID),
            by = c("season", "team_id"),
            suffix = c("_cmt", "_tfr")) %>% 
  replace(is.na(.), 0)

glimpse(offseason_summary)

# Build models-------------------------------------------------------------------
## Pace -------------------------------------------------------------------
pace_data <- rating.pace %>% 
  select(season, team_id, ortg, drtg) %>% 
  mutate(next_season = season + 1,
         last_season = season - 1,
         last2_season = season - 2) %>% 
  left_join(rating.pace %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  left_join(rating.pace %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "last_season" = "season"),
            suffix = c("", "_l1")) %>% 
  left_join(rating.pace %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "last2_season" = "season"),
            suffix = c("", "_l2")) %>% 
  left_join(offseason_summary, by = c("season", "team_id")) %>% 
  filter(!is.na(ortg_l1), !is.na(ortg_l2),
         !is.na(ortg_next), !is.na(drtg_next)) %>%
  replace(is.na(.), 0) %>% 
  select(-c(next_season:last2_season)) %>% 
  relocate(contains("ortg"), .after = team_id)

### Create offense model -------------------------------------
## Single season
pace_model <- rstanarm::stan_glm(formula = ortg_next ~ ortg +
                                   factor(team_id),
                                 data = pace_data,
                                 algorithm = "sampling",
                                 iter = 5000)

## Multi-season
pace_model_multi <- rstanarm::stan_glm(formula = ortg_next ~ ortg +
                                         pct_minutes_stay + 
                                         commits + rtg_scaled_cmt +
                                         transfers + rtg_scaled_tfr +
                                         factor(team_id),
                                       data = pace_data,
                                       algorithm = "sampling",
                                       iter = 5000)

### Posterior draws ---------
pace_post <- posterior_predict(pace_model,
                               draws = 500)

pace_post_multi <- posterior_predict(pace_model_multi,
                                     draws = 500)

summary(pace_model)

# Set color scheme
color_scheme_set(scheme = "brightblue")

## Graph comparison
ppc_dens_overlay(y = pace_data$ortg,
                 yrep = pace_post)

ppc_dens_overlay(y = pace_data$ortg,
                 yrep = pace_post_multi)

# Compare error on posterior draws ---------------
error_pace <- data.frame(obs = pace_post) %>%
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

error_pace_multi <- data.frame(obs = pace_post_ungrp) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

## Collect summary data 
summary_pace <- error_pace %>% 
  left_join(error_pace_multi, by = "name",
            suffix = c("", "_multi")) %>% 
  bind_cols(data.frame(actual = pace_data$ortg)) %>% 
  summarise(rmse_single = Metrics::rmse(actual, estimate),
            rmse_multi = Metrics::rmse(actual, estimate_multi),
            cor_single = cor(actual, estimate),
            cor_multi = cor(actual, estimate_multi)) %>% 
  mutate(score_single = case_when(
    rmse_single <= rmse_multi & cor_single >= cor_multi ~ 2,
    rmse_single <= rmse_multi | cor_single >= cor_multi ~ 1,
    TRUE ~ 0),
    score_multi = 2 - score_single) %>% 
  mutate(score_single = case_when(
    score_single == score_multi & rmse_single <= rmse_multi ~ score_single + 0.5,
    TRUE ~ score_single),
    score_multi = case_when(
      score_single == score_multi & rmse_multi < rmse_single ~ score_multi + 0.5,
      TRUE ~ score_multi))

# Print
summary_pace

# Collect final model
pace_final_off <- if(summary_pace$score_single > summary_pace$score_multi){
  pace_model
} else {
  pace_model_multi
}

## Clean environment 
rm(pace_model, pace_model_multi,
   pace_post, pace_post_multi,
   error_pace, error_pace_multi,
   summary_pace)

### Create defense model ---------------------------
## Single season
pace_model <- rstanarm::stan_glm(formula = drtg_next ~ drtg +
                                   factor(team_id),
                                 data = pace_data,
                                 algorithm = "sampling",
                                 iter = 5000)

## Multi-season
pace_model_multi <- rstanarm::stan_glm(formula = drtg_next ~ drtg + drtg_l1 + drtg_l2 +
                                         factor(team_id),
                                       data = pace_data,
                                       algorithm = "sampling",
                                       iter = 5000)

### Posterior draws ---------
pace_post <- posterior_predict(pace_model,
                               draws = 500)

pace_post_multi <- posterior_predict(pace_model_multi,
                                     draws = 500)

# Set color scheme
color_scheme_set(scheme = "brightblue")

## Graph comparison
ppc_dens_overlay(y = pace_data$drtg,
                 yrep = pace_post)

ppc_dens_overlay(y = pace_data$drtg,
                 yrep = pace_post_multi)

# Compare error on posterior draws ---------------
error_pace <- data.frame(obs = pace_post) %>%
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

error_pace_multi <- data.frame(obs = pace_post_multi) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

## Collect summary data 
summary_pace <- error_pace %>% 
  left_join(error_pace_multi, by = "name",
            suffix = c("", "_multi")) %>% 
  bind_cols(data.frame(actual = pace_data$drtg)) %>% 
  summarise(rmse_single = Metrics::rmse(actual, estimate),
            rmse_multi = Metrics::rmse(actual, estimate_multi),
            cor_single = cor(actual, estimate),
            cor_multi = cor(actual, estimate_multi)) %>% 
  mutate(score_single = case_when(
    rmse_single <= rmse_multi & cor_single >= cor_multi ~ 2,
    rmse_single <= rmse_multi | cor_single >= cor_multi ~ 1,
    TRUE ~ 0),
    score_multi = 2 - score_single) %>% 
  mutate(score_single = case_when(
    score_single == score_multi & rmse_single <= rmse_multi ~ score_single + 0.5,
    TRUE ~ score_single),
    score_multi = case_when(
      score_single == score_multi & rmse_multi < rmse_single ~ score_multi + 0.5,
      TRUE ~ score_multi))

# Print
summary_pace

# Collect final model
pace_final_def <- if(summary_pace$score_single > summary_pace$score_multi){
  pace_model
} else {
  pace_model_multi
}

## Clean environment 
rm(pace_model, pace_model_multi,
   pace_post, pace_post_multi,
   error_pace, error_pace_multi,
   summary_pace,
   # Remove the overall data to prevent any confusion
   pace_data)


## Rebounding -------------------------------------------------------------------
oreb_data <- rating.oreb %>% 
  select(season, team_id, ortg, drtg) %>% 
  mutate(next_season = season + 1,
         last_season = season - 1,
         last2_season = season - 2) %>% 
  left_join(rating.oreb %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  left_join(rating.oreb %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "last_season" = "season"),
            suffix = c("", "_l1")) %>% 
  left_join(rating.oreb %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "last2_season" = "season"),
            suffix = c("", "_l2")) %>% 
  left_join(offseason_summary, by = c("season", "team_id")) %>% 
  filter(!is.na(ortg_l1), !is.na(ortg_l2)) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(next_season:last2_season)) %>% 
  relocate(contains("ortg"), .after = team_id)


### Create offense model -------------------------------------
## Single season
oreb_model <- rstanarm::stan_glm(formula = ortg_next ~ ortg +
                                   factor(team_id),
                                 data = oreb_data,
                                 algorithm = "sampling",
                                 iter = 5000)

## Multi-season
oreb_model_multi <- rstanarm::stan_glm(formula = ortg_next ~ ortg + 
                                         pct_minutes_stay + oreb_rate_stay +
                                         commits + rtg_scaled_cmt +
                                         transfers + rtg_scaled_tfr + oreb_rate +
                                         factor(team_id),
                                       data = oreb_data,
                                       algorithm = "sampling",
                                       iter = 5000)

## Model summaries
summary(oreb_model)
summary(oreb_model_multi)

### Posterior draws ---------
oreb_post <- posterior_predict(oreb_model,
                               draws = 500)

oreb_post_multi <- posterior_predict(oreb_model_multi,
                                     draws = 500)

# Set color scheme
color_scheme_set(scheme = "brightblue")

## Graph comparison
ppc_dens_overlay(y = oreb_data$ortg,
                 yrep = oreb_post)

ppc_dens_overlay(y = oreb_data$ortg,
                 yrep = oreb_post_multi)

# Compare error on posterior draws ---------------
error_oreb <- data.frame(obs = oreb_post) %>%
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

error_oreb_multi <- data.frame(obs = oreb_post_multi) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

## Collect summary data 
summary_oreb <- error_oreb %>% 
  left_join(error_oreb_multi, by = "name",
            suffix = c("", "_multi")) %>% 
  bind_cols(data.frame(actual = oreb_data$ortg)) %>% 
  summarise(rmse_single = Metrics::rmse(actual, estimate),
            rmse_multi = Metrics::rmse(actual, estimate_multi),
            cor_single = cor(actual, estimate),
            cor_multi = cor(actual, estimate_multi)) %>% 
  mutate(score_single = case_when(
    rmse_single <= rmse_multi & cor_single >= cor_multi ~ 2,
    rmse_single <= rmse_multi | cor_single >= cor_multi ~ 1,
    TRUE ~ 0),
    score_multi = 2 - score_single) %>% 
  mutate(score_single = case_when(
    score_single == score_multi & rmse_single <= rmse_multi ~ score_single + 0.5,
    TRUE ~ score_single),
    score_multi = case_when(
      score_single == score_multi & rmse_multi < rmse_single ~ score_multi + 0.5,
      TRUE ~ score_multi))

# Print
summary_oreb

# Collect final model
oreb_final_off <- if(summary_oreb$score_single > summary_oreb$score_multi){
  oreb_model
} else {
  oreb_model_multi
}

# I want to manually override lol
#oreb_final_off <- oreb_model_multi

## Clean environment 
rm(oreb_model, oreb_model_multi,
   oreb_post, oreb_post_multi,
   error_oreb, error_oreb_multi,
   summary_oreb)

### Create defense model ---------------------------
## Single season
dreb_model <- rstanarm::stan_glm(formula = drtg_next ~ drtg +
                                   factor(team_id),
                                 data = oreb_data,
                                 algorithm = "sampling",
                                 iter = 5000)

## Multi-season
dreb_model_multi <- rstanarm::stan_glm(formula = drtg_next ~ drtg +
                                         pct_minutes_stay + dreb_rate_stay +
                                         commits + rtg_scaled_cmt +
                                         transfers + rtg_scaled_tfr + dreb_rate +
                                         factor(team_id),
                                       data = oreb_data,
                                       algorithm = "sampling",
                                       iter = 5000)

### Posterior draws ---------
dreb_post <- posterior_predict(dreb_model,
                               draws = 500)

dreb_post_multi <- posterior_predict(dreb_model_multi,
                                     draws = 500)

# Set color scheme
color_scheme_set(scheme = "brightblue")

## Graph comparison
ppc_dens_overlay(y = oreb_data$drtg,
                 yrep = dreb_post)

ppc_dens_overlay(y = oreb_data$drtg,
                 yrep = dreb_post_multi)

# Compare error on posterior draws ---------------
error_dreb <- data.frame(obs = dreb_post) %>%
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

error_dreb_multi <- data.frame(obs = dreb_post_multi) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

## Collect summary data 
summary_dreb <- error_dreb %>% 
  left_join(error_dreb_multi, by = "name",
            suffix = c("", "_multi")) %>% 
  bind_cols(data.frame(actual = oreb_data$drtg)) %>% 
  summarise(rmse_single = Metrics::rmse(actual, estimate),
            rmse_multi = Metrics::rmse(actual, estimate_multi),
            cor_single = cor(actual, estimate),
            cor_multi = cor(actual, estimate_multi)) %>% 
  mutate(score_single = case_when(
    rmse_single <= rmse_multi & cor_single >= cor_multi ~ 2,
    rmse_single <= rmse_multi | cor_single >= cor_multi ~ 1,
    TRUE ~ 0),
    score_multi = 2 - score_single) %>% 
  mutate(score_single = case_when(
    score_single == score_multi & rmse_single <= rmse_multi ~ score_single + 0.5,
    TRUE ~ score_single),
    score_multi = case_when(
      score_single == score_multi & rmse_multi < rmse_single ~ score_multi + 0.5,
      TRUE ~ score_multi))

# Print
summary_dreb

# Collect final model
dreb_final_def <- if(summary_dreb$score_single > summary_dreb$score_multi){
  dreb_model
} else {
  dreb_model_multi
}

# If I want to manually override lol
#dreb_final_def <- dreb_model_multi

## Clean environment 
rm(dreb_model, dreb_model_multi,
   dreb_post, dreb_post_multi,
   error_dreb, error_dreb_multi,
   summary_dreb,
   oreb_data)


## Turnovers -------------------------------------------------------------------
to_data <- rating.to %>% 
  select(season, team_id, ortg, drtg) %>% 
  mutate(next_season = season + 1,
         last_season = season - 1,
         last2_season = season - 2) %>% 
  left_join(rating.to %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  left_join(rating.to %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "last_season" = "season"),
            suffix = c("", "_l1")) %>% 
  left_join(rating.to %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "last2_season" = "season"),
            suffix = c("", "_l2")) %>% 
  left_join(offseason_summary, by = c("season", "team_id")) %>% 
  filter(!is.na(ortg_l1), !is.na(ortg_l2)) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(next_season:last2_season)) %>% 
  relocate(contains("ortg"), .after = team_id)


### Create offense model -------------------------------------
## Single season
to_model <- rstanarm::stan_glm(formula = ortg_next ~ ortg +
                                   factor(team_id),
                                 data = to_data,
                                 algorithm = "sampling",
                                 iter = 5000)

## Multi-season
to_model_multi <- rstanarm::stan_glm(formula = ortg_next ~ ortg + #ortg_l1 + ortg_l2 +
                                       pct_minutes_stay + tov_stay +
                                       commits + rtg_scaled_cmt +
                                       transfers + rtg_scaled_tfr + tov +
                                       factor(team_id),
                                     data = to_data,
                                     algorithm = "sampling",
                                     iter = 5000)

## Model summaries
summary(to_model)
summary(to_model_multi)

### Posterior draws ---------
to_post <- posterior_predict(to_model,
                             draws = 500)

to_post_multi <- posterior_predict(to_model_multi,
                                   draws = 500)

# Set color scheme
color_scheme_set(scheme = "brightblue")

## Graph comparison
ppc_dens_overlay(y = to_data$ortg,
                 yrep = to_post)

ppc_dens_overlay(y = to_data$ortg,
                 yrep = to_post_multi)

# Compare error on posterior draws ---------------
error_to <- data.frame(obs = to_post) %>%
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

error_to_multi <- data.frame(obs = to_post_multi) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

## Collect summary data 
summary_to <- error_to %>% 
  left_join(error_to_multi, by = "name",
            suffix = c("", "_multi")) %>% 
  bind_cols(data.frame(actual = to_data$ortg)) %>% 
  summarise(rmse_single = Metrics::rmse(actual, estimate),
            rmse_multi = Metrics::rmse(actual, estimate_multi),
            cor_single = cor(actual, estimate),
            cor_multi = cor(actual, estimate_multi)) %>% 
  mutate(score_single = case_when(
    rmse_single <= rmse_multi & cor_single >= cor_multi ~ 2,
    rmse_single <= rmse_multi | cor_single >= cor_multi ~ 1,
    TRUE ~ 0),
    score_multi = 2 - score_single) %>% 
  mutate(score_single = case_when(
    score_single == score_multi & rmse_single <= rmse_multi ~ score_single + 0.5,
    TRUE ~ score_single),
    score_multi = case_when(
      score_single == score_multi & rmse_multi < rmse_single ~ score_multi + 0.5,
      TRUE ~ score_multi))

# Print
summary_to

# Collect final model
to_final_off <- if(summary_to$score_single > summary_to$score_multi){
  to_model
} else {
  to_model_multi
}

# I want to manually override lol
#to_final_off <- to_model_multi

## Clean environment 
rm(to_model, to_model_multi,
   to_post, to_post_multi,
   error_to, error_to_multi,
   summary_to)

### Create defense model ---------------------------
## Single season
to_model <- rstanarm::stan_glm(formula = drtg_next ~ drtg +
                                   factor(team_id),
                                 data = to_data,
                                 algorithm = "sampling",
                                 iter = 5000)

## Multi-season
to_model_multi <- rstanarm::stan_glm(formula = drtg_next ~ drtg + #drtg_l1 + drtg_l2 +
                                       pct_minutes_stay + stl_stay +
                                       commits + rtg_scaled_cmt +
                                       transfers + rtg_scaled_tfr + stl +
                                       factor(team_id),
                                     data = to_data,
                                     algorithm = "sampling",
                                     iter = 5000)

### Posterior draws ---------
to_post <- posterior_predict(to_model,
                             draws = 500)

to_post_multi <- posterior_predict(to_model_multi,
                                   draws = 500)

# Set color scheme
color_scheme_set(scheme = "brightblue")

## Graph comparison
ppc_dens_overlay(y = to_data$drtg,
                 yrep = to_post)

ppc_dens_overlay(y = to_data$drtg,
                 yrep = to_post_multi)

# Compare error on posterior draws ---------------
error_to <- data.frame(obs = to_post) %>%
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

error_to_multi <- data.frame(obs = to_post_multi) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

## Collect summary data 
summary_to <- error_to %>% 
  left_join(error_to_multi, by = "name",
            suffix = c("", "_multi")) %>% 
  bind_cols(data.frame(actual = to_data$drtg)) %>% 
  summarise(rmse_single = Metrics::rmse(actual, estimate),
            rmse_multi = Metrics::rmse(actual, estimate_multi),
            cor_single = cor(actual, estimate),
            cor_multi = cor(actual, estimate_multi)) %>% 
  mutate(score_single = case_when(
    rmse_single <= rmse_multi & cor_single >= cor_multi ~ 2,
    rmse_single <= rmse_multi | cor_single >= cor_multi ~ 1,
    TRUE ~ 0),
    score_multi = 2 - score_single) %>% 
  mutate(score_single = case_when(
    score_single == score_multi & rmse_single <= rmse_multi ~ score_single + 0.5,
    TRUE ~ score_single),
    score_multi = case_when(
      score_single == score_multi & rmse_multi < rmse_single ~ score_multi + 0.5,
      TRUE ~ score_multi))

# Print
summary_to

# Collect final model
to_final_def <- if(summary_to$score_single > summary_to$score_multi){
  to_model
} else {
  to_model_multi
}

# If I want to manually override lol
#to_final_def <- to_model_multi



## Clean environment 
rm(to_model, to_model_multi,
   to_post, to_post_multi,
   error_to, error_to_multi,
   summary_to,
   to_data)


## Assists -------------------------------------------------------------------
ast_data <- rating.ast %>% 
  select(season, team_id, ortg, drtg) %>% 
  mutate(next_season = season + 1,
         last_season = season - 1,
         last2_season = season - 2) %>% 
  left_join(rating.ast %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  left_join(rating.ast %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "last_season" = "season"),
            suffix = c("", "_l1")) %>% 
  left_join(rating.ast %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "last2_season" = "season"),
            suffix = c("", "_l2")) %>% 
  left_join(offseason_summary, by = c("season", "team_id")) %>% 
  filter(!is.na(ortg_l1), !is.na(ortg_l2)) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(next_season:last2_season)) %>% 
  relocate(contains("ortg"), .after = team_id)


### Create offense model -------------------------------------
## Single season
ast_model <- rstanarm::stan_glm(formula = ortg_next ~ ortg +
                                 factor(team_id),
                               data = ast_data,
                               algorithm = "sampling",
                               iter = 5000)

## Multi-season
ast_model_multi <- rstanarm::stan_glm(formula = ortg_next ~ ortg + #ortg_l1 + ortg_l2 +
                                       pct_minutes_stay + ast_stay +
                                       commits + rtg_scaled_cmt +
                                       transfers + rtg_scaled_tfr + ast +
                                       factor(team_id),
                                     data = ast_data,
                                     algorithm = "sampling",
                                     iter = 5000)

## Model summaries
summary(ast_model)
summary(ast_model_multi)

### Posterior draws ---------
ast_post <- posterior_predict(ast_model,
                             draws = 500)

ast_post_multi <- posterior_predict(ast_model_multi,
                                   draws = 500)

# Set color scheme
color_scheme_set(scheme = "brightblue")

## Graph comparison
ppc_dens_overlay(y = ast_data$ortg,
                 yrep = ast_post)

ppc_dens_overlay(y = ast_data$ortg,
                 yrep = ast_post_multi)

# Compare error on posterior draws ---------------
error_ast <- data.frame(obs = ast_post) %>%
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

error_ast_multi <- data.frame(obs = ast_post_multi) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

## Collect summary data 
summary_ast <- error_ast %>% 
  left_join(error_ast_multi, by = "name",
            suffix = c("", "_multi")) %>% 
  bind_cols(data.frame(actual = ast_data$ortg)) %>% 
  summarise(rmse_single = Metrics::rmse(actual, estimate),
            rmse_multi = Metrics::rmse(actual, estimate_multi),
            cor_single = cor(actual, estimate),
            cor_multi = cor(actual, estimate_multi)) %>% 
  mutate(score_single = case_when(
    rmse_single <= rmse_multi & cor_single >= cor_multi ~ 2,
    rmse_single <= rmse_multi | cor_single >= cor_multi ~ 1,
    TRUE ~ 0),
    score_multi = 2 - score_single) %>% 
  mutate(score_single = case_when(
    score_single == score_multi & rmse_single <= rmse_multi ~ score_single + 0.5,
    TRUE ~ score_single),
    score_multi = case_when(
      score_single == score_multi & rmse_multi < rmse_single ~ score_multi + 0.5,
      TRUE ~ score_multi))

# Print
summary_ast

# Collect final model
ast_final_off <- if(summary_ast$score_single > summary_ast$score_multi){
  ast_model
} else {
  ast_model_multi
}

# I want to manually override lol
#ast_final_off <- ast_model_multi

## Clean environment 
rm(ast_model, ast_model_multi,
   ast_post, ast_post_multi,
   error_ast, error_ast_multi,
   summary_ast)

### Create defense model ---------------------------
## Single season
ast_model <- rstanarm::stan_glm(formula = drtg_next ~ drtg +
                                 factor(team_id),
                               data = ast_data,
                               algorithm = "sampling",
                               iter = 5000)

## Multi-season
ast_model_multi <- rstanarm::stan_glm(formula = drtg_next ~ drtg + #drtg_l1 + drtg_l2 +
                                       pct_minutes_stay + dbpm_stay +
                                       commits + rtg_scaled_cmt +
                                       transfers + rtg_scaled_tfr + dbpm +
                                       factor(team_id),
                                     data = ast_data,
                                     algorithm = "sampling",
                                     iter = 5000)

### Posterior draws ---------
ast_post <- posterior_predict(ast_model,
                             draws = 500)

ast_post_multi <- posterior_predict(ast_model_multi,
                                   draws = 500)

# Set color scheme
color_scheme_set(scheme = "brightblue")

## Graph comparison
ppc_dens_overlay(y = ast_data$drtg,
                 yrep = ast_post)

ppc_dens_overlay(y = ast_data$drtg,
                 yrep = ast_post_multi)

# Compare error on posterior draws ---------------
error_ast <- data.frame(obs = ast_post) %>%
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

error_ast_multi <- data.frame(obs = ast_post_multi) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

## Collect summary data 
summary_ast <- error_ast %>% 
  left_join(error_ast_multi, by = "name",
            suffix = c("", "_multi")) %>% 
  bind_cols(data.frame(actual = ast_data$drtg)) %>% 
  summarise(rmse_single = Metrics::rmse(actual, estimate),
            rmse_multi = Metrics::rmse(actual, estimate_multi),
            cor_single = cor(actual, estimate),
            cor_multi = cor(actual, estimate_multi)) %>% 
  mutate(score_single = case_when(
    rmse_single <= rmse_multi & cor_single >= cor_multi ~ 2,
    rmse_single <= rmse_multi | cor_single >= cor_multi ~ 1,
    TRUE ~ 0),
    score_multi = 2 - score_single) %>% 
  mutate(score_single = case_when(
    score_single == score_multi & rmse_single <= rmse_multi ~ score_single + 0.5,
    TRUE ~ score_single),
    score_multi = case_when(
      score_single == score_multi & rmse_multi < rmse_single ~ score_multi + 0.5,
      TRUE ~ score_multi))

# Print
summary_ast

# Collect final model
ast_final_def <- if(summary_ast$score_single > summary_ast$score_multi){
  ast_model
} else {
  ast_model_multi
}

# If I want to manually override lol
#ast_final_def <- ast_model_multi



## Clean environment 
rm(ast_model, ast_model_multi,
   ast_post, ast_post_multi,
   error_ast, error_ast_multi,
   summary_ast,
   ast_data)

## Rating -------------------------------------------------------------------
rtg_data <- rating.rtg %>% 
  select(season, team_id, ortg, drtg) %>% 
  mutate(next_season = season + 1,
         last_season = season - 1,
         last2_season = season - 2) %>% 
  left_join(rating.rtg %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  left_join(rating.rtg %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "last_season" = "season"),
            suffix = c("", "_l1")) %>% 
  left_join(rating.rtg %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "last2_season" = "season"),
            suffix = c("", "_l2")) %>% 
  left_join(offseason_summary, by = c("season", "team_id")) %>% 
  filter(!is.na(ortg_l1), !is.na(ortg_l2)) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(next_season:last2_season)) %>% 
  relocate(contains("ortg"), .after = team_id)


### Create offense model -------------------------------------
## Single season
rtg_model <- rstanarm::stan_glm(formula = ortg_next ~ ortg + 
                                  factor(team_id),
                                data = rtg_data,
                                algorithm = "sampling",
                                iter = 5000)

## Multi-season
rtg_model_multi <- rstanarm::stan_glm(formula = ortg_next ~ ortg + #ortg_l1 + ortg_l2 +
                                        pct_minutes_stay + efg_stay + obpm_stay +
                                        commits + rtg_scaled_cmt +
                                        transfers + rtg_scaled_tfr +
                                        efg + obpm +
                                        factor(team_id),
                                      data = rtg_data,
                                      algorithm = "sampling",
                                      iter = 5000)

## Model summaries
summary(rtg_model)
summary(rtg_model_multi)

### Posterior draws ---------
rtg_post <- posterior_predict(rtg_model,
                              draws = 500)

rtg_post_multi <- posterior_predict(rtg_model_multi,
                                    draws = 500)

# Set color scheme
color_scheme_set(scheme = "brightblue")

## Graph comparison
ppc_dens_overlay(y = rtg_data$ortg,
                 yrep = rtg_post)

ppc_dens_overlay(y = rtg_data$ortg,
                 yrep = rtg_post_multi)

# Compare error on posterior draws ---------------
error_rtg <- data.frame(obs = rtg_post) %>%
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

error_rtg_multi <- data.frame(obs = rtg_post_multi) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

## Collect summary data 
summary_rtg <- error_rtg %>% 
  left_join(error_rtg_multi, by = "name",
            suffix = c("", "_multi")) %>% 
  bind_cols(data.frame(actual = rtg_data$ortg)) %>% 
  summarise(rmse_single = Metrics::rmse(actual, estimate),
            rmse_multi = Metrics::rmse(actual, estimate_multi),
            cor_single = cor(actual, estimate),
            cor_multi = cor(actual, estimate_multi)) %>% 
  mutate(score_single = case_when(
    rmse_single <= rmse_multi & cor_single >= cor_multi ~ 2,
    rmse_single <= rmse_multi | cor_single >= cor_multi ~ 1,
    TRUE ~ 0),
    score_multi = 2 - score_single) %>% 
  mutate(score_single = case_when(
    score_single == score_multi & rmse_single <= rmse_multi ~ score_single + 0.5,
    TRUE ~ score_single),
    score_multi = case_when(
      score_single == score_multi & rmse_multi < rmse_single ~ score_multi + 0.5,
      TRUE ~ score_multi))

# Print
summary_rtg

# Collect final model
rtg_final_off <- if(summary_rtg$score_single > summary_rtg$score_multi){
  rtg_model
} else {
  rtg_model_multi
}

# I want to manually override lol
#rtg_final_off <- rtg_model_multi

## Clean environment 
rm(rtg_model, rtg_model_multi,
   rtg_post, rtg_post_multi,
   error_rtg, error_rtg_multi,
   summary_rtg)

### Create defense model ---------------------------
## Single season
rtg_model <- rstanarm::stan_glm(formula = drtg_next ~ drtg +
                                  factor(team_id),
                                data = rtg_data,
                                algorithm = "sampling",
                                iter = 5000)

## Multi-season
rtg_model_multi <- rstanarm::stan_glm(formula = drtg_next ~ drtg + #drtg_l1 + drtg_l2 +
                                        pct_minutes_stay + dbpm_stay +
                                        commits + rtg_scaled_cmt +
                                        transfers + rtg_scaled_tfr + dbpm +
                                        factor(team_id),
                                      data = rtg_data,
                                      algorithm = "sampling",
                                      iter = 5000)

### Posterior draws ---------
rtg_post <- posterior_predict(rtg_model,
                              draws = 500)

rtg_post_multi <- posterior_predict(rtg_model_multi,
                                    draws = 500)

# Set color scheme
color_scheme_set(scheme = "brightblue")

## Graph comparison
ppc_dens_overlay(y = rtg_data$drtg,
                 yrep = rtg_post)

ppc_dens_overlay(y = rtg_data$drtg,
                 yrep = rtg_post_multi)

# Compare error on posterior draws ---------------
error_rtg <- data.frame(obs = rtg_post) %>%
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

error_rtg_multi <- data.frame(obs = rtg_post_multi) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

## Collect summary data 
summary_rtg <- error_rtg %>% 
  left_join(error_rtg_multi, by = "name",
            suffix = c("", "_multi")) %>% 
  bind_cols(data.frame(actual = rtg_data$drtg)) %>% 
  summarise(rmse_single = Metrics::rmse(actual, estimate),
            rmse_multi = Metrics::rmse(actual, estimate_multi),
            cor_single = cor(actual, estimate),
            cor_multi = cor(actual, estimate_multi)) %>% 
  mutate(score_single = case_when(
    rmse_single <= rmse_multi & cor_single >= cor_multi ~ 2,
    rmse_single <= rmse_multi | cor_single >= cor_multi ~ 1,
    TRUE ~ 0),
    score_multi = 2 - score_single) %>% 
  mutate(score_single = case_when(
    score_single == score_multi & rmse_single <= rmse_multi ~ score_single + 0.5,
    TRUE ~ score_single),
    score_multi = case_when(
      score_single == score_multi & rmse_multi < rmse_single ~ score_multi + 0.5,
      TRUE ~ score_multi))

# Print
summary_rtg

# Collect final model
rtg_final_def <- if(summary_rtg$score_single > summary_rtg$score_multi){
  rtg_model
} else {
  rtg_model_multi
}

# If I want to manually override lol
#rtg_final_def <- rtg_model_multi


## Clean environment 
rm(rtg_model, rtg_model_multi,
   rtg_post, rtg_post_multi,
   error_rtg, error_rtg_multi,
   summary_rtg,
   rtg_data)


## Effective FG % -------------------------------------------------------------------
efg_data <- rating.efg %>% 
  select(season, team_id, ortg, drtg) %>% 
  mutate(next_season = season + 1,
         last_season = season - 1,
         last2_season = season - 2) %>% 
  left_join(rating.efg %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  left_join(rating.efg %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "last_season" = "season"),
            suffix = c("", "_l1")) %>% 
  left_join(rating.efg %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "last2_season" = "season"),
            suffix = c("", "_l2")) %>% 
  left_join(offseason_summary, by = c("season", "team_id")) %>% 
  filter(!is.na(ortg_l1), !is.na(ortg_l2)) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(next_season:last2_season)) %>% 
  relocate(contains("ortg"), .after = team_id)


### Create offense model -------------------------------------
## Single season
efg_model <- rstanarm::stan_glm(formula = ortg_next ~ ortg + 
                                  factor(team_id),
                                data = efg_data,
                                algorithm = "sampling",
                                iter = 5000)

## Multi-season
efg_model_multi <- rstanarm::stan_glm(formula = ortg_next ~ ortg + #ortg_l1 + ortg_l2 +
                                        pct_minutes_stay + efg_stay + obpm_stay +
                                        commits + rtg_scaled_cmt +
                                        transfers + rtg_scaled_tfr +
                                        efg + obpm +
                                        factor(team_id),
                                      data = efg_data,
                                      algorithm = "sampling",
                                      iter = 5000)

## Model summaries
summary(efg_model)
summary(efg_model_multi)

### Posterior draws ---------
efg_post <- posterior_predict(efg_model,
                              draws = 500)

efg_post_multi <- posterior_predict(efg_model_multi,
                                    draws = 500)

# Set color scheme
color_scheme_set(scheme = "brightblue")

## Graph comparison
ppc_dens_overlay(y = efg_data$ortg,
                 yrep = efg_post)

ppc_dens_overlay(y = efg_data$ortg,
                 yrep = efg_post_multi)

# Compare error on posterior draws ---------------
error_efg <- data.frame(obs = efg_post) %>%
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

error_efg_multi <- data.frame(obs = efg_post_multi) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

## Collect summary data 
summary_efg <- error_efg %>% 
  left_join(error_efg_multi, by = "name",
            suffix = c("", "_multi")) %>% 
  bind_cols(data.frame(actual = efg_data$ortg)) %>% 
  summarise(rmse_single = Metrics::rmse(actual, estimate),
            rmse_multi = Metrics::rmse(actual, estimate_multi),
            cor_single = cor(actual, estimate),
            cor_multi = cor(actual, estimate_multi)) %>% 
  mutate(score_single = case_when(
    rmse_single <= rmse_multi & cor_single >= cor_multi ~ 2,
    rmse_single <= rmse_multi | cor_single >= cor_multi ~ 1,
    TRUE ~ 0),
    score_multi = 2 - score_single) %>% 
  mutate(score_single = case_when(
    score_single == score_multi & rmse_single <= rmse_multi ~ score_single + 0.5,
    TRUE ~ score_single),
    score_multi = case_when(
      score_single == score_multi & rmse_multi < rmse_single ~ score_multi + 0.5,
      TRUE ~ score_multi))

# Print
summary_efg

# Collect final model
efg_final_off <- if(summary_efg$score_single > summary_efg$score_multi){
  efg_model
} else {
  efg_model_multi
}

# I want to manually override lol
#efg_final_off <- efg_model_multi

## Clean environment 
rm(efg_model, efg_model_multi,
   efg_post, efg_post_multi,
   error_efg, error_efg_multi,
   summary_efg)

### Create defense model ---------------------------
## Single season
efg_model <- rstanarm::stan_glm(formula = drtg_next ~ drtg +
                                  factor(team_id),
                                data = efg_data,
                                algorithm = "sampling",
                                iter = 5000)

## Multi-season
efg_model_multi <- rstanarm::stan_glm(formula = drtg_next ~ drtg + #defg_l1 + defg_l2 +
                                        pct_minutes_stay + dbpm_stay +
                                        commits + rtg_scaled_cmt +
                                        transfers + rtg_scaled_tfr + dbpm +
                                        factor(team_id),
                                      data = efg_data,
                                      algorithm = "sampling",
                                      iter = 5000)

### Posterior draws ---------
efg_post <- posterior_predict(efg_model,
                              draws = 500)

efg_post_multi <- posterior_predict(efg_model_multi,
                                    draws = 500)

# Set color scheme
color_scheme_set(scheme = "brightblue")

## Graph comparison
ppc_dens_overlay(y = efg_data$drtg,
                 yrep = efg_post)

ppc_dens_overlay(y = efg_data$drtg,
                 yrep = efg_post_multi)

# Compare error on posterior draws ---------------
error_efg <- data.frame(obs = efg_post) %>%
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

error_efg_multi <- data.frame(obs = efg_post_multi) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

## Collect summary data 
summary_efg <- error_efg %>% 
  left_join(error_efg_multi, by = "name",
            suffix = c("", "_multi")) %>% 
  bind_cols(data.frame(actual = efg_data$drtg)) %>% 
  summarise(rmse_single = Metrics::rmse(actual, estimate),
            rmse_multi = Metrics::rmse(actual, estimate_multi),
            cor_single = cor(actual, estimate),
            cor_multi = cor(actual, estimate_multi)) %>% 
  mutate(score_single = case_when(
    rmse_single <= rmse_multi & cor_single >= cor_multi ~ 2,
    rmse_single <= rmse_multi | cor_single >= cor_multi ~ 1,
    TRUE ~ 0),
    score_multi = 2 - score_single) %>% 
  mutate(score_single = case_when(
    score_single == score_multi & rmse_single <= rmse_multi ~ score_single + 0.5,
    TRUE ~ score_single),
    score_multi = case_when(
      score_single == score_multi & rmse_multi < rmse_single ~ score_multi + 0.5,
      TRUE ~ score_multi))

# Print
summary_efg

# Collect final model
efg_final_def <- if(summary_efg$score_single > summary_efg$score_multi){
  efg_model
} else {
  efg_model_multi
}

# If I want to manually override lol
#efg_final_def <- efg_model_multi


## Clean environment 
rm(efg_model, efg_model_multi,
   efg_post, efg_post_multi,
   error_efg, error_efg_multi,
   summary_efg,
   efg_data)

## Raw Rating -------------------------------------------------------------------
rtg_raw_data <- rating.raw_rtg %>% 
  select(season, team_id, ortg, drtg) %>% 
  mutate(next_season = season + 1,
         last_season = season - 1,
         last2_season = season - 2) %>% 
  left_join(rating.raw_rtg %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  left_join(rating.raw_rtg %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "last_season" = "season"),
            suffix = c("", "_l1")) %>% 
  left_join(rating.raw_rtg %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "last2_season" = "season"),
            suffix = c("", "_l2")) %>% 
  left_join(offseason_summary, by = c("season", "team_id")) %>% 
  filter(!is.na(ortg_l1), !is.na(ortg_l2)) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(next_season:last2_season)) %>% 
  relocate(contains("ortg"), .after = team_id)

### Create offense model -------------------------------------
## Single season
rtg_raw_model <- rstanarm::stan_glm(formula = ortg_next ~ ortg + 
                                  factor(team_id),
                                data = rtg_raw_data,
                                algorithm = "sampling",
                                iter = 5000)

## Multi-season
rtg_raw_model_multi <- rstanarm::stan_glm(formula = ortg_next ~ ortg + #ortg_raw_l1 + ortg_raw_l2 +
                                            pct_minutes_stay + efg_stay + obpm_stay +
                                            oreb_rate_stay + ast_stay + tov_stay +
                                            commits + rtg_scaled_cmt +
                                            transfers + rtg_scaled_tfr +
                                            efg + obpm + oreb_rate + ast + tov +
                                            factor(team_id),
                                          data = rtg_raw_data,
                                          algorithm = "sampling",
                                          iter = 5000)

## Model summaries
summary(rtg_raw_model)
summary(rtg_raw_model_multi)

### Posterior draws ---------
rtg_raw_post <- posterior_predict(rtg_raw_model,
                                  draws = 500)

rtg_raw_post_multi <- posterior_predict(rtg_raw_model_multi,
                                        draws = 500)

# Set color scheme
color_scheme_set(scheme = "brightblue")

## Graph comparison
ppc_dens_overlay(y = rtg_raw_data$ortg,
                 yrep = rtg_raw_post)

ppc_dens_overlay(y = rtg_raw_data$ortg,
                 yrep = rtg_raw_post_multi)

# Compare error on posterior draws ---------------
error_rtg_raw <- data.frame(obs = rtg_raw_post) %>%
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

error_rtg_raw_raw_multi <- data.frame(obs = rtg_raw_post_multi) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

## Collect summary data 
summary_rtg_raw <- error_rtg_raw %>% 
  left_join(error_rtg_raw_raw_multi, by = "name",
            suffix = c("", "_multi")) %>% 
  bind_cols(data.frame(actual = rtg_raw_data$ortg)) %>% 
  summarise(rmse_single = Metrics::rmse(actual, estimate),
            rmse_multi = Metrics::rmse(actual, estimate_multi),
            cor_single = cor(actual, estimate),
            cor_multi = cor(actual, estimate_multi)) %>% 
  mutate(score_single = case_when(
    rmse_single <= rmse_multi & cor_single >= cor_multi ~ 2,
    rmse_single <= rmse_multi | cor_single >= cor_multi ~ 1,
    TRUE ~ 0),
    score_multi = 2 - score_single) %>% 
  mutate(score_single = case_when(
    score_single == score_multi & rmse_single <= rmse_multi ~ score_single + 0.5,
    TRUE ~ score_single),
    score_multi = case_when(
      score_single == score_multi & rmse_multi < rmse_single ~ score_multi + 0.5,
      TRUE ~ score_multi))

# Print
summary_rtg_raw

# Collect final model
rtg_raw_final_off <- if(summary_rtg_raw$score_single > summary_rtg_raw$score_multi){
  rtg_raw_model
} else {
  rtg_raw_model_multi
}

# I want to manually override lol
#rtg_raw_final_off <- rtg_raw_model_multi

## Clean environment 
rm(rtg_raw_model, rtg_raw_model_multi,
   rtg_raw_post, rtg_raw_post_multi,
   error_rtg_raw, error_rtg_raw_raw_multi,
   summary_rtg_raw)

### Create defense model ---------------------------
## Single season
rtg_raw_model <- rstanarm::stan_glm(formula = drtg_next ~ drtg +
                                  factor(team_id),
                                data = rtg_raw_data,
                                algorithm = "sampling",
                                iter = 5000)

## Multi-season
rtg_raw_model_multi <- rstanarm::stan_glm(formula = drtg_next ~ drtg + #drtg_raw_l1 + drtg_raw_l2 +
                                            pct_minutes_stay + efg_stay + dbpm_stay +
                                            dreb_rate_stay + stl_stay +
                                            commits + rtg_scaled_cmt +
                                            transfers + rtg_scaled_tfr +
                                            dbpm + dreb_rate + stl +
                                            factor(team_id),
                                          data = rtg_raw_data,
                                          algorithm = "sampling",
                                          iter = 5000)

### Posterior draws ---------
rtg_raw_post <- posterior_predict(rtg_raw_model,
                                  draws = 500)

rtg_raw_post_multi <- posterior_predict(rtg_raw_model_multi,
                                        draws = 500)

# Set color scheme
color_scheme_set(scheme = "brightblue")

## Graph comparison
ppc_dens_overlay(y = rtg_raw_data$drtg,
                 yrep = rtg_raw_post)

ppc_dens_overlay(y = rtg_raw_data$drtg,
                 yrep = rtg_raw_post_multi)

# Compare error on posterior draws ---------------
error_rtg_raw <- data.frame(obs = rtg_raw_post) %>%
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

error_rtg_raw_raw_multi <- data.frame(obs = rtg_raw_post_multi) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(estimate = value)

## Collect summary data 
summary_rtg_raw <- error_rtg_raw %>% 
  left_join(error_rtg_raw_raw_multi, by = "name",
            suffix = c("", "_multi")) %>% 
  bind_cols(data.frame(actual = rtg_raw_data$drtg)) %>% 
  summarise(rmse_single = Metrics::rmse(actual, estimate),
            rmse_multi = Metrics::rmse(actual, estimate_multi),
            cor_single = cor(actual, estimate),
            cor_multi = cor(actual, estimate_multi)) %>% 
  mutate(score_single = case_when(
    rmse_single <= rmse_multi & cor_single >= cor_multi ~ 2,
    rmse_single <= rmse_multi | cor_single >= cor_multi ~ 1,
    TRUE ~ 0),
    score_multi = 2 - score_single) %>% 
  mutate(score_single = case_when(
    score_single == score_multi & rmse_single <= rmse_multi ~ score_single + 0.5,
    TRUE ~ score_single),
    score_multi = case_when(
      score_single == score_multi & rmse_multi < rmse_single ~ score_multi + 0.5,
      TRUE ~ score_multi))

# Print
summary_rtg_raw

# Collect final model
rtg_raw_final_def <- if(summary_rtg_raw$score_single > summary_rtg_raw$score_multi){
  rtg_raw_model
} else {
  rtg_raw_model_multi
}

# If I want to manually override lol
#rtg_raw_final_def <- rtg_raw_model_multi


## Clean environment 
rm(rtg_raw_model, rtg_raw_model_multi,
   rtg_raw_post, rtg_raw_post_multi,
   error_rtg_raw, error_rtg_raw_raw_multi,
   summary_rtg_raw,
   rtg_raw_data)

# SAVE ALL MODELS ----------------------------------------
all_models <- list(pace_off = pace_final_off$call, pace_def = pace_final_def$call,
                   ast_off = ast_final_off$call, ast_def = ast_final_def$call,
                   oreb_off = oreb_final_off$call, dreb_def = dreb_final_def$call,
                   to_off = to_final_off$call, to_def = to_final_def$call,
                   rtg_off = rtg_final_off$call, rtg_def = rtg_final_def$call,
                   efg_off = efg_final_off$call, efg_def = efg_final_def$call,
                   rtg_raw_off = rtg_raw_final_off$call, rtg_raw_def = rtg_raw_final_def$call)

saveRDS(all_models, 'Models/all_priors_models.rds')
