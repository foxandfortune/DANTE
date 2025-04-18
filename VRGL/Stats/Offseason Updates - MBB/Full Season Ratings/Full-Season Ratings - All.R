library(tidyverse)
library(glmnet)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')
setwd('..')

# Set season/country/tier
season <- 2025

# Negate in formula
`%!in%` = Negate(`%in%`)

# Load teams and summary data -----
teams <- readRDS(glue::glue("Teams/team_database.rds"))

summary <- readRDS(glue::glue("Power Ratings/Raw Data/poss_stats_with_types_{season}.rds")) %>% 
  # Filter out teams in team dataset
  filter(team_id %in% teams$team_id & opponent_team_id %in% teams$team_id) %>% 
  with_groups(game_id, mutate, tm_ct = n()) %>%
  # Only include games where both teams are in teams dataset
  filter(tm_ct == 2) %>% 
  select(-tm_ct) %>% 
  as.data.frame() %>% 
  rename(opp_id = opponent_team_id)

# Load create ratings function
create_ratings <- readRDS("Tools/create_ratings_stat.rds")

# Pace (Possessions per 40 minutes) -----------------------------------
stat_name <- "poss_per_40"

## Features ----------
list <- c('game_date', 'team_id', 'opp_id')

## Load weights -----
wgt_var <- readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  rename(year = season) %>%
  filter(year == season) %>% 
  mutate(rank = dense_rank(rmse)) %>% 
  arrange(rank, desc(corr)) %>% 
  slice(1) %>%
  select(wgt) %>% 
  as.double()

print(wgt_var)

## Load lambda adj -----
lambda_adj <- readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  rename(year = season) %>%
  filter(year == season) %>% 
  mutate(rank = dense_rank(rmse)) %>% 
  arrange(rank, desc(corr)) %>% 
  slice(1) %>%
  select(lambda_adj) %>% 
  as.double()

print(lambda_adj)

## Create pace ratings ---------------------
raw.coeff.pace <- create_ratings(df = summary,
                            stat_name = {stat_name},
                            name_list = {list},
                            seed = 421,
                            wgt = {wgt_var},
                            lambda_adj = {lambda_adj})

head(raw.coeff.pace)

## Clear stuff out  --------
rm(stat_name, list, wgt_var)

# Assist Rate -----------------------------------
stat_name <- "ast_rt"

## Features ----------
list <- c('game_date', 'team_id', 'opp_id')

## Load weights -----
wgt_var <- 0.98 #readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  rename(year = season) %>%
  filter(year == season) %>% 
  mutate(rank = dense_rank(rmse)) %>% 
  arrange(rank, desc(corr)) %>% 
  slice(1) %>%
  select(wgt) %>% 
  as.double()

print(wgt_var)

## Load lambda adj -----
lambda_adj <- 0 #readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  rename(year = season) %>%
  filter(year == season) %>% 
  mutate(rank = dense_rank(rmse)) %>% 
  arrange(rank, desc(corr)) %>% 
  slice(1) %>%
  select(lambda_adj) %>% 
  as.double()

print(lambda_adj)

## Create assist rate ratings ---------------------
raw.coeff.ast <- create_ratings(df = summary,
                            stat_name = {stat_name},
                            name_list = {list},
                            seed = 421,
                            wgt = {wgt_var},
                            lambda_adj = {lambda_adj})

head(raw.coeff.ast)

## Clear stuff out  --------
rm(stat_name, list, wgt_var)

# Offensive rebound rate -----------------------------------
stat_name <- "oreb_rt"

## Features ----------
list <- c('game_date', 'team_id', 'opp_id')

## Load weights -----
wgt_var <- 0.97 #readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  rename(year = season) %>%
  filter(year == season) %>% 
  mutate(rank = dense_rank(rmse)) %>% 
  arrange(rank, desc(corr)) %>% 
  slice(1) %>%
  select(wgt) %>% 
  as.double()

print(wgt_var)

## Load lambda adj -----
lambda_adj <- 0  #readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  rename(year = season) %>%
  filter(year == season) %>% 
  mutate(rank = dense_rank(rmse)) %>% 
  arrange(rank, desc(corr)) %>% 
  slice(1) %>%
  select(lambda_adj) %>% 
  as.double()

print(lambda_adj)

## Create offensive rebround rate ratings ---------------------
raw.coeff.oreb <- create_ratings(df = summary,
                            stat_name = {stat_name},
                            name_list = {list},
                            seed = 421,
                            wgt = {wgt_var},
                            lambda_adj = {lambda_adj})

head(raw.coeff.oreb)

## Clear stuff out  --------
rm(stat_name, list, wgt_var)

# Turnover Rate -----------------------------------
stat_name <- "to_rt"

## Features ----------
list <- c('game_date', 'team_id', 'opp_id')

## Load weights -----
wgt_var <- 0.96 #readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  rename(year = season) %>%
  filter(year == season) %>% 
  mutate(rank = dense_rank(rmse)) %>% 
  arrange(rank, desc(corr)) %>% 
  slice(1) %>%
  select(wgt) %>% 
  as.double()

print(wgt_var)

## Load lambda adj -----
lambda_adj <- 0 #readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  rename(year = season) %>%
  filter(year == season) %>% 
  mutate(rank = dense_rank(rmse)) %>% 
  arrange(rank, desc(corr)) %>% 
  slice(1) %>%
  select(lambda_adj) %>% 
  as.double()

print(lambda_adj)

## Create turnover rate ratings ---------------------
raw.coeff.to <- create_ratings(df = summary,
                            stat_name = {stat_name},
                            name_list = {list},
                            seed = 421,
                            wgt = {wgt_var},
                            lambda_adj = {lambda_adj})


head(raw.coeff.to)

## Clear stuff out  --------
rm(stat_name, list, wgt_var)

# Effective FG % -----------------------------------
stat_name <- "efg_adj"

## Features ----------
list <- c('game_date', 'team_id', 'opp_id')

## Load weights -----
wgt_var <- 0.99 #readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  rename(year = season) %>%
  filter(year == season) %>% 
  mutate(rank = dense_rank(rmse)) %>% 
  arrange(rank, desc(corr)) %>% 
  slice(1) %>%
  select(wgt) %>% 
  as.double()

print(wgt_var)

## Load lambda adj -----
lambda_adj <- 0 #readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  rename(year = season) %>%
  filter(year == season) %>% 
  mutate(rank = dense_rank(rmse)) %>% 
  arrange(rank, desc(corr)) %>% 
  slice(1) %>%
  select(lambda_adj) %>% 
  as.double()

print(lambda_adj)

## Create EFG % ratings ---------------------
raw.coeff.efg <- create_ratings(df = summary,
                            stat_name = {stat_name},
                            name_list = {list},
                            seed = 421,
                            wgt = {wgt_var},
                            lambda_adj = {lambda_adj})

head(raw.coeff.efg)

## Clear stuff out  --------
rm(stat_name, list, wgt_var)

# Team Ratings -----------------------------------------
stat_name <- "rtg"

## Features ----------
list <- c('game_date', 'team_id', 'opp_id',
          'poss_per_40', 'ast_rt', 'oreb_rt', 'to_rt',
          'is_home', 'neutral_site', 'days_rest', 'travel')

# Load weights -----
wgt_var <- 0.97 #readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  mutate(wgt = as.double(wgt)) %>% 
  rename(year = season) %>%
  with_groups(.groups = year, mutate, rank = dense_rank(rmse)) %>% 
  #filter(year == season) %>% 
  filter(rank == 1, year != 2021) %>%
  mutate(weight = .97^(season - as.numeric(year))) %>% 
  summarise(wgt = weighted.mean(x = wgt, w = weight)) %>% 
  select(wgt) %>% 
  as.double()

print(wgt_var)

## Load lambda adj -----
lambda_adj <- 0.75 #readRDS(glue::glue('Power Ratings/Weights/single_season_{stat_name}_weights.rds')) %>%
  mutate(wgt = as.double(wgt), lambda_adj = as.double(lambda)) %>% 
  rename(year = season) %>%
  filter(year <= season) %>% 
  with_groups(.groups = year, mutate, rank = dense_rank(rmse)) %>% 
  filter(rank == 1) %>%
  mutate(weight = .97^(season - as.numeric(year))) %>% 
  summarise(lambda_adj = weighted.mean(x = lambda_adj, w = weight)) %>% 
  select(lambda_adj) %>% 
  as.double()

print(lambda_adj)


# Create Rtg ratings ---------------------
raw.coeff.rtg <- create_ratings(df = summary,
                                stat_name = {stat_name},
                                name_list = {list},
                                lambda_adj = {lambda_adj},
                                seed = 421,
                                wgt = {wgt_var})

## Look to see who's in the top 68 -------------
raw.coeff.rtg %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(team_id = str_remove_all(name, c("team_id_")),
         team_id = str_replace_all(team_id, "_", " ")) %>% 
  rename(OffRtg = value) %>% 
  left_join(raw.coeff.rtg %>% 
              filter(str_detect(name, "opp_id_")) %>% 
              mutate(team_id = str_remove_all(name, c("opp_id_")),
                     team_id = str_replace_all(team_id, "_", " ")) %>% 
              rename(DefRtg = value), by = "team_id") %>% 
  left_join(teams %>% 
              select(team_id, team), by = "team_id") %>% 
  select(team, OffRtg, DefRtg) %>% 
  mutate(Total = OffRtg - DefRtg) %>% 
  arrange(desc(Total)) %>% 
  head(68)

rm(stat_name, list, wgt_var)

# Raw %eam Ratings (DO NOT INCLUDE OTHER STATS) -----------------------------------------
stat_name <- "rtg"

## Features ----------
list <- c('game_date', 'team_id', 'opp_id',
          'poss_per_40',
          'is_home', 'neutral_site', 'days_rest', 'travel')

# Load weights -----
wgt_var <- 0.97 #readRDS(glue::glue('Power Ratings/Weights/single_season_rtg_weights.rds')) %>%
  mutate(wgt = as.double(wgt)) %>% 
  rename(year = season) %>%
  with_groups(.groups = year, mutate, rank = dense_rank(rmse)) %>% 
  #filter(year == season) %>% 
  filter(rank == 1) %>%
  mutate(weight = .97^(season - as.numeric(year))) %>% 
  summarise(wgt = weighted.mean(x = wgt, w = weight)) %>% 
  select(wgt) %>% 
  as.double()

print(wgt_var)

## Load lambda adj -----
lambda_adj <- 0.75 #readRDS(glue::glue('Power Ratings/Weights/single_season_rtg_weights.rds')) %>%
  mutate(wgt = as.double(wgt), lambda_adj = as.double(lambda)) %>% 
  rename(year = season) %>%
  filter(year <= season) %>% 
  with_groups(.groups = year, mutate, rank = dense_rank(rmse)) %>% 
  filter(rank == 1) %>%
  mutate(weight = .97^(season - as.numeric(year))) %>% 
  summarise(lambda_adj = weighted.mean(x = lambda_adj, w = weight)) %>% 
  select(lambda_adj) %>% 
  as.double()

print(lambda_adj)


# Create Rtg ratings ---------------------
raw.coeff.rtg.raw <- create_ratings(df = summary,
                                    stat_name = {stat_name},
                                    name_list = {list},
                                    lambda_adj = {lambda_adj},
                                    seed = 421,
                                    wgt = {wgt_var})

## Look to see who's in the top 68 -------------
raw.coeff.rtg.raw %>% 
  filter(str_detect(name, "team_id_")) %>% 
  mutate(team_id = str_remove_all(name, c("team_id_")),
         team_id = str_replace_all(team_id, "_", " ")) %>% 
  rename(OffRtg = value) %>% 
  left_join(raw.coeff.rtg.raw %>% 
              filter(str_detect(name, "opp_id_")) %>% 
              mutate(team_id = str_remove_all(name, c("opp_id_")),
                     team_id = str_replace_all(team_id, "_", " ")) %>% 
              rename(DefRtg = value), by = "team_id") %>% 
  left_join(teams %>% 
              select(team_id, team), by = "team_id") %>% 
  select(team, OffRtg, DefRtg) %>% 
  mutate(Total = OffRtg - DefRtg) %>% 
  arrange(desc(Total)) %>% 
  head(68)


# Combine all ratings in to single list --------------------
rtgs.all <- list(pace = {raw.coeff.pace},
                 ast = {raw.coeff.ast},
                 oreb = {raw.coeff.oreb},
                 to = {raw.coeff.to},
                 rtg = {raw.coeff.rtg},
                 #Additional for site
                 efg = {raw.coeff.efg},
                 raw_rating = {raw.coeff.rtg.raw})

## Save ratings --------
saveRDS(rtgs.all,
        glue::glue("Power Ratings/Team Ratings/Full Season/ratings_all_{season}.rds"))
