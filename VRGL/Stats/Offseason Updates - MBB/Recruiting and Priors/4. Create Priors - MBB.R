library(cbbdata)
library(rstanarm)
library(bayesplot)
library(tidyverse)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')

# Negate in formula
`%!in%` = Negate(`%in%`)

# Set season for priors 
prior_season <- 2024

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

# Load models 
all_models <- readRDS('Models/all_priors_models.rds')

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
factors.pace <- rating.pace %>% 
  filter(!str_detect(name, "team_id_"),
         !str_detect(name, "opp_id_"),
         season >= {min_season - 2})

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
factors.oreb <- rating.oreb %>% 
  filter(!str_detect(name, "team_id_"),
         !str_detect(name, "opp_id_"),
         season >= {min_season - 2})

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
factors.ast <- rating.ast %>% 
  filter(!str_detect(name, "team_id_"),
         !str_detect(name, "opp_id_"),
         season >= {min_season - 2})

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
factors.to <- rating.to %>% 
  filter(!str_detect(name, "team_id_"),
         !str_detect(name, "opp_id_"),
         season >= {min_season - 2})


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
factors.rtg <- rating.rtg %>% 
  filter(!str_detect(name, "team_id_"),
         !str_detect(name, "opp_id_"),
         season >= {min_season - 2})

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
factors.efg <- rating.efg %>% 
  filter(!str_detect(name, "team_id_"),
         !str_detect(name, "opp_id_"),
         season >= {min_season - 2})

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
factors.raw_rtg <- rating.raw_rtg %>% 
  filter(!str_detect(name, "team_id_"),
         !str_detect(name, "opp_id_"),
         season >= {min_season - 2})

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

# Build models-------------------------------------------------------------------
## Pace--------------------------------------------------------------------------
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
  filter(!is.na(ortg_l1), !is.na(ortg_l2)) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(next_season:last2_season)) %>% 
  relocate(contains("ortg"), .after = team_id)

## Train data
train_data <- pace_data %>% 
  filter(season < {prior_season - 1},
         !is.na(ortg_next), !is.na(drtg_next),
         season %in% summary.team$season)

unique(train_data$season)

## Test data 
test_data <- pace_data %>% 
  filter(season == {prior_season - 1},
         !is.na(ortg_next), !is.na(drtg_next))

### Team ----------
print(all_models$pace_off)

model.pace.tm <- rstanarm::stan_glm(formula = ortg_next ~ ortg + factor(team_id), 
                                    data = train_data,
                                    iter = 10000,
                                    algorithm = "sampling")


### Missing team ids
missing_ids <- test_data %>% 
  filter(team_id %!in% train_data$team_id) %>% 
  pull(team_id)

# Posterior draws
pace_post.tm <- posterior_predict(model.pace.tm,
                                  draws = 500,
                                  newdata = test_data %>% 
                                    # Filter for teams not in new data
                                    filter(team_id %!in% missing_ids)
                                  )

# Summarize ------------
pace_priors_off <- data.frame(obs = pace_post.tm) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(ortg = value) %>% 
  bind_cols(data.frame(test_data %>% 
                         filter(team_id %!in% missing_ids)) %>% 
              select(team_id, ortg_prior = ortg)) %>% 
  select(team_id, ortg, ortg_prior)
  
print(pace_priors_off)

### Opponent ------------
print(all_models$pace_def)

model.pace.opp <- rstanarm::stan_glm(formula = drtg_next ~ drtg + factor(team_id), 
                                     data = train_data,
                                     iter = 10000,
                                     algorithm = "sampling")


# Posterior draws
pace_post.opp <- posterior_predict(model.pace.opp,
                                  draws = 500,
                                  newdata = test_data %>% 
                                    # Filter for teams not in new data
                                    filter(team_id %!in% missing_ids)
                                  )

# Summarize ------------
pace_priors_def <- data.frame(obs = pace_post.opp) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(drtg = value) %>% 
  bind_cols(data.frame(test_data %>% 
                         filter(team_id %!in% missing_ids)) %>% 
              select(team_id, drtg_prior = drtg)) %>% 
  select(team_id, drtg, drtg_prior)

print(pace_priors_def)

## Create priors ----------------
priors.pace <- pace_priors_off %>%
  mutate(name = paste0("team_id_", team_id)) %>%
  rename(value = ortg) %>% 
  select(-contains("_prior")) %>% 
  rbind(pace_priors_def %>% 
          mutate(name = paste0("opp_id_", team_id)) %>% 
          rename(value = drtg) %>% 
          select(-contains("_prior"))) %>% 
  select(name, value)

# Add back teams missing 
priors.pace.missing <- pace_data %>% 
  filter(team_id %in% missing_ids) %>% 
  mutate(name = paste0("team_id_", team_id),
         value = 0.8 * ortg + 0.2 * ortg_l1) %>% 
  select(name, value) %>% 
  bind_rows(pace_data %>% 
              filter(team_id %in% missing_ids) %>% 
              mutate(name = paste0("opp_id_", team_id),
                     value = 0.8 * drtg + 0.2 * drtg_l1) %>% 
              select(name, value))


## Get Intercept ---------
prior.factor.pace <- factors.pace %>% 
  mutate(weighting = 0.97 ^ (prior_season - season)) %>% 
  with_groups(.groups = name,
              summarise,
              value = weighted.mean(value, weighting)) %>%
  # Pace usually 3 possessions higher at start vs end of season
  mutate(value = value + 3)

## Combine into data frame -------
priors.pace <- bind_rows(prior.factor.pace,
                         priors.pace,
                         priors.pace.missing) %>% 
  data.frame(row.names = NULL)

head(priors.pace, 12)

# Clean the environment 
rm(train_data, test_data,
   model.pace.tm, model.pace.opp,
   missing_ids,
   pace_post.tm, pace_post.opp,
   pace_priors_off, pace_priors_def,
   priors.pace.missing, prior.factor.pace,
   pace_data)

## Assist --------------------------------------------------------------------------
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

## Train data
train_data <- ast_data %>% 
  filter(season < {prior_season - 1},
         !is.na(ortg_next), !is.na(drtg_next),
         season %in% summary.team$season)

## Test data 
test_data <- ast_data %>% 
  filter(season == {prior_season - 1},
         !is.na(ortg_next), !is.na(drtg_next))

### Team ----------
print(all_models$ast_off)

model.ast.tm <- rstanarm::stan_glm(formula = ortg_next ~ ortg + pct_minutes_stay + 
                                     ast_stay + commits + rtg_scaled_cmt + transfers + rtg_scaled_tfr + 
                                     ast + factor(team_id),
                                   data = train_data,
                                   iter = 10000,
                                   algorithm = "sampling")


### Missing team ids
missing_ids <- test_data %>% 
  filter(team_id %!in% train_data$team_id) %>% 
  pull(team_id)

# Posterior draws
ast_post.tm <- posterior_predict(model.ast.tm,
                                  draws = 500,
                                  newdata = test_data %>% 
                                    # Filter for teams not in new data
                                    filter(team_id %!in% missing_ids)
                                 )

# Summarize ------------
ast_priors_off <- data.frame(obs = ast_post.tm) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(ortg = value) %>% 
  bind_cols(data.frame(test_data %>% 
                         filter(team_id %!in% missing_ids)) %>% 
              select(team_id, ortg_prior = ortg)) %>% 
  select(team_id, ortg, ortg_prior)

print(ast_priors_off)

### Opponent ------------
print(all_models$ast_def)

model.ast.opp <- rstanarm::stan_glm(formula = drtg_next ~ drtg + factor(team_id), 
                                    data = train_data,
                                    iter = 10000,
                                    algorithm = "sampling")


# Posterior draws
ast_post.opp <- posterior_predict(model.ast.opp,
                                   draws = 500,
                                   newdata = test_data %>% 
                                     # Filter for teams not in new data
                                     filter(team_id %!in% missing_ids)
                                  )

# Summarize ------------
ast_priors_def <- data.frame(obs = ast_post.opp) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(drtg = value) %>% 
  bind_cols(data.frame(test_data %>% 
                         filter(team_id %!in% missing_ids)) %>% 
              select(team_id, drtg_prior = drtg)) %>% 
  select(team_id, drtg, drtg_prior)

print(ast_priors_def)

## Create priors ----------------
priors.ast <- ast_priors_off %>%
  mutate(name = paste0("team_id_", team_id)) %>%
  rename(value = ortg) %>% 
  select(-contains("_prior")) %>% 
  rbind(ast_priors_def %>% 
          mutate(name = paste0("opp_id_", team_id)) %>% 
          rename(value = drtg) %>% 
          select(-contains("_prior"))) %>% 
  select(name, value)

# Add back teams missing 
priors.ast.missing <- ast_data %>% 
  filter(team_id %in% missing_ids) %>% 
  mutate(name = paste0("team_id_", team_id),
         value = 0.8 * ortg + 0.2 * ortg_l1) %>% 
  select(name, value) %>% 
  bind_rows(ast_data %>% 
              filter(team_id %in% missing_ids) %>% 
              mutate(name = paste0("opp_id_", team_id),
                     value = 0.8 * drtg + 0.2 * drtg_l1) %>% 
              select(name, value))


## Get Intercept ---------
prior.factor.ast <- factors.ast %>% 
  mutate(weighting = 0.97 ^ (prior_season - season)) %>% 
  with_groups(.groups = name,
              summarise,
              value = weighted.mean(value, weighting))

## Combine into data frame -------
priors.ast <- bind_rows(prior.factor.ast,
                         priors.ast,
                         priors.ast.missing) %>% 
  data.frame(row.names = NULL)

head(priors.ast, 12)

# Clean the environment 
rm(train_data, test_data,
   model.ast.tm, model.ast.opp,
   missing_ids,
   ast_post.tm, ast_post.opp,
   ast_priors_off, ast_priors_def,
   priors.ast.missing, prior.factor.ast,
   ast_data)

## Rebounding --------------------------------------------------------------------------
reb_data <- rating.oreb %>% 
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

## Train data
train_data <- reb_data %>% 
  filter(season < {prior_season - 1},
         !is.na(ortg_next), !is.na(drtg_next),
         season %in% summary.team$season)

## Test data 
test_data <- reb_data %>% 
  filter(season == {prior_season - 1},
         !is.na(ortg_next), !is.na(drtg_next))

### Team ----------
print(all_models$oreb_off)

model.reb.tm <- rstanarm::stan_glm(formula = ortg_next ~ ortg + pct_minutes_stay + 
                                     oreb_rate_stay + commits + rtg_scaled_cmt +
                                     transfers + rtg_scaled_tfr + 
                                     oreb_rate + factor(team_id),
                                   data = train_data,
                                   iter = 10000, 
                                   algorithm = "sampling")


### Missing team ids
missing_ids <- test_data %>% 
  filter(team_id %!in% train_data$team_id) %>% 
  pull(team_id)

# Posterior draws
reb_post.tm <- posterior_predict(model.reb.tm,
                                 draws = 500,
                                 newdata = test_data %>% 
                                   # Filter for teams not in new data
                                   filter(team_id %!in% missing_ids)
                                 )

# Summarize ------------
reb_priors_off <- data.frame(obs = reb_post.tm) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(ortg = value) %>% 
  bind_cols(data.frame(test_data %>% 
                         filter(team_id %!in% missing_ids)) %>% 
              select(team_id, ortg_prior = ortg)) %>% 
  select(team_id, ortg, ortg_prior)

print(reb_priors_off)

### Opponent ------------
print(all_models$dreb_def)

model.reb.opp <- rstanarm::stan_glm(formula = drtg_next ~ drtg + pct_minutes_stay + 
                                      dreb_rate_stay + commits + rtg_scaled_cmt +
                                      transfers + rtg_scaled_tfr + 
                                      dreb_rate + factor(team_id),
                                    data = train_data,
                                    iter = 10000, 
                                    algorithm = "sampling")


# Posterior draws
reb_post.opp <- posterior_predict(model.reb.opp,
                                  draws = 500,
                                  newdata = test_data %>% 
                                    # Filter for teams not in new data
                                    filter(team_id %!in% missing_ids)
                                  )

# Summarize ------------
reb_priors_def <- data.frame(obs = reb_post.opp) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(drtg = value) %>% 
  bind_cols(data.frame(test_data %>% 
                         filter(team_id %!in% missing_ids)) %>% 
              select(team_id, drtg_prior = drtg)) %>% 
  select(team_id, drtg, drtg_prior)

print(reb_priors_def)

## Create priors ----------------
priors.reb <- reb_priors_off %>%
  mutate(name = paste0("team_id_", team_id)) %>%
  rename(value = ortg) %>% 
  select(-contains("_prior")) %>% 
  rbind(reb_priors_def %>% 
          mutate(name = paste0("opp_id_", team_id)) %>% 
          rename(value = drtg) %>% 
          select(-contains("_prior"))) %>% 
  select(name, value)

# Add back teams missing 
priors.reb.missing <- reb_data %>% 
  filter(team_id %in% missing_ids) %>% 
  mutate(name = paste0("team_id_", team_id),
         value = 0.8 * ortg + 0.2 * ortg_l1) %>% 
  select(name, value) %>% 
  bind_rows(reb_data %>% 
              filter(team_id %in% missing_ids) %>% 
              mutate(name = paste0("opp_id_", team_id),
                     value = 0.8 * drtg + 0.2 * drtg_l1) %>% 
              select(name, value))

## Get Intercept ---------
prior.factor.reb <- factors.oreb %>% 
  mutate(weighting = 0.97 ^ (prior_season - season)) %>% 
  with_groups(.groups = name,
              summarise,
              value = weighted.mean(value, weighting))

## Combine into data frame -------
priors.reb <- bind_rows(prior.factor.reb,
                        priors.reb,
                        priors.reb.missing) %>% 
  data.frame(row.names = NULL)

head(priors.reb, 12)

# Clean the environment 
rm(train_data, test_data,
   model.reb.tm, model.reb.opp,
   missing_ids,
   reb_post.tm, reb_post.opp,
   reb_priors_off, reb_priors_def,
   priors.reb.missing, prior.factor.reb,
   reb_data)

## Turnovers --------------------------------------------------------------------------
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


## Train data
train_data <- to_data %>% 
  filter(season < {prior_season - 1},
         !is.na(ortg_next), !is.na(drtg_next),
         season %in% summary.team$season)

## Test data 
test_data <- to_data %>% 
  filter(season == {prior_season - 1},
         !is.na(ortg_next), !is.na(drtg_next))

### Team ----------
print(all_models$to_off)

model.to.tm <- rstanarm::stan_glm(formula = ortg_next ~ ortg + pct_minutes_stay +
                                    tov_stay + commits + rtg_scaled_cmt +
                                    transfers + rtg_scaled_tfr + 
                                    tov + factor(team_id),
                                  data = train_data,
                                  iter = 10000,
                                  algorithm = "sampling")


### Missing team ids
missing_ids <- test_data %>% 
  filter(team_id %!in% train_data$team_id) %>% 
  pull(team_id)

# Posterior draws
to_post.tm <- posterior_predict(model.to.tm,
                                 draws = 500,
                                 newdata = test_data %>% 
                                   # Filter for teams not in new data
                                   filter(team_id %!in% missing_ids)
                                )

# Summarize ------------
to_priors_off <- data.frame(obs = to_post.tm) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(ortg = value) %>% 
  bind_cols(data.frame(test_data %>% 
                         filter(team_id %!in% missing_ids)) %>% 
              select(team_id, ortg_prior = ortg)) %>% 
  select(team_id, ortg, ortg_prior)

print(to_priors_off)

### Opponent ------------
print(all_models$to_def)

model.to.opp <- rstanarm::stan_glm(formula = drtg_next ~ drtg + pct_minutes_stay + 
                                     stl_stay + commits + rtg_scaled_cmt +
                                     transfers + rtg_scaled_tfr + 
                                     stl + factor(team_id),
                                   data = train_data,
                                   iter = 10000,
                                   algorithm = "sampling")


# Posterior draws
to_post.opp <- posterior_predict(model.to.opp,
                                  draws = 500,
                                  newdata = test_data %>% 
                                    # Filter for teams not in new data
                                    filter(team_id %!in% missing_ids)
                                 )

# Summarize ------------
to_priors_def <- data.frame(obs = to_post.opp) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(drtg = value) %>% 
  bind_cols(data.frame(test_data %>% 
                         filter(team_id %!in% missing_ids)) %>% 
              select(team_id, drtg_prior = drtg)) %>% 
  select(team_id, drtg, drtg_prior)

print(to_priors_def)

## Create priors ----------------
priors.to <- to_priors_off %>%
  mutate(name = paste0("team_id_", team_id)) %>%
  rename(value = ortg) %>% 
  select(-contains("_prior")) %>% 
  rbind(to_priors_def %>% 
          mutate(name = paste0("opp_id_", team_id)) %>% 
          rename(value = drtg) %>% 
          select(-contains("_prior"))) %>% 
  select(name, value)

# Add back teams missing 
priors.to.missing <- to_data %>% 
  filter(team_id %in% missing_ids) %>% 
  mutate(name = paste0("team_id_", team_id),
         value = 0.8 * ortg + 0.2 * ortg_l1) %>% 
  select(name, value) %>% 
  bind_rows(to_data %>% 
              filter(team_id %in% missing_ids) %>% 
              mutate(name = paste0("opp_id_", team_id),
                     value = 0.8 * drtg + 0.2 * drtg_l1) %>% 
              select(name, value))

## Get Intercept ---------
prior.factor.to <- factors.to %>% 
  mutate(weighting = 0.97 ^ (prior_season - season)) %>% 
  with_groups(.groups = name,
              summarise,
              value = weighted.mean(value, weighting))

## Combine into data frame -------
priors.to <- bind_rows(prior.factor.to,
                        priors.to,
                        priors.to.missing) %>% 
  data.frame(row.names = NULL)

head(priors.to, 12)

# Clean the environment 
rm(train_data, test_data,
   model.to.tm, model.to.opp,
   missing_ids,
   to_post.tm, to_post.opp,
   to_priors_off, to_priors_def,
   priors.to.missing, prior.factor.to,
   to_data)

## Rating w/ Factors --------------------------------------------------------------------
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

## Train data
train_data <- rtg_data %>% 
  filter(season < {prior_season - 1},
         !is.na(ortg_next), !is.na(drtg_next),
         season %in% summary.team$season)

## Test data 
test_data <- rtg_data %>% 
  filter(season == {prior_season - 1},
         !is.na(ortg_next), !is.na(drtg_next))

### Team ----------
print(all_models$rtg_off)

model.rtg.tm <- rstanarm::stan_glm(formula = ortg_next ~ ortg + pct_minutes_stay +
                                     efg_stay + obpm_stay +
                                     commits + rtg_scaled_cmt +
                                     transfers + rtg_scaled_tfr + efg + obpm +
                                     factor(team_id),
                                  data = train_data, 
                                  iter = 10000,
                                  algorithm = "sampling")


### Missing team ids
missing_ids <- test_data %>% 
  filter(team_id %!in% train_data$team_id) %>% 
  pull(team_id)

# Posterior draws
rtg_post.tm <- posterior_predict(model.rtg.tm,
                                 draws = 500,
                                 newdata = test_data %>%
                                   # Filter for teams not in new data
                                   filter(team_id %!in% missing_ids)
                                 )

# Summarize ------------
rtg_priors_off <- data.frame(obs = rtg_post.tm) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(ortg = value) %>% 
  bind_cols(data.frame(test_data %>% 
                         filter(team_id %!in% missing_ids)) %>% 
              select(team_id, ortg_prior = ortg)) %>% 
  select(team_id, ortg, ortg_prior)

print(rtg_priors_off)

### Opponent ------------
print(all_models$rtg_def)

model.rtg.opp <- rstanarm::stan_glm(formula = drtg_next ~ drtg + pct_minutes_stay +
                                      dbpm_stay + commits + rtg_scaled_cmt +
                                      transfers + rtg_scaled_tfr + 
                                      dbpm + factor(team_id),
                                    data = train_data,
                                    iter = 10000,
                                    algorithm = "sampling")


# Posterior draws
rtg_post.opp <- posterior_predict(model.rtg.opp,
                                  draws = 500,
                                  newdata = test_data %>% 
                                    # Filter for teams not in new data
                                    filter(team_id %!in% missing_ids)
                                  )

# Summarize ------------
rtg_priors_def <- data.frame(obs = rtg_post.opp) %>%
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(drtg = value) %>% 
  bind_cols(data.frame(test_data %>% 
                         filter(team_id %!in% missing_ids)) %>% 
              select(team_id, drtg_prior = drtg)) %>% 
  select(team_id, drtg, drtg_prior)

print(rtg_priors_def)

## Create priors ----------------
priors.rtg <- rtg_priors_off %>%
  mutate(name = paste0("team_id_", team_id)) %>%
  rename(value = ortg) %>% 
  select(-contains("_prior")) %>% 
  rbind(rtg_priors_def %>% 
          mutate(name = paste0("opp_id_", team_id)) %>% 
          rename(value = drtg) %>% 
          select(-contains("_prior"))) %>% 
  select(name, value)

# Add back teams missing 
priors.rtg.missing <- rtg_data %>% 
  filter(team_id %in% missing_ids) %>% 
  mutate(name = paste0("team_id_", team_id),
         value = 0.8 * ortg + 0.2 * ortg_l1) %>% 
  select(name, value) %>% 
  bind_rows(rtg_data %>% 
              filter(team_id %in% missing_ids) %>% 
              mutate(name = paste0("opp_id_", team_id),
                     value = 0.8 * drtg + 0.2 * drtg_l1) %>% 
              select(name, value))

## Get Intercept ---------
prior.factor.rtg <- factors.rtg %>% 
  mutate(weighting = 0.97 ^ (prior_season - season)) %>% 
  with_groups(.groups = name,
              summarise,
              value = weighted.mean(value, weighting))

## Combine into data frame -------
priors.rtg <- bind_rows(prior.factor.rtg,
                        priors.rtg,
                        priors.rtg.missing) %>%
  data.frame(row.names = NULL)

head(priors.rtg, 12)

# Clean the environment 
rm(train_data, test_data,
   model.rtg.tm, model.rtg.opp,
   missing_ids,
   rtg_post.tm, rtg_post.opp,
   rtg_priors_off, rtg_priors_def,
   priors.rtg.missing, prior.factor.rtg,
   rtg_data)


## EFG --------------------------------------------------------------------
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

## Train data
train_data <- efg_data %>% 
  filter(season < {prior_season - 1},
         !is.na(ortg_next), !is.na(drtg_next),
         season %in% summary.team$season)

## Test data 
test_data <- efg_data %>% 
  filter(season == {prior_season - 1},
         !is.na(ortg_next), !is.na(drtg_next))

### Team ----------
print(all_models$efg_off)

model.efg.tm <- rstanarm::stan_glm(formula = ortg_next ~ ortg + pct_minutes_stay +
                                     efg_stay + obpm_stay +
                                     commits + rtg_scaled_cmt +
                                     transfers + rtg_scaled_tfr +
                                     efg + obpm + factor(team_id),
                                   data = train_data, 
                                   iter = 10000,
                                   algorithm = "sampling")


### Missing team ids
missing_ids <- test_data %>% 
  filter(team_id %!in% train_data$team_id) %>% 
  pull(team_id)

# Posterior draws
efg_post.tm <- posterior_predict(model.efg.tm,
                                 draws = 500,
                                 newdata = test_data %>%
                                   # Filter for teams not in new data
                                   filter(team_id %!in% missing_ids)
                                 )

# Summarize ------------
efg_priors_off <- data.frame(obs = efg_post.tm) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(ortg = value) %>% 
  bind_cols(data.frame(test_data %>% 
                         filter(team_id %!in% missing_ids)) %>% 
              select(team_id, ortg_prior = ortg)) %>% 
  select(team_id, ortg, ortg_prior)

print(efg_priors_off)

### Opponent ------------
print(all_models$efg_def)

model.efg.opp <- rstanarm::stan_glm(formula = drtg_next ~ drtg + pct_minutes_stay + 
                                      dbpm_stay + commits + rtg_scaled_cmt +
                                      transfers + rtg_scaled_tfr + 
                                      dbpm + factor(team_id),
                                    data = train_data,
                                    iter = 10000,
                                    algorithm = "sampling")


# Posterior draws
efg_post.opp <- posterior_predict(model.efg.opp,
                                  draws = 500,
                                  newdata = test_data %>% 
                                    # Filter for teams not in new data
                                    filter(team_id %!in% missing_ids)
                                  )

# Summarize ------------
efg_priors_def <- data.frame(obs = efg_post.opp) %>%
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(drtg = value) %>% 
  bind_cols(data.frame(test_data %>% 
                         filter(team_id %!in% missing_ids)) %>% 
              select(team_id, drtg_prior = drtg)) %>% 
  select(team_id, drtg, drtg_prior)

print(efg_priors_def)

## Create priors ----------------
priors.efg <- efg_priors_off %>%
  mutate(name = paste0("team_id_", team_id)) %>%
  rename(value = ortg) %>% 
  select(-contains("_prior")) %>% 
  rbind(efg_priors_def %>% 
          mutate(name = paste0("opp_id_", team_id)) %>% 
          rename(value = drtg) %>% 
          select(-contains("_prior"))) %>% 
  select(name, value)

# Add back teams missing 
priors.efg.missing <- efg_data %>% 
  filter(team_id %in% missing_ids) %>% 
  mutate(name = paste0("team_id_", team_id),
         value = 0.8 * ortg + 0.2 * ortg_l1) %>% 
  select(name, value) %>% 
  bind_rows(efg_data %>% 
              filter(team_id %in% missing_ids) %>% 
              mutate(name = paste0("opp_id_", team_id),
                     value = 0.8 * drtg + 0.2 * drtg_l1) %>% 
              select(name, value))

## Get Intercept ---------
prior.factor.efg <- factors.efg %>% 
  mutate(weighting = 0.97 ^ (prior_season - season)) %>% 
  with_groups(.groups = name,
              summarise,
              value = weighted.mean(value, weighting))

## Combine into data frame -------
priors.efg <- bind_rows(prior.factor.efg,
                        priors.efg,
                        priors.efg.missing) %>%
  data.frame(row.names = NULL)

head(priors.efg, 12)

# Clean the environment 
rm(train_data, test_data,
   model.efg.tm, model.efg.opp,
   missing_ids,
   efg_post.tm, efg_post.opp,
   efg_priors_off, efg_priors_def,
   priors.efg.missing, prior.factor.efg,
   efg_data)


## Raw Ratings --------------------------------------------------------------------
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

## Train data
train_data <- rtg_raw_data %>% 
  filter(season < {prior_season - 1},
         !is.na(ortg_next), !is.na(drtg_next),
         season %in% summary.team$season)

## Test data 
test_data <- rtg_raw_data %>% 
  filter(season == {prior_season - 1},
         !is.na(ortg_next), !is.na(drtg_next))

### Team ----------
print(all_models$rtg_raw_off)

model.rtg_raw.tm <- rstanarm::stan_glm(formula = ortg_next ~ ortg + pct_minutes_stay + 
                                     efg_stay + obpm_stay + oreb_rate_stay +
                                     ast_stay + tov_stay + commits + rtg_scaled_cmt +
                                     transfers + rtg_scaled_tfr + efg + 
                                     obpm + oreb_rate + ast + tov + factor(team_id),
                                   data = train_data, 
                                   iter = 10000,
                                   algorithm = "sampling")


### Missing team ids
missing_ids <- test_data %>% 
  filter(team_id %!in% train_data$team_id) %>% 
  pull(team_id)

# Posterior draws
rtg_raw_post.tm <- posterior_predict(model.rtg_raw.tm,
                                 draws = 500,
                                 newdata = test_data %>%
                                   # Filter for teams not in new data
                                   filter(team_id %!in% missing_ids)
                                 )

# Summarize ------------
rtg_raw_priors_off <- data.frame(obs = rtg_raw_post.tm) %>% 
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(ortg = value) %>% 
  bind_cols(data.frame(test_data %>% 
                         filter(team_id %!in% missing_ids)) %>% 
              select(team_id, ortg_prior = ortg)) %>% 
  select(team_id, ortg, ortg_prior)

print(rtg_raw_priors_off)

### Opponent ------------
print(all_models$rtg_raw_def)

model.rtg_raw.opp <- rstanarm::stan_glm(formula = drtg_next ~ drtg + pct_minutes_stay + 
                                          efg_stay + dbpm_stay + dreb_rate_stay +
                                          stl_stay + commits + rtg_scaled_cmt +
                                          transfers + rtg_scaled_tfr + dbpm + dreb_rate + 
                                          stl + factor(team_id),
                                        data = train_data,
                                        iter = 10000, 
                                        algorithm = "sampling")


# Posterior draws
rtg_raw_post.opp <- posterior_predict(model.rtg_raw.opp,
                                      draws = 500,
                                      newdata = test_data %>%
                                        # Filter for teams not in new data
                                        filter(team_id %!in% missing_ids)
                                      )

# Summarize ------------
rtg_raw_priors_def <- data.frame(obs = rtg_raw_post.opp) %>%
  summarise_all(~ mean(.)) %>% 
  pivot_longer(cols = everything()) %>% 
  rename(drtg = value) %>% 
  bind_cols(data.frame(test_data %>% 
                         filter(team_id %!in% missing_ids)) %>% 
              select(team_id, drtg_prior = drtg)) %>% 
  select(team_id, drtg, drtg_prior)

print(rtg_raw_priors_def)

## Create priors ----------------
priors.rtg_raw <- rtg_raw_priors_off %>%
  mutate(name = paste0("team_id_", team_id)) %>%
  rename(value = ortg) %>% 
  select(-contains("_prior")) %>% 
  rbind(rtg_raw_priors_def %>% 
          mutate(name = paste0("opp_id_", team_id)) %>% 
          rename(value = drtg) %>% 
          select(-contains("_prior"))) %>% 
  select(name, value)

# Add back teams missing 
priors.rtg_raw.missing <- rtg_raw_data %>% 
  filter(team_id %in% missing_ids) %>% 
  mutate(name = paste0("team_id_", team_id),
         value = 0.8 * ortg + 0.2 * ortg_l1) %>% 
  select(name, value) %>% 
  bind_rows(rtg_raw_data %>% 
              filter(team_id %in% missing_ids) %>% 
              mutate(name = paste0("opp_id_", team_id),
                     value = 0.8 * drtg + 0.2 * drtg_l1) %>% 
              select(name, value))

## Get Intercept ---------
prior.factor.rtg_raw <- factors.raw_rtg %>% 
  mutate(weighting = 0.97 ^ (prior_season - season)) %>% 
  with_groups(.groups = name,
              summarise,
              value = weighted.mean(value, weighting))

## Combine into data frame -------
priors.rtg_raw <- bind_rows(prior.factor.rtg_raw,
                        priors.rtg_raw,
                        priors.rtg_raw.missing) %>%
  data.frame(row.names = NULL)

head(priors.rtg_raw, 12)

# Clean the environment 
rm(train_data, test_data,
   model.rtg_raw.tm, model.rtg_raw.opp,
   missing_ids,
   rtg_raw_post.tm, rtg_raw_post.opp,
   rtg_raw_priors_off, rtg_raw_priors_def,
   priors.rtg_raw.missing, prior.factor.rtg_raw,
   rtg_raw_data)

# Combine all ratings in to single list --------------------
priors.all <- list(pace = {priors.pace},
                   ast = {priors.ast},
                   oreb = {priors.reb},
                   to = {priors.to},
                   rtg = {priors.rtg},
                   # Additional for site
                   efg = {priors.efg},
                   raw_rating = {priors.rtg_raw}
                   )

## Save ratings --------
saveRDS(priors.all,
        glue::glue("Power Ratings/Team Ratings/Priors/priors_{prior_season}.rds"))



