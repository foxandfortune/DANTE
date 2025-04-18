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
prior_season <- 2025

# Set minimum year for now 
min_season <- 2021

# Load Teams 
all_teams <- readRDS('Teams/team_database_wbb.rds')

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
# Build models-------------------------------------------------------------------
## Pace--------------------------------------------------------------------------
pace_data <- rating.pace %>% 
  select(season, team_id, ortg, drtg) %>% 
  mutate(next_season = season + 1) %>% 
  left_join(rating.pace %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(next_season)) %>% 
  relocate(contains("ortg"), .after = team_id)

unique(pace_data$season)

## Train data
train_data <- pace_data %>% 
  filter(season < {prior_season - 1},
         !is.na(ortg_next), !is.na(drtg_next))

unique(train_data$season)

## Test data 
test_data <- pace_data %>% 
  filter(season == {prior_season - 1})

### Team ----------
model.pace.tm <- rstanarm::stan_glm(formula = ortg_next ~ ortg + factor(team_id), 
                                    data = train_data,
                                    iter = 5000,
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
model.pace.opp <- rstanarm::stan_glm(formula = drtg_next ~ drtg + factor(team_id), 
                                     data = train_data,
                                     iter = 5000,
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
         value = 0.8 * ortg) %>% 
  select(name, value) %>% 
  bind_rows(pace_data %>% 
              filter(team_id %in% missing_ids) %>% 
              mutate(name = paste0("opp_id_", team_id),
                     value = 0.8 * drtg) %>% 
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
  mutate(next_season = season + 1) %>% 
  left_join(rating.ast %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(next_season)) %>% 
  relocate(contains("ortg"), .after = team_id)

## Train data
train_data <- ast_data %>% 
  filter(season < {prior_season - 1},
         !is.na(ortg_next), !is.na(drtg_next))

unique(train_data$season)

## Test data 
test_data <- ast_data %>% 
  filter(season == {prior_season - 1})

unique(test_data$season)

### Team ----------
model.ast.tm <- rstanarm::stan_glm(formula = ortg_next ~ ortg + factor(team_id),
                                   data = train_data,
                                   iter = 5000,
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
model.ast.opp <- rstanarm::stan_glm(formula = drtg_next ~ drtg + factor(team_id), 
                                    data = train_data,
                                    iter = 5000,
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
         value = 0.8 * ortg) %>% 
  select(name, value) %>% 
  bind_rows(ast_data %>% 
              filter(team_id %in% missing_ids) %>% 
              mutate(name = paste0("opp_id_", team_id),
                     value = 0.8 * drtg) %>% 
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
  mutate(next_season = season + 1) %>% 
  left_join(rating.oreb %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(next_season)) %>% 
  relocate(contains("ortg"), .after = team_id)

## Train data
train_data <- reb_data %>% 
  filter(season < {prior_season - 1},
         !is.na(ortg_next), !is.na(drtg_next))

unique(train_data$season)

## Test data 
test_data <- reb_data %>% 
  filter(season == {prior_season - 1})

unique(test_data$season)


### Team ----------
model.reb.tm <- rstanarm::stan_glm(formula = ortg_next ~ ortg + factor(team_id),
                                   data = train_data,
                                   iter = 5000, 
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

head(test_data)

### Opponent ------------
model.reb.opp <- rstanarm::stan_glm(formula = drtg_next ~ drtg + factor(team_id),
                                    data = train_data,
                                    iter = 5000, 
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
head(test_data)

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
         value = 0.8 * ortg) %>% 
  select(name, value) %>% 
  bind_rows(reb_data %>% 
              filter(team_id %in% missing_ids) %>% 
              mutate(name = paste0("opp_id_", team_id),
                     value = 0.8 * drtg) %>% 
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
  mutate(next_season = season + 1) %>% 
  left_join(rating.to %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(next_season)) %>% 
  relocate(contains("ortg"), .after = team_id)

## Train data
train_data <- to_data %>% 
  filter(season < {prior_season - 1},
         !is.na(ortg_next), !is.na(drtg_next))

unique(train_data$season)

## Test data 
test_data <- to_data %>% 
  filter(season == {prior_season - 1})

unique(test_data$season)


### Team ----------
model.to.tm <- rstanarm::stan_glm(formula = ortg_next ~ ortg + factor(team_id),
                                  data = train_data,
                                  iter = 5000,
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
head(test_data)

### Opponent ------------
model.to.opp <- rstanarm::stan_glm(formula = drtg_next ~ drtg + factor(team_id),
                                   data = train_data,
                                   iter = 5000,
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
head(test_data)

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
         value = 0.8 * ortg) %>% 
  select(name, value) %>% 
  bind_rows(to_data %>% 
              filter(team_id %in% missing_ids) %>% 
              mutate(name = paste0("opp_id_", team_id),
                     value = 0.8 * drtg) %>% 
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
  mutate(next_season = season + 1) %>% 
  left_join(rating.rtg %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(next_season)) %>% 
  relocate(contains("ortg"), .after = team_id)

## Train data
train_data <- rtg_data %>% 
  filter(season < {prior_season - 1},
         !is.na(ortg_next), !is.na(drtg_next))

unique(train_data$season)

## Test data 
test_data <- rtg_data %>% 
  filter(season == {prior_season - 1})

unique(test_data$season)

### Team ----------
model.rtg.tm <- rstanarm::stan_glm(formula = ortg_next ~ ortg + factor(team_id),
                                  data = train_data, 
                                  iter = 5000,
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
head(test_data)

### Opponent ------------
model.rtg.opp <- rstanarm::stan_glm(formula = drtg_next ~ drtg + factor(team_id),
                                    data = train_data,
                                    iter = 5000,
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
head(test_data)

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
         value = 0.8 * ortg) %>% 
  select(name, value) %>% 
  bind_rows(rtg_data %>% 
              filter(team_id %in% missing_ids) %>% 
              mutate(name = paste0("opp_id_", team_id),
                     value = 0.8 * drtg) %>% 
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
  mutate(next_season = season + 1) %>% 
  left_join(rating.efg %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(next_season)) %>% 
  relocate(contains("ortg"), .after = team_id)

## Train data
train_data <- efg_data %>% 
  filter(season < {prior_season - 1},
         !is.na(ortg_next), !is.na(drtg_next))

unique(train_data$season)

## Test data 
test_data <- efg_data %>% 
  filter(season == {prior_season - 1})

unique(test_data$season)

### Team ----------
print(all_models$efg_off)

model.efg.tm <- rstanarm::stan_glm(formula = ortg_next ~ ortg + factor(team_id),
                                   data = train_data, 
                                   iter = 5000,
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
head(test_data)

### Opponent ------------
model.efg.opp <- rstanarm::stan_glm(formula = drtg_next ~ drtg + factor(team_id),
                                    data = train_data,
                                    iter = 5000,
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
head(test_data)

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
         value = 0.8 * ortg) %>% 
  select(name, value) %>% 
  bind_rows(efg_data %>% 
              filter(team_id %in% missing_ids) %>% 
              mutate(name = paste0("opp_id_", team_id),
                     value = 0.8 * drtg) %>% 
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
  mutate(next_season = season + 1) %>% 
  left_join(rating.raw_rtg %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(next_season)) %>% 
  relocate(contains("ortg"), .after = team_id)

## Train data
train_data <- rtg_raw_data %>% 
  filter(season < {prior_season - 1},
         !is.na(ortg_next), !is.na(drtg_next))

unique(train_data$season)

## Test data 
test_data <- rtg_raw_data %>% 
  filter(season == {prior_season - 1})

unique(test_data$season)

### Team ----------
model.rtg_raw.tm <- rstanarm::stan_glm(formula = ortg_next ~ ortg + factor(team_id),
                                   data = train_data, 
                                   iter = 5000,
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
head(test_data)

### Opponent ------------
print(all_models$rtg_raw_def)

model.rtg_raw.opp <- rstanarm::stan_glm(formula = drtg_next ~ drtg + factor(team_id),
                                        data = train_data,
                                        iter = 5000, 
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
head(test_data)

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
         value = 0.8 * ortg) %>% 
  select(name, value) %>% 
  bind_rows(rtg_raw_data %>% 
              filter(team_id %in% missing_ids) %>% 
              mutate(name = paste0("opp_id_", team_id),
                     value = 0.8 * drtg) %>% 
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
        glue::glue("Power Ratings/Team Ratings/Priors/priors_wbb_{prior_season}.rds"))



