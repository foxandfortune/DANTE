library(cbbdata)
library(tidyverse)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')

# Negate in formula
`%!in%` = Negate(`%in%`)

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
  
  rm(temp, yr, i)
}


## Get the ratings data into normal format ------------
head(rating.pace)
str(all_teams)

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
            by = c("team_id", "season"))

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
            by = c("team_id", "season"))

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
            by = c("team_id", "season"))

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
            by = c("team_id", "season"))

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
            by = c("team_id", "season"))


# Load Commits/Transfers/Draft Entrants -----------------
files.commits <- unlist(map(list.files(path = 'Recruiting', pattern = 'commits_clean'),
                            ~glue::glue("Recruiting/", ., sep = "")))

files.transfers <- unlist(map(list.files(path = 'Recruiting', pattern = 'transfers_clean'),
                              ~glue::glue("Recruiting/", ., sep = "")))

files.draftees <- unlist(map(list.files(path = 'Recruiting', pattern = 'draft_entrants_clean'),
                             ~glue::glue("Recruiting/", ., sep = "")))

files.graduates <- unlist(map(list.files(path = 'Recruiting', pattern = 'graduates_clean'),
                             ~glue::glue("Recruiting/", ., sep = "")))


commits <- map_df(files.commits, readRDS)
transfers <- map_df(files.transfers, readRDS)
draftees <- map_df(files.draftees, readRDS)
graduates <- map_df(files.graduates, readRDS)

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
### Get players that are missing possitions
player.stats %>%
  filter(is.na(pos)) %>% 
  select(match_name, season, team_id, Team) 

### Fix manually
missing_pos <- player.stats %>%
  filter(is.na(pos)) %>% 
  select(match_name, season, team_id) %>% 
  mutate(pos_no_fix = c(2, 2, 2, 1,
                    2, 5, 2))

### Summarize player stats by: Season/Team ID/Left Program/Position Group ------
summary.team.grp <- player.stats %>% 
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
  left_join(missing_pos, by =c("match_name", "season", "team_id")) %>% 
  mutate(left_school = case_when(
    drafted == 1 | grad == 1 | transfer ==1 ~ -1,
    TRUE ~ 0),
    total_min = g * mpg,
    pos_no = case_when(
      pos %in% c("Scoring PG", "Pure PG") ~ 1,
      pos %in% c("Combo G", "Wing G") ~ 2,
      pos %in% c("Wing F") ~ 3,
      pos %in% c("Stretch 4") ~ 4,
      pos %in% c("PF/C", "C") ~ 5,
      is.na(pos) ~ pos_no_fix),
    pos_grp = case_when(
      pos_no %in% c(1, 2, 3) ~ "BC",
      pos_no %in% c(4, 5) ~ "FC")) %>%
  replace(is.na(.), 0) %>% 
  with_groups(.groups = c(season, team_id, Team,
                          pos_grp, 
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
  with_groups(.groups = c(season, team_id, pos_grp),
              mutate,
              pct_minutes = minutes / sum(minutes)) %>% 
  relocate(pct_minutes, .after = minutes)

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
  left_join(missing_pos, by =c("match_name", "season", "team_id")) %>% 
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
  relocate(pct_minutes, .after = minutes)



# Create summary of Commits, Transfers, Drafted/Graduated ----------
## Commits ---------
summary.commit.grp <- commits %>% 
  mutate(pos_grp = case_when(
    pos %in% c("SG", "PG", "CG", "SF") ~ "BC",
    pos %in% c("C", "PF") ~ "FC")) %>%
  with_groups(.groups = c(season, team_id, pos_grp),
              summarise,
              commits = n_distinct(player),
              avg_rtg = mean(rating),
              max_rtg = max(rating),
              min_rtg = min(rating))

summary.commit <- commits %>% 
  mutate(pos_grp = case_when(
    pos %in% c("SG", "PG", "CG", "SF") ~ "BC",
    pos %in% c("C", "PF") ~ "FC")) %>%
  with_groups(.groups = c(season, team_id),
              summarise,
              commits = n_distinct(player),
              avg_rtg = mean(rating),
              max_rtg = max(rating),
              min_rtg = min(rating))

## Transfers ---------
summary.transfers.grp <- transfers %>% 
  mutate(pos_grp = case_when(
    pos %in% c("SG", "PG", "CG", "SF") ~ "BC",
    pos %in% c("C", "PF") ~ "FC")) %>%
  replace(is.na(.), 0) %>% 
  with_groups(.groups = c(season, transtoID, pos_grp
                          ),
              summarise,
              transfers = n_distinct(match_name),
              minutes = sum(total_min),
              avg_rtg = mean(rating),
              max_rtg = max(rating),
              min_rtg = min(rating),
              oreb_rate = weighted.mean(oreb_rate, total_min),
              dreb_rate = weighted.mean(dreb_rate, total_min),
              ast = weighted.mean(ast, total_min),
              tov = weighted.mean(tov, total_min),
              stl = weighted.mean(stl, total_min),
              efg = weighted.mean(efg, total_min),
              obpm = weighted.mean(obpm, total_min),
              dbpm = weighted.mean(dbpm, total_min)) %>% 
  replace(is.na(.), 0)

summary.transfers <- transfers %>% 
  mutate(pos_grp = case_when(
    pos %in% c("SG", "PG", "CG", "SF") ~ "BC",
    pos %in% c("C", "PF") ~ "FC")) %>%
  replace(is.na(.), 0) %>% 
  with_groups(.groups = c(season, transtoID),
              summarise,
              transfers = n_distinct(match_name),
              minutes = sum(total_min),
              avg_rtg = mean(rating),
              max_rtg = max(rating),
              min_rtg = min(rating),
              oreb_rate = weighted.mean(oreb_rate, total_min),
              dreb_rate = weighted.mean(dreb_rate, total_min),
              ast = weighted.mean(ast, total_min),
              tov = weighted.mean(tov, total_min),
              stl = weighted.mean(stl, total_min),
              efg = weighted.mean(efg, total_min),
              obpm = weighted.mean(obpm, total_min),
              dbpm = weighted.mean(dbpm, total_min)) %>%
  replace(is.na(.), 0)


# START MODELING --------------------------------------------------
## Rebounding ---------------
### Offensive Rebounds ---------- 
oreb_data.grp <- rating.oreb %>% 
  select(season, team_id, ortg) %>% 
  mutate(next_season = season + 1) %>% 
  left_join(rating.oreb %>% 
              select(season, team_id, ortg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  filter(!is.na(ortg_next),
         season %in% summary.team$season) %>% 
  left_join(summary.team.grp %>% 
              mutate(status = case_when(
                left_school == -1 ~ "left",
                left_school == 0 ~ "stay")) %>% 
              select(season:pos_grp, status, players, pct_minutes, oreb_rate, -left_school) %>%
              pivot_wider(names_from = c(status, pos_grp), values_from = c(players:oreb_rate)) %>% 
              mutate_at(vars(contains("_BC"), contains("_FC")), ~replace_na(., 0)) %>% 
              mutate_at(vars(contains("_left")), ~ . * -1),
            by = c("season", "team_id")) %>% 
  left_join(summary.commit.grp %>% 
              pivot_wider(names_from = c(pos_grp), values_from = c(commits:min_rtg)) %>% 
              mutate_at(vars(contains("_BC"), contains("_FC")), ~ replace_na(., 0)),
            by = c("season", "team_id")) %>% 
  left_join(summary.transfers.grp %>% 
              select(season, team_id = transtoID, pos_grp:oreb_rate, -minutes) %>% 
              pivot_wider(names_from = pos_grp, values_from = c(transfers:oreb_rate)) %>% 
              mutate_at(vars(contains("_BC"), contains("_FC")), ~ replace_na(., 0)),
            by = c("season", "team_id"),
            suffix = c("_cmt", "_tfr")) %>% 
  mutate_at(vars(contains("_FC"), contains("_BC")), ~ replace_na(., 0)) %>% 
  select(-c(team_id, Team, next_season))

oreb_data <- rating.oreb %>% 
  select(season, team_id, ortg) %>% 
  mutate(next_season = season + 1) %>% 
  left_join(rating.oreb %>% 
              select(season, team_id, ortg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  filter(!is.na(ortg_next),
         season %in% summary.team$season) %>% 
  left_join(summary.team %>% 
              mutate(status = case_when(
                left_school == -1 ~ "left",
                left_school == 0 ~ "stay")) %>% 
              select(season:Team, status, players, pct_minutes, oreb_rate, -left_school) %>%
              pivot_wider(names_from = c(status), values_from = c(players:oreb_rate)) %>% 
              #mutate_at(vars(contains("_BC"), contains("_FC")), ~replace_na(., 0)) %>% 
              mutate_at(vars(contains("_left")), ~ . * -1),
            by = c("season", "team_id")) %>% 
  left_join(summary.commit,
            by = c("season", "team_id")) %>% 
  left_join(summary.transfers %>% 
              select(season, team_id = transtoID, transfers:oreb_rate, -minutes),
            by = c("season", "team_id"),
            suffix = c("_cmt", "_tfr")) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(team_id, Team, next_season))

### Create model
test_season <- 2022

oreb_model <- glm(ortg_next ~ ortg + pct_minutes_stay + oreb_rate_stay +
                    commits + avg_rtg_cmt + max_rtg_cmt +
                    transfers + avg_rtg_tfr + max_rtg_tfr + oreb_rate,
                      data = oreb_data %>%
                        filter(season != test_season) %>% 
                        select(-season))

oreb_model.grp <- glm(ortg_next ~ .,
                      data = oreb_data.grp %>%
                        filter(season != test_season) %>%
                        select(-season))

oreb_model_simple <- glm(ortg_next ~ ortg,
                         data = oreb_data %>%
                           filter(season != test_season))


summary(oreb_model)
summary(oreb_model.grp)
summary(oreb_most_simple)

oreb_data %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(oreb_model, oreb_data %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, ortg_next), rmse = Metrics::rmse(ortg_next, preds))

oreb_data.grp %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(oreb_model.grp, oreb_data.grp %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, ortg_next), rmse = Metrics::rmse(ortg_next, preds))

oreb_data %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(oreb_model_simple, oreb_data %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, ortg_next), rmse = Metrics::rmse(ortg_next, preds))


### Defensive Rebounds ---------------- 
dreb_data.grp <- rating.oreb %>% 
  select(season, team_id, drtg) %>% 
  mutate(next_season = season + 1) %>% 
  left_join(rating.oreb %>% 
              select(season, team_id, drtg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  filter(!is.na(drtg_next),
         season %in% summary.team$season) %>% 
  left_join(summary.team.grp %>% 
              mutate(status = case_when(
                left_school == -1 ~ "left",
                left_school == 0 ~ "stay")) %>% 
              select(season:pos_grp, status, players, pct_minutes, dreb_rate, -left_school) %>%
              pivot_wider(names_from = c(status, pos_grp), values_from = c(players:dreb_rate)) %>% 
              mutate_at(vars(contains("_BC"), contains("_FC")), ~replace_na(., 0)) %>% 
              mutate_at(vars(contains("_left")), ~ . * -1),
            by = c("season", "team_id")) %>% 
  left_join(summary.commit.grp %>% 
              pivot_wider(names_from = c(pos_grp), values_from = c(commits:min_rtg)) %>% 
              mutate_at(vars(contains("_BC"), contains("_FC")), ~ replace_na(., 0)),
            by = c("season", "team_id")) %>% 
  left_join(summary.transfers.grp %>% 
              select(season, team_id = transtoID, pos_grp:dreb_rate, -minutes) %>% 
              pivot_wider(names_from = pos_grp, values_from = c(transfers:dreb_rate)) %>% 
              mutate_at(vars(contains("_BC"), contains("_FC")), ~ replace_na(., 0)),
            by = c("season", "team_id"),
            suffix = c("_cmt", "_tfr")) %>% 
  mutate_at(vars(contains("_FC"), contains("_BC")), ~ replace_na(., 0)) %>% 
  select(-c(team_id, Team, next_season))

dreb_data <- rating.oreb %>% 
  select(season, team_id, drtg) %>% 
  mutate(next_season = season + 1) %>% 
  left_join(rating.oreb %>% 
              select(season, team_id, drtg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  filter(!is.na(drtg_next),
         season %in% summary.team$season) %>% 
  left_join(summary.team %>% 
              mutate(status = case_when(
                left_school == -1 ~ "left",
                left_school == 0 ~ "stay")) %>% 
              select(season:Team, status, players, pct_minutes, dreb_rate, -left_school) %>%
              pivot_wider(names_from = c(status), values_from = c(players:dreb_rate)) %>% 
              mutate_at(vars(contains("_left")), ~ . * -1),
            by = c("season", "team_id")) %>% 
  left_join(summary.commit,
            by = c("season", "team_id")) %>% 
  left_join(summary.transfers %>% 
              select(season, team_id = transtoID, transfers:dreb_rate, -minutes),
            by = c("season", "team_id"),
            suffix = c("_cmt", "_tfr")) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(team_id, Team, next_season))

### Create model
test_season <- 2021

dreb_model <- glm(drtg_next ~ drtg + pct_minutes_stay + dreb_rate_stay +
                    commits + avg_rtg_cmt + max_rtg_cmt +
                    transfers + avg_rtg_tfr + max_rtg_tfr + dreb_rate,
                  data = dreb_data %>%
                    filter(season != test_season) %>% 
                    select(-season))

dreb_model.grp <- glm(drtg_next ~ .,
                      data = dreb_data.grp %>%
                        filter(season != test_season) %>%
                        select(-season))

dreb_model_simple <- glm(drtg_next ~ drtg,
                         data = dreb_data %>%
                           filter(season != test_season))


summary(dreb_model)
summary(dreb_model.grp)
summary(dreb_model_simple)

dreb_data %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(dreb_model, dreb_data %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, drtg_next), rmse = Metrics::rmse(drtg_next, preds))

dreb_data.grp %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(dreb_model.grp, dreb_data.grp %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, drtg_next), rmse = Metrics::rmse(drtg_next, preds))

dreb_data %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(dreb_model_simple, dreb_data %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, drtg_next), rmse = Metrics::rmse(drtg_next, preds))

## Turnover rates ------------------------
### Offensive Turnover Rates ---------
oto_data.grp <- rating.to %>% 
  select(season, team_id, ortg) %>% 
  mutate(next_season = season + 1) %>% 
  left_join(rating.to %>% 
              select(season, team_id, ortg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  filter(!is.na(ortg_next),
         season %in% summary.team$season) %>% 
  left_join(summary.team.grp %>% 
              mutate(status = case_when(
                left_school == -1 ~ "left",
                left_school == 0 ~ "stay")) %>% 
              select(season:pos_grp, status, players, pct_minutes, tov, -left_school) %>%
              pivot_wider(names_from = c(status, pos_grp), values_from = c(players:tov)) %>% 
              mutate_at(vars(contains("_BC"), contains("_FC")), ~replace_na(., 0)) %>% 
              mutate_at(vars(contains("_left")), ~ . * -1),
            by = c("season", "team_id")) %>% 
  left_join(summary.commit.grp %>% 
              pivot_wider(names_from = c(pos_grp), values_from = c(commits:min_rtg)) %>% 
              mutate_at(vars(contains("_BC"), contains("_FC")), ~ replace_na(., 0)),
            by = c("season", "team_id")) %>% 
  left_join(summary.transfers.grp %>% 
              select(season, team_id = transtoID, pos_grp:min_rtg, tov, -minutes) %>% 
              pivot_wider(names_from = pos_grp, values_from = c(transfers:tov)) %>% 
              mutate_at(vars(contains("_BC"), contains("_FC")), ~ replace_na(., 0)),
            by = c("season", "team_id"),
            suffix = c("_cmt", "_tfr")) %>% 
  mutate_at(vars(contains("_FC"), contains("_BC")), ~ replace_na(., 0)) %>% 
  select(-c(team_id, Team, next_season))

oto_data <- rating.to %>% 
  select(season, team_id, ortg) %>% 
  mutate(next_season = season + 1) %>% 
  left_join(rating.to %>% 
              select(season, team_id, ortg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  filter(!is.na(ortg_next),
         season %in% summary.team$season) %>% 
  left_join(summary.team %>% 
              mutate(status = case_when(
                left_school == -1 ~ "left",
                left_school == 0 ~ "stay")) %>% 
              select(season:Team, status, players, pct_minutes, tov, -left_school) %>%
              pivot_wider(names_from = c(status), values_from = c(players:tov)) %>% 
              mutate_at(vars(contains("_left")), ~ . * -1),
            by = c("season", "team_id")) %>% 
  left_join(summary.commit,
            by = c("season", "team_id")) %>% 
  left_join(summary.transfers %>% 
              select(season, team_id = transtoID, transfers:min_rtg, tov, -minutes),
            by = c("season", "team_id"),
            suffix = c("_cmt", "_tfr")) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(team_id, Team, next_season))

### Create model
test_season <- 2022

oto_model <- glm(ortg_next ~ ortg + pct_minutes_stay + tov_stay +
                    commits + avg_rtg_cmt + max_rtg_cmt +
                    transfers + avg_rtg_tfr + max_rtg_tfr + tov,
                  data = oto_data %>%
                    filter(season != test_season) %>% 
                    select(-season))

oto_model.grp <- glm(ortg_next ~ .,
                     data = oto_data.grp %>%
                        filter(season != test_season) %>%
                        select(-season))

oto_model_simple <- glm(ortg_next ~ ortg,
                         data = oto_data %>%
                           filter(season != test_season))


summary(oto_model)
summary(oto_model.grp)
summary(oto_model_simple)

oto_data %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(oto_model, oto_data %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, ortg_next), rmse = Metrics::rmse(ortg_next, preds))

oto_data.grp %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(oto_model.grp, oto_data.grp %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, ortg_next), rmse = Metrics::rmse(ortg_next, preds))  

oto_data %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(oto_model_simple, oto_data %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, ortg_next), rmse = Metrics::rmse(ortg_next, preds))

### Defensive Turnover Rates ---------
dto_data <- rating.to %>% 
  select(season, team_id, drtg) %>% 
  mutate(next_season = season + 1) %>% 
  left_join(rating.to %>% 
              select(season, team_id, drtg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  filter(!is.na(drtg_next),
         season %in% summary.team$season) %>% 
  left_join(summary.team %>% 
              mutate(status = case_when(
                left_school == -1 ~ "left",
                left_school == 0 ~ "stay")) %>% 
              select(season:Team, status, players, pct_minutes, stl, -left_school) %>%
              pivot_wider(names_from = c(status), values_from = c(players:stl)) %>% 
              mutate_at(vars(contains("_left")), ~ . * -1),
            by = c("season", "team_id")) %>% 
  left_join(summary.commit,
            by = c("season", "team_id")) %>% 
  left_join(summary.transfers %>% 
              select(season, team_id = transtoID, transfers:min_rtg, stl, -minutes),
            by = c("season", "team_id"),
            suffix = c("_cmt", "_tfr")) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(team_id, Team, next_season))

### Create model
test_season <- 2021

dto_model <- glm(drtg_next ~ drtg + pct_minutes_stay + stl_stay +
                   commits + avg_rtg_cmt + max_rtg_cmt +
                   transfers + avg_rtg_tfr + max_rtg_tfr + stl,
                 data = dto_data %>%
                   filter(season != test_season) %>% 
                   select(-season))

dto_model_simple <- glm(drtg_next ~ drtg,
                        data = dto_data %>%
                          filter(season != test_season))


summary(dto_model)
summary(dto_model_simple)

dto_data %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(dto_model, dto_data %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, drtg_next), rmse = Metrics::rmse(drtg_next, preds))

dto_data %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(dto_model_simple, dto_data %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, drtg_next), rmse = Metrics::rmse(drtg_next, preds))

## Assist Rates --------------------------------------
### Offensive Assist rates -----------------------
ast_data <- rating.ast %>% 
  select(season, team_id, ortg) %>% 
  mutate(next_season = season + 1) %>% 
  left_join(rating.ast %>% 
              select(season, team_id, ortg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  filter(!is.na(ortg_next),
         season %in% summary.team$season) %>% 
  left_join(summary.team %>% 
              mutate(status = case_when(
                left_school == -1 ~ "left",
                left_school == 0 ~ "stay")) %>% 
              select(season:Team, status, players, pct_minutes, ast, efg, -left_school) %>%
              pivot_wider(names_from = c(status), values_from = c(players:efg)) %>% 
              mutate_at(vars(contains("_left")), ~ . * -1),
            by = c("season", "team_id")) %>% 
  left_join(summary.commit,
            by = c("season", "team_id")) %>% 
  left_join(summary.transfers %>% 
              select(season, team_id = transtoID, transfers:min_rtg, ast, efg, -minutes),
            by = c("season", "team_id"),
            suffix = c("_cmt", "_tfr")) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(team_id, Team, next_season))

### Create model
test_season <- 2023

ast_model <- glm(ortg_next ~ ortg + pct_minutes_stay + ast_stay + efg_stay +
                   commits + avg_rtg_cmt + max_rtg_cmt +
                   transfers + avg_rtg_tfr + max_rtg_tfr + ast + efg,
                 data = ast_data %>%
                   filter(season != test_season) %>% 
                   select(-season))

ast_model_simple <- glm(ortg_next ~ ortg,
                        data = ast_data %>%
                          filter(season != test_season))


summary(ast_model)
summary(ast_model_simple)

ast_data %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(ast_model, ast_data %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, ortg_next), rmse = Metrics::rmse(ortg_next, preds))

ast_data %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(ast_model_simple, ast_data %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, ortg_next), rmse = Metrics::rmse(ortg_next, preds))

### Defensive Assist rates -----------------------
astd_data <- rating.ast %>% 
  select(season, team_id, drtg) %>% 
  mutate(next_season = season + 1) %>% 
  left_join(rating.ast %>% 
              select(season, team_id, drtg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  filter(!is.na(drtg_next),
         season %in% summary.team$season) %>% 
  left_join(summary.team %>% 
              mutate(status = case_when(
                left_school == -1 ~ "left",
                left_school == 0 ~ "stay")) %>% 
              select(season:Team, status, players, pct_minutes, ast, -left_school) %>%
              pivot_wider(names_from = c(status), values_from = c(players:ast)) %>% 
              mutate_at(vars(contains("_left")), ~ . * -1),
            by = c("season", "team_id")) %>% 
  left_join(summary.commit,
            by = c("season", "team_id")) %>% 
  left_join(summary.transfers %>% 
              select(season, team_id = transtoID, transfers:min_rtg, ast, -minutes),
            by = c("season", "team_id"),
            suffix = c("_cmt", "_tfr")) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(team_id, Team, next_season))

### Create model
test_season <- 2022

astd_model <- glm(drtg_next ~ drtg + pct_minutes_stay + 
                   commits + avg_rtg_cmt + max_rtg_cmt +
                   transfers + avg_rtg_tfr + max_rtg_tfr,
                 data = astd_data %>%
                   filter(season != test_season) %>% 
                   select(-season))

astd_model_simple <- glm(drtg_next ~ drtg,
                        data = astd_data %>%
                          filter(season != test_season))


summary(astd_model)
summary(astd_model_simple)

astd_data %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(astd_model, astd_data %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, drtg_next), rmse = Metrics::rmse(drtg_next, preds))

astd_data %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(astd_model_simple, astd_data %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, drtg_next), rmse = Metrics::rmse(drtg_next, preds))

## Pace --------------------------
### Offensive Pace -------------------
pace_data <- rating.pace %>% 
  select(season, team_id, ortg, drtg) %>% 
  mutate(next_season = season + 1) %>% 
  left_join(rating.pace %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  filter(!is.na(ortg_next), !is.na(drtg_next),
         season %in% summary.team$season) %>% 
  left_join(summary.team %>% 
              mutate(status = case_when(
                left_school == -1 ~ "left",
                left_school == 0 ~ "stay")) %>% 
              select(season:Team, status, players, pct_minutes, -left_school) %>%
              pivot_wider(names_from = c(status), values_from = c(players:pct_minutes)) %>% 
              mutate_at(vars(contains("_left")), ~ . * -1),
            by = c("season", "team_id")) %>% 
  left_join(summary.commit,
            by = c("season", "team_id")) %>% 
  left_join(summary.transfers %>% 
              select(season, team_id = transtoID, transfers:min_rtg, -minutes),
            by = c("season", "team_id"),
            suffix = c("_cmt", "_tfr")) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(team_id, Team, next_season))

### Create offense model
test_season <- 2022

pace_model <- glm(ortg_next ~ ortg + pct_minutes_stay + 
                   commits + avg_rtg_cmt + max_rtg_cmt +
                   transfers + avg_rtg_tfr + max_rtg_tfr,
                 data = pace_data %>%
                   filter(season != test_season) %>% 
                   select(-season))

pace_model_simple <- glm(ortg_next ~ ortg,
                        data = pace_data %>%
                          filter(season != test_season))


summary(pace_model)
summary(pace_model_simple)

pace_data %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(pace_model, pace_data %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, ortg_next), rmse = Metrics::rmse(ortg_next, preds))

pace_data %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(pace_model_simple, pace_data %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, ortg_next), rmse = Metrics::rmse(ortg_next, preds))

### Create defense model
test_season <- 2021

dpace_model <- glm(drtg_next ~ drtg + pct_minutes_stay + 
                    commits + avg_rtg_cmt + max_rtg_cmt +
                    transfers + avg_rtg_tfr + max_rtg_tfr,
                  data = pace_data %>%
                    filter(season != test_season) %>% 
                    select(-season))

dpace_model_simple <- glm(drtg_next ~ drtg,
                         data = pace_data %>%
                           filter(season != test_season))


summary(dpace_model)
summary(dpace_model_simple)

pace_data %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(dpace_model, pace_data %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, drtg_next), rmse = Metrics::rmse(drtg_next, preds))

pace_data %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(dpace_model_simple, pace_data %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, drtg_next), rmse = Metrics::rmse(drtg_next, preds))

## Rating (with other stats) -------------------
### Offensive Rating -------------------
head(summary.team)
rating_data <- rating.rtg %>% 
  select(season, team_id, ortg, drtg) %>% 
  mutate(next_season = season + 1) %>% 
  left_join(rating.rtg %>% 
              select(season, team_id, ortg, drtg),
            by = c("team_id",
                   "next_season" = "season"),
            suffix = c("", "_next")) %>% 
  filter(!is.na(ortg_next), !is.na(drtg_next),
         season %in% summary.team$season) %>% 
  left_join(summary.team %>% 
              mutate(status = case_when(
                left_school == -1 ~ "left",
                left_school == 0 ~ "stay")) %>% 
              select(season:Team, status, players, pct_minutes, -left_school,
                     efg, tov, stl, obpm, dbpm) %>%
              pivot_wider(names_from = c(status), values_from = c(players:dbpm)) %>% 
              mutate_at(vars(contains("_left")), ~ . * -1),
            by = c("season", "team_id")) %>% 
  left_join(summary.commit,
            by = c("season", "team_id")) %>% 
  left_join(summary.transfers %>% 
              select(season, team_id = transtoID, transfers:min_rtg, -minutes,
                     efg, tov, stl, obpm, dbpm),
            by = c("season", "team_id"),
            suffix = c("_cmt", "_tfr")) %>% 
  replace(is.na(.), 0) %>% 
  select(-c(team_id, Team, next_season))

### Create offense model
test_season <- 2021

rating_model <- glm(ortg_next ~ ortg + pct_minutes_stay + efg_stay + obpm_stay +
                    commits + avg_rtg_cmt + max_rtg_cmt +
                    transfers + avg_rtg_tfr + max_rtg_tfr +
                      efg + obpm,
                  data = rating_data %>%
                    filter(season != test_season) %>% 
                    select(-season))

rating_model_simple <- glm(ortg_next ~ ortg,
                         data = rating_data %>%
                           filter(season != test_season))


summary(rating_model)
summary(rating_model_simple)

rating_data %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(rating_model, rating_data %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, ortg_next), rmse = Metrics::rmse(ortg_next, preds))

rating_data %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(rating_model_simple, rating_data %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, ortg_next), rmse = Metrics::rmse(ortg_next, preds))

### Create defensive rating model ----------------
test_season <- 2023

ratingd_model <- glm(drtg_next ~ drtg + pct_minutes_stay + dbpm_stay +
                      commits + avg_rtg_cmt + max_rtg_cmt +
                      transfers + avg_rtg_tfr + max_rtg_tfr +
                      dbpm,
                    data = rating_data %>%
                      filter(season != test_season) %>% 
                      select(-season))

ratingd_model_simple <- glm(drtg_next ~ drtg,
                           data = rating_data %>%
                             filter(season != test_season))


summary(rating_model)
summary(rating_model_simple)

rating_data %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(ratingd_model, rating_data %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, drtg_next), rmse = Metrics::rmse(drtg_next, preds))

rating_data %>%
  filter(season == test_season) %>% 
  cbind(preds = predict(ratingd_model_simple, rating_data %>% filter(season == test_season))) %>% 
  summarise(cor = cor(preds, drtg_next), rmse = Metrics::rmse(drtg_next, preds))

##############################################################################
# Save the models formats for carrying over ----------------------------------
## Pace (Offense/Defense) -------
### Offense -------------
pace_model <- glm(ortg_next ~ ortg,
                         data = pace_data)

### Defense --------
dpace_model <- glm(drtg_next ~ drtg,
                          data = pace_data)

## Assist (Offense/Defense) -------------
### Offense -------------
ast_model <- glm(ortg_next ~ ortg + pct_minutes_stay + ast_stay + efg_stay +
                   commits + avg_rtg_cmt + max_rtg_cmt +
                   transfers + avg_rtg_tfr + max_rtg_tfr + ast + efg,
                 data = ast_data)

### Defense --------
astd_model <- glm(drtg_next ~ drtg,
                  data = astd_data)

## Rebounding (Offense/Defense) ---------------------------
### Offense -------------
oreb_model <- glm(ortg_next ~ ortg + pct_minutes_stay + oreb_rate_stay +
                    commits + avg_rtg_cmt + max_rtg_cmt +
                    transfers + avg_rtg_tfr + max_rtg_tfr + oreb_rate,
                  data = oreb_data)

### Defense --------
dreb_model <- glm(drtg_next ~ drtg + pct_minutes_stay + dreb_rate_stay +
                    commits + avg_rtg_cmt + max_rtg_cmt +
                    transfers + avg_rtg_tfr + max_rtg_tfr + dreb_rate,
                  data = dreb_data)

## Turnover Rates (Offense/Defense) -------------
### Offense -------------
oto_model <- glm(ortg_next ~ ortg + pct_minutes_stay + tov_stay +
                   commits + avg_rtg_cmt + max_rtg_cmt +
                   transfers + avg_rtg_tfr + max_rtg_tfr + tov,
                 data = oto_data)

### Defense --------
dto_model <- glm(drtg_next ~ drtg + pct_minutes_stay + stl_stay +
                   commits + avg_rtg_cmt + max_rtg_cmt +
                   transfers + avg_rtg_tfr + max_rtg_tfr + stl,
                 data = dto_data)

## Rating ----------------------------
### Offense -------------
ortg_model <- glm(ortg_next ~ ortg + pct_minutes_stay + efg_stay + obpm_stay +
                      commits + avg_rtg_cmt + max_rtg_cmt +
                      transfers + avg_rtg_tfr + max_rtg_tfr +
                      efg + obpm,
                    data = rating_data)


### Defense --------
drtg_model <- glm(drtg_next ~ drtg + pct_minutes_stay + dbpm_stay +
                       commits + avg_rtg_cmt + max_rtg_cmt +
                       transfers + avg_rtg_tfr + max_rtg_tfr +
                       dbpm,
                     data = rating_data)

