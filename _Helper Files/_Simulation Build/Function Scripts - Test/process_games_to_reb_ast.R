process_games <- function(teams, games, ratings, round_num, ...) {
  
  # Rename ratings file just cuz
  ratings_all <- ratings
  
  # Pull out types of ratings from ratings
  ratings_pace <- ratings_all$pace
  ratings_rtg <- ratings_all$rtg
  ratings_oreb <-ratings_all$oreb
  ratings_to <- ratings_all$to
  ratings_ast <- ratings_all$ast
  
  disp_rtg <- ratings_all$disp_Rtg
  
  ## Get base ratings for each ratings system -------------
  ## Get base ratings for each ratings system -------------
  base_pace <- ratings_pace %>% 
    filter(!str_detect(name, "team_") & !str_detect(name, "opp_"))
  
  base_rtg <- ratings_rtg %>% 
    filter(!str_detect(name, "team_") & !str_detect(name, "opp_"))
  
  base_oreb <- ratings_oreb %>% 
    filter(!str_detect(name, "team_") & !str_detect(name, "opp_"))
  
  base_to <- ratings_to %>% 
    filter(!str_detect(name, "team_") & !str_detect(name, "opp_"))
  
  base_ast <- ratings_ast %>% 
    filter(!str_detect(name, "team_") & !str_detect(name, "opp_"))
  
  # Create team ratings -----------------------------------------------------
  ## Pace -----------------
  tm_ratings_pace <- ratings_pace %>% 
    filter(str_detect(name, "team_")) %>% 
    mutate(name = str_remove_all(name, "team_id_"),
           name = as.integer(name),
           value = value + base_pace$value[base_pace$name == "(Intercept)"]) %>% 
    rename(team_id = name,
           OffRtg = value) %>% 
    left_join(ratings_pace %>% 
                filter(str_detect(name, "opp_")) %>% 
                mutate(name = str_remove_all(name, "opp_id_"),
                       name = as.integer(name)) %>% 
                rename(team_id = name,
                       DefRtg = value), by = "team_id") %>% 
    filter(team_id %in% teams$team_id)
  
  ## Offensive Rebounds -----------------
  tm_ratings_oreb <- ratings_oreb %>% 
    filter(str_detect(name, "team_")) %>% 
    mutate(name = str_remove_all(name, "team_id_"),
           name = as.integer(name),
           value = value + base_oreb$value[base_oreb$name == "(Intercept)"]) %>% 
    rename(team_id = name,
           OffRtg = value) %>% 
    left_join(ratings_oreb %>% 
                filter(str_detect(name, "opp_")) %>% 
                mutate(name = str_remove_all(name, "opp_id_"),
                       name = as.integer(name)) %>% 
                rename(team_id = name,
                       DefRtg = value), by = "team_id") %>% 
    filter(team_id %in% teams$team_id)
  
  ## Turnovers -----------------
  tm_ratings_to <- ratings_to %>% 
    filter(str_detect(name, "team_")) %>% 
    mutate(name = str_remove_all(name, "team_id_"),
           name = as.integer(name),
           value = value + base_to$value[base_to$name == "(Intercept)"]) %>% 
    rename(team_id = name,
           OffRtg = value) %>% 
    left_join(ratings_to %>% 
                filter(str_detect(name, "opp_")) %>% 
                mutate(name = str_remove_all(name, "opp_id_"),
                       name = as.integer(name)) %>% 
                rename(team_id = name,
                       DefRtg = value), by = "team_id") %>% 
    filter(team_id %in% teams$team_id)
  
  ## Assists -----------------
  tm_ratings_ast <- ratings_ast %>% 
    filter(str_detect(name, "team_")) %>% 
    mutate(name = str_remove_all(name, "team_id_"),
           name = as.integer(name),
           value = value + base_ast$value[base_ast$name == "(Intercept)"]) %>% 
    rename(team_id = name,
           OffRtg = value) %>% 
    left_join(ratings_ast %>% 
                filter(str_detect(name, "opp_")) %>% 
                mutate(name = str_remove_all(name, "opp_id_"),
                       name = as.integer(name)) %>% 
                rename(team_id = name,
                       DefRtg = value), by = "team_id") %>% 
    filter(team_id %in% teams$team_id)
  
  ## Expected Rating -----------------------------
  tm_ratings_rtg <- ratings_rtg %>% 
    filter(str_detect(name, "team_")) %>% 
    mutate(name = str_remove_all(name, "team_id_"),
           name = as.integer(name),
           value = value + base_rtg$value[base_rtg$name == "(Intercept)"]) %>% 
    rename(team_id = name,
           OffRtg = value) %>% 
    left_join(ratings_rtg %>% 
                filter(str_detect(name, "opp_")) %>% 
                mutate(name = str_remove_all(name, "opp_id_"),
                       name = as.integer(name)) %>% 
                rename(team_id = name,
                       DefRtg = value), by = "team_id") %>% 
    filter(team_id %in% teams$team_id)
  
  ## Rating --------------------------------
  fctrs_rtg <- base_rtg %>% 
    filter(name %in% c("days_rest", "neutral_site", "is_home", "travel", "poss_per_40",
                       "oreb_rt", "to_rt", "ast_rt")) %>% 
    pivot_wider() %>% 
    mutate(sd_rtg = disp_rtg)
  

  played_games <- games %>% 
    filter(!is.na(result))
  
  unplayed_games <- games %>% 
    filter(is.na(result))
  
  # mark estimate, wp, and result for games
  unplayed_games <- unplayed_games %>% 
    # Join pace ratings for away team
    left_join(tm_ratings_pace, by = c("away_id" = "team_id")) %>% 
    rename(away_Off_Pace = OffRtg, away_Def_Pace = DefRtg) %>% 
    # Join pace ratings for home team
    left_join(tm_ratings_pace, by = c("home_id" = "team_id")) %>% 
    rename(home_Off_Pace = OffRtg, home_Def_Pace = DefRtg) %>%
    # Calculate away/home bases
    mutate(away_base = away_Off_Pace + home_Def_Pace,
           home_base = home_Off_Pace + away_Def_Pace,
           # Get average of home and away
           mean_base = (away_base + home_base) / 2) %>%
    mutate(pace = mean_base) %>% 
    # Remove pace columns, leaving only pace 
    select(-c(away_Off_Pace:home_Def_Pace, away_base, home_base, mean_base)) %>%
    
    # Join oreb ratings for away team
    left_join(tm_ratings_oreb, by = c("away_id" = "team_id")) %>% 
    rename(away_Off_Oreb = OffRtg, away_Def_Oreb = DefRtg) %>% 
    # Join oreb ratings for home team
    left_join(tm_ratings_oreb, by = c("home_id" = "team_id")) %>% 
    rename(home_Off_Oreb = OffRtg, home_Def_Oreb = DefRtg) %>%
    # Calculate away/home offensive rebound rates
    mutate(away_oreb = away_Off_Oreb + home_Def_Oreb,
           home_oreb = home_Off_Oreb + away_Def_Oreb) %>%
    select(-c(away_Off_Oreb:home_Def_Oreb)) %>%
    
    # Join TO ratings for away team
    left_join(tm_ratings_to, by = c("away_id" = "team_id")) %>% 
    rename(away_Off_TO = OffRtg, away_Def_TO = DefRtg) %>% 
    # Join TO ratings for home team
    left_join(tm_ratings_to, by = c("home_id" = "team_id")) %>% 
    rename(home_Off_TO = OffRtg, home_Def_TO = DefRtg) %>%
    # Calculate away/home turnover rates
    mutate(away_to = away_Off_TO + home_Def_TO,
           home_to = home_Off_TO + away_Def_TO) %>%
    select(-c(away_Off_TO:home_Def_TO)) %>%
    
    # Join Assist ratings for away team
    left_join(tm_ratings_ast, by = c("away_id" = "team_id")) %>% 
    rename(away_Off_ast = OffRtg, away_Def_ast = DefRtg) %>% 
    # Join Assist ratings for home team
    left_join(tm_ratings_ast, by = c("home_id" = "team_id")) %>% 
    rename(home_Off_ast = OffRtg, home_Def_ast = DefRtg) %>%
    # Calculate away/home assist rates
    mutate(away_ast = away_Off_ast + home_Def_ast,
           home_ast = home_Off_ast + away_Def_ast) %>%
    select(-c(away_Off_ast:home_Def_ast)) %>%
    
    # Join rtg ratings for away team
    left_join(tm_ratings_rtg, by = c("away_id" = "team_id")) %>% 
    rename(away_Off_rtg = OffRtg, away_Def_rtg = DefRtg) %>%
    # Join xRtg ratings for home team
    left_join(tm_ratings_rtg, by = c("home_id" = "team_id")) %>% 
    rename(home_Off_rtg = OffRtg, home_Def_rtg = DefRtg) %>% 
    # Add factors for xRtg
    bind_cols(fctrs_rtg) %>% 
    # Calculate away/home bases
    mutate(away_base = away_Off_rtg + home_Def_rtg + (pace * poss_per_40) +
             (away_rest * days_rest) + (away_dist * travel) + (neutral_site) +
             (away_oreb * oreb_rt) + (away_to * to_rt) + (away_ast * ast_rt),
           home_base = home_Off_rtg + away_Def_rtg + (pace * poss_per_40) +
             (home_rest * days_rest) + (home_dist * travel) + (neutral_site) + 
             (home_oreb * oreb_rt) + (home_to * to_rt) + (home_ast * ast_rt)) %>%
    # Adjustment for underestimations
    mutate(away_base = away_base + (0.5 * 0.3 * (away_base - home_base)),
           home_base = home_base + (0.5 * 0.3 * (home_base - away_base))) %>% 
    with_groups(.groups = c(away_id, home_id, sim), mutate,
                # Using base and sd, randomize expected ratings for away/home
                away_rtg = away_base + rnorm(1, mean = 0, sd = sd_rtg),
                home_rtg = home_base + rnorm(1, mean = 0, sd = sd_rtg)) %>% 
    # Calculate scores
    mutate(away_score_reg = round((away_rtg * pace / 100), 0),
           home_score_reg = round((home_rtg * pace / 100), 0)) %>% 
    # Add tie-breaker just in case they end up the same
    with_groups(.groups = c(away_id, home_id, sim), mutate,
                ot_adj_home = case_when(
                  home_score_reg == away_score_reg ~ round((pace / 8) * (home_base/100), 0),
                  TRUE ~ 0),
                ot_adj_away = case_when(
                  home_score_reg == away_score_reg ~ round((pace / 8) * (away_base/100), 0),
                  TRUE ~ 0)) %>% 
    mutate(away_score = away_score_reg + ot_adj_away,
           home_score = home_score_reg + ot_adj_home) %>% 
    # Add tie-breaker just in case they end up the same
    mutate(home_score = case_when(
      home_score == away_score ~ home_score + 1,
      TRUE ~ home_score)) %>% 
    # Calculate result
    mutate(result = home_score - away_score) %>% 
  # Remove extra columns
  select(sim,
         region, round_name, round_no,
         away_seed, away_id, away_rest, away_dist, away_score,
         home_seed, home_id, home_rest, home_dist, home_score,
         result,
         winner_to)
  
  games <- bind_rows(played_games, unplayed_games)
  
  teams <- as_tibble(teams)
  
  games <- as_tibble(games)
  
  return(list(teams = teams, games = games))
}

saveRDS(process_games, 'Simulation Backup/Functions/process_games_to_reb_ast.rds')
