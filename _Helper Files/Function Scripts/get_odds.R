get_odds <- function(df,
                     bk_name = c("Consensus", "Open", "DraftKings",
                                   "FanDuel", "PointsBet", "BetMGM"),
                     league_nm = c("ncaab", "ncaaw")){
  
  if (!(bk_name %in% c("Consensus", "Open", "DraftKings",
                         "FanDuel", "PointsBet", "BetMGM"))) {
    cli::cli_abort("The book you have selected is not a valid option!")
  }
  
  odds <- data.frame()
  books <- readRDS('Season Simulation Backup/books.rds')
  
  
  for(i in 1:length(df$game_date)){
    day <- df$game_date[i]
    
    URL = case_when(
      lubridate::day(day) < 10 & lubridate::month(day) < 10 ~
        paste0('https://api.actionnetwork.com/web/v1/scoreboard/',
        {league_nm},'?division=D1&date=',
        lubridate::year(day), 0, lubridate::month(day), 0, lubridate::day(day)),
      lubridate::day(day) < 10 & lubridate::month(day) >= 10 ~
        paste0('https://api.actionnetwork.com/web/v1/scoreboard/',
               {league_nm},'?division=D1&date=',
               lubridate::year(day), lubridate::month(day), 0, lubridate::day(day)),
      lubridate::day(day) >= 10 & lubridate::month(day) < 10 ~
        paste0('https://api.actionnetwork.com/web/v1/scoreboard/',
               {league_nm},'?division=D1&date=',
               lubridate::year(day), 0, lubridate::month(day), lubridate::day(day)),
      lubridate::day(day) >= 10 & lubridate::month(day) >= 10 ~
        paste0('https://api.actionnetwork.com/web/v1/scoreboard/',
               {league_nm},'?division=D1&date=',
               lubridate::year(day), lubridate::month(day), lubridate::day(day))
    )
    
    # Read in odds from Action Network
    data <- suppressWarnings(jsonlite::fromJSON(readLines(URL)))
    
    matches <- data.frame(data$games)
    
    if(length(matches) != 0){
      matchups.AN <- data.frame(game_id = matches$id,
                                type = "away",
                                team_id = matches$away_team_id,
                                opp_id = matches$home_team_id) %>%
        rbind(data.frame(game_id = matches$id,
                         type = "home",
                         team_id = matches$home_team_id, 
                         opp_id = matches$away_team_id)) %>% 
        arrange(game_id) %>% 
        left_join(teams %>% 
                    select(team, 
                           espn_tm = team_id,
                           action_network_id), by = c("team_id" = "action_network_id")) %>%
        left_join(teams %>% 
                    select(opponent = team, 
                           espn_opp = team_id,
                           action_network_id), by = c("opp_id" = "action_network_id")) %>% 
        mutate(date = day) %>% 
        filter(!is.na(team), !is.na(opponent))
      
      temp.odds <- data.frame()
      
      for(j in 1:length(matches$id)){
        temp = if(!is.null(matches$odds[[j]]) & length(matches$odds[[j]]) > 0){
          data.frame(matches$odds[[j]]) %>% 
            left_join(books, by = "book_id") %>% 
            filter(type == "game", book_name == {bk_name}) %>% 
            select(ml = ml_away, spread = spread_away, spread_line = spread_away_line,
                   tm_total = away_total, tm_over = away_over, tm_under = away_under,
                   total, over, under) %>% 
            bind_rows(data.frame(matches$odds[[j]]) %>% 
                        left_join(books, by = "book_id") %>% 
                        filter(type == "game", book_name == {bk_name}) %>% 
                        select(ml = ml_home, spread = spread_home, spread_line = spread_home_line,
                               tm_total = home_total, tm_over = home_over, tm_under = home_under,
                               total, over, under))
        }
        
        if(length(temp$ml) == 2){
          temp = temp %>% 
            cbind(type = c("away", "home")) %>% 
            mutate(game_id = matches$id[[j]]) %>% 
            relocate(type) %>% 
            left_join(matchups.AN, by = c("game_id", "type")) %>% 
            mutate(date = df$game_date[i]) %>% 
            rename(action_id = team_id)
        }
        
        temp.odds = bind_rows(temp.odds, temp) %>% 
          relocate(team)
      }
      
      odds <- rbind(odds, temp.odds) %>% 
        distinct()
      
      print(i)
    } else {
      print(glue::glue("No games for {df$game_date[i]}."))
    }
    
    if(i < length(df$game_date)){
      Sys.sleep(2)
    }
  }
  
  return(odds)
}

saveRDS(get_odds, 'Simulation Backup/Functions/get_odds.rds')
