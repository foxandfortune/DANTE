simulate_round <- function(sim_round,
                           sim_rounds,
                          sims_per_round,
                          schedule,
                          simulations,
                          rounds_to_sim,
                          process_games,
                          ...,
                          tiebreaker_depth,
                          test_round,
                          .debug,
                          playoff_seeds,
                          p,
                          sim_include) {
  
  # iteration sims
  iter_sims <- sims_per_round * (sim_round - 1) + seq_len(sims_per_round)
  iter_sims <- iter_sims[iter_sims <= simulations]
  iter_sims_num <- length(iter_sims)
  
  # games have copies per sim
  sched_rows <- nrow(schedule)
  games <- schedule[rep(seq_len(sched_rows), each = iter_sims_num), ] %>%
    mutate(sim = rep(iter_sims, sched_rows)) %>%
    select(sim, everything())
  
  # load tournament teams
  teams <- readRDS(glue::glue('March Madness Backup/tourney_teams_{ncaa_season}.rds'))
  teams <- teams[rep(seq_len(nrow(teams)), iter_sims_num), ] %>%
    mutate(sim = rep(iter_sims, each = nrow(teams))) %>%
    select(sim, everything())
  
  # get tourney structure
  tourney_structure <- readRDS(glue::glue('March Madness Backup/tourney_structure_{ncaa_season}.rds'))
  structure_rows <- nrow(tourney_structure)
  tourney_structure <- tourney_structure[rep(seq_len(structure_rows), each = iter_sims_num), ] %>%
    mutate(sim = rep(iter_sims, structure_rows)) %>%
    select(sim, everything())
  
  # playoff seeds bounds checking
  max_seeds <- teams %>%
    group_by(sim, region) %>%
    summarize(count=n()) %>%
    ungroup() %>%
    pull(count) %>%
    max()
  if (playoff_seeds < 1 || playoff_seeds > max_seeds) {
    stop("`playoff_seeds` must be between 1 and ",max_seeds)
  }
  
  # function to simulate a round
  simulate_week <- function(teams, games, round_num, test_round, ...) {
    
    # recall old data for comparison
    old_teams <- teams
    old_games <- games %>%
      rename(.old_result = result)
    
    # estimate and simulate games
    return_value <- process_games(teams, games, round_num, ...)
    
    # testing?
    if (!is.null(test_round) && round_num == test_round) {
      return(return_value)
    }
    
    # did we get the right data back?
    problems <- c()
    if (typeof(return_value) != "list") {
      problems[length(problems) + 1] <- "the returned value was not a list"
    } else {
      if (!("teams" %in% names(return_value))) {
        problems[length(problems) + 1] <- "`teams` was not in the returned list"
      } else {
        teams <- return_value$teams
        if (!is_tibble(teams)) {
          problems[length(problems) + 1] <- "`teams` was not a tibble"
        } else {
          if (nrow(teams) != nrow(old_teams)) {
            problems[length(problems) + 1] <- paste(
              "`teams` changed from", nrow(old_teams), "to",
              nrow(teams), "rows",
              collapse = " "
            )
          }
          for (cname in colnames(old_teams)) {
            if (!(cname %in% colnames(teams))) {
              problems[length(problems) + 1] <- paste(
                "`teams` column `", cname, "` was removed"
              )
            }
          }
        }
      }
      if (!("games" %in% names(return_value))) {
        problems[length(problems) + 1] <- "`games` was not in the returned list"
      } else {
        games <- return_value$games
        if (!is_tibble(games)) {
          problems[length(problems) + 1] <- "`games` was not a tibble"
        } else {
          if (nrow(games) != nrow(old_games)) {
            problems[length(problems) + 1] <- paste(
              "`games` changed from", nrow(old_games), "to",
              nrow(games), "rows",
              collapse = " "
            )
          }
          for (cname in colnames(old_games)) {
            if (!(cname %in% colnames(games)) && cname != ".old_result") {
              problems[length(problems) + 1] <- paste(
                "`teams` column `", cname, "` was removed"
              )
            }
          }
        }
      }
    }
    
    # report data structure problems
    problems <- paste(problems, collapse = ", ")
    if (problems != "") {
      stop(
        "During Week ", round_num, ", your `process_games()` function had the ",
        "following issues: ", problems, ". "
      )
    }
    
    # identify improper results values
    problems <- old_games %>%
      inner_join(games, by = intersect(colnames(old_games), colnames(games))) %>%
      mutate(problem = case_when(
        round_no == round_num & is.na(result) ~
          "a result from the current week is missing",
        round_no != round_num & !is.na(.old_result) & is.na(result) ~
          "a known result outside the current week was blanked out",
        round_no != round_num & is.na(.old_result) & !is.na(result) ~
          "a result outside the current week was entered",
        round_no != round_num & .old_result != result ~
          "a known result outside the current week was updated",
        !is.na(.old_result) & is.na(result) ~
          "a known result was blanked out",
        !is.na(result) & result == 0 ~
          "a game resulted in a tie (had result == 0)",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(problem)) %>%
      pull(problem) %>%
      unique() %>%
      paste(collapse = ", ")
    
    # report result value problems
    if (problems != "") {
      stop(
        "During Week ", round_num, ", your `process_games()` function had the",
        "following issues: ", problems, ". Make sure you only change results ",
        "when round_no == round_num & is.na(result)"
      )
    }
    
    return(list(teams = teams, games = games))
  }
  
  # simulate remaining regular season games
  for (round_num in rounds_to_sim)
  {
    return_value <-
      simulate_week(teams, games, round_num, test_round, ...)
    if (!is.null(test_round) && round_num == test_round) {
      return(return_value)
    }
    list[teams, games] <- return_value
  }
  
  #### FIND DIVISIONAL STANDINGS AND PLAYOFF SEEDINGS ####
  
  standings_and_h2h <- compute_conference_seeds(
    teams = teams, games = games
  )
  
  teams <- teams %>% 
    select(-c(winner_to)) %>% 
    left_join(standings_and_h2h$standings,
              by = c("region", "sim", "seed", "team_id", "team_name", "first_four"))
  
  #### PLAYOFFS ####
  if (sim_include == "POST"){# sim_include allows us to skip playoff simulation
    
    # week tracker
    round_num <- games %>%
      filter(!is.na(result)) %>%
      pull(round_no) %>%
      max()
    
    # identify playoff teams
    tourney_teams <- teams %>%
      filter(is.na(exit)) %>%
      select(sim, region, seed, team_id, winner_to) %>%
      arrange(sim, region, winner_to, seed)
    
    # num teams tracker
    num_teams <- tourney_teams %>%
      group_by(sim, region) %>%
      summarize(count = n()) %>%
      pull(count) %>%
      max()
    
    # first playoff week
    first_playoff_week <- round_num + 1
    
    # final week of season (Super Bowl week)
    week_max <- round_num +
      ceiling(log(num_teams * length(unique(tourney_teams$region)), 2))
    
    # playoff weeks
    for (round_num in first_playoff_week:week_max) {
      report(paste("Processing Playoffs Week", round_num))
      
      # seed_numeate games if they don't already exist
      if (!any(games$round_no == round_num)) {
        # teams playing this round
        add_teams <- tourney_teams %>% 
          arrange(sim, region, seed) %>% 
          group_by(sim, region, winner_to) %>% 
          mutate(round_rank = row_number()) %>% 
          ungroup()
        
        # games to seed_numeate
        add_games <- tourney_structure %>%
          filter(round_no == round_num) %>% 
          inner_join(add_teams, by = c("sim", "region", "game" = "winner_to")) %>% 
          filter(round_rank == 2) %>% 
          rename(away_seed = seed, away_id = team_id) %>% 
          select(-c(round_rank)) %>% 
          inner_join(add_teams, by = c("sim", "region", "game" = "winner_to")) %>% 
          filter(round_rank == 1) %>% 
          rename(home_seed = seed, home_id = team_id) %>% 
          select(-c(round_rank)) %>% 
          mutate(
            home_rest = case_when(
              round_no %in% c(5) ~ 5,
              round_no %in% c(2, 3, 6) ~ 2,
              round_no %in% c(1, 4) ~ 4),
            away_rest = case_when(
              round_no == 1 & away_id %in% games$away_id[games$round_no == 0] ~ 2,
              round_no == 1 & away_id %in% games$home_id[games$round_no == 0] ~ 2,
              round_no %in% c(5) ~ 5,
              round_no %in% c(2, 3, 6) ~ 2,
              round_no %in% c(1, 4) ~ 4)
          ) %>% 
          select(sim, region, round_name, round_no,
                 away_seed, away_id, away_rest, away_dist,
                 home_seed, home_id, home_rest, home_dist,
                 winner_to)
        
        # add to games
        games <- bind_rows(games, add_games)
      }
      
      # process any new games
      return_value <-
        simulate_week(teams, games, round_num, test_round, ...)
      if (!is.null(test_round) && round_num == test_round) {
        return(return_value)
      }
      list[teams, games] <- return_value
      
      # record losers
      teams <- games %>%
        filter(round_no == round_num) %>%
        double_games() %>%
        filter(outcome == 0) %>%
        select(sim, team_id, outcome) %>%
        right_join(teams, by = c("sim", "team_id")) %>%
        mutate(exit = ifelse(!is.na(outcome), round_num, exit)) %>%
        select(-outcome)
      
      # if super bowl, record winner
      if (any(tourney_teams$region == "All")) {
        # super bowl winner exit is +1 to SB week
        teams <- games %>%
          filter(round_no == round_num) %>%
          double_games() %>%
          filter(outcome == 1) %>%
          select(sim, team_id, outcome) %>%
          right_join(teams, by = c("sim", "team_id")) %>%
          mutate(exit = ifelse(!is.na(outcome), round_num + 1, exit)) %>%
          select(-outcome)
      }
      
      # filter to winners or byes
      tourney_teams <- games %>%
        filter(round_no == round_num) %>%
        double_games() %>%
        right_join(tourney_teams, by = c("sim", "team_id")) %>%
        filter(is.na(result) | result > 0) %>%
        select(sim, region, seed, team_id, winner_to = winner_to.x) %>%
        arrange(sim, region, seed)
      
      # update number of teams
      num_teams <- tourney_teams %>%
        group_by(sim, region) %>%
        summarize(count = n()) %>%
        pull(count) %>%
        max()
      
      # if at one team per region, check for total teams; if 4, set region to Left/Right
      if (num_teams == 1 && round_num == 4) {
        tourney_teams <- tourney_teams %>%
          mutate(region = case_when(
            region %in% left_bracket ~ "Left",
            region %in% right_bracket ~ "Right")) %>% 
          with_groups(.groups = c(sim, region), mutate, seed = rep(c(1,2)))
        num_teams <- 4
      }
      
      # if at one team per region, loop once more for the Championship
      if (num_teams == 1 && !any(tourney_teams$region == "All")) {
        tourney_teams <- tourney_teams %>%
          mutate(region = "All", seed = 1)
        num_teams <- 2
      }
    } # end playoff loop
  }
  
  
  p(sprintf("finished sim round %g", sim_round))
  
  list("teams" = teams, "games" = games)
}

saveRDS(simulate_round, 'Simulation Backup/Functions/simulate_round.rds')
