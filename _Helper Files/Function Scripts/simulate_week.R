simulate_ncaa_week <- function(ncaa_season = NULL,
                          process_games = NULL,
                          ...,
                          if_ended_today = FALSE,
                          test_round = NULL,
                          simulations = 1000,
                          fresh_season = FALSE,
                          schedule = NULL,
                          sims_per_round = max(ceiling(simulations / future::availableCores() * 2), 100),
                          .debug = FALSE,
                          print_summary = FALSE,
                          sim_include = c("REG", "POST")) {
  
  sim_include <- rlang::arg_match0(sim_include, c("REG", "POST"))
  
  # Catch invalid input
  
  if (!all(
    is.null(ncaa_season) || is_single_digit_numeric(ncaa_season),
    is.null(test_round) || is_single_digit_numeric(test_round),
    is_single_digit_numeric(simulations),
    is_single_digit_numeric(sims_per_round)
  )) {
    cli::cli_abort(
      "One or more of the parameters {.arg ncaa_season}, \\
      {.arg test_round}, {.arg simulations} and {.arg sims_per_round} are not \\
      single digit numeric values!"
    )
  }
  
  if (!is.function(process_games) | is.null(process_games)) {
    cli::cli_abort("The parameter {.arg process_games} has to be a function!")
  }
  
  if (ncaa_season < 2001) {
    cli::cli_abort("The earliest season that can be simulated is 2001.")
  }
  
  #### LOAD DATA ####
  
  # load games data
  report("Loading games data")
  
  # Add sim days
  schedule <- schedule %>% 
    arrange(game_date) %>% 
    mutate(day_no = dense_rank(game_date)) %>% 
    relocate(day_no)
  
  if (is.null(ncaa_season)) {
    cli::cli_abort("You must enter a valid season.")
  }
  
  if (nrow(schedule) == 0)
  {
    fn <- paste0(
      "https://github.com/nflverse/nfldata/blob/master/fake_schedule_",
      ncaa_season,
      ".csv?raw=true"
    )
    tryCatch({
      schedule <- data.table::fread(fn)
      cli::cli_alert_info("No actual schedule exists for {.val {ncaa_season}}, using fake schedule with correct opponents.")
    }, error = function(cond) {
      cli::cli_abort("Unable to locate a schedule for {.val {ncaa_season}}")
    })
  }
  
  #### PREPROCESSING ####
  
  # if simulating fresh season, clear out all results and playoff games
  if(isTRUE(fresh_season)) {
    schedule <- schedule %>% 
      mutate(home_score = NA_real_, away_score = NA_real_,
             result = home_score - away_score) %>%
      filter(is.na(result)) %>% 
      mutate(day_no = dense_rank(game_date)) %>% 
      relocate(day_no)
  }
  
  # if ended today just needs one simulation
  if (isTRUE(if_ended_today)) {
    schedule <- schedule %>%
      filter(!is.na(result)) %>% 
      mutate(day_no = dense_rank(game_date)) %>% 
      relocate(day_no)
    simulations <- 1
  }
  
  # weeks to sim
  rounds_to_sim <- schedule %>%
    filter(is.na(result)) %>%
    pull(day_no) %>%
    unique() %>%
    sort()
  
  #### SET UP SIMULATIONS ####
  sim_rounds <- ceiling(simulations / sims_per_round)
  if (!is.null(test_round)) {
    sim_rounds <- 1
  }
  
  if (sim_rounds > 1 && is_sequential()) {
    cli::cli_inform(c(
      "i" = "Computation in multiple rounds can be accelerated
            with parallel processing.",
      "i" = "You should consider calling a {.code future::plan()}.
            Please see the function documentation for further information.",
      "i" = "Will go on sequentially..."
    ), wrap = TRUE
    )
  }
  
  report(
    "Beginning simulation of {.val {simulations}} season{?s} \\
    in {.val {sim_rounds}} round{?s}"
  )
  
  p <- progressr::progressor(along = seq_len(sim_rounds))
  
  run <- quote({
    all <- furrr::future_map(
      .x = seq_len(sim_rounds),
      .f = simulate_round,
      sim_rounds = sim_rounds,
      sims_per_round = sims_per_round,
      schedule = schedule,
      simulations = simulations,
      rounds_to_sim = rounds_to_sim,
      process_games = process_games,
      ...,
      test_round = test_round,
      .debug = .debug,
      p = p,
      sim_include = sim_include,
      .options = furrr::furrr_options(seed = TRUE)
    )
  })
  
  if (isTRUE(.debug)) eval(run) else suppressMessages(eval(run))
  
  if (!is.null(test_round)) {
    report(
      "Aborting and returning your {.code process_games} function's \\
      results from Week {test_round}"
      , wrap = TRUE
    )
    return(all[[1]])
  }
  
  report("Combining simulation data")
  
  # `all` is a list of rounds where every round is containing the dataframes
  # "teams" and "games". We loop over the list with purrr (that's not really bad
  # because the length of the loop only is the number of rounds) but don't
  # convert to a dataframe/tibble because dplyr::bind_rows() is too slow.
  # Instead, we bind with data.table afterwards, it's a reverse dependency
  # through nflreadr anyways.
  all_teams <- data.table::rbindlist(purrr::map(all, ~ .x$teams))
  all_games <- data.table::rbindlist(purrr::map(all, ~ .x$games))
  
  report("Aggregating across simulations")
  
  # we need the exit number of the champ winner to compute champ and Final 4 percentages
  # with "exit." Need to remove NAs here because Exit will be NA
  # for postseason teams
  #champ_exit <- max(all_teams$exit, na.rm = TRUE)
  
  # If test round is used, set champ exit to NA as we don't really simulate
  # full tournament then and set champ_exit to NA which result in NA percentages of sb
  # and conf columns
  #if(champ_exit < 6) champ_exit <- NA_real_
  
  overall <- all_teams %>%
    group_by(team_id) %>%
    summarize(
      wins = mean(wins),
      ) %>%
    ungroup()
  
  #team_wins <-
  #  tibble(
  #    team = rep(sort(unique(all_teams$team_id)), each = max(all_teams$games) * 2 + 1),
  #    wins = rep(seq(0, max(all_teams$games), 0.5), length(unique(all_teams$team_id)))
  #  ) %>%
  #  inner_join(
  #    all_teams %>% select(team_id, true_wins),
  #    by = c("team_id")
  #  ) %>%
  #  group_by(team_id, wins) %>%
  #  summarize(
  #    over_prob = mean(true_wins > wins),
  #    under_prob = mean(true_wins < wins)
  #  ) %>%
  #  ungroup()
  
  game_summary <-
    all_games %>%
    group_by(game_date, day_no, away_id, home_id) %>%
    summarise(
      away_wins = sum(result < 0),
      home_wins = sum(result > 0),
      result = mean(result),
      # != number of simulations in the postseason
      games_played = away_wins + home_wins,
      away_percentage = (away_wins) / games_played,
      home_percentage = (home_wins) / games_played
    ) %>%
    ungroup() %>%
    arrange(day_no)
  
  report("DONE!")
  
  if (isTRUE(print_summary)) print(overall)
  
  out <- structure(
    list(
      "teams" = all_teams,
      "games" = all_games,
      "overall" = overall,
      #"team_wins" = team_wins,
      "game_summary" = game_summary,
      "sim_params" = list(
        "ncaa_season" = ncaa_season,
        "if_ended_today" = if_ended_today,
        "fresh_season" = fresh_season,
        "test_round" = test_round,
        "simulations" = simulations,
        "sims_per_round" = sims_per_round,
        ".debug" = .debug,
        "print_summary" = print_summary,
        "sim_include" = sim_include,
        "finished_at" = Sys.time()
      )
    ),
    class = "ncaa_simulation"
  )
  
  out
}

saveRDS(simulate_ncaa_week, 'Simulation Backup/Functions/simulate_ncaa_wk.rds')

#### Sim Round ----------------------------
simulate_round_week <- function(sim_round,
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
  teams <- readRDS('Season Simulation Backup/team_database.rds') %>% 
    mutate(team_id = as.integer(team_id))
  teams <- teams[rep(seq_len(nrow(teams)), iter_sims_num), ] %>%
    mutate(sim = rep(iter_sims, each = nrow(teams))) %>%
    select(sim, everything())
  
  # playoff seeds bounds checking
  #max_seeds <- teams %>%
  #  group_by(sim, region) %>%
  #  summarize(count=n()) %>%
  #  ungroup() %>%
  #  pull(count) %>%
  #  min()
  #if (playoff_seeds < 1 || playoff_seeds > max_seeds) {
  #  stop("`playoff_seeds` must be between 1 and ",max_seeds)
  #}
  
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
        day_no == round_num & is.na(result) ~
          "a result from the current week is missing",
        day_no != round_num & !is.na(.old_result) & is.na(result) ~
          "a known result outside the current week was blanked out",
        day_no != round_num & is.na(.old_result) & !is.na(result) ~
          "a result outside the current week was entered",
        day_no != round_num & .old_result != result ~
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
        "when day_no == round_num & is.na(result)"
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
  
  standings_and_h2h <- compute_league_ranks(
    teams = teams, games = games
  )
  
  teams <- teams %>% 
    left_join(standings_and_h2h$standings,
              by = c("sim","team_id"))
  
  #### PLAYOFFS ####
  if (sim_include == "POST"){# sim_include allows us to skip playoff simulation
    
    # week tracker
    round_num <- games %>%
      filter(!is.na(result)) %>%
      pull(day_no) %>%
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
      if (!any(games$day_no == round_num)) {
        # teams playing this round
        add_teams <- tourney_teams %>% 
          arrange(sim, region, seed) %>% 
          group_by(sim, region, winner_to) %>% 
          mutate(round_rank = row_number()) %>% 
          ungroup()
        
        # games to seed_numeate
        add_games <- tourney_structure %>%
          filter(day_no == round_num) %>% 
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
              day_no %in% c(5) ~ 5,
              day_no %in% c(2, 3, 6) ~ 2,
              day_no %in% c(1, 4) ~ 4),
            away_rest = case_when(
              day_no == 1 & away_id %in% games$away_id[games$day_no == 0] ~ 2,
              day_no == 1 & away_id %in% games$home_id[games$day_no == 0] ~ 2,
              day_no %in% c(5) ~ 5,
              day_no %in% c(2, 3, 6) ~ 2,
              day_no %in% c(1, 4) ~ 4)
          ) %>% 
          select(sim, region, round_name, day_no,
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
        filter(day_no == round_num) %>%
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
          filter(day_no == round_num) %>%
          double_games() %>%
          filter(outcome == 1) %>%
          select(sim, team_id, outcome) %>%
          right_join(teams, by = c("sim", "team_id")) %>%
          mutate(exit = ifelse(!is.na(outcome), round_num + 1, exit)) %>%
          select(-outcome)
      }
      
      # filter to winners or byes
      tourney_teams <- games %>%
        filter(day_no == round_num) %>%
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

saveRDS(simulate_round_week, 'Simulation Backup/Functions/simulate_round_wk.rds')

# Compute ranks -----------------------------
compute_league_ranks <- function(games,
                                 teams = NULL,
                                 .debug = FALSE) {
  
  required_vars <- c(
    "sim",
    "day_no",
    "away_id",
    "home_id",
    "result"
  )
  
  if (!sum(names(games) %in% required_vars, na.rm = TRUE) >= 5 | !is.data.frame(games)) {
    stop(
      "The argument `games` has to be a data frame including ",
      "all of the following variables: ",
      glue::glue_collapse(required_vars, sep = ", ", last = " and "),
      "!"
    )
  }
  
  if (is.null(teams)) { # compute teams df from games df
    pivot_games <- games %>%
      select(sim, home_id, away_id) %>%
      pivot_longer(cols = c("home_id", "away_id"), values_to = "team_id") %>%
      select(sim, team_id)
    
    teams <- bind_rows(
      data.frame(team = unique(games$away_id)),
      data.frame(team = unique(games$home_id))
    ) %>%
      distinct() %>%
      left_join(pivot_games, by = "team_id", copy = TRUE) %>%
      select(sim, everything()) %>%
      distinct() %>%
      arrange(team_id, sim)
  }
  
  # double games
  games_doubled <- double_games(games)
  
  # record of each team
  teams <- as.data.frame(teams) %>%
    inner_join(games_doubled, by = c("sim", "team_id"), copy = TRUE) %>%
    group_by(sim, team_id) %>%
    summarize(
      games = n(),
      wins = sum(outcome),
      true_wins = sum(outcome == 1),
      losses = sum(outcome == 0),
      pd = sum(result)
    ) %>%
    ungroup() %>% 
    arrange(sim, true_wins, desc(pd)) %>% 
    with_groups(.groups = c(sim), mutate, Rk = row_number())
  
  max_week <- max(games$day_no, na.rm = TRUE)
  
  return(list(standings = teams))
}

saveRDS(compute_league_ranks, 'Simulation Backup/Functions/compute_league_ranks.rds')

# this makes it so there's two rows per game (one/team)
double_games_week <- function(g) {
  g1 <- g %>%
    select(sim, day_no, away_id, home_id, result) %>%
    rename(team_id = away_id, opp_id = home_id) %>%
    mutate(result = -1 * result)
  g2 <- g %>%
    select(sim, day_no, away_id, home_id, result) %>%
    rename(team_id = home_id, opp_id = away_id)
  g <- bind_rows(g1, g2) %>%
    mutate(outcome = case_when(
      result > 0 ~ 1,
      result < 0 ~ 0,
      TRUE ~ NA_real_
    ))
  return(g)
}

saveRDS(double_games_week, 'Simulation Backup/Functions/double_games_wk.rds')
