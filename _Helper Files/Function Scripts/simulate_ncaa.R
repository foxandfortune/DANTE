simulate_ncaa <- function(ncaa_season = NULL,
                          process_games = NULL,
                          ...,
                          playoff_seeds = 17,
                          left_bracket = c("South", "East"),
                          right_bracket = c("West", "Midwest"),
                          if_ended_today = FALSE,
                          fresh_tourney = FALSE,
                          test_round = NULL,
                          simulations = 1000,
                          sims_per_round = max(ceiling(simulations / future::availableCores() * 2), 100),
                          .debug = FALSE,
                          print_summary = FALSE,
                          sim_include = c("FIRST4", "POST")) {
  
  sim_include <- rlang::arg_match0(sim_include, c("FIRST4", "POST"))
  
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
  
  tourney_structure <- readRDS(glue::glue('March Madness Backup/tourney_structure_{ncaa_season}.rds'))
  
  if(isTRUE(fresh_tourney)) {
    schedule <- readRDS(glue::glue('March Madness Backup/first_four_{ncaa_season}.rds')) %>%
      select(
        region, round_name, round_no,
        away_seed, away_id, away_rest, away_dist, away_score,
        home_seed, home_id, home_rest, home_dist, home_score,
        result,
        winner_to
      )
  } else {
    schedule <- load_completed_games(season = ncaa_season,
                                     left_bracket = {left_bracket},
                                     right_bracket = {right_bracket}) %>%
      select(
        region, round_name, round_no,
        away_seed, away_id, away_rest, away_dist, away_score,
        home_seed, home_id, home_rest, home_dist, home_score,
        home_seed, home_id, home_rest, home_dist,
        result,
        winner_to
      )
  }
  
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
  if (isTRUE(fresh_tourney)) {
    schedule <- schedule %>%
      filter(round_name == "First Four") %>%
      mutate(result = NA_real_)
  }
  
  # if ended today just needs one simulation
  if (isTRUE(if_ended_today)) {
    schedule <- schedule %>%
      filter(!is.na(result))
    simulations <- 1
  }
  
  # weeks to sim
  rounds_to_sim <- schedule %>%
    filter(is.na(result)) %>%
    pull(round_no) %>%
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
      playoff_seeds = playoff_seeds,
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
  champ_exit <- max(all_teams$exit, na.rm = TRUE)
  # If test round is used, set champ exit to NA as we don't really simulate
  # full tournament then and set champ_exit to NA which result in NA percentages of sb
  # and conf columns
  if(champ_exit < 6) champ_exit <- NA_real_
  
  overall <- all_teams %>%
    group_by(region, team_id) %>%
    summarize(
      #wins = mean(result > 0),
      round_2 = mean(exit >= 2),
      sweet_sixteen = mean(exit >= 3),
      elite_eight = mean(exit >= 4),
      final_four = mean(exit >= 5),
      final = mean(exit >= 6),
      won_title = mean(exit == champ_exit)
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
    group_by(round_name, round_no, away_id, home_id) %>%
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
    arrange(round_no)
  
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
        "playoff_seeds" = playoff_seeds,
        "left_bracket" = left_bracket,
        "right_bracket" = right_bracket,
        "if_ended_today" = if_ended_today,
        "fresh_tourney" = fresh_tourney,
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

saveRDS(simulate_ncaa, 'Simulation Backup/Functions/simulate_ncaa.rds')
