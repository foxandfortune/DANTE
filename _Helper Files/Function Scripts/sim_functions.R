# ──────────────────────────────────────────────────────────────────────────────
# sim_functions.R
#
# Everything the daily simulations (VRGL/4. Run Simulation - MBB.R and
# BTRC/4. Run Simulation - WBB.R) need, in one place:
#
#   simulate_ncaa_week()         runs N simulations of the games in a schedule
#   simulate_round_week()        one batch of simulations, day by day (called by the above);
#                                team_file picks the men's or women's team database
#   compute_league_ranks()       W-L and point differential per team per simulation
#   double_games_week()          one row per team per game
#   process_games_calibrated()   the game engine: calibrated margin/total/noise (production)
#   process_games_to_oreb_ast()  the previous engine (x1.3 stretch + disp_Rtg), kept as a fallback
#   report(), is_single_digit_numeric(), is_sequential()   small helpers
#   get_odds()                   Action Network odds for a set of dates (uses `teams` from the script)
#   double_odds_week()           one row per team per simulated game, with scores
#   load_sim_ratings()           the ratings list for a sim: in-season ratings, or the
#                                preseason priors before any games have been played
#   summarise_sim_results()      one row per team per game: win %, margin, projected scores
#   add_no_prior_results()       adds the no-prior sim's win % and margin
#   build_bets()                 bets with value against the odds (men's)
#
# Use:  source("_Helper Files/Function Scripts/sim_functions.R")
#
# These are the same functions the scripts used to load from the .rds files in
# _Helper Files/Simulation Functions and _Helper Files/Other (checked identical),
# with two changes: simulate_round_week() takes team_file instead of having a men's
# and a women's copy, and the functions call each other by these names.
# The .rds files are left in place for the March Madness scripts.
#
# Needs: tidyverse, gsubfn (for list[a, b] <- ...), furrr, progressr, data.table, cli,
# and odds.converter for build_bets().
# ──────────────────────────────────────────────────────────────────────────────

# Season simulation ------------------------------------------------------------
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
      .f = simulate_round_week,
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
                           sim_include,
                           team_file = "_Helper Files/Team Data/team_database.rds") {
  
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
  teams <- readRDS(team_file) %>% 
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
        double_games_week() %>%
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
          double_games_week() %>%
          filter(outcome == 1) %>%
          select(sim, team_id, outcome) %>%
          right_join(teams, by = c("sim", "team_id")) %>%
          mutate(exit = ifelse(!is.na(outcome), round_num + 1, exit)) %>%
          select(-outcome)
      }
      
      # filter to winners or byes
      tourney_teams <- games %>%
        filter(day_no == round_num) %>%
        double_games_week() %>%
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

# Standings ---------------------------------------------------------------------
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
  games_doubled <- double_games_week(games)
  
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

# Two rows per game (one per team) ----------------------------------------------
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

# Game engines ------------------------------------------------------------------
process_games_calibrated <- function(teams, games, ratings, day_no, ...) {
  # Calibrated daily game engine ------------------------------------------------
  # Same ratings and expected-efficiency formula as process_games_wk_to_oreb_ast,
  # but the fixed x1.3 margin stretch and single disp_Rtg are replaced by a
  # calibration table (ratings$calibration, built by _Backtest/build_calibration.R):
  #   * margin  = k[phase] * raw margin + hfa (non-neutral home team)
  #   * total   = raw total + total_bias[phase]
  #   * scoring noise drawn so the margin sd and total sd each match history
  #     (the two teams' noise is correlated; total sd > margin sd)
  # Self-contained on purpose (it was saved as an .rds closure).

  cal <- ratings$calibration
  if (is.null(cal)) stop("ratings$calibration is missing; load calibration_*.rds into the ratings list")

  ratings_pace <- ratings$pace
  ratings_rtg  <- ratings$rtg
  ratings_oreb <- ratings$oreb
  ratings_to   <- ratings$to
  ratings_ast  <- ratings$ast

  base_of <- function(r) r$value[r$name == "(Intercept)"]
  team_tbl <- function(r) {
    off <- r %>% filter(str_detect(name, "team_id_")) %>%
      mutate(team_id = as.integer(str_remove_all(name, "team_id_")),
             OffRtg = value + base_of(r)) %>% select(team_id, OffRtg)
    def <- r %>% filter(str_detect(name, "opp_id_")) %>%
      mutate(team_id = as.integer(str_remove_all(name, "opp_id_")),
             DefRtg = value) %>% select(team_id, DefRtg)
    left_join(off, def, by = "team_id") %>% filter(team_id %in% teams$team_id)
  }
  tm_pace <- team_tbl(ratings_pace); tm_oreb <- team_tbl(ratings_oreb)
  tm_to   <- team_tbl(ratings_to);   tm_ast  <- team_tbl(ratings_ast)
  tm_rtg  <- team_tbl(ratings_rtg)

  coefs <- ratings_rtg %>% filter(!str_detect(name, "team_") & !str_detect(name, "opp_"))
  cf <- function(n) { v <- coefs$value[coefs$name == n]; if (length(v)) v else 0 }

  phase_of <- function(d) {
    m <- as.integer(format(as.Date(d), "%m"))
    dplyr::case_when(m %in% c(10, 11) ~ "1 Nov", m == 12 ~ "2 Dec",
                     m %in% 1:2 ~ "3 Jan-Feb", TRUE ~ "4 Mar-Apr")
  }

  played_games   <- games %>% filter(!is.na(result))
  unplayed_games <- games %>% filter(is.na(result))
  if (nrow(unplayed_games) == 0) {
    return(list(teams = as_tibble(teams), games = as_tibble(games)))
  }

  pair <- function(df, tbl, a, b, prefix) {
    df %>%
      left_join(tbl %>% rename(!!paste0(prefix, "_a_off") := OffRtg, !!paste0(prefix, "_a_def") := DefRtg),
                by = setNames("team_id", a)) %>%
      left_join(tbl %>% rename(!!paste0(prefix, "_b_off") := OffRtg, !!paste0(prefix, "_b_def") := DefRtg),
                by = setNames("team_id", b))
  }

  has_date <- "game_date" %in% names(unplayed_games)
  # Tournament schedules have no is_neutral column: every game is neutral there
  if (!"is_neutral" %in% names(unplayed_games)) unplayed_games$is_neutral <- TRUE
  g <- unplayed_games %>%
    pair(tm_pace, "away_id", "home_id", "pace") %>%
    pair(tm_oreb, "away_id", "home_id", "oreb") %>%
    pair(tm_to,   "away_id", "home_id", "to") %>%
    pair(tm_ast,  "away_id", "home_id", "ast") %>%
    pair(tm_rtg,  "away_id", "home_id", "rtg") %>%
    mutate(
      .neutral = as.numeric(is_neutral),
      pace = ((pace_a_off + pace_b_def) + (pace_b_off + pace_a_def)) / 2,
      away_oreb = oreb_a_off + oreb_b_def, home_oreb = oreb_b_off + oreb_a_def,
      away_to   = to_a_off + to_b_def,     home_to   = to_b_off + to_a_def,
      away_ast  = ast_a_off + ast_b_def,   home_ast  = ast_b_off + ast_a_def,
      away_base = rtg_a_off + rtg_b_def + cf("poss_per_40") * pace +
        cf("days_rest") * away_rest + cf("travel") * away_dist + cf("neutral_site") * .neutral +
        cf("oreb_rt") * away_oreb + cf("to_rt") * away_to + cf("ast_rt") * away_ast,
      home_base = rtg_b_off + rtg_a_def + cf("poss_per_40") * pace +
        cf("days_rest") * home_rest + cf("travel") * home_dist + cf("neutral_site") * .neutral +
        cf("is_home") * (1 - .neutral) +
        cf("oreb_rt") * home_oreb + cf("to_rt") * home_to + cf("ast_rt") * home_ast,
      .phase = if (has_date) phase_of(game_date) else "4 Mar-Apr") %>%
    left_join(cal$phase_table %>% rename(.phase = phase), by = ".phase") %>%
    mutate(
      .raw_margin = (home_base - away_base) * pace / 100,
      .raw_total  = (home_base + away_base) * pace / 100,
      .mu_margin  = k * .raw_margin + cal$hfa * (1 - .neutral),
      .mu_total   = .raw_total + total_bias,
      .s   = sqrt((sd_margin^2 + sd_total^2) / 4),
      .rho = (sd_total^2 - sd_margin^2) / (sd_total^2 + sd_margin^2),
      .z1 = rnorm(n()), .z2 = rnorm(n()),
      .e_home = .s * .z1,
      .e_away = .s * (.rho * .z1 + sqrt(1 - .rho^2) * .z2),
      home_score = round((.mu_total + .mu_margin) / 2 + .e_home),
      away_score = round((.mu_total - .mu_margin) / 2 + .e_away),
      # Ties: a 5-minute overtime decides it (total points are already
      # calibrated to include overtime on average, so only the winner is drawn)
      .ot = home_score == away_score,
      .p_ot_home = pnorm(.mu_margin * (5 / 40) / (sd_margin * sqrt(5 / 40))),
      .ot_home = runif(n()) < .p_ot_home,
      home_score = ifelse(.ot & .ot_home, home_score + 1, home_score),
      away_score = ifelse(.ot & !.ot_home, away_score + 1, away_score),
      result = home_score - away_score)

  # same columns as the input (home_score / away_score / result now filled in)
  unplayed_games <- g[, union(names(games), c("home_score", "away_score", "result"))]

  games <- bind_rows(played_games, unplayed_games)
  list(teams = as_tibble(teams), games = as_tibble(games))
}

process_games_to_oreb_ast <- function(teams, games, ratings, day_no, ...) {
  
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
             (away_rest * days_rest) + (away_dist * travel) + (is_neutral * neutral_site) +
             (away_oreb * oreb_rt) + (away_to * to_rt) + (away_ast * ast_rt),
           home_base = home_Off_rtg + away_Def_rtg + (pace * poss_per_40) +
             (home_rest * days_rest) + (home_dist * travel) + (is_neutral * neutral_site) +
             (is_home * (1 - is_neutral)) + 
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
    mutate(result = home_score - away_score)  %>% 
    # Remove extra columns
    select(-c(away_Off_rtg, away_Def_rtg, away_base, away_rtg, away_score_reg, 
              home_Off_rtg, home_Def_rtg, home_base, home_rtg, home_score_reg,
              pace, poss_per_40, days_rest, travel, neutral_site, is_home,
              home_oreb, home_to, home_ast,
              away_oreb, away_to, away_ast,
              oreb_rt, to_rt, ast_rt, 
              sd_rtg, ot_adj_home, ot_adj_away))
  
  games <- bind_rows(played_games, unplayed_games)
  
  teams <- as_tibble(teams)
  
  games <- as_tibble(games)
  
  return(list(teams = teams, games = games))
}

# Small helpers -----------------------------------------------------------------
# progress report using rlang to avoid usethis dependency
report <- function(msg,
                   ...,
                   .cli_fct = cli::cli_alert_info,
                   .envir = parent.frame()) {
  .cli_fct(c(format(Sys.time(), '%H:%M:%S'), " | ", msg), ..., .envir = .envir)
}

is_single_digit_numeric <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x)

# Identify sessions with sequential future resolving
is_sequential <- function() inherits(future::plan(), "sequential")

# Odds ------------------------------------------------------------------------------
get_odds <- function(df,
                     bk_name = c("Consensus", "Open", "DraftKings",
                                 "FanDuel", "PointsBet", "BetMGM"),
                     league_nm = c("ncaab", "ncaaw")){
  
  if (!(bk_name %in% c("Consensus", "Open", "DraftKings",
                       "FanDuel", "PointsBet", "BetMGM"))) {
    cli::cli_abort("The book you have selected is not a valid option!")
  }
  
  odds <- data.frame()
  books <- readRDS('_Helper Files/Other/books.rds')
  
  
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
        
        if(!is.null(matches$odds[[j]])){
          temp.odds = bind_rows(temp.odds, temp) %>% 
            relocate(team)
        }
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

# Two rows per simulated game (one per team), with scores
double_odds_week <- function(g) {
  g1 <- g %>%
    select(sim, away_id, home_id, away_score, result) %>%
    rename(team_id = away_id, team_score = away_score, opp_id = home_id) %>%
    mutate(result = -1 * result)
  g2 <- g %>%
    select(sim, away_id, home_id, home_score, result) %>%
    rename(team_id = home_id, team_score = home_score, opp_id = away_id)
  g <- bind_rows(g1, g2) %>%
    mutate(outcome = case_when(
      result > 0 ~ 1,
      result < 0 ~ 0,
      TRUE ~ NA_real_
    ))
  return(g)
}

# Ratings for a simulation ------------------------------------------------------
# In-season ratings if they exist, else the preseason priors (first day of the
# season, before any games). no_prior = TRUE gives the ratings fit without priors,
# or NULL if they don't exist yet (no games played).
load_sim_ratings <- function(league = c("mbb", "wbb"), season, no_prior = FALSE) {
  league <- match.arg(league)
  base <- if (league == "mbb") "VRGL/Stats/Team and Player Stats/Power Ratings" else
    "BTRC/Stats/Team and Player Stats - WBB/Power Ratings"
  tag  <- if (league == "mbb") "" else "wbb_"
  cal_file <- file.path(base, "Weights", sprintf("calibration_%s%s.rds", league, if (no_prior) "_no_prior" else ""))

  if (no_prior) {
    f <- file.path(base, "Team Ratings", "Inseason", sprintf("inseason_ratings_all_no_prior_%s%d.rds", tag, season))
    if (!file.exists(f)) return(NULL)
  } else {
    f <- file.path(base, "Team Ratings", "Inseason", sprintf("inseason_ratings_all_%s%d.rds", tag, season))
    if (!file.exists(f)) f <- file.path(base, "Team Ratings", "Priors", sprintf("priors_%s%d.rds", tag, season))
    if (!file.exists(f)) stop("No in-season ratings or priors for ", season, ": ", f)
  }
  report("Ratings: {.file {f}}")
  r <- readRDS(f)
  list(pace = r$pace,
       ast = r$ast,
       oreb = r$oreb,
       to = r$to,
       rtg = r$rtg,
       calibration = readRDS(cal_file),
       disp_Rtg = 13)  # only used by process_games_to_oreb_ast
}

# Results and bets from a simulation ----------------------------------------------
# One row per team per game: win probability, median margin, projected scores.
# game_times: game_id and game_date (tip time) for the games, from the full schedule.
summarise_sim_results <- function(object, schedule, teams, game_times) {
  as.data.frame(object$games) %>% 
    double_odds_week() %>% 
    left_join(schedule %>% 
                select(game_date, team_id = home_id, opp_id = away_id) %>% 
                bind_rows(schedule %>% 
                            select(game_date, team_id = away_id, opp_id = home_id)),
              by = c("team_id", "opp_id")) %>% 
    mutate(opp_score = team_score - result,
           game_total = team_score + opp_score) %>% 
    left_join(teams %>% 
                select(team_id, Team = team), by = "team_id") %>%
    left_join(teams %>% 
                select(opp_id = team_id, Opp = team), by = "opp_id") %>% 
    with_groups(.groups = c(team_id, Team,
                            opp_id, Opp, game_date),
                summarise, win = mean(outcome),
                result = -median(result),
                team_total = round(mean(team_score)/.5, 0) * 0.5,
                opp_total = round(mean(opp_score)/.5, 0) * 0.5,
                game_total = round(mean(game_total)/.5, 0) * 0.5) %>% 
    left_join(schedule %>% 
                select(game_date, team_id = home_id, game_id) %>% 
                bind_rows(schedule %>% 
                            select(game_date, team_id = away_id, game_id)),
              by = c("team_id", "game_date")) %>% 
    select(-game_date) %>% 
    left_join(game_times, by = "game_id") %>% 
    relocate(c(game_date, game_date, game_id)) %>% 
    arrange(game_date, game_id)
}

# Add the no-prior sim's win probability and margin next to the main ones
# (NA columns when there is no no-prior sim, e.g. before any games are played)
add_no_prior_results <- function(all_results, all_results_no_prior = NULL) {
  if (is.null(all_results_no_prior)) {
    all_results <- all_results %>% mutate(win_noprior = NA_real_, result_noprior = NA_real_)
  } else {
    all_results <- all_results %>% 
      left_join(all_results_no_prior %>% 
                  select(game_id, team_id,
                         win_noprior = win,
                         result_noprior = result), by = c("game_id", "team_id"))
  }
  all_results %>% 
    relocate(win_noprior, .after = win) %>% 
    relocate(result_noprior, .after = result)
}

# Drops games whose odds contradict themselves. Action Network occasionally posts
# a game's moneylines and team totals on the wrong sides while the spread is right
# (2026-11-03: Arkansas at +3300 and a 62.5 team total against Arkansas-Pine Bluff,
# with Pine Bluff +38.5). When a team favored by 3+ on the spread has the longer
# moneyline or the lower team total, there's no telling which numbers are right,
# so all of that game's odds are dropped.
drop_inconsistent_odds <- function(odds, min_spread = 3) {
  checks <- odds %>%
    filter(!is.na(spread)) %>%
    group_by(game_id) %>%
    filter(n() == 2) %>%
    summarise(
      fav_by   = -min(spread),
      ml_wrong = !anyNA(ml) && ml[which.min(spread)] > ml[which.max(spread)],
      tt_wrong = !anyNA(tm_total) && tm_total[which.min(spread)] < tm_total[which.max(spread)],
      teams    = paste(team, collapse = " vs "),
      .groups  = "drop"
    ) %>%
    filter(fav_by >= min_spread, ml_wrong | tt_wrong)

  for (i in seq_len(nrow(checks))) {
    what <- c("moneyline", "team totals")[c(checks$ml_wrong[i], checks$tt_wrong[i])]
    report(paste0("Dropped odds for ", checks$teams[i], ": ",
                  paste(what, collapse = " and "), " on the wrong side of the spread"),
           .cli_fct = cli::cli_alert_warning)
  }

  odds %>% filter(!game_id %in% checks$game_id)
}

# Bets with value against the odds: moneyline, wrong team favored, spread, team
# over/under, game over/under. Returns NULL when there are no odds.
build_bets <- function(object, odds, schedule) {
  if (is.null(odds) || nrow(odds) == 0) {
    report("No odds found; skipping bets")
    return(NULL)
  }
  odds <- drop_inconsistent_odds(odds)
  us2prob <- odds.converter::odds.us2prob
  
  game_ids <- schedule %>% 
    select(game_id, game_date, team_id = home_id) %>% 
    bind_rows(schedule %>% 
                select(game_id, game_date, team_id = away_id))
  
  # Team bets ---------------------------------------------
  team_bets <- as.data.frame(object$games) %>% 
    double_odds_week() %>% 
    left_join(odds %>% 
                select(game_id, date,
                       espn_tm, team,
                       espn_opp, opponent,
                       ml, spread, spread_line, tm_total,
                       tm_over, tm_under) %>% 
                mutate(espn_tm = as.integer(espn_tm),
                       espn_opp = as.integer(espn_opp)), by = c("team_id" = "espn_tm",
                                                                "opp_id" = "espn_opp")) %>% 
    mutate(is_over = case_when(team_score > tm_total ~ 1,
                               TRUE ~ 0),
           is_under = case_when(team_score < tm_total ~ 1,
                                TRUE ~ 0),
           cover = case_when(result + spread > 0 ~ 1,
                             TRUE ~ 0)) %>% 
    with_groups(.groups = c(game_id, date,
                            team_id, team,
                            opp_id, opponent),
                summarise, 
                ml = mean(ml),
                win = mean(outcome),
                result = -median(result),
                spread = mean(spread),
                spread_line = mean(spread_line),
                cover = mean(cover),
                total = mean(tm_total, na.rm = TRUE),
                est_total = round(mean(team_score)/.5, 0) * 0.5,
                is_over = mean(is_over),
                is_under = mean(is_under),
                tm_over = mean(tm_over),
                tm_under = mean(tm_under)) %>% 
    mutate(ml_odds = us2prob(ml),
           over_odds = us2prob(tm_over),
           under_odds = us2prob(tm_under),
           spread_odds = us2prob(spread_line),
           ml_val = round(win - ml_odds, 3),
           over_val = round(is_over - over_odds, 3),
           under_val = round(is_under - under_odds, 3)) %>% 
    relocate(c(ml_odds, ml_val), .after = win) %>% 
    relocate(spread_odds, .after = spread_line) %>% 
    relocate(c(over_odds, over_val, tm_over), .after = is_over) %>% 
    relocate(c(under_odds, under_val), .after = is_under) %>% 
    filter(!is.na(team))
  
  ## Moneyline bets ------------------------------------------
  moneyline <- team_bets %>% 
    filter(ml_val >= 0) %>%
    mutate(payout = case_when(
      ml > 0 ~ ml / 100,
      ml < 0 ~ -100 / ml)) %>% 
    select(game_date = date, team_id, team,
           ml, payout, win, ml_odds,
           ml_val) %>% 
    left_join(game_ids, by = c("game_date", "team_id")) %>% 
    relocate(game_id)
  
  ## Wrong team favored ----------------------
  wtf <- team_bets %>% 
    filter(result < 0 & spread > 0) %>% 
    mutate(payout = case_when(
      ml > 0 ~ ml / 100,
      ml < 0 ~ -100 / ml)) %>% 
    select(game_date = date, team_id, team,
           result, spread, ml, payout) %>% 
    left_join(game_ids, by = c("game_date", "team_id")) %>% 
    relocate(game_id)
  
  ## Spread bets --------------
  spread <- team_bets %>% 
    mutate(spread_val = spread - result,
           payout = case_when(
             spread_line > 0 ~ spread_line / 100,
             spread_line < 0 ~ -100 / spread_line)) %>% 
    filter(spread_val >= 0.5) %>% 
    select(game_date = date, team_id, team,
           spread_line, payout, result, spread, spread_val,
           cover) %>%
    left_join(game_ids, by = c("game_date", "team_id")) %>% 
    relocate(game_id)
  
  ## Team overs -----------------
  tm_over <- team_bets %>%  
    filter(over_val > 0) %>% 
    mutate(payout = case_when(
      tm_over > 0 ~ tm_over / 100,
      tm_over < 0 ~ -100 / tm_over)) %>% 
    select(game_date = date, team_id, team,
           tm_over, payout, est_total, total,
           is_over, over_odds, over_val) %>% 
    left_join(game_ids, by = c("game_date", "team_id")) %>% 
    relocate(game_id)
  
  ## Team unders ---------
  tm_under <- team_bets %>% 
    filter(under_val > 0) %>% 
    mutate(payout = case_when(
      tm_under > 0 ~ tm_under / 100,
      tm_under < 0 ~ -100 / tm_under)) %>% 
    select(game_date = date, team_id, team,
           tm_under, payout, est_total, total,
           is_under, under_odds, under_val) %>% 
    left_join(game_ids, by = c("game_date", "team_id")) %>% 
    relocate(game_id)
  
  # Game bets ---------------------------------------
  game_bets <- as.data.frame(object$games) %>% 
    select(-game_id) %>% 
    mutate(est_total = away_score + home_score) %>% 
    left_join(odds %>% 
                filter(type == "away") %>% 
                mutate(espn_tm = as.double(espn_tm),
                       espn_opp = as.double(espn_opp)) %>% 
                select(game_id, date, away_id = espn_tm, away_tm = team,
                       home_id = espn_opp, home_tm = opponent,
                       total, over, under), by = c("away_id", "home_id")) %>% 
    filter(!is.na(game_id)) %>% 
    mutate(is_over = case_when(est_total > total ~ TRUE,
                               TRUE ~  FALSE),
           is_under = case_when(est_total < total ~ TRUE,
                                TRUE ~  FALSE)) %>% 
    with_groups(.groups = c(game_id, date, away_id, away_tm, home_id, home_tm),
                summarise, 
                total = mean(total),
                est_total = round(mean(est_total)/.5, 0) * 0.5,
                is_over = mean(is_over),
                over = mean(over),
                is_under = mean(is_under),
                under = mean(under)) %>% 
    mutate(over_odds = us2prob(over),
           under_odds = us2prob(under),
           over_val = round(is_over - over_odds, 3),
           under_val = round(is_under - under_odds, 3)) %>% 
    relocate(c(over_odds, over_val), .before = over) %>% 
    relocate(c(under_odds, under_val), .before = under)
  
  ## Game overs ----------------------------
  game_over <- game_bets %>% 
    mutate(total_val = abs(total - est_total)) %>% 
    filter(over_val > 0) %>% 
    mutate(payout = case_when(
      over > 0 ~ over / 100,
      over < 0 ~ -100 / over)) %>% 
    select(-game_id) %>% 
    left_join(schedule %>% 
                select(game_date, game_id, away_id, home_id),
              by = c("date" = "game_date", "away_id", "home_id")) %>% 
    select(game_id, game_date = date,
           away_id, away_tm, home_id, home_tm,
           over, payout, est_total, total, is_over, over_odds,
           total_val)
  
  ## Game unders -----------------------------
  game_under <- game_bets %>% 
    mutate(total_val = abs(total - est_total)) %>% 
    filter(under_val > 0) %>% 
    mutate(payout = case_when(
      under > 0 ~ under / 100,
      under < 0 ~ -100 / under)) %>% 
    select(-game_id) %>% 
    left_join(schedule %>% 
                select(game_date, game_id, away_id, home_id),
              by = c("date" = "game_date", "away_id", "home_id")) %>% 
    select(game_id, game_date = date,
           away_id, away_tm, home_id, home_tm,
           under, payout, est_total, total, is_under, under_odds,
           total_val)
  
  list(ml = moneyline,
       wtf = wtf,
       spread = spread,
       tm_over = tm_over,
       tm_under = tm_under,
       game_over = game_over,
       game_under = game_under)
}
