compute_conference_seeds <- function(teams,
                                     games,
                                     .debug = FALSE,
                                     playoff_seeds = 17) {
  # catch invalid input
  if (!any((names(teams) %in% "seed")) | !is.data.frame(teams)) {
    cli::cli_abort(
      "The argument {.arg teams} has to be a data frame including \\
      the variable {.val seed}!"
    )
  }
  
  teams$exit <- NA_real_
  
  games <- games %>% 
    filter(!is.na(result))
  
  played_rounds <- games %>% 
    pull(round_no) %>% 
    unique()
  
  # seed loop
  for (played_round in played_rounds)
  {
    report("Calculating seeds #{played_round}")
    
    # find teams at this seed
    update <- games %>%
      filter(round_no == played_round) %>%
      double_games() %>%
      select(sim, team_id, outcome, winner_to) %>% 
      right_join(teams %>% select(-winner_to), by = c("sim", "team_id")) %>% 
      mutate(exit = ifelse(outcome == 0, played_round, NA_real_)) %>% 
      filter(!is.na(winner_to)) %>% 
      select(sim, team_id, winner_to, exit)
    
    # store updates
    teams <- teams %>% 
      left_join(update, by = c("sim", "team_id")) %>%
      mutate(exit = case_when(
        is.na(exit.x) & is.na(exit.y) ~ NA_real_,
        is.na(exit.x) & !is.na(exit.y) ~ exit.y,
        !is.na(exit.x) ~ exit.x),
        winner_to = case_when(
          played_round == 0 ~ winner_to.x,
          TRUE ~ winner_to.y
        )) %>% 
      select(region, sim, seed, team_id, team_name, first_four, winner_to, exit)
    
  } # end conference rank loop
  
  
  list(
    "standings" = tibble::as_tibble(teams)
  )
}

saveRDS(compute_conference_seeds, 'Simulation Backup/Functions/compute_conference_seeds.rds')
