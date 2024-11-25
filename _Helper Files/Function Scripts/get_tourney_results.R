library(tidyverse)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')

# Add NOT IN function:
`%!in%` = Negate(`%in%`)

season <- 2023

# Load schedule filter tournament games
sched <- hoopR::load_mbb_schedule(seasons = {season}) %>% 
  filter(tournament_id == 22) %>% 
  arrange(date)

pull_results <- function(df){
  # make double games 
  get_ids <- function(g) {
    g1 <- g %>%
      select(round_name, round_no, away_id, result) %>%
      rename(team_id = away_id) %>%
      mutate(result = -1 * result)
    g2 <- g %>%
      select(round_name, round_no, home_id, result) %>%
      rename(team_id = home_id)
    g <- bind_rows(g1, g2) %>%
      mutate(outcome = case_when(
        result > 0 ~ 1,
        result < 0 ~ 0,
        TRUE ~ NA_real_
      ))
    return(g)
  }
  
  # 
  res <- df %>%
    mutate(round_name = case_when(
      str_detect(notes_headline, "First Four") ~ "First Four",
      str_detect(notes_headline, "1st Round") ~ "First",
      str_detect(notes_headline, "2nd Round") ~ "Second",
      str_detect(notes_headline, "Sweet 16") ~ "Sweet 16",
      str_detect(notes_headline, "Elite 8") ~ "Elite 8",
      str_detect(notes_headline, "Final Four") ~ "Final 4",
      str_detect(notes_headline, "National Championship") ~ "Championship"),
      round_no = case_when(
        round_name == "First Four" ~ 0, round_name == "First" ~ 1,
        round_name == "Second" ~ 2, round_name == "Sweet 16" ~ 3,
        round_name == "Elite 8" ~ 4, round_name == "Final 4" ~ 5,
        round_name == "Championship" ~ 6),
      result = home_score - away_score) %>% 
    get_ids() %>% 
    arrange(round_no)
  
  # Get team_ids for each round
  champ <- res %>% 
    filter(round_name == "Championship" & result > 0) %>% 
    pull(team_id)
  finalist <- res %>% 
    filter(round_name == "Championship") %>% 
    pull(team_id)
  final_4 <- res %>% 
    filter(round_name == "Final 4") %>% 
    pull(team_id)
  elite_8 = res %>% 
    filter(round_name == "Elite 8") %>% 
    pull(team_id)
  sweet_16 <- res %>% 
    filter(round_name == "Sweet 16") %>% 
    pull(team_id)
  second_rd = res %>% 
    filter(round_name == "Second") %>% 
    pull(team_id)
  
  return(list(champ = champ, finalist = finalist,
              final_4 = final_4, elite_8 = elite_8,
              sweet_16 = sweet_16, second_rd = second_rd))
}


saveRDS(pull_results, 'Simulation Backup/Functions/pull_results.rds')
