library(tidyverse)

# Set library
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd('..')

## Set season and date ----------
season <- 2027
today <- Sys.Date()

# Pull results-----------------------------------
## MBB ------------------------------------------- 
results_mbb <- readRDS(glue::glue('VRGL/Stats/Season Schedules/Results/{season}/all_results_{today}.rds'))

## WBB ------------------------------------------- 
results_wbb <- readRDS(glue::glue('BTRC/Stats/Season Schedules/Results/{season}/all_results_{today}.rds'))

## Bets (MBB) ------------------------------------------- 
bets <- readRDS(glue::glue('VRGL/Stats/Season Schedules/Results/{season}/all_bets_{today}.rds'))

# ---- PRINT RESULTS FOR MBB -------------------------
## OVERALL ---------------------------
results_mbb %>% 
  with_groups(.groups = game_id,
              slice,
              1) %>% 
  select(-c(game_id, team_id, opp_id,
            win_noprior, result_noprior)) |> 
  arrange(game_date, Team)

## SPREAD -----------------------------
bets$spread %>%
  filter(spread_val > 1.0) %>% 
  arrange(team)



## MONEY LINE ---------------------
bets$ml %>% 
  arrange(desc(ml_val)) %>% 
  filter(ml_val >= 0.05)

## GAME TOTALS --------------
### OVER -----------------
bets$game_over

### UNDER ------------------
bets$game_under |> 
  mutate(odds_val = is_under - under_odds) |> 
  arrange(desc(odds_val))

# ---- PRINT RESULTS FOR WBB ---------------------
results_wbb %>%
  with_groups(.groups = game_id,
              slice,
              1) %>% 
  arrange(game_date, Team)

