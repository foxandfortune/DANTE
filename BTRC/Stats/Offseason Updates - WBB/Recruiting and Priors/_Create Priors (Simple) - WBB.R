library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

# ──────────────────────────────────────────────────────────────────────────────
# _Create Priors (Simple) - WBB.R
#
# Preseason prior for every rating (pace, ast, oreb, to, rtg, efg, raw_rating),
# offense and defense, from the last two seasons' full-season ratings, the other
# side's rating and conference-mates' ratings. There is no women's recruiting data
# yet. Which terms each stat uses was chosen by the backtest
# (_Backtest/priors/_Test Priors - All Stats.R) and is saved in
# prior_models_wbb.rds. Every model is a plain linear regression refit here on all
# past seasons.
#
# Teams new to D1: no last-season rating, so it is 0 and the new_d1 flag carries
# the offset.
#
# Needs, for prior_season S:
#   - full-season ratings through S-1 in "Full Season - Current Weights"
#     (Full-Season Ratings WBB - All.R writes there);
#   - Rosters/roster_wbb_{S-1}.rds;
#   - Conferences/wbb_conf_{S}.rds (_Get CBB Conferences.R). Without it, last
#     season's teams are used and teams new to D1 get no prior.
# ──────────────────────────────────────────────────────────────────────────────

# Set working directory
if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  setwd('../../../..')
}

# Set season for priors
prior_season <- 2027

source('_Helper Files/Function Scripts/prior_functions.R')

# Check inputs ---------
needed <- glue::glue('BTRC/Stats/Team and Player Stats - WBB/Rosters/roster_wbb_{prior_season - 1}.rds')
if (!file.exists(needed)) stop("Missing input: ", needed)

models <- readRDS('BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Weights/prior_models_wbb.rds')
fs_dir <- 'BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Team Ratings/Full Season - Current Weights'

# Build ---------
priors.all <- build_priors("WBB", prior_season, models, fs_dir = fs_dir)

# Checks ---------
teams <- readRDS('_Helper Files/Team Data/team_database_wbb.rds')
stopifnot(all(c("pace", "ast", "oreb", "to", "rtg", "efg", "raw_rating") %in% names(priors.all)),
          !any(map_lgl(priors.all, ~ any(is.na(.x$value)))))

top <- priors.all$raw_rating %>%
  filter(str_detect(name, "_id_")) %>%
  mutate(side = if_else(str_starts(name, "team_id_"), "off", "def"),
         team_id = str_extract(name, "\\d+$")) %>%
  select(team_id, side, value) %>%
  pivot_wider(names_from = side, values_from = value) %>%
  mutate(net = off - def) %>%
  left_join(teams %>% transmute(team_id = as.character(team_id), team), by = "team_id") %>%
  arrange(desc(net))
print(head(top, 25), n = 25)

cat("\nTeams new to D1:", paste(teams$team[as.character(teams$team_id) %in% attr(priors.all, "new_d1")], collapse = ", "), "\n")
cat("Teams in the prior:", nrow(top), "\n")

## Save priors --------
saveRDS(priors.all,
        glue::glue("BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Team Ratings/Priors/priors_wbb_{prior_season}.rds"))

### As starter for all ratings --------
saveRDS(priors.all,
        glue::glue("BTRC/Stats/Team and Player Stats - WBB/Power Ratings/Team Ratings/Inseason/inseason_ratings_all_wbb_{prior_season}.rds"))
