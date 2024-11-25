

# Create team power ratings --------------------
create_ratings <- function(df, stat_name, name_list,
                           seed, wgt, lambda_adj){
  if (!({stat_name} %in% names(df))) {
    stop(
      "The stat selected is not in the data frame."
    )
  }
  
  # Create model data frame
  model.data <- df %>%
    # Select variables
    select({stat_name}, all_of({name_list})) %>%
    with_groups(.groups = team_id, arrange, desc(game_date)) %>% 
    mutate(team_id = as.character(team_id),
           opp_id = as.character(opp_id)) %>% 
    with_groups(.groups = team_id, mutate,
                game = row_number(),
                weight = wgt ^ game) %>% 
    arrange(game_date) %>% 
    select(-c(game, game_date)) %>%
    # Create dummy columns for all teams
    fastDummies::dummy_cols(select_columns = c("team_id", "opp_id"), remove_first_dummy = FALSE) %>% 
    # Remove team and opponent names
    select(-c(team_id, opp_id)) %>% 
    relocate(weight, .after = last_col()) %>% 
    relocate({stat_name})
  
  # select y and x
  y <- model.data[, 1]
  
  x <- data.matrix(model.data[, 2:(length(model.data)-1)])
  
  weight <- model.data$weight
  
  # Create glmnet model
  model <- glmnet(x, y, weights = weight, alpha = 0)
  
  #perform k-fold cross-validation to find optimal lambda value
  set.seed({seed})
  
  cv_model <- cv.glmnet(x, y, weights = weight, alpha = 0)
  
  #find optimal lambda value that minimizes test MSE
  best_lambda <- cv_model$lambda.min * (1 + {lambda_adj})
  
  # Use best lambda to create model
  best_model <- glmnet(x, y, weights = weight, alpha = 0, lambda = best_lambda)
  
  # Convert coeffecient values into data frame
  raw.coeff <- as.data.frame(as.matrix(coef(best_model))) %>% 
    rownames_to_column(var = "name")
  
  names(raw.coeff) <- c("name", "value")
  
  return(raw.coeff)
}


saveRDS(create_ratings, "Tools/create_ratings_stat.rds")

#############################################################################################
