### using game_logs, create a model that projects the number of yards/tds/catches people will get

## filter for target leader for each week (receiver stats only)
game_logs %>%
  filter(season == 2023,
         position == "WR") %>%
  group_by(week, season, opponent_team) %>%
  arrange(desc(targets), desc(receptions)) %>%  # Arrange by targets and receptions in descending order
  slice(1) %>%  # Keep only the first row for each week based on targets (or receptions in case of a tie)
  select(player_display_name, receptions, receiving_yards,
         target_share, receiving_air_yards) %>% 
  ungroup() -> wr_data


## using wr data, make a grouped table that shows season level stats for teams
season_stats <- wr_data %>%
  group_by(opponent_team) %>%
  summarise(
    total_receptions = sum(receptions, na.rm = TRUE),
    total_receiving_yards = sum(receiving_yards, na.rm = TRUE),
    avg_target_share = mean(target_share, na.rm = TRUE),
    total_receiving_air_yards = sum(receiving_air_yards, na.rm = TRUE)
    # Add other statistics you want to calculate here
  ) %>%
  ungroup()

player_season_stats <- wr_data %>% 
  group_by(player_display_name) %>% 
  summarise(total_rec_player = sum(receptions, na.rm = T),
            avg_rec_player = mean(receptions, na.rm = T))



glimpse(wr_data)

## function to pull previous week stats
get_previous_stats <- function(df) {
  df <- df %>%
    arrange(player_display_name, week) %>%
    group_by(player_display_name) %>%
    mutate(
      prev_week = lag(week),
      prev_2_weeks = lag(week, 2),
      prev_receptions = lag(receptions),
      prev_receiving_yards = lag(receiving_yards),
      prev_target_share = lag(target_share),
      prev_receiving_air_yards = lag(receiving_air_yards),
      prev_2_receptions = lag(receptions, 2),
      prev_2_receiving_yards = lag(receiving_yards, 2),
      prev_2_target_share = lag(target_share, 2),
      prev_2_receiving_air_yards = lag(receiving_air_yards, 2)
    ) %>%
    filter(week != min(week, na.rm = TRUE))
  
  return(df)
}

# Applying the function to create the table with previous week's stats
data_with_prev_stats <- get_previous_stats(wr_data)

## merge table from above with season level stats
data_with_prev_stats %>% 
  left_join(season_stats,
            by = "opponent_team") -> full_wr_data

full_wr_data %>% 
  left_join(player_season_stats,
            by = 'player_display_name') -> full_wr_data


## split the data into train and testing sets. train the model and test it

library(xgboost)
library(dplyr)
library(caret)

# Splitting data into training and testing sets (80% train, 20% test)
set.seed(123)
train_index <- createDataPartition(full_wr_data$receptions, p = 0.8, list = FALSE)
train_data <- full_wr_data[train_index, ]
test_data <- full_wr_data[-train_index, ]

# Define predictors and target variable
predictors <- c("prev_receptions", 'total_receptions', 'prev_2_receptions', "avg_rec_player")
target <- "receptions"

# Training the XGBoost model
xgb_model <- xgboost(
  data = as.matrix(train_data[predictors]),
  label = train_data[[target]],
  nrounds = 100, # Number of boosting rounds
  objective = "reg:squarederror", # Objective function for regression
  eval_metric = "rmse" # Evaluation metric: Root Mean Squared Error
)

# Making predictions on the test set
predictions <- predict(xgb_model, as.matrix(test_data[predictors]))

# Evaluating the model
rmse <- sqrt(mean((test_data[[target]] - predictions)^2))

# Displaying RMSE
print(paste("RMSE:", rmse))


## predict
new_data <- data.frame(
  prev_receptions = 6,
  total_receptions = 95,
  prev_2_receptions = 12,
  avg_rec_player = 4.25
)

# Make predictions using the trained model for the new data
prediction <- predict(xgb_model, as.matrix(new_data))

# Display the prediction
print(prediction)
