library(dplyr)
library(readr)

schedule <- read_rds("schedule.rds")

glimpse(schedule)


### load in schedule from seedr
schedule_full <- nflseedR::load_schedules()

schedule_full %>% 
  select(home_team, away_team, home_score, away_score, season, week, gameday) -> schedule_full


schedule %>% 
  left_join(schedule_full,
            by = c('home_team', "away_team", "home_score", "away_score",
                   "season")) -> schedule

library(dplyr)

# Assuming your dataframe is named 'schedule'

# Combine data for both home and away games
home_data <- schedule %>%
  select(season, gameday, home_team, home_score, away_team, away_score) %>%
  rename(team = home_team, score = home_score, opponent_team = away_team, opponent_score = away_score)

away_data <- schedule %>%
  select(season, gameday, away_team, away_score, home_team, home_score) %>%
  rename(team = away_team, score = away_score, opponent_team = home_team, opponent_score = home_score)

combined_data <- bind_rows(home_data, away_data) %>%
  arrange(team, gameday)

# Function to calculate scores and opponents' scores from one and two weeks prior
calculate_previous_scores <- function(df, team_col, score_col, opp_team_col, opp_score_col, prev_score_1wk_col, prev_opp_score_1wk_col, prev_score_2wk_col, prev_opp_score_2wk_col) {
  df <- df %>%
    group_by({{ team_col }}) %>%
    mutate({{ prev_score_1wk_col }} := lag({{ score_col }}, default = 0),
           {{ prev_opp_score_1wk_col }} := lag({{ opp_score_col }}, default = 0),
           {{ prev_score_2wk_col }} := lag({{ score_col }}, n = 2, default = 0),
           {{ prev_opp_score_2wk_col }} := lag({{ opp_score_col }}, n = 2, default = 0))
  return(df)
}

# Calculate scores and opponents' scores from one and two weeks prior for each team
combined_data <- calculate_previous_scores(combined_data, team, score, opponent_team, opponent_score, prev_score_1wk, prev_opp_score_1wk, prev_score_2wk, prev_opp_score_2wk)

# Merge the calculated scores and opponents' scores from one and two weeks prior back into the original schedule dataframe
schedule <- left_join(schedule, combined_data %>% select(season, gameday, team, prev_score_1wk, prev_opp_score_1wk, prev_score_2wk, prev_opp_score_2wk), by = c("season", "gameday", "home_team" = "team"))
schedule <- left_join(schedule, combined_data %>% select(season, gameday, team, prev_score_1wk, prev_opp_score_1wk, prev_score_2wk, prev_opp_score_2wk), by = c("season", "gameday", "away_team" = "team"))
# View the updated dataset with the new variables
glimpse(schedule)


## filter schedule for week > 2

#### model building ----
schedule %>% 
  filter(week > 2) -> schedule_full

## split into train and test sets
set.seed(21)
dt = sort(sample(nrow(schedule_full), nrow(schedule_full)*.7))
train<-schedule_full[dt,]
test<-schedule_full[-dt,]


## create a basic model, using previous scores as predictors
model_1 <- lm(home_score ~ prev_score_1wk.x + prev_score_1wk.y +
                prev_opp_score_1wk.x + prev_opp_score_1wk.y +
                prev_score_2wk.x + prev_score_2wk.y +
                prev_opp_score_2wk.x + prev_opp_score_2wk.y +
                offense_dvoa_rank.x + offense_dvoa_rank.y + 
                defense_dvoa_rank.x + defense_dvoa_rank.y +
                spread_line*total_line,
              data = train)

model_2 <-  lm(away_score ~ prev_score_1wk.x + prev_score_1wk.y +
                 prev_opp_score_1wk.x + prev_opp_score_1wk.y +
                 prev_score_2wk.x + prev_score_2wk.y +
                 prev_opp_score_2wk.x + prev_opp_score_2wk.y +
                 offense_dvoa_rank.x + offense_dvoa_rank.y + 
                  defense_dvoa_rank.x + defense_dvoa_rank.y +
                 spread_line*total_line,
               data = train)



summary(aic_mod)

## create estimates
test$home_pred <- predict(model_1, newdata = test)
test$away_pred <-predict(model_2, newdata = test) 

test %>% 
  mutate(home_diff_pred = home_pred - away_pred) -> test

mae <- mean(abs(test$home_diff_pred - test$home_diff))
rmse <- sqrt(mean((test$home_diff_pred - test$home_diff)^2))

# Print the evaluation metrics
cat("Mean Absolute Error (MAE): ", mae, "\n")
cat("Root Mean Squared Error (RMSE): ", rmse, "\n")


## apply previous score method to current schedule

nflseedR::load_sharpe_games() %>% 
  filter(season == 2023) -> schedule2023

## load in current season dvoa
dvoa2023 <- read_csv("models/total_team_dvoa.csv") %>% 
  janitor::clean_names() %>% 
  select(team, total_dvoa_rank, total_dvoa, "offense_dvoa_rank" = offense_rank, 
         offense_dvoa, "defense_dvoa_rank" = defense_rank, defense_dvoa,
         "special_teams_dvoa_rank" = special_teams_rank, special_teams_dvoa)

dvoa2023[dvoa2023 == "JAC"] <- "JAX"

write_csv(dvoa2023, "models/dvoa2023.csv")



schedule2023 %>% 
  left_join(dvoa2023, by = c("home_team" = "team")) %>% 
  left_join(dvoa2023, by = c("away_team" = "team")) -> schedule2023

schedule2023[schedule2023 == ""] <- "outdoors"


home_data <- schedule2023 %>%
  select(season, gameday, home_team, home_score, away_team, away_score) %>%
  rename(team = home_team, score = home_score, opponent_team = away_team, opponent_score = away_score)

away_data <- schedule2023 %>%
  select(season, gameday, away_team, away_score, home_team, home_score) %>%
  rename(team = away_team, score = away_score, opponent_team = home_team, opponent_score = home_score)

combined_data <- bind_rows(home_data, away_data) %>%
  arrange(team, gameday)

# Function to calculate scores and opponents' scores from one and two weeks prior
calculate_previous_scores <- function(df, team_col, score_col, opp_team_col, opp_score_col, prev_score_1wk_col, prev_opp_score_1wk_col, prev_score_2wk_col, prev_opp_score_2wk_col) {
  df <- df %>%
    group_by({{ team_col }}) %>%
    mutate({{ prev_score_1wk_col }} := lag({{ score_col }}, default = 0),
           {{ prev_opp_score_1wk_col }} := lag({{ opp_score_col }}, default = 0),
           {{ prev_score_2wk_col }} := lag({{ score_col }}, n = 2, default = 0),
           {{ prev_opp_score_2wk_col }} := lag({{ opp_score_col }}, n = 2, default = 0))
  return(df)
}

# Calculate scores and opponents' scores from one and two weeks prior for each team
combined_data <- calculate_previous_scores(combined_data, team, score, opponent_team, opponent_score, prev_score_1wk, prev_opp_score_1wk, prev_score_2wk, prev_opp_score_2wk)

# Merge the calculated scores and opponents' scores from one and two weeks prior back into the original schedule dataframe
schedule2023 <- left_join(schedule2023, combined_data %>% select(season, gameday, team, prev_score_1wk, prev_opp_score_1wk, prev_score_2wk, prev_opp_score_2wk), by = c("season", "gameday", "home_team" = "team"))
schedule2023 <- left_join(schedule2023, combined_data %>% select(season, gameday, team, prev_score_1wk, prev_opp_score_1wk, prev_score_2wk, prev_opp_score_2wk), by = c("season", "gameday", "away_team" = "team"))
# 

schedule2023 %>% filter(week > 2) -> schedule_correct


schedule_correct$home_pred <- predict(model_1, newdata = schedule_correct)
schedule_correct$away_pred <-predict(model_2, newdata = schedule_correct) 


schedule_correct %>% 
  select(gameday, home_team, home_pred, away_team, away_pred) %>% view



schedule_correct %>% 
  mutate(spread_line = spread_line*-1,
         total_est = home_pred + away_pred,
         diff_est = home_pred - away_pred,
         est_win2 = ifelse(home_pred > away_pred, 1, 0),
         cover = ifelse(spread_line < 0, ifelse(result*-1 < spread_line, 1, 0),
                        ifelse(result >= -1*spread_line, 1, 0)),
         est_cover = ifelse(spread_line < 0, ifelse(diff_est*-1 < spread_line, 1, 0),
                            ifelse(diff_est >= -1*spread_line, 1, 0)),
         # est_cover2 = ifelse(spread_line < 0, ifelse(est*-1 < spread_line, 1, 0),
         #                     ifelse(est >= -1*spread_line, 1, 0)),
         correct = ifelse(cover == est_cover, 1, 0),
         #agree = ifelse(est_cover == est_cover2, 1, 0),
         # correct2 = ifelse(cover == est_cover2, 1, 0),
         under = ifelse(total < total_line, 1, 0),
         est_under = ifelse(home_pred + away_pred < total_line, 1, 0),
         correct_under = ifelse(under == est_under, 1, 0)) -> metric.2023
         

metric.2023 %>% 
  pull(correct) %>% mean(na.rm = T)


### re-create using xgboost
library(xgboost)

# Assume 'train' is your training dataset

# Convert your data to DMatrix format (required for xgboost)
train_matrix <- xgb.DMatrix(data = as.matrix(train[, c("prev_score_1wk.x", "prev_score_1wk.y",
                                                       "prev_opp_score_1wk.x", "prev_opp_score_1wk.y",
                                                       "prev_score_2wk.x", "prev_score_2wk.y",
                                                       "prev_opp_score_2wk.x", "prev_opp_score_2wk.y",
                                                       "offense_dvoa_rank.x", "offense_dvoa_rank.y",
                                                       "defense_dvoa_rank.x", "defense_dvoa_rank.y",
                                                       "spread_line")]),
                            label = train$home_score)

# Define parameters for the XGBoost model
params <- list(
  objective = "reg:squarederror",  # Regression task
  eta = 0.1,  # Learning rate
  max_depth = 6,  # Maximum tree depth
  subsample = 0.7,  # Subsample ratio of the training instances
  colsample_bytree = 0.7  # Subsample ratio of columns when constructing each tree
)

# Train the model for home_score prediction
model_1 <- xgb.train(params = params, data = train_matrix, nrounds = 100)

model_2 <- xgb.train(params = params, data = train_matrix_away, nrounds = 100)


### add in schedule
nflseedR::load_sharpe_games() %>% 
  filter(season == 2023) -> schedule2023

## load in current season dvoa
dvoa2023 <- read_csv("models/total_team_dvoa.csv") %>% 
  janitor::clean_names() %>% 
  select(team, total_dvoa_rank, total_dvoa, "offense_dvoa_rank" = offense_rank, 
         offense_dvoa, "defense_dvoa_rank" = defense_rank, defense_dvoa,
         "special_teams_dvoa_rank" = special_teams_rank, special_teams_dvoa)

dvoa2023[dvoa2023 == "JAC"] <- "JAX"

write_csv(dvoa2023, "models/dvoa2023.csv")



schedule2023 %>% 
  left_join(dvoa2023, by = c("home_team" = "team")) %>% 
  left_join(dvoa2023, by = c("away_team" = "team")) -> schedule2023

schedule2023[schedule2023 == ""] <- "outdoors"


home_data <- schedule2023 %>%
  select(season, gameday, home_team, home_score, away_team, away_score) %>%
  rename(team = home_team, score = home_score, opponent_team = away_team, opponent_score = away_score)

away_data <- schedule2023 %>%
  select(season, gameday, away_team, away_score, home_team, home_score) %>%
  rename(team = away_team, score = away_score, opponent_team = home_team, opponent_score = home_score)

combined_data <- bind_rows(home_data, away_data) %>%
  arrange(team, gameday)

# Function to calculate scores and opponents' scores from one and two weeks prior
calculate_previous_scores <- function(df, team_col, score_col, opp_team_col, opp_score_col, prev_score_1wk_col, prev_opp_score_1wk_col, prev_score_2wk_col, prev_opp_score_2wk_col) {
  df <- df %>%
    group_by({{ team_col }}) %>%
    mutate({{ prev_score_1wk_col }} := lag({{ score_col }}, default = 0),
           {{ prev_opp_score_1wk_col }} := lag({{ opp_score_col }}, default = 0),
           {{ prev_score_2wk_col }} := lag({{ score_col }}, n = 2, default = 0),
           {{ prev_opp_score_2wk_col }} := lag({{ opp_score_col }}, n = 2, default = 0))
  return(df)
}

# Calculate scores and opponents' scores from one and two weeks prior for each team
combined_data <- calculate_previous_scores(combined_data, team, score, opponent_team, opponent_score, prev_score_1wk, prev_opp_score_1wk, prev_score_2wk, prev_opp_score_2wk)

# Merge the calculated scores and opponents' scores from one and two weeks prior back into the original schedule dataframe
schedule2023 <- left_join(schedule2023, combined_data %>% select(season, gameday, team, prev_score_1wk, prev_opp_score_1wk, prev_score_2wk, prev_opp_score_2wk), by = c("season", "gameday", "home_team" = "team"))
schedule2023 <- left_join(schedule2023, combined_data %>% select(season, gameday, team, prev_score_1wk, prev_opp_score_1wk, prev_score_2wk, prev_opp_score_2wk), by = c("season", "gameday", "away_team" = "team"))
# 

schedule2023 %>% filter(week > 2) -> schedule_correct


schedule_correct$home_pred <- predict(model_1, newdata = schedule_correct)
schedule_correct$away_pred <-predict(model_2, newdata = schedule_correct) 


schedule_correct %>% 
  select(gameday, home_team, home_pred, away_team, away_pred) %>% view



# Convert test data to matrix format
test_matrix <- as.matrix(test[, c("prev_score_1wk.x", "prev_score_1wk.y",
                                       "prev_opp_score_1wk.x", "prev_opp_score_1wk.y",
                                       "prev_score_2wk.x", "prev_score_2wk.y",
                                       "prev_opp_score_2wk.x", "prev_opp_score_2wk.y",
                                       "offense_dvoa_rank.x", "offense_dvoa_rank.y",
                                       "defense_dvoa_rank.x", "defense_dvoa_rank.y")])

# Predict home scores using model_1
home_score_predictions <- predict(model_1, test_matrix)

# Predict away scores using model_2
away_score_predictions <- predict(model_2, test_matrix)

# Assuming you want to combine predictions with test_data
predicted_scores <- data.frame(test, home_pred = home_score_predictions, away_pred = away_score_predictions)

# View the predicted scores
print(predicted_scores)


### ----
schedule23.matrix <- as.matrix(schedule_correct[, c("prev_score_1wk.x", "prev_score_1wk.y",
                                  "prev_opp_score_1wk.x", "prev_opp_score_1wk.y",
                                  "prev_score_2wk.x", "prev_score_2wk.y",
                                  "prev_opp_score_2wk.x", "prev_opp_score_2wk.y",
                                  "offense_dvoa_rank.x", "offense_dvoa_rank.y",
                                  "defense_dvoa_rank.x", "defense_dvoa_rank.y")])

# Predict home scores using model_1
home_score_predictions <- predict(model_1, schedule23.matrix)

# Predict away scores using model_2
away_score_predictions <- predict(model_2, schedule23.matrix)

# Assuming you want to combine predictions with test_data
predicted_scores <- data.frame(schedule_correct, home_pred = home_score_predictions, away_pred = away_score_predictions)


predicted_scores %>% 
  mutate(spread_line = spread_line*-1,
         total_est = home_pred + away_pred,
         diff_est = home_pred - away_pred,
         est_win2 = ifelse(home_pred > away_pred, 1, 0),
         cover = ifelse(spread_line < 0, ifelse(result*-1 < spread_line, 1, 0),
                        ifelse(result >= -1*spread_line, 1, 0)),
         est_cover = ifelse(spread_line < 0, ifelse(diff_est*-1 < spread_line, 1, 0),
                            ifelse(diff_est >= -1*spread_line, 1, 0)),
         # est_cover2 = ifelse(spread_line < 0, ifelse(est*-1 < spread_line, 1, 0),
         #                     ifelse(est >= -1*spread_line, 1, 0)),
         correct = ifelse(cover == est_cover, 1, 0),
         #agree = ifelse(est_cover == est_cover2, 1, 0),
         # correct2 = ifelse(cover == est_cover2, 1, 0),
         under = ifelse(total < total_line, 1, 0),
         est_under = ifelse(home_pred + away_pred < total_line, 1, 0),
         correct_under = ifelse(under == est_under, 1, 0)) -> metric.2023


metric.2023 %>% pull(correct) %>% mean(na.rm=T)



### begin tuning model
param_grid <- list(
  eta = c(0.05, 0.1, 0.3),  # Learning rate
  max_depth = c(3, 6, 9),   # Maximum tree depth
  subsample = c(0.6, 0.8, 1),  # Subsample ratio of the training instances
  colsample_bytree = c(0.6, 0.8, 1)  # Subsample ratio of columns when constructing each tree
)


best_params <- NULL
best_rmse <- Inf

for (eta in param_grid$eta) {
  for (depth in param_grid$max_depth) {
    for (subsample in param_grid$subsample) {
      for (colsample in param_grid$colsample_bytree) {
        
        params <- list(
          objective = "reg:squarederror",
          eta = eta,
          max_depth = depth,
          subsample = subsample,
          colsample_bytree = colsample
        )
        
        cv <- xgb.cv(params = params, data = train_matrix, nfold = 5, nrounds = 100, metrics = "rmse", early_stopping_rounds = 10)
        
        mean_rmse <- tail(cv$evaluation_log[, "test_rmse_mean"], 1)
        
        if (mean_rmse < best_rmse) {
          best_rmse <- mean_rmse
          best_params <- params
        }
      }
    }
  }
}

### re-train model ----
model_1 <- xgb.train(params = best_params, data = train_matrix, nrounds = 100)
model_2 <- xgb.train(params = best_params, data = train_matrix, nrounds = 100)


schedule23.matrix <- as.matrix(schedule_correct[, c("prev_score_1wk.x", "prev_score_1wk.y",
                                                    "prev_opp_score_1wk.x", "prev_opp_score_1wk.y",
                                                    "prev_score_2wk.x", "prev_score_2wk.y",
                                                    "prev_opp_score_2wk.x", "prev_opp_score_2wk.y",
                                                    "offense_dvoa_rank.x", "offense_dvoa_rank.y",
                                                    "defense_dvoa_rank.x", "defense_dvoa_rank.y",
                                                    "spread_line")])

# Predict home scores using model_1
home_score_predictions <- predict(model_1, schedule23.matrix)

# Predict away scores using model_2
away_score_predictions <- predict(model_2, schedule23.matrix)

# Assuming you want to combine predictions with test_data
predicted_scores <- data.frame(schedule_correct, home_pred = home_score_predictions, away_pred = away_score_predictions)


predicted_scores %>% 
  mutate(spread_line = spread_line*-1,
         total_est = home_pred + away_pred,
         diff_est = home_pred - away_pred,
         est_win2 = ifelse(home_pred > away_pred, 1, 0),
         cover = ifelse(spread_line < 0, ifelse(result*-1 < spread_line, 1, 0),
                        ifelse(result >= -1*spread_line, 1, 0)),
         est_cover = ifelse(spread_line < 0, ifelse(diff_est*-1 < spread_line, 1, 0),
                            ifelse(diff_est >= -1*spread_line, 1, 0)),
         # est_cover2 = ifelse(spread_line < 0, ifelse(est*-1 < spread_line, 1, 0),
         #                     ifelse(est >= -1*spread_line, 1, 0)),
         correct = ifelse(cover == est_cover, 1, 0),
         #agree = ifelse(est_cover == est_cover2, 1, 0),
         # correct2 = ifelse(cover == est_cover2, 1, 0),
         under = ifelse(total < total_line, 1, 0),
         est_under = ifelse(home_pred + away_pred < total_line, 1, 0),
         correct_under = ifelse(under == est_under, 1, 0)) -> metric.2023


metric.2023 %>% pull(correct) %>% mean(na.rm=T)


predicted_scores %>% 
  select(gameday, week, home_team, home_score, home_pred, 
         away_team, away_score, away_pred) %>% view



#### create new model ----
library(xgboost)

# Assuming 'train' is your training dataset
set.seed(21)
dt = sort(sample(nrow(schedule_full), nrow(schedule_full)*.7))
train<-schedule_full[dt,]
test<-schedule_full[-dt,]

colnames(train)

train %>% 
  select(-c(season, game_type, weekday, home_team, away_team, home_score, 
            away_score, total, wind, temp, week, gameday, home_diff, home_win)) -> train

## 1 = turf, 0 = grass
## 1 = outdoors, 0 = indoors

train %>% 
  mutate(surface = ifelse(grepl("turf", surface), 1, 0),
         roof = ifelse(roof %in% c("outdoors", "open"), 1, 0)) -> train


# Convert training data to matrix format
features <- train[, -c("result")]
labels <- train$result

train_matrix <- xgb.DMatrix(label = labels, data = as.matrix(features))

# Define model parameters
params <- list(
  objective = "reg:squarederror",  # Regression task
  eta = 0.1,                      # Learning rate
  max_depth = 6,                  # Maximum tree depth
  subsample = 0.8,                # Subsample ratio of the training instances
  colsample_bytree = 0.8          # Subsample ratio of columns when constructing each tree
)

# Train the model
model <- xgb.train(params = params, data = train_matrix, nrounds = 100)


# Feature Importance Scores
importance_scores <- xgb.importance(model = model)

# View importance scores
print(importance_scores)

# Visualize Feature Importance
xgb.plot.importance(importance_matrix = importance_scores)


# Assuming 'feature_importance' is your feature importance dataframe
threshold <- 0.03  # Set your threshold here

selected_features <- importance_scores[Gain > threshold, Feature]


train_matrix[, selected_features]

xgb.train(params = params, train = train_matrix[, selected_features])
