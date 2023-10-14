#### xg boost model build ####
library(xgboost)
library(caret)
# Create a random seed for reproducibility
set.seed(21)

schedule <- readRDS("schedule.rds")
schedule$wind <- replace_na(schedule$wind, 0)
schedule$temp <- replace_na(schedule$temp, 0)
schedule$roof <- as.integer(factor(schedule$roof))
schedule$surface <- as.integer(factor(schedule$surface))

schedule[,-c(1:7)] -> schedule

schedule <- schedule %>% 
  select(-c(total, home_win)) %>% 
  mutate(cover = ifelse(spread_line < 0, ifelse(result*-1 < spread_line, 1, 0),
                        ifelse(result >= -1*spread_line, 1, 0)))

# Split your data into training and testing sets
# data: Your dataset
# p: The proportion of data to be used for training (e.g., 0.7 for a 70-30 split)
# list: Set to TRUE to return the indices as a list
train_indices <- createDataPartition(schedule$home_diff, p = 0.7, list = FALSE)

# Create the training and testing datasets
training_data <- schedule[train_indices, ]
testing_data <- schedule[-train_indices, ]

tibble(training_data) -> training_data
tibble(testing_data) -> testing_data
# define target variable

target_variable <- training_data$cover




features <- training_data[c(
  "spread_line", "total_line", "home_rest", "away_rest",
  "temp", "wind", "total_dvoa_rank.x", "total_dvoa.x",
  "offense_dvoa_rank.x", "offense_dvoa.x", "defense_dvoa_rank.x", "defense_dvoa.x",
  "special_teams_dvoa_rank.x", "special_teams_dvoa.x", "total_dvoa_rank.y", "total_dvoa.y",
  "offense_dvoa_rank.y", "offense_dvoa.y", "defense_dvoa_rank.y", "defense_dvoa.y",
  "special_teams_dvoa_rank.y", "special_teams_dvoa.y"
)]



## train the model
# Convert your data to DMatrix format (required by xgboost)
dtrain <- xgb.DMatrix(data = as.matrix(training_data %>% select(-c(result, home_diff, cover))), label = target_variable)


# Train the XGBoost model
xgb_model <- xgboost(objective = "binary:logistic", 
                     eta = 0.01,
                     max_depth = 6,
                     nrounds = 100, 
                     data = dtrain)

dtest <- xgb.DMatrix(data = as.matrix(testing_data %>% select(-c(result, home_diff, cover))))


testing_data$predicting <- predict(xgb_model, dtest)


testing_data %>% 
  mutate(cover = ifelse(spread_line < 0, ifelse(result*-1 < spread_line, 1, 0),
                        ifelse(result >= -1*spread_line, 1, 0)),
         est_cover = ifelse(predicting>= 0.5, 1, 0),
         correct = ifelse(cover == est_cover, 1, 0)) %>% 
  pull(correct) %>% mean()


### test on current data

nflseedR::load_sharpe_games() %>% 
  filter(season == 2023) %>% 
  mutate(home_win = ifelse(result > 0, 1, 0),
         home_diff = result,
         spread_line = -1*spread_line) -> schedule2023

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
schedule2023$roof <- as.integer(factor(schedule2023$roof))
schedule2023$surface <- as.integer(factor(schedule2023$surface))
schedule2023$wind <- replace_na(schedule2023$wind, 0)
schedule2023$temp <- replace_na(schedule2023$temp, 0)


schedule2023_pred <- schedule2023 %>% 
  select(colnames(schedule))


schedule2023_pred <- xgb.DMatrix(as.matrix(schedule2023_pred %>% select(-c(result, home_diff))))

schedule2023$estimate <- predict(xgb_model, schedule2023_pred)


schedule2023 %>% 
  mutate(cover = ifelse(spread_line < 0, ifelse(result*-1 < spread_line, 1, 0),
                        ifelse(result >= -1*spread_line, 1, 0)),
         est_cover = ifelse(spread_line < 0, ifelse(estimate*-1 < spread_line, 1, 0),
                            ifelse(estimate >= -1*spread_line, 1, 0)),
         correct = ifelse(cover == est_cover, 1, 0)) %>% 
  pull(correct) %>% mean(na.rm=T)


colnames(schedule2023_pred)
colnames(schedule)

`%ni%` <- negate(`%in%`)

colnames(training_data)[which(colnames(training_data) %ni% colnames(schedule2023_pred))]


schedule2023 %>% 
  mutate(cover = ifelse(spread_line < 0, ifelse(result*-1 < spread_line, 1, 0),
                        ifelse(result >= -1*spread_line, 1, 0)),
         est_cover = ifelse(spread_line < 0, ifelse(estimate*-1 < spread_line, 1, 0),
                            ifelse(estimate >= -1*spread_line, 1, 0)),
         correct = ifelse(cover == est_cover, 1, 0)) %>% 
  select(home_team, home_score, away_team, away_score, spread_line, estimate,
         cover, est_cover, correct) %>% view()


