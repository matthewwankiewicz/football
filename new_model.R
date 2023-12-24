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
                defense_dvoa_rank.x + defense_dvoa_rank.y,
              data = train)

model_2 <-  lm(away_score ~ prev_score_1wk.x + prev_score_1wk.y +
                 prev_opp_score_1wk.x + prev_opp_score_1wk.y +
                 prev_score_2wk.x + prev_score_2wk.y +
                 prev_opp_score_2wk.x + prev_opp_score_2wk.y +
                 offense_dvoa_rank.x + offense_dvoa_rank.y + 
                  defense_dvoa_rank.x + defense_dvoa_rank.y,
               data = train)

step(model_1) -> aic_mod


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



schedule2023$home_pred <- predict(model_1, newdata = test)
schedule2023$away_pred <-predict(model_2, newdata = test) 
