library(nflseedR)
library(tidyverse)

load_sharpe_games() -> seasons


seasons %>% 
  filter(season == 2023 & (
         home_team == "DET" | home_team == "ATL" |
           away_team == "DET" | away_team == "ATL")) %>% 
  select(season, week, home_team, home_score, away_team, away_score) %>% 
  view()


home_df <- seasons %>% select(season, week, team = home_team, score = home_score, opponent = away_team, opponent_score = away_score)
away_df <- seasons %>% select(season, week, team = away_team, score = away_score, opponent = home_team, opponent_score = home_score)

# Bind the two data frames together
wide_format_data <- bind_rows(home_df, away_df)

season23 <- wide_format_data %>% 
  filter(season == 2023) %>% 
  arrange(week) %>% 
  mutate(total_score = score + opponent_score,
         score_diff = score - opponent_score)

season23 %>% 
  filter(team == "DET" | team == "ATL") %>%
  group_by(team) %>% summarise(avg_total = mean(total_score, na.rm = T))
# 
