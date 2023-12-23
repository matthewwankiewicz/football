### using game logs file, filter for top target receiving tight end
### then pull their average points per game and merge it with the game logs
### Create a variable that tells us whether or not the player exceeded their 
### average total

tight_end <- game_logs %>%
  filter(position == "TE", season == 2023) %>% 
  group_by(opponent_team, week) %>% 
  slice_max(targets, n = 1, with_ties = F)



game_logs %>% 
  filter(season == 2023 &
           player_display_name %in% tight_end$player_display_name) %>% 
  group_by(player_display_name) %>% 
  summarise(avg_fan_pts = mean(fantasy_points_half_ppr, na.rm = T)) -> tight_end_averages


tight_end %>% 
  left_join(tight_end_averages,
            by = "player_display_name") %>% 
  select(opponent_team, player_display_name, week, fantasy_points_half_ppr, avg_fan_pts) %>% 
  mutate(passed_average = ifelse(fantasy_points_half_ppr > avg_fan_pts, 1, 0)) -> tight_end.merged


tight_end.merged %>% 
  group_by(opponent_team) %>% 
  summarise(big_game_rate = mean(passed_average)) %>% 
  arrange(desc(big_game_rate)) %>% 
  filter(big_game_rate < 0.5) %>% pull(opponent_team) -> better_teams


tight_end.merged %>% 
  filter(player_display_name == 'Travis Kelce',
         opponent_team %in% better_teams) %>% 
  pull(fantasy_points_half_ppr) %>% mean
