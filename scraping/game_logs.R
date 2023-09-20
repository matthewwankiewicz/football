library(nflfastR)
library(tidyverse)
library(plotly)

game_logs <- nflfastR::load_player_stats(seasons = 2022:2023)

game_logs <- game_logs %>% 
  mutate(fantasy_points_half_ppr = fantasy_points + 0.5*receptions)

snap_counts <- nflreadr::load_snap_counts(seasons = 2022:2023)

snap_counts <- snap_counts %>% 
  select(player, week, season, offense_pct)

game_logs <- game_logs %>% 
  left_join(snap_counts, 
            by = c("player_display_name" = "player", "week", "season"))

game_logs %>%
  filter(player_display_name == "Stefon Diggs") %>%
  ggplot(aes(x = week)) +
  geom_line(aes(y = target_share*50), color = "blue") +
  geom_line(aes(y = fantasy_points_half_ppr), color = "red") +  # Scale the second y-axis for better visualization
  scale_y_continuous(name = "Target Share", sec.axis = sec_axis(~./50, name = "Fantasy Points")) +
  labs(title = "Keenan Allen's Target Share and Fantasy Points Over Weeks") +
  theme_classic()

ggplotly(season_plot)


write_rds(game_logs, "football_plots/gamelogs.rds")







