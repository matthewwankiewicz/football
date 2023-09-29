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

write_rds(game_logs, "football_plots/gamelogs.rds")







