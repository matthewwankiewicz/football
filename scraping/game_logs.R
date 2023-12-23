library(nflfastR)
library(tidyverse)
library(plotly)

game_logs <- nflfastR::load_player_stats(seasons = 2022:2023)

game_logs <- game_logs %>% 
  mutate(fantasy_points_half_ppr = fantasy_points + 0.5*receptions,
         total_tds = rushing_tds + receiving_tds)

snap_counts <- nflreadr::load_snap_counts(seasons = 2022:2023)

snap_counts <- snap_counts %>% 
  filter(position %in% c("QB", "WR", "RB", "TE")) %>% 
  select(player, week, season, offense_pct)

game_logs <- game_logs %>% 
  left_join(snap_counts, 
            by = c("player_display_name" = "player", "week", "season"))

write_rds(game_logs, "football_plots/gamelogs.rds")


max_week <- ifelse(weekdays(Sys.Date()) %in% c("Friday", "Saturday",
                                               "Sunday", "Monday"), 
                   max(game_logs %>% 
                         filter(season == 2023) %>% 
                         pull(week)),
                   max(game_logs %>% 
                         filter(season == 2023) %>% 
                         pull(week))+1
                   )

nflseedR::load_sharpe_games() %>% 
  filter(season == 2023,
         week == max_week) %>% 
  select(away_team, home_team) %>% 
  mutate(game = paste(away_team, "@", home_team)) %>% 
  pull(game) -> games


write_rds(games, "football_plots/current_week.rds")


game_logs %>% 
  filter(position == "TE") %>% 
  group_by(player_display_name) %>% 
  summarise(target_share = mean(target_share, na.rm = T),
            avg_fppg = mean(fantasy_points_half_ppr, na.rm = T),
            receptions = mean(receptions, na.rm = T),
            snap_share = mean(offense_pct, na.rm = T),
            air_yards_share = mean(air_yards_share, na.rm = T)) %>% 
  select(-player_display_name) %>% 
  drop_na() -> te_data


scaled_data <- scale(te_data)  


num_clusters <- 10


kmeans_model <- kmeans(scaled_data, centers = num_clusters)

# Add cluster assignments to the original data frame
game_logs %>% 
  filter(position == "TE") %>% 
  group_by(player_display_name) %>% 
  summarise(target_share = mean(target_share, na.rm = T),
            avg_fppg = mean(fantasy_points_half_ppr, na.rm = T),
            receptions = mean(receptions, na.rm = T),
            snap_share = mean(offense_pct, na.rm = T),
            air_yards_share = mean(air_yards_share, na.rm = T)) %>%
  drop_na() %>% 
  mutate(cluster = kmeans_model$cluster) -> te_data

te_data %>% 
  ggplot(aes(target_share, avg_fppg, color = factor(cluster))) +
  geom_point() +
  geom_text(data = . %>% 
              filter(target_share > 0.2,
                     avg_fppg > 10),
            aes(label = player_display_name))



## clustering for rb
game_logs %>% 
  filter(position == "RB",
         targets > 0,
         season == 2023) %>% 
  group_by(player_display_name) %>% 
  summarise(target_share = mean(target_share, na.rm = T),
            avg_fppg = mean(fantasy_points_half_ppr, na.rm = T),
            var_fppg = var(fantasy_points_half_ppr, na.rm = T),
            receptions = mean(receptions, na.rm = T),
            rushing_yards = mean(rushing_yards, na.rm = T),
            receiving_yards = mean(receiving_yards, na.rm = T)) %>% 
  select(-player_display_name) %>% 
  drop_na(var_fppg) ->  rb_data


scaled_data <- scale(rb_data)  


num_clusters <- 10


kmeans_model <- kmeans(scaled_data, centers = num_clusters)

# Add cluster assignments to the original data frame
game_logs %>% 
  filter(position == "RB",
         targets > 0, season == 2023) %>% 
  group_by(player_display_name) %>% 
  summarise(target_share = mean(target_share, na.rm = T),
            avg_fppg = mean(fantasy_points_half_ppr, na.rm = T),
            var_fppg = var(fantasy_points_half_ppr, na.rm = T),
            receptions = mean(receptions, na.rm = T),
            rushing_yards = mean(rushing_yards),
            receiving_yards = mean(receiving_yards, na.rm = T)) %>%
  drop_na(var_fppg) %>% 
  mutate(cluster = kmeans_model$cluster) -> rb_data

rb_data %>% 
  ggplot(aes(target_share, avg_fppg, color = factor(cluster))) +
  geom_point() +
  geom_polygon(alpha = 0.2) +
  geom_text(aes(label = cluster), hjust = 1, size = 4)
  # geom_text(data = . %>% 
  #             filter(target_share > 0.2,
  #                    avg_fppg > 10),
  #           aes(label = player_display_name))



te_logs <- game_logs %>% filter(position == "TE") %>% 
  select(c("receiving_yards_after_catch", 
           "target_share", "receiving_yards",
           "receptions", "air_yards_share", "fantasy_points_half_ppr", 
           player_display_name)) %>% 
  group_by(player_display_name) %>% 
  summarise_all(.funs = list(mean = ~mean(.,na.rm = T))) %>% 
  select(-player_display_name)


correlation_matrix <- cor(te_logs)

# Visualize the correlation matrix using corrplot
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust")

# Alternatively, you can create a heatmap using ggplot2
ggplot(data = melt(correlation_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  labs(title = "Correlation Heatmap")
    



game_logs %>% 
  filter(position == "QB",
         offense_pct >= 0.5) %>% 
  group_by(opponent_team) %>% 
  summarise(min_yds = min(passing_yards),
            min_tds = min(passing_tds),
            min_com = min(completions),
            min_att = min(attempts))


game_logs %>% 
  filter(position == "WR",
         offense_pct >= 0.75) %>%
  group_by(opponent_team) %>% 
  summarise(min_yds = min(receiving_yards),
            min_tds = min(receiving_tds + rushing_tds),
            min_rec = min(receptions)) %>% view()


game_logs %>% 
  filter(opponent_team == "CHI",
         position == "QB") %>% view()



filter_leaders <- function(){
  rec_leaders <<- game_logs %>% 
    filter(season == 2023,
           position %in% c("WR", "TE")) %>% 
    group_by(recent_team, player_display_name, position) %>% 
    summarise(avg_share = mean(target_share),
              n = n()) %>%
    drop_na(avg_share) %>%
    filter(n >= 2) %>% 
    select(-n) %>% 
    group_by(position, recent_team) %>% 
    filter(avg_share == max(avg_share, na.rm = T))
  
  rush_leaders <<- game_logs %>% 
    filter(season == 2023,
           position == "RB") %>% 
    group_by(recent_team, player_display_name, position) %>% 
    summarise(avg_share = mean(carries),
              n = n()) %>%
    filter(n >= 2) %>% 
    select(-n) %>% 
    drop_na(avg_share) %>% 
    group_by(position, recent_team) %>% 
    filter(avg_share == max(avg_share, na.rm = T))
  
  game_logs %>% 
    filter(player_display_name %in% rec_leaders$player_display_name |
           player_display_name %in% rush_leaders$player_display_name,
           season == 2023) %>% 
    group_by(opponent_team, position) %>% 
    summarise(avg_rec_yds = mean(receiving_yards),
              avg_rush = mean(rushing_yards),
              avg_rec = mean(receptions),
              avg_car = mean(carries),
              avg_tds = mean(receiving_tds + rushing_tds),
              min_rec_yds = min(receiving_yards),
              min_rush = min(rushing_yards),
              min_rec = min(receptions),
              min_car = min(carries),
              min_tds = min(receiving_tds + rushing_tds))
}


target_leaders <- game_logs %>% 
  filter(season == 2023,
         position != "QB") %>% 
  group_by(recent_team, player_display_name, position) %>% 
  summarise(avg_share = mean(air_yards_share)) %>%
  drop_na(avg_share) %>% 
  group_by(position, recent_team) %>% 
  filter(avg_share == max(avg_share, na.rm = T))



game_logs %>% 
  filter(player_display_name %in% target_leaders$player_display_name) %>% 
  group_by(opponent_team, position) %>% 
  summarise(avg_yds = min(receiving_yards),
            avg_rec = min(receptions),
            avg_tds = min(receiving_tds + rushing_tds)) %>% 
  view()

game_logs %>% 
  filter(position == "RB", carries >= 10,
         opponent_team == "WAS") %>% 
  select(week, player_name, carries, rushing_yards, total_tds) %>% 
  view()




filter_leaders() -> stat_leaders

rec_leaders %>% view()




#### create a rolling plot for each team to see who is improving and who is not ####


library(dplyr)
library(ggplot2)

# Assuming your data frame is named 'fantasy_data'

# Step 1: Filter the data to only include tight ends ("TE").
tight_ends_data <- game_logs %>%
  filter(position == "TE", season == 2023, targets>2)

# Step 2: Group by week and opponent_team, then filter for the player with the most targets for each opponent per week.
top_targets_data <- tight_ends_data 

# Step 3: Calculate the rolling average of fantasy points scored using a 3-game rolling window.
rolling_avg_data <- top_targets_data %>%
  arrange(week) %>%
  group_by(week, opponent_team) %>%
  summarise(avg_points = mean(fantasy_points_half_ppr, na.rm = T)) %>% 
  mutate(rolling_avg = zoo::rollmean(avg_points, k = 5, fill = NA, align = "right")) %>%
  ungroup() %>% 
  select(opponent_team, week, rolling_avg)

# Step 4: Plot the rolling average of fantasy points scored by each team over weeks.
plotly::ggplotly(ggplot(rolling_avg_data, aes(x = week, y = rolling_avg, color = opponent_team)) +
  geom_line() +
  labs(x = "Week", y = "Rolling Average Points", title = "Rolling Average Points Scored by Team"))
