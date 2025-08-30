### dfs script ----
library(tidyverse)


### need to:
## format draftkings file
## create dfs variable in gamelogs file, find player averages
## using dfs variable, find the opponent points allowed

### format draftkings file ----

read_csv('DKSalaries (7).csv') %>% 
  select(Instructions) %>% 
  .[-c(1:5),] -> nfl_lineups


separate(nfl_lineups, Instructions, into = paste0("Column", 1:10), sep = ",", extra = "merge") %>% 
  janitor::row_to_names(1) %>% 
  select(1:9) -> nfl_lineups


nfl_lineups %>% 
  mutate(Salary = as.numeric(Salary),
         AvgPointsPerGame = as.numeric(AvgPointsPerGame)) -> nfl_lineups


### create dfs variable for dfs points ----
game_logs %>% 
  filter(season == 2023) %>% 
  mutate(passing_300_yards = ifelse(passing_yards >= 300, 1, 0),
         rushing_100_yards = ifelse(rushing_yards >= 100, 1, 0),
         receiving_100_yards = ifelse(receiving_yards >= 100, 1, 0),
         fumbles_lost = sack_fumbles_lost + rushing_fumbles_lost + 
           receiving_fumbles_lost,
         daily_fan_pts = 0.04*passing_yards + 0.1*rushing_yards +
           0.1*receiving_yards + 3*passing_300_yards + 3*rushing_100_yards +
           3*receiving_100_yards + 4*passing_tds + 6*rushing_tds +
           6*receiving_tds + 6*special_teams_tds - 2*interceptions +
           receptions - 2*fumbles_lost + 2*receiving_2pt_conversions) -> nfl_dfs_data

#### group by opponent team and position to get positional strength ----
nfl_dfs_data %>% 
  filter((position == "QB" & attempts >= 15) | 
           (position %in% c("WR", "TE") & targets >= 5) |
           (position == "RB" & carries >= 10)) %>% 
  group_by(opponent_team, position) %>% 
  summarise(avg_pts_allowed = mean(daily_fan_pts)) -> nfl_opponent_points

nfl_dfs_data %>% 
  group_by(recent_team) %>% 
  summarise(avg_dfs_points = mean(daily_fan_pts)) %>% 
  mutate(position = "DST") %>% 
  rename("opponent_team" = "recent_team",
         "avg_pts_allowed" = "avg_dfs_points") -> dst_fan_pts

dst_fan_pts$avg_pts_allowed <- rev(dst_fan_pts$avg_pts_allowed)

rbind(nfl_opponent_points, dst_fan_pts) -> nfl_opponent_points

nfl_opponent_points %>% write_csv("nfl_opponent_stats.csv")


### filter for last month and get player averages
getOpponentTeam <- function(gameInfo, teamAbbrev) {
  # Split the Game Info string to get the part with team abbreviations
  teamsInfo <- unlist(strsplit(gameInfo, " "))[1]
  
  # Split the team info on '@' to get the individual team abbreviations
  teams <- unlist(strsplit(teamsInfo, "@"))
  
  # Identify the opponent team by excluding the team in TeamAbbrev
  opponentTeam <- teams[teams != teamAbbrev]
  
  return(opponentTeam)
}

# Example usage with your dataset
# Assuming your dataframe is named df
nfl_lineups$Opponent <- mapply(getOpponentTeam, nfl_lineups$`Game Info`, nfl_lineups$TeamAbbrev)


### create defense rankings ----
nfl_lineups %>% 
  left_join(nfl_opponent_points, 
            by = c("Position" = "position",
                   "Opponent" = "opponent_team")) -> nfl_lineups 


nfl_lineups %>% 
  write_csv("dk_lines.csv")



# Install and load the zoo package
library(zoo)

# Example data
data <- c(1, 2, 3, 4, 5, 6, 7, 20, 9, 10)

# Calculate rolling standard deviation with a window size of 3
roll_sd <- rollapply(dvoa2022$defense_dvoa, width = 3, FUN = sd, align = "right", fill = NA)

# Print the result
print(roll_sd)
