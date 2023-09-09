library(nflseedR)
library(data.table)


## load in schedules

games <- nflseedR::load_sharpe_games()
games <- data.table(games)

superbowls <- games[game_type == "SB"]


games[, winner := ifelse(home_score > away_score, home_team, away_team)]
games[, loser := ifelse(home_score > away_score, away_team, home_team)]

previous_winner <- lag(superbowls$winner, 1)
previous_loser <- lag(superbowls$loser)

games$previous_winner <- previous_winner
games$previous_loser <- previous_loser


superbowls[, winner := ifelse(home_score > away_score, home_team, away_team)]
superbowls[, loser := ifelse(home_score > away_score, away_team, home_team)]


games[previous_loser == winner]

sb_winners <- games[, winner]


games[order(season), by = home_team,
      .(previous_exit_home = last(lag(game_type)))]

games %>%
  arrange(season) %>%
  group_by(home_team) %>%
  mutate(previous_exit_home = lag(last, default = NA)) %>%
  group_by(winner, season) %>%
  summarise(previous_exit = last(previous_exit, na.rm = TRUE))


library(dplyr)

# Assuming your data frame is named 'games'

games %>%
  arrange(season) %>%
  group_by(home_team) %>%
  mutate(previous_exit = lag(game_type, default = NA)) %>%
  group_by(home_team, season) %>%
  summarise(previous_exit = last(previous_exit, na.rm = TRUE))



previous_season_exit <- list()
seasons <- unique(games$season)
seasons <- seasons[-c(1, 25)]

games %>% 
  filter(game_type == "REG") %>% 
  group_by(season, winner) %>% 
  summarise(n = n()) -> win_totals

# Create a data frame with NFL teams and abbreviations
nfl_teams <- data.frame(
  Abbreviation = c(
    "ATL", "CHI", "CLE", "GB", "IND", "JAX", "NO", "NYJ", "PHI", "SEA", "STL", "TB", "TEN",
    "WAS", "DEN", "BAL", "BUF", "CAR", "CIN", "DET", "KC", "MIA", "MIN", "NE", "NYG", "SF",
    "DAL", "OAK", "PIT", "SD", "ARI", "HOU", "LA", "LAC", "LV"
  ),
  Division = c(
    "NFC South", "NFC North", "AFC North", "NFC North", "AFC South", "AFC South", "NFC South", "AFC East",
    "NFC East", "NFC West", "NFC West", "NFC South", "AFC South", "NFC East", "AFC South", "AFC North",
    "AFC East", "NFC South", "AFC North", "NFC North", "AFC West", "AFC East", "NFC North",
    "AFC East", "NFC East", "NFC West", "NFC East", "AFC North", "NFC West", "NFC West", "NFC South",
    "AFC South", "NFC West", "AFC West", "AFC West"
  )
)


games %>% 
  left_join(nfl_teams, by = c("home_team" = "Abbreviation")) %>% 
  rename("home_division" = "Division") %>% 
  left_join(nfl_teams, by = c("away_team" = "Abbreviation")) %>% 
  rename("away_division" = "Division") %>% 
  mutate(divisional_game = ifelse(home_division == away_division, 1, 0)) %>% 
  filter(game_type == "REG", divisional_game == 1) %>% 
  group_by(season, winner) %>% 
  summarise(n = n()) -> divisional_win_totals


for(year in seasons){
  print(year)
  
  # Step 1: Filter Super Bowl games
  superbowl_games <- superbowls[game_type == "SB" & season == year, ]
  
  # Step 2: Identify the Super Bowl winner
  superbowl_winner <- superbowl_games$winner
  
  # Step 3: Filter games from the previous season for winning team
  previous_season_games <- games[season == (year - 1) & (home_team == superbowl_winner | away_team == superbowl_winner), ]
  
  # Step 4: Find the Super Bowl winner's last game from the previous season
  superbowl_winner_last_game <- previous_season_games[nrow(previous_season_games), game_type]
  
  last_season_win_total <- win_totals %>% 
    filter(season == year & winner == superbowl_winner) %>% pull()
  
  last_season_division_wins <- divisional_win_totals %>% 
    filter(season == year & winner == superbowl_winner) %>% pull()
  
  last_season_division_wins <- ifelse(missing(last_season_division_wins), 0, last_season_division_wins)
  
  print(paste(year, superbowl_winner, last_season_win_total, last_season_division_wins, superbowl_winner_last_game))
  
  previous_season_exit[[year]] <- data.frame("season" = year, 
                                            "winner" = c(superbowl_winner),
                                            "win_total" = last_season_win_total,
                                            "division_win_total" = last_season_division_wins,
                                            "winner_last_game" = c(superbowl_winner_last_game))
}



winner_finishes <- do.call(rbind, previous_season_exit)

winner_finishes[is.na(winner_finishes)] <- 0


winner_finishes %>% 
  pull(division_win_total) %>% table()


divisional_win_totals %>% 
  filter(season == 2022) %>% arrange(desc(n)) %>% 
  print(n = 32)


  



