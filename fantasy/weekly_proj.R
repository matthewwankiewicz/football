library(nflfastR)
library(tidyverse)
library(lme4)

stats <- load_player_stats(season=2024) %>% 
  mutate(
    fantasy_points = 
      # Passing
      (passing_yards / 25) + 
      (passing_tds * 4) + 
      (interceptions * -2) +
      (passing_2pt_conversions * 2) +
      
      # Rushing
      (rushing_yards / 10) + 
      (rushing_tds * 6) + 
      (rushing_2pt_conversions * 2) +
      
      # Receiving
      (receptions * 0.5) +
      (receiving_yards / 10) + 
      (receiving_tds * 6) + 
      (receiving_2pt_conversions * 2) +
      
      # Special Teams & Fumbles
      (special_teams_tds * 6) +
      ((rushing_fumbles_lost + receiving_fumbles_lost + sack_fumbles_lost) * -2),
    no_td_fantasy_points = 
      # Passing
      (passing_yards / 25) + 
      (passing_tds * 4) + 
      (interceptions * -2) +
      (passing_2pt_conversions * 2) +
      
      # Rushing
      (rushing_yards / 10) + 
      (rushing_2pt_conversions * 2) +
      
      # Receiving
      (receptions * 1) +
      (receiving_yards / 10) + 
      (receiving_2pt_conversions * 2) +
      
      # Special Teams & Fumbles
      ((rushing_fumbles_lost + receiving_fumbles_lost + sack_fumbles_lost) * -2)
    # fantasy_points = ifelse(position == 'TE', fantasy_points + receptions, fantasy_points),
  )






stats %>% 
  filter(player_display_name %in% c("Jonnu Smith", "David Montgomery", 'Tucker Kraft'), season == 2024) %>% 
  mutate(proj = fantasy_points) %>%
  ggplot(aes(week, proj, colour = player_display_name)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = c(1:15))



lm(no_td_fantasy_points ~ air_yards_share:target_share:receiving_yards_after_catch, 
   data = stats %>% filter(position %in% c("WR", "TE"))) %>% 
  summary()


stats %>% 
  filter(season == 2024) %>% 
  group_by(player_display_name) %>% 
  summarise(avg_pts = mean(fantasy_points, na.rm=T),
            total_pts = sum(fantasy_points, na.rm=T),
            median_pts = median(fantasy_points, na.rm=T)) %>% 
  filter(player_display_name %in% c("Ladd McConkey", "D.J. Moore", 
                                    "Amari Cooper", "Elijah Moore", "Demario Douglas",
                                    "Marvin Harrison Jr.", "Justin Jefferson", "Jerry Jeudy", 
                                    'Tucker Kraft', 'Jonnu Smith', 'Trey McBride', 'David Montgomery', 
                                    'Tony Pollard')) %>% 
  arrange(desc(median_pts))


### points model
model <- glmer(fantasy_points ~ (1|player_display_name) + (1|opponent_team:position), 
               data = stats %>%
                 filter(position %in% c("QB", "TE", "RB", "WR")) %>% 
                 mutate(fantasy_points = ifelse(fantasy_points < 0, 0, fantasy_points)), 
               family = poisson())

sjPlot::tab_model(model)

intercepts <- ranef(model)

x <- intercepts$player_display_name %>% 
  rownames_to_column(var = "player_name")

View(x)


intercepts$`opponent_team:position` %>% 
  rownames_to_column(var = "matchup") %>% View


test <- data.frame(player_display_name = c("Hunter Henry", "Brenton Strange"), 
                   opponent_team = c("BUF", "LV"), 
                   position = "TE")


test$est <- predict(model, newdata = test, type = "response")


test %>% arrange(desc(est))
