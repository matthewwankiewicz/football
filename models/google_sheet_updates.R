#install.packages("googlesheets4")
library(googlesheets4)
library(tidyverse)

#gs4_auth()

## function to simulate model estimates
simulate_spreads <- function(spread, estimate, sigma){
  n_sims = 10000 # set number of sims
  
  ## simulate and saved estimates
  sims <- rnorm(n_sims, mean = estimate, sd = sigma**2)
  
  ## return proportion of estimates > spread line
  return_value <- ifelse(spread < 0,
                         sum(sims >= -1*spread),
                         sum(sims > -1*spread))/n_sims
  
  
  return(round(return_value, 3))
}

if(T %in% grepl("Total Team DVOA.csv", list.files("~/Downloads/", pattern = ".csv"))){
  file.rename("~/Downloads/Total Team DVOA.csv", "~/Documents/football/models/total_team_dvoa.csv")
  file.remove("~/Downloads/Total Team DVOA.csv")
}


## read in models
home_model <- read_rds("models/home_model.rds")
away_model <- read_rds("models/away_model.rds")
total_mod <- read_rds("models/total_mod.rds")
model1 <- read_rds("models/spread_mod.rds")
model.prob <- read_rds("models/win_prob.rds")

## add in sheets link
sheet <- "https://docs.google.com/spreadsheets/d/1FkMX0BRIIG2lfxjzqWwmM8K-Z-_ngrKdrHHSVIVLKbU/edit#gid=0"

#### historical schedule -------------
### using schedule from models/model_creation.R

schedule <- read_rds("schedule.rds")

### add estimates for home score, away score, total, home_diff and win prob
schedule$home_score.pred <- predict(home_model, newdata = schedule)
schedule$away_score.pred <- predict(away_model, newdata = schedule)
#schedule$total_estimate <- predict(total_mod, newdata = schedule)
#schedule$difference_est <- predict(model1, newdata = schedule)
#schedule$difference_est_sd <- predict(model1, newdata = schedule, se.fit = T)$se.fit
schedule$win_prob <- predict(model.prob, newdata = schedule,
                             type = "response")

# schedule <- schedule %>%
#   rowwise() %>% 
#   mutate(cover_prob = simulate_spreads(spread_line, difference_est, difference_est_sd))

schedule_sheet1 <- schedule %>% 
  select(c(1:11, home_score.pred, away_score.pred, win_prob))

#sheet_write(schedule_sheet1, ss = sheet, sheet = 2)

## create train and test split

set.seed(21)
dt = sort(sample(nrow(schedule), nrow(schedule)*.7))
train<-schedule[dt,]
test<-schedule[-dt,]

#### current season ----------------

nflseedR::load_sharpe_games() %>% 
  filter(season == 2024) -> schedule2024

## load in current season dvoa
dvoa2024 <- read_csv("models/total_team_dvoa.csv") %>% 
  janitor::clean_names() %>% 
  select(team, "total_dvoa" = tot_dvoa, "total_dvoa_rank" = rank_3, "offense_dvoa_rank" = rank_8, 
         "offense_dvoa" = off_dvoa, "defense_dvoa_rank" = rank_10, "defense_dvoa" = def_dvoa,
         "special_teams_dvoa_rank" = rank_12, "special_teams_dvoa" = st_dvoa)

dvoa2024[dvoa2024 == "JAC"] <- "JAX"
dvoa2024[dvoa2024 == "LAR"] <- "LA"

write_csv(dvoa2024, "models/dvoa2024.csv")



schedule2024 %>% 
  left_join(dvoa2024, by = c("home_team" = "team")) %>% 
  left_join(dvoa2024, by = c("away_team" = "team")) -> schedule2024

schedule2024[schedule2024 == ""] <- "outdoors"

schedule2024$home_score_est <- predict(home_model, newdata = schedule2024)
schedule2024$visitor_score_est <- predict(away_model, newdata = schedule2024)
# schedule2023$est_total <- predict(total_mod, newdata = schedule2023)
# schedule2023$difference_est <- predict(model1, newdata = schedule2023)
# schedule2023$difference_est_sd <- predict(model1, newdata = schedule2023, se.fit = T)$se.fit
schedule2024$win_prob <- predict(model.prob, newdata = schedule2024,
                             type = "response")

schedule2024 -> full_data23

schedule2024 %>% 
  select(week, home_team, home_score_est, away_team, visitor_score_est,
         "home_score_actual" = home_score, "away_score_actual" = away_score, 
         spread_line, home_moneyline, away_moneyline, total_line, win_prob) %>% 
  mutate(spread_line = spread_line * -1,
         home_score_est = round(home_score_est, digits = 1),
         visitor_score_est = round(visitor_score_est, digits = 1),
         win_prob = round(win_prob, 3)) %>% 
  # rowwise() %>% 
  # mutate(cover_prob = simulate_spreads(spread = spread_line,
  #                                      estimate = difference_est,
  #                                      sigma = difference_est_sd)) %>%
  # select(-difference_est_sd) %>% 
  relocate(home_score_actual, away_score_actual, .after = last_col()) -> schedule2024
  
  
  # relocate(week, home_team, home_score_est, away_team, visitor_score_est, spread_line,
  #          home_moneyline, total_line)-> schedule2023

sheet_write(schedule2024, ss = sheet, sheet = 1)


#### accuracy ------------------
metrics <- read_sheet(ss = sheet, sheet = 3)




## record results for current season
schedule2024 <- schedule %>% 
  mutate(result = home_score_actual - away_score_actual,
         total = home_score_actual + away_score_actual,
         home_win = home_score_actual > away_score_actual,
         total_est = home_score_est + visitor_score_est,
         diff_est = home_score_est - visitor_score_est,
         est_win = ifelse(win_prob > 0.5, 1, 0),
         est_win2 = ifelse(home_score_est > visitor_score_est, 1, 0),
         cover = ifelse(spread_line < 0, ifelse(result*-1 < spread_line, 1, 0),
                        ifelse(result >= -1*spread_line, 1, 0)),
         est_cover = ifelse(spread_line < 0, ifelse(diff_est*-1 < spread_line, 1, 0),
                            ifelse(diff_est >= -1*spread_line, 1, 0)),correct = ifelse(cover == est_cover, 1, 0),
         under = ifelse(total < total_line, 1, 0),
         est_under = ifelse(home_score_est + visitor_score_est < total_line, 1, 0),
         correct_under = ifelse(under == est_under, 1, 0),
         correct_win = ifelse(est_win == home_win, 1, 0))
  

stats <- schedule %>% 
  mutate(home_score_actual = home_score,
         away_score_actual = away_score,
         home_score_est = home_score.pred,
         visitor_score_est = away_score.pred,
         result = home_score_actual - away_score_actual,
         total = home_score_actual + away_score_actual,
         home_win = home_score_actual > away_score_actual,
         total_est = home_score_est + visitor_score_est,
         diff_est = home_score_est - visitor_score_est,
         est_win = ifelse(win_prob > 0.5, 1, 0),
         est_win2 = ifelse(home_score_est > visitor_score_est, 1, 0),
         cover = ifelse(spread_line < 0, ifelse(result*-1 < spread_line, 1, 0),
                        ifelse(result >= -1*spread_line, 1, 0)),
         est_cover = ifelse(spread_line < 0, ifelse(diff_est*-1 < spread_line, 1, 0),
                            ifelse(diff_est >= -1*spread_line, 1, 0)),correct = ifelse(cover == est_cover, 1, 0),
         under = ifelse(total < total_line, 1, 0),
         est_under = ifelse(home_score_est + visitor_score_est < total_line, 1, 0),
         correct_under = ifelse(under == est_under, 1, 0),
         correct_win = ifelse(est_win2 == home_win, 1, 0))



stats %>% pull(correct_win) %>% mean(na.rm=T)



## moneyline
moneyline_accuracy <- schedule2024 %>% 
  pull(correct_win) %>% mean(na.rm = T)

## ats
test_fullscore_ats.acc <- schedule2024 %>% 
  pull(correct) %>% mean(na.rm = T)

## o/u
test_fullscore_ou.acc <- schedule2024 %>% 
  pull(correct_under) %>% mean(na.rm = T)

#### 2023 season -------
## moneyline
moneyline_accuracy.23_pres <- stats %>% filter(season==2023) %>%
  pull(correct_win) %>% mean(na.rm = T)

## ats
test_fullscore_ats.acc.23_pres <- stats %>% filter(season==2023) %>% 
  pull(correct) %>% mean(na.rm = T)

## o/u
test_fullscore_ou.acc_.23pres <- stats %>% filter(season==2023) %>% 
  pull(correct_under) %>% mean(na.rm = T)


szn <- c(moneyline_accuracy, 
         test_fullscore_ats.acc, 
         test_fullscore_ou.acc)

tst <- c(moneyline_accuracy.23_pres, 
         test_fullscore_ats.acc.23_pres, 
         test_fullscore_ou.acc_.23pres)

axs <- data.frame(szn, tst)

rownames(axs) <- c("Moneyline Accuracy", 
                   "ATS Accuracy",
                   "Total Score Accuracy")

colnames(axs) <- c("2024 Season", "2023 Season (285 games)")



##write sheet
sheet_write(axs %>% 
              rownames_to_column() %>% 
              rename(" " = rowname), ss = sheet, sheet = 3)

beepr::beep(4)


test2 %>% 
  group_by(week, season) %>% 
  summarise(ml_acc = mean(correct_win),
            ats_acc = mean(correct),
            ou_acc = mean(correct_under)) -> weekly_acs
  
weekly_acs %>% 
  ggplot(aes(week)) +
  geom_point(aes(y = ml_acc), colour = "green") +
  geom_line(aes(y = ats_acc), colour = "blue") +
  geom_line(aes(y = ou_acc), colour = "red") +
  geom_hline(yintercept = 0.55)



szn_fullscore_ml.acc <- full_data.t %>% 
  pull(correct_win) %>% mean(na.rm = T)

# szn_est_ml.acc <- full_data.t %>% 
#   pull(correct_win2) %>% mean(na.rm = T)

szn_fullscore_ats.acc <- full_data.t %>% 
  pull(correct) %>% mean(na.rm = T)

# szn_est_ats.acc <- full_data.t %>% 
#   pull(correct2) %>% mean(na.rm = T)

szn_fullscore_ou.acc <- full_data.t %>% 
  pull(correct_under) %>% mean(na.rm = T)




nflseedR::load_sharpe_games() -> all_games

all_games %>% 
  filter(season %in% c(2020:2022)) %>% 
  select(season, home_team, away_team, week) %>% 
  merge(test) -> test



ggplot(weekly_acs %>% filter(week <=18), aes(x = factor(week), y = ats_acc - 0.55, fill = ats_acc > 0.55)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "green")) +
  labs(title = "ats_acc vs. Week",
       x = "Week",
       y = "ats_acc") +
  theme_minimal() +
  scale_y_continuous(limits = c(-0.55, 0.45)) +
  facet_wrap(~season)


full_data.t %>% 
  group_by(away_team) %>% 
  summarise(wins = sum(est_win)) %>% 
  arrange((wins))


### find the accuracy of the against the spread model for each week in the 2023 season using a line graph

full_data.t %>% 
  filter(season == 2023) %>% 
  group_by(week) %>% 
  summarise(games = n(),
            ats_win = sum(correct, na.rm = T),
            ats_loss = games - ats_win) %>% view
  ggplot(aes(x = factor(week), y = ats_acc, group = 1)) +
  geom_line() +
  labs(title = "ATS Accuracy vs. Week",
       x = "Week",
       y = "ATS Accuracy") +
  geom_hline(yintercept = 0.55)
