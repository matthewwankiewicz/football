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

if(T %in% grepl("total_team_dvoa.csv", list.files("~/Downloads/", pattern = ".csv"))){
  file.rename("~/Downloads/total_team_dvoa.csv", "~/Documents/football/models/total_team_dvoa.csv")
  file.remove("~/Downloads/total_team_dvoa.csv")
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
### using schedule from model_creation.R

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

#sheet_write(schedule_sheet1, ss = sheet, sheet = 2))

## create train and test split

set.seed(21)
dt = sort(sample(nrow(schedule), nrow(schedule)*.7))
train<-schedule[dt,]
test<-schedule[-dt,]

#### current season ----------------

nflseedR::load_sharpe_games() %>% 
  filter(season == 2023) -> schedule2023

## load in current season dvoa
dvoa2023 <- read_csv("models/total_team_dvoa.csv") %>% 
  janitor::clean_names() %>% 
  select(team, total_dvoa_rank, total_dvoa, "offense_dvoa_rank" = offense_rank, 
         offense_dvoa, "defense_dvoa_rank" = defense_rank, defense_dvoa,
         "special_teams_dvoa_rank" = special_teams_rank, special_teams_dvoa)

dvoa2023[dvoa2023 == "JAC"] <- "JAX"

write_csv(dvoa2023, "models/dvoa2023.csv")



schedule2023 %>% 
  left_join(dvoa2023, by = c("home_team" = "team")) %>% 
  left_join(dvoa2023, by = c("away_team" = "team")) -> schedule2023

schedule2023[schedule2023 == ""] <- "outdoors"

schedule2023$home_score_est <- predict(home_model, newdata = schedule2023)
schedule2023$visitor_score_est <- predict(away_model, newdata = schedule2023)
# schedule2023$est_total <- predict(total_mod, newdata = schedule2023)
# schedule2023$difference_est <- predict(model1, newdata = schedule2023)
# schedule2023$difference_est_sd <- predict(model1, newdata = schedule2023, se.fit = T)$se.fit
schedule2023$win_prob <- predict(model.prob, newdata = schedule2023,
                             type = "response")

schedule2023 -> full_data23

schedule2023 %>% 
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
  relocate(home_score_actual, away_score_actual, .after = last_col()) -> schedule2023
  
  
  # relocate(week, home_team, home_score_est, away_team, visitor_score_est, spread_line,
  #          home_moneyline, total_line)-> schedule2023

sheet_write(schedule2023, ss = sheet, sheet = 1)


#### accuracy ------------------
full_data.t <-full_data23 %>% drop_na(spread_line) %>% 
  mutate(home_diff = home_score - away_score,
         home_win = ifelse(home_score > away_score, 1, 0))
#full_data.t$est <- predict(model1, newdata = full_data.t %>% drop_na(spread_line))
full_data.t$win_prob <- predict(model.prob, newdata = full_data.t, type = "response")


full_data.t <- full_data.t %>%
  mutate(spread_line = spread_line*-1,
         total_est = home_score_est + visitor_score_est,
         diff_est = home_score_est - visitor_score_est,
         est_win = ifelse(win_prob > 0.5, 1, 0),
         est_win2 = ifelse(home_score_est > visitor_score_est, 1, 0),
         cover = ifelse(spread_line < 0, ifelse(result*-1 < spread_line, 1, 0),
                        ifelse(result >= -1*spread_line, 1, 0)),
         est_cover = ifelse(spread_line < 0, ifelse(diff_est*-1 < spread_line, 1, 0),
                            ifelse(diff_est >= -1*spread_line, 1, 0)),
         # est_cover2 = ifelse(spread_line < 0, ifelse(est*-1 < spread_line, 1, 0),
         #                     ifelse(est >= -1*spread_line, 1, 0)),
         correct = ifelse(cover == est_cover, 1, 0),
         #agree = ifelse(est_cover == est_cover2, 1, 0),
         # correct2 = ifelse(cover == est_cover2, 1, 0),
         under = ifelse(total < total_line, 1, 0),
         est_under = ifelse(home_score_est + visitor_score_est < total_line, 1, 0),
         correct_under = ifelse(under == est_under, 1, 0),
         # est_under2 = ifelse(est_total < total_line, 1, 0),
         # correct_under2 = ifelse(under == est_under2, 1, 0),
         correct_win = ifelse(home_win == est_win, 1, 0))
         # correct_win2 = ifelse(home_win == est_win2, 1, 0))


# 
# view(full_data.t %>% 
#        select(home_team, away_team,
#               spread_line, total_line, est, est_total, win_prob, home_score_est, visitor_score_est,
#               est_under, est_under2) %>% 
#        mutate(win_prob = ifelse(win_prob >= 0.5,
#                                 win_prob*100/(1-(win_prob))*-1,
#                                 100/win_prob-100)))

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

# szn_est_ou.acc <- full_data.t %>% 
#   pull(correct_under2) %>% mean(na.rm = T)


### add data for test
test$home_score_est <- round(predict(home_model, newdata = test))
test$visitor_score_est <- round(predict(away_model, newdata = test))
#test$est <- predict(model1, newdata = test, allow.new.levels = T)
test$win_prob <- predict(model.prob, newdata = test, type = "response")


#test$est_total <- round(predict(total_mod, newdata = test))

test2 <- test %>% 
  select(home_team, away_team, home_score, away_score, total, total_line, win_prob, home_win,
         home_score_est, visitor_score_est, spread_line, result) %>% 
  mutate(total_est = home_score_est + visitor_score_est,
         diff_est = home_score_est - visitor_score_est,
         est_win = ifelse(win_prob > 0.5, 1, 0),
         est_win2 = ifelse(home_score_est > visitor_score_est, 1, 0),
         cover = ifelse(spread_line < 0, ifelse(result*-1 < spread_line, 1, 0),
                        ifelse(result >= -1*spread_line, 1, 0)),
         est_cover = ifelse(spread_line < 0, ifelse(diff_est*-1 < spread_line, 1, 0),
                            ifelse(diff_est >= -1*spread_line, 1, 0)),
         # est_cover2 = ifelse(spread_line < 0, ifelse(est*-1 < spread_line, 1, 0),
         #                     ifelse(est >= -1*spread_line, 1, 0)),
         correct = ifelse(cover == est_cover, 1, 0),
         #agree = ifelse(est_cover == est_cover2, 1, 0),
         #correct2 = ifelse(cover == est_cover2, 1, 0),
         under = ifelse(total < total_line, 1, 0),
         est_under = ifelse(home_score_est + visitor_score_est < total_line, 1, 0),
         correct_under = ifelse(under == est_under, 1, 0),
         #est_under2 = ifelse(est_total < total_line, 1, 0),
         #correct_under2 = ifelse(under == est_under2, 1, 0),
         correct_win = ifelse(est_win == home_win, 1, 0))
         #correct_win2 = ifelse(home_win == est_win2, 1, 0))



test_fullscore_ml.acc <- test2 %>% 
  pull(correct_win) %>% mean(na.rm = T)

# test_est_ml.acc <- test2 %>% 
#   pull(correct_win2) %>% mean(na.rm = T)

test_fullscore_ats.acc <- test2 %>% 
  pull(correct) %>% mean(na.rm = T)

# test_est_ats.acc <- test2 %>% 
#   pull(correct2) %>% mean(na.rm = T)

test_fullscore_ou.acc <- test2 %>% 
  pull(correct_under) %>% mean(na.rm = T)

# test_est_ou.acc <- test2 %>% 
#   pull(correct_under2) %>% mean(na.rm = T)


szn <- c(szn_fullscore_ml.acc, 
         #szn_est_ml.acc, 
         szn_fullscore_ats.acc, 
         #szn_est_ats.acc, 
         szn_fullscore_ou.acc)
         #szn_est_ou.acc)

tst <- c(test_fullscore_ml.acc, 
         #test_est_ml.acc,
         test_fullscore_ats.acc, 
         #test_est_ats.acc, 
         test_fullscore_ou.acc)
         #test_est_ou.acc)

axs <- data.frame(szn, tst)

rownames(axs) <- c(#"Win Prob Model Moneyline Accuracy",
                   "Moneyline Accuracy", 
                   #"Score Difference ATS Accuracy",
                   "ATS Accuracy",
                   #"Total Score Model Accuracy", 
                   "Total Score Accuracy")

colnames(axs) <- c("2023 Season", "Testing Data (Sample of 244 games)")



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
