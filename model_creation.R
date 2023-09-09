library(tidyverse)
library(rvest)
library(lme4)
library(nflseedR)

nfl_games <- nflseedR::load_sharpe_games()

nfl_games <- nfl_games %>% 
  filter(season >= 2020) %>% 
  select(season, game_type, weekday, home_team, away_team, home_score, away_score, spread_line, total_line, result, total,
         home_rest, away_rest, roof, div_game, surface, temp, wind)


scale2 <- function(x) (x - mean(x)) / sd(x)
### ---------------------------- download 2020 -------------------------------

schedule2020 <- nfl_games %>% 
  filter(season == 2020,
         game_type == "REG")

schedule2020 <- schedule2020 %>% 
  mutate(home_win = ifelse(home_score > away_score, 1, 0))

dvoa20 <- read_csv("dvoa2020.csv") %>% 
  janitor::clean_names()

dvoa20[dvoa20 == "LAR"] <- "LA"
dvoa20[dvoa20 == "JAC"] <- "JAX"

home_data <- left_join(schedule2020, dvoa20, by = c("home_team" = "team"))
full_data20 <- left_join(home_data, dvoa20, by = c("away_team" = "team"))%>% 
  mutate(spread_line = -1*spread_line)

### ----------------------------------- download 2021 --------------------------------------------

schedule2021 <- nfl_games %>% 
  filter(season == 2021,
         game_type == "REG")

schedule2021 <- schedule2021 %>% 
  mutate(home_win = ifelse(home_score > away_score, 1, 0))

dvoa21 <- read_csv("dvoa2021.csv") %>% 
  janitor::clean_names()

dvoa21[dvoa21 == "LAR"] <- "LA"
dvoa21[dvoa21 == "JAC"] <- "JAX"

home_data <- left_join(schedule2021, dvoa21, by = c("home_team" = "team"))
full_data21 <- left_join(home_data, dvoa21, by = c("away_team" = "team"))%>% 
  mutate(spread_line = -1*spread_line)


### get 2022 data ------------

### ----------------------------------- download 2022 --------------------------------------------

schedule2022 <- nfl_games %>% 
  filter(season == 2022)

schedule2022 <- schedule2022 %>% 
  mutate(home_win = ifelse(home_score > away_score, 1, 0))

dvoa22 <- read_csv("total_team_dvoa.csv") %>% 
  janitor::clean_names() %>% 
  select(team, total_dvoa_rank, total_dvoa, "offense_dvoa_rank" = offense_rank, 
         offense_dvoa, "defense_dvoa_rank" = defense_rank, defense_dvoa,
         "special_teams_dvoa_rank" = special_teams_rank, special_teams_dvoa)

dvoa22[dvoa22 == "LAR"] <- "LA"
dvoa22[dvoa22 == "JAC"] <- "JAX"

home_data <- left_join(schedule2022, dvoa22, by = c("home_team" = "team"))
full_data22 <- left_join(home_data, dvoa22, by = c("away_team" = "team")) %>% 
  mutate(spread_line = -1*spread_line)


## COMBINE THE DATASET --------------------------------------------------------


schedule <- rbind(rbind(full_data20, full_data21), full_data22) %>% 
  mutate(home_diff = home_score - away_score)

write_rds(schedule, "schedule.rds")


set.seed(21)
dt = sort(sample(nrow(schedule), nrow(schedule)*.7))
train<-schedule[dt,]
test<-schedule[-dt,]

train <- train %>% 
  mutate(fav = case_when(
    spread_line <= -6.5 ~ "large_fav",
    spread_line < 0 ~ "small_fav",
    spread_line <= 6.5 ~ "small_dog",
    spread_line > 6.5 ~ "big_dog"
  ),
  dvoa_adv = case_when(
    offense_dvoa_rank.x < offense_dvoa_rank.y & defense_dvoa_rank.x < defense_dvoa_rank.y ~ 2,
    offense_dvoa_rank.x > offense_dvoa_rank.y & defense_dvoa_rank.x < defense_dvoa_rank.y |
      offense_dvoa_rank.x < offense_dvoa_rank.y & defense_dvoa_rank.x > defense_dvoa_rank.y ~ 1,
    offense_dvoa_rank.x > offense_dvoa_rank.y & defense_dvoa_rank.x > defense_dvoa_rank.y ~ 0
  ))

test <- test %>% 
  mutate(fav = case_when(
    spread_line <= -6.5 ~ "large_fav",
    spread_line < 0 ~ "small_fav",
    spread_line <= 6.5 ~ "small_dog",
    spread_line > 6.5 ~ "big_dog"
  ),
  dvoa_adv = case_when(
    offense_dvoa_rank.x < offense_dvoa_rank.y & defense_dvoa_rank.x < defense_dvoa_rank.y ~ 2,
    offense_dvoa_rank.x > offense_dvoa_rank.y & defense_dvoa_rank.x < defense_dvoa_rank.y |
      offense_dvoa_rank.x < offense_dvoa_rank.y & defense_dvoa_rank.x > defense_dvoa_rank.y ~ 1,
    offense_dvoa_rank.x > offense_dvoa_rank.y & defense_dvoa_rank.x > defense_dvoa_rank.y ~ 0
  ))


full_data22 <- full_data22 %>% 
  mutate(fav = case_when(
    spread_line <= -6.5 ~ "large_fav",
    spread_line < 0 ~ "small_fav",
    spread_line <= 6.5 ~ "small_dog",
    spread_line > 6.5 ~ "big_dog"
  ),
  dvoa_adv = case_when(
    offense_dvoa_rank.x < offense_dvoa_rank.y & defense_dvoa_rank.x < defense_dvoa_rank.y ~ 2,
    offense_dvoa_rank.x > offense_dvoa_rank.y & defense_dvoa_rank.x < defense_dvoa_rank.y |
      offense_dvoa_rank.x < offense_dvoa_rank.y & defense_dvoa_rank.x > defense_dvoa_rank.y ~ 1,
    offense_dvoa_rank.x > offense_dvoa_rank.y & defense_dvoa_rank.x > defense_dvoa_rank.y ~ 0
  ))


# model1 <- lm(result ~ offense_dvoa_rank.x + offense_dvoa_rank.y + 
#                defense_dvoa_rank.x + defense_dvoa_rank.y,
#              data = train)
# 
# model.prob <- glm(home_win ~  offense_dvoa_rank.x + offense_dvoa_rank.y + 
#                     defense_dvoa_rank.x + defense_dvoa_rank.y, data = train,
#                   family = "binomial")

sjPlot::tab_model(model1)

full_data.t <-full_data22 %>% drop_na(spread_line) %>% 
  mutate(home_diff = home_score - away_score)

full_data.t$est <- predict(model1, newdata = full_data22 %>% drop_na(spread_line))
full_data.t$win_prob <- predict(model.prob, newdata = full_data.t, type = "response")

test$est <- predict(model1, newdata = test, allow.new.levels = T)
test$win_prob <- predict(model.prob, newdata = test, type = "response")

testing_full <- full_data.t %>% 
  mutate(est_diff = est,
         est_win = ifelse(win_prob > 0.5, 1, 0),
         diff = est - result,
         cover = ifelse(spread_line < 0, ifelse(result*-1 < spread_line, 1, 0),
                        ifelse(result >= -1*spread_line, 1, 0)),
         est_cover = ifelse(spread_line < 0, ifelse(est_diff*-1 < spread_line, 1, 0),
                            ifelse(est_diff >= -1*spread_line, 1, 0)),
         correct = ifelse(cover == est_cover, 1, 0),
         correct_win = ifelse(est_win == home_win, 1, 0))

full_data.t %>% 
  mutate(est_diff = est,
         est_win = ifelse(win_prob > 0.5, 1, 0),
         diff = est - result,
         cover = ifelse(spread_line < 0, ifelse(result*-1 < spread_line, 1, 0),
                        ifelse(result >= -1*spread_line, 1, 0)),
         est_cover = ifelse(spread_line < 0, ifelse(est_diff*-1 < spread_line, 1, 0),
                            ifelse(est_diff >= -1*spread_line, 1, 0)),
         correct = ifelse(cover == est_cover, 1, 0),
         correct_win = ifelse(est_win == home_win, 1, 0)) %>% 
  select(correct) %>% 
  pull() %>% 
  mean(na.rm = T)


# view(full_data.t %>% 
#        mutate(est_diff = est,
#               cover = ifelse(spread_line < 0, ifelse(result*-1 < spread_line, 1, 0),
#                              ifelse(result >= -1*spread_line, 1, 0)),
#               est_cover = ifelse(spread_line < 0, ifelse(est_diff*-1 < spread_line, 1, 0),
#                                  ifelse(est_diff >= -1*spread_line, 1, 0))) %>% 
#        mutate(correct = ifelse(cover == est_cover, 1, 0)) %>% 
#        select(home_team, away_team, spread_line, est_diff, win_prob, correct, dvoa_adv))


full_data.t %>% 
  mutate(est_diff = est,
         disagree = ifelse(sign(spread_line) == sign(est), spread_line - est, abs(spread_line) - abs(est)),
         cover = ifelse(spread_line < 0, ifelse(home_diff*-1 < spread_line, 1, 0),
                        ifelse(home_diff >= -1*spread_line, 1, 0)),
         est_cover = ifelse(spread_line < 0, ifelse(est_diff*-1 < spread_line, 1, 0),
                            ifelse(est_diff >= -1*spread_line, 1, 0)),
         dog = ifelse(spread_line > 0, 1, 0),
         correct = ifelse(cover == est_cover, 1, 0),
         est_win = ifelse(est_diff > 0, 1, 0),
         correct_win = ifelse(est_win == home_win, 1, 0),
         dis_class = case_when(
           disagree < -10 ~ "major dog",
           disagree < 0 ~ "slight dog",
           disagree == 0 ~ "agree",
           disagree < 4 ~ "slight fav",
           disagree >= 5 ~ "major fav"
         )) %>% 
  filter(is.na(dis_class))


test %>% 
  mutate(est_diff = est,
         cover = ifelse(spread_line < 0, ifelse(home_diff*-1 < spread_line, 1, 0),
                        ifelse(home_diff >= -1*spread_line, 1, 0)),
         est_cover = ifelse(spread_line < 0, ifelse(est_diff*-1 < spread_line, 1, 0),
                            ifelse(est_diff >= -1*spread_line, 1, 0)),
         dog = ifelse(spread_line > 0, 1, 0),
         correct = ifelse(cover == est_cover, 1, 0),
         est_win = ifelse(win_prob > 0.5, 1, 0),
         correct_win = ifelse(est_win == home_win, 1, 0),
         line_type = case_when(
           spread_line < -7 ~ "big_fav",
           spread_line < 0 ~ "fav",
           spread_line > 7 ~ "big_dog",
           spread_line > 0 ~ "dog"
         )) %>%
  group_by(dvoa_adv, est_cover) %>% 
  summarise(n(),
            acc = mean(correct, na.rm = T))

test <- test %>% 
  mutate(est_diff = est,
         cover = ifelse(spread_line < 0, ifelse(home_diff*-1 < spread_line, 1, 0),
                        ifelse(home_diff >= -1*spread_line, 1, 0)),
         est_cover = ifelse(spread_line < 0, ifelse(est_diff*-1 < spread_line, 1, 0),
                            ifelse(est_diff >= -1*spread_line, 1, 0)),
         dog = ifelse(spread_line > 0, 1, 0),
         correct = ifelse(cover == est_cover, 1, 0),
         est_win = ifelse(win_prob > 0.5, 1, 0),
         correct_win = ifelse(est_win == home_win, 1, 0))

test %>% 
  mutate(est_diff = est,
         cover = ifelse(spread_line < 0, ifelse(home_diff*-1 < spread_line, 1, 0),
                        ifelse(home_diff >= -1*spread_line, 1, 0)),
         est_cover = ifelse(spread_line < 0, ifelse(est_diff*-1 < spread_line, 1, 0),
                            ifelse(est_diff >= -1*spread_line, 1, 0)),
         dog = ifelse(spread_line > 0, 1, 0),
         correct = ifelse(cover == est_cover, 1, 0),
         est_win = ifelse(win_prob > 0.5, 1, 0),
         correct_win = ifelse(est_win == home_win, 1, 0)) %>% 
  select(correct) %>% 
  pull() %>% 
  mean(na.rm = T)



### MODEL CREATION --------------------

library(tidyverse)


# home_model <- lm(home_score ~ offense_dvoa_rank.x + offense_dvoa_rank.y + 
#                    defense_dvoa_rank.x + defense_dvoa_rank.y + special_teams_dvoa_rank.x +
#                    special_teams_dvoa_rank.y,
#                  data = train)
# 
# away_model <- lm(away_score ~ offense_dvoa_rank.x + offense_dvoa_rank.y + 
#                    defense_dvoa_rank.x + defense_dvoa_rank.y + special_teams_dvoa_rank.x +
#                    special_teams_dvoa_rank.y,
#                  data = train)
# 
# total_mod <- lm(total ~  home_rest + away_rest + spread_line + total_line +
#                   roof + offense_dvoa_rank.x + offense_dvoa_rank.y + 
#                   defense_dvoa_rank.x + defense_dvoa_rank.y, 
#                 data = train)
# 
# summary(total_mod)

# write_rds(home_model, "home_model.rds")
# write_rds(away_model, "away_model.rds")
# write_rds(total_mod, "total_mod.rds")
# write_rds(model1, "spread_mod.rds")
# write_rds(model.prob, "win_prob.rds")

home_model <- read_rds("home_model.rds")
away_model <- read_rds("away_model.rds")
total_mod <- read_rds("total_mod.rds")
model1 <- read_rds("spread_mod.rds")
model.prob <- read_rds("win_prob.rds")

### test models ---------------

test$home_score_est <- round(predict(home_model, newdata = test))
test$visitor_score_est <- round(predict(away_model, newdata = test))



test$est_total <- round(predict(total_mod, newdata = test))

test2 <- test %>% 
  select(home_team, away_team, home_score, away_score, total, total_line, win_prob, home_win,
         home_score_est, visitor_score_est, spread_line, result, est, est_total) %>% 
  mutate(total_est = home_score_est + visitor_score_est,
         diff_est = home_score_est - visitor_score_est,
         est_win = ifelse(win_prob > 0.5, 1, 0),
         est_win2 = ifelse(home_score_est > visitor_score_est, 1, 0),
         cover = ifelse(spread_line < 0, ifelse(result*-1 < spread_line, 1, 0),
                        ifelse(result >= -1*spread_line, 1, 0)),
         est_cover = ifelse(spread_line < 0, ifelse(diff_est*-1 < spread_line, 1, 0),
                            ifelse(diff_est >= -1*spread_line, 1, 0)),
         est_cover2 = ifelse(spread_line < 0, ifelse(est*-1 < spread_line, 1, 0),
                             ifelse(est >= -1*spread_line, 1, 0)),
         correct = ifelse(cover == est_cover, 1, 0),
         agree = ifelse(est_cover == est_cover2, 1, 0),
         correct2 = ifelse(cover == est_cover2, 1, 0),
         under = ifelse(total < total_line, 1, 0),
         est_under = ifelse(home_score_est + visitor_score_est < total_line, 1, 0),
         correct_under = ifelse(under == est_under, 1, 0),
         est_under2 = ifelse(est_total < total_line, 1, 0),
         correct_under2 = ifelse(under == est_under2, 1, 0),
         correct_win = ifelse(est_win == home_win, 1, 0),
         correct_win2 = ifelse(home_win == est_win2, 1, 0))

full_data.t <- full_data.t %>% 
  mutate_at(.vars = "roof", ~ replace_na(.,"closed"))

full_data.t$home_score_est <- round(predict(home_model, newdata = full_data.t))
full_data.t$visitor_score_est <- round(predict(away_model, newdata = full_data.t))

full_data.t$est_total <- round(predict(total_mod, newdata = full_data.t))


full_data.t <- full_data.t %>%
  mutate(total_est = home_score_est + visitor_score_est,
         diff_est = home_score_est - visitor_score_est,
         est_win = ifelse(win_prob > 0.5, 1, 0),
         est_win2 = ifelse(home_score_est > visitor_score_est, 1, 0),
         cover = ifelse(spread_line < 0, ifelse(result*-1 < spread_line, 1, 0),
                        ifelse(result >= -1*spread_line, 1, 0)),
         est_cover = ifelse(spread_line < 0, ifelse(diff_est*-1 < spread_line, 1, 0),
                            ifelse(diff_est >= -1*spread_line, 1, 0)),
         est_cover2 = ifelse(spread_line < 0, ifelse(est*-1 < spread_line, 1, 0),
                             ifelse(est >= -1*spread_line, 1, 0)),
         correct = ifelse(cover == est_cover, 1, 0),
         agree = ifelse(est_cover == est_cover2, 1, 0),
         correct2 = ifelse(cover == est_cover2, 1, 0),
         under = ifelse(total < total_line, 1, 0),
         est_under = ifelse(home_score_est + visitor_score_est < total_line, 1, 0),
         correct_under = ifelse(under == est_under, 1, 0),
         est_under2 = ifelse(est_total < total_line, 1, 0),
         correct_under2 = ifelse(under == est_under2, 1, 0),
         correct_win = ifelse(home_win == est_win, 1, 0),
         correct_win2 = ifelse(home_win == est_win2, 1, 0))

# view(full_data.t %>% 
#        select(home_team, away_team, home_score, away_score, total, total_line, win_prob, est_total,
#               home_score_est, visitor_score_est, spread_line, result, est, roof) %>% 
#        mutate(total_est = home_score_est + visitor_score_est,
#               diff_est = home_score_est - visitor_score_est,
#               est_win = ifelse(win_prob > 0.5, 1, 0),
#               est_win2 = ifelse(home_score_est > visitor_score_est, 1, 0),
#               cover = ifelse(spread_line < 0, ifelse(result*-1 < spread_line, 1, 0),
#                              ifelse(result >= -1*spread_line, 1, 0)),
#               est_cover = ifelse(spread_line < 0, ifelse(diff_est*-1 < spread_line, 1, 0),
#                                  ifelse(diff_est >= -1*spread_line, 1, 0)),
#               est_cover2 = ifelse(spread_line < 0, ifelse(est*-1 < spread_line, 1, 0),
#                                   ifelse(est >= -1*spread_line, 1, 0)),
#               correct = ifelse(cover == est_cover, 1, 0),
#               agree = ifelse(est_cover == est_cover2, 1, 0),
#               correct2 = ifelse(cover == est_cover2, 1, 0),
#               under = ifelse(total < total_line, 1, 0),
#               est_under = ifelse(home_score_est + visitor_score_est < total_line, 1, 0),
#               correct_under = ifelse(under == est_under, 1, 0),
#               est_under2 = ifelse(est_total < total_line, 1, 0),
#               correct_under2 = ifelse(under == est_under2, 1, 0),
#               correct_win = ifelse(home_win == est_win, 1, 0),
#               correct_win2 = ifelse(home_win == est_win2, 1, 0)))
# 
# view(full_data.t %>% 
#        select(home_team, away_team,
#               spread_line, total_line, est, est_total, win_prob, home_score_est, visitor_score_est,
#               est_under, est_under2) %>% 
#        mutate(win_prob = ifelse(win_prob >= 0.5,
#                                 win_prob*100/(1-(win_prob))*-1,
#                                 100/win_prob-100)))

szn_fullscore_ats.acc <- full_data.t %>% 
  pull(correct_win) %>% mean(na.rm = T)

szn_est_ats.acc <- full_data.t %>% 
  pull(correct_win2) %>% mean(na.rm = T)

szn_fullscore_ou.acc <- full_data.t %>% 
  pull(correct_under) %>% mean(na.rm = T)

szn_est_ou.acc <- full_data.t %>% 
  pull(correct_under2) %>% mean(na.rm = T)


test_fullscore_ats.acc <- test2 %>% 
  pull(correct_win) %>% mean(na.rm = T)

test_est_ats.acc <- test2 %>% 
  pull(correct_win2) %>% mean(na.rm = T)

test_fullscore_ou.acc <- test2 %>% 
  pull(correct_under) %>% mean(na.rm = T)

test_est_ou.acc <- test2 %>% 
  pull(correct_under2) %>% mean(na.rm = T)


szn <- c(szn_fullscore_ats.acc, szn_est_ats.acc, szn_fullscore_ou.acc, szn_est_ou.acc)

tst <- c(test_fullscore_ats.acc, test_est_ats.acc, test_fullscore_ou.acc, test_est_ou.acc)

axs <- data.frame(szn, tst)

rownames(axs) <- c("full score ATS accuracy", "model estimate ATS accuracy", "full score over/under accuracy",
                   "model estimate over/under accuracy")

axs



