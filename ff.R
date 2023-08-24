library(ffanalytics)
library(tidyverse)
library(leaps)

### HELPER FUNCTIONS -------------------------------

format_name <- function(name){
  final_name <- paste(substr(name, start = 1, stop = 1), ".", 
                      substr(name, start = gregexpr(" ", name)[[1]][1], stop = str_length(name)), 
                      sep = "")
}


format_name2 <- function(name){
  if(str_count(name, " ") == 2){
    trimws(paste(substr(name, start = 1, stop = 1), ".", 
                 substr(name, start = gregexpr(" ", name)[[1]][1], stop = gregexpr(" ", name)[[1]][2]), 
                 sep = ""))
  }
  else{
    paste(substr(name, start = 1, stop = 1), ".", 
          substr(name, start = gregexpr(" ", name)[[1]][1], stop = str_length(name)), 
          sep = "")
  }
}

format_name3 <- function(name){
  x <- gregexpr(pattern =sub('.*(\\b[A-Z]+\\b).*','\\1', name), name)
  substr(name, 1, (x[[1]]-2))
}


##################################################### 2017 ##############################################

link <- "https://www.pro-football-reference.com/years/2017/passing.htm"
webpage <- read_html(link)

table <- webpage %>% html_table()
table <- table[[1]]
colnames(table)[26] <- "sack_yards"
table <- table %>% 
  filter(Pos == "QB" | Pos == "qb")
table <- table %>% 
  select(-c(Rk, Tm, Pos, G, GS, QBrec, `4QC`, GWD))

names <- table$Player
nnn <- c()
for(i in 1:length(names)){
  nnn <- c(nnn, format_name(names[i]))
}

nnn <- str_replace_all(nnn, "\\*", "")
nnn <- str_replace_all(nnn, "\\+", "")

table$Player <- nnn

# GET FANTASY POINTS ------------------------------------

link <- "https://www.fantasypros.com/nfl/reports/leaders/qb.php?year=2018"
page <- read_html(link)

fan_pts <- html_table(page)
fan_pts <- fan_pts[[1]]

names <- fan_pts$Player
nnn <- c()
for(i in 1:length(names)){
  nnn <- c(nnn, format_name2(names[i]))
}

fan_pts$Player <- nnn

# RE-FORMAT NAMES

qb2017 <- table %>% 
  filter(Player %in% fan_pts$Player)

qb.stats <- left_join(qb2017, fan_pts, by = "Player")

final <- qb.stats %>% 
  select(-c(Team, Rank))

##################################################### 2018 ##############################################

link <- "https://www.pro-football-reference.com/years/2018/passing.htm"
webpage <- read_html(link)

table <- webpage %>% html_table()
table <- table[[1]]
colnames(table)[26] <- "sack_yards"
table <- table %>% 
  filter(Pos == "QB" | Pos == "qb")
table <- table %>% 
  select(-c(Rk, Tm, Pos, G, GS, QBrec, `4QC`, GWD))

names <- table$Player
nnn <- c()
for(i in 1:length(names)){
  nnn <- c(nnn, format_name(names[i]))
}

nnn <- str_replace_all(nnn, "\\*", "")
nnn <- str_replace_all(nnn, "\\+", "")

table$Player <- nnn

# GET FANTASY POINTS ------------------------------------

link <- "https://www.fantasypros.com/nfl/reports/leaders/qb.php?year=2019"
page <- read_html(link)

fan_pts <- html_table(page)
fan_pts <- fan_pts[[1]]

names <- fan_pts$Player
nnn <- c()
for(i in 1:length(names)){
  nnn <- c(nnn, format_name2(names[i]))
}

fan_pts$Player <- nnn

# RE-FORMAT NAMES

qb2018 <- table %>% 
  filter(Player %in% fan_pts$Player)

qb.stats <- left_join(qb2018, fan_pts, by = "Player")

qb.stats <- qb.stats %>% 
  select(-c(Team, Rank))

final <- rbind(final, qb.stats)

###################################################################################

## SCRAPE 2019 --------------

link <- "https://www.cbssports.com/fantasy/football/stats/QB/2018/season/stats/nonppr/"
webpage <- read_html(link)

table <- webpage %>% html_table()
table <- table[[1]]
table[1,1] <- "Player\n"

for(i in 1:ncol(table)){
  table[1, i] <- substring(table[1, i], first = 1, last = (gregexpr("\n", table[i, 1])[[1]][1] - 1))
  table[1, i] <- stringi::stri_replace_all_charclass(table[1, i], "\\p{WHITE_SPACE}", "")
}

colnames(table) <- table[1,]
table <- table[-1,]

for(i in 1:nrow(table)){
  table[i, 1] <- substring(table[i, 1], first = 1, last = (gregexpr("\n", table[i, 1])[[1]][1] - 1))
}

table[,2:ncol(table)] <- lapply(table[,2:ncol(table)], as.numeric)

colnames(table)[1] <- "player"
colnames(table)[3] <- "pass_att"
colnames(table)[6] <- "avg_pass_yds"
colnames(table)[5] <- "pass_yds"
colnames(table)[7] <- "pass_td"
colnames(table)[10] <- "rush_att"
colnames(table)[11] <- "rush_yds"

### ADP 2019 ----------

link <- "https://www.fantasypros.com/nfl/adp/qb.php?year=2019"
webpage <- read_html(link)

adp <- webpage %>% 
  html_table()

adp <- adp[[1]]

names <- adp$`Player Team (Bye)`
nnn <- c()
for(i in 1:length(names)){
  nnn <- c(nnn, format_name2(names[i]))
}

adp$`Player Team (Bye)` <- nnn
colnames(adp)[3] <- "player"
colnames(adp)[2] <- "adp"

adp <- adp %>% 
  select(player, adp)

qb2018 <- table %>% 
  filter(player %in% adp$player)

qb.stats <- left_join(qb2018, adp, by = "player")
qb.stats <- na.omit(qb.stats)

final <- rbind(final, qb.stats)

##################################################### 2019 ##############################################

link <- "https://www.pro-football-reference.com/years/2019/passing.htm"
webpage <- read_html(link)

table <- webpage %>% html_table()
table <- table[[1]]
colnames(table)[26] <- "sack_yards"
table <- table %>% 
  filter(Pos == "QB" | Pos == "qb")
table <- table %>% 
  select(-c(Rk, Tm, Pos, G, GS, QBrec, `4QC`, GWD))

names <- table$Player
nnn <- c()
for(i in 1:length(names)){
  nnn <- c(nnn, format_name(names[i]))
}

nnn <- str_replace_all(nnn, "\\*", "")
nnn <- str_replace_all(nnn, "\\+", "")

table$Player <- nnn

# GET FANTASY POINTS ------------------------------------

link <- "https://www.fantasypros.com/nfl/reports/leaders/qb.php?year=2020"
page <- read_html(link)

fan_pts <- html_table(page)
fan_pts <- fan_pts[[1]]

names <- fan_pts$Player
nnn <- c()
for(i in 1:length(names)){
  nnn <- c(nnn, format_name2(names[i]))
}

fan_pts$Player <- nnn

# RE-FORMAT NAMES

qb2019 <- table %>% 
  filter(Player %in% fan_pts$Player)

qb.stats <- left_join(qb2019, fan_pts, by = "Player")

qb.stats <- qb.stats %>% 
  select(-c(Team, Rank))

final <- rbind(final, qb.stats)

##################################################### 2020 ##############################################

link <- "https://www.pro-football-reference.com/years/2020/passing.htm"
webpage <- read_html(link)

table <- webpage %>% html_table()
table <- table[[1]]
colnames(table)[26] <- "sack_yards"
table <- table %>% 
  filter(Pos == "QB" | Pos == "qb")
table <- table %>% 
  select(-c(Rk, Tm, Pos, G, GS, QBrec, `4QC`, GWD))

names <- table$Player
nnn <- c()
for(i in 1:length(names)){
  nnn <- c(nnn, format_name(names[i]))
}

nnn <- str_replace_all(nnn, "\\*", "")
nnn <- str_replace_all(nnn, "\\+", "")

table$Player <- nnn

# GET FANTASY POINTS ------------------------------------

link <- "https://www.fantasypros.com/nfl/reports/leaders/qb.php?year=2021"
page <- read_html(link)

fan_pts <- html_table(page)
fan_pts <- fan_pts[[1]]

names <- fan_pts$Player
nnn <- c()
for(i in 1:length(names)){
  nnn <- c(nnn, format_name2(names[i]))
}

fan_pts$Player <- nnn

# RE-FORMAT NAMES

qb2020 <- table %>% 
  filter(Player %in% fan_pts$Player)

qb.stats <- left_join(qb2020, fan_pts, by = "Player")

qb.stats <- qb.stats %>% 
  select(-c(Team, Rank))

final <- rbind(final, qb.stats)

#################################################

# Reformat -------------------------------------------------------------------------------------

final_table <- final

final_table[,2:ncol(final_table)] <- lapply(final_table[,2:ncol(final_table)], as.numeric)

# Get 2021 Data -------------------------------------------------------------------------------

link <- "https://www.pro-football-reference.com/years/2021/passing.htm"
webpage <- read_html(link)

table <- webpage %>% html_table()
table <- table[[1]]
colnames(table)[26] <- "sack_yards"
table <- table %>% 
  filter(Pos == "QB" | Pos == "qb")
table <- table %>% 
  select(-c(Rk, Tm, Pos, G, GS, QBrec, `4QC`, GWD))

names <- table$Player
nnn <- c()
for(i in 1:length(names)){
  nnn <- c(nnn, format_name(names[i]))
}

nnn <- str_replace_all(nnn, "\\*", "")
nnn <- str_replace_all(nnn, "\\+", "")

table$Player <- nnn

table[,2:ncol(table)] <- lapply(table[,2:ncol(table)], as.numeric)


names <- table$Player
nnn <- c()
for(i in 1:length(names)){
  nnn <- c(nnn, format_name2(names[i]))
}

table$Player <- nnn

table21 <- table

## GET PREDICTED POINTS -----------------------------------------------------------------------------

link <- "https://www.cbssports.com/fantasy/football/stats/QB/2022/restofseason/projections/nonppr/?sortcol=misc_fpts&sortdir=descending"
page <- read_html(link)

table <- html_table(page)
table <- table[[1]]

table[1,1] <- "Player\n"

for(i in 1:ncol(table)){
  table[1, i] <- substring(table[1, i], first = 1, last = (gregexpr("\n", table[i, 1])[[1]][1] - 1))
  table[1, i] <- stringi::stri_replace_all_charclass(table[1, i], "\\p{WHITE_SPACE}", "")
}

colnames(table) <- table[1,]
table <- table[-1,]

for(i in 1:nrow(table)){
  table[i, 1] <- substring(table[i, 1], first = 1, last = (gregexpr("\n", table[i, 1])[[1]][1] - 1))
}

proj <- table %>% 
  select(Player, fpts)

## MODEL CREATION -----------------------------------------------------------------------------

mod <- lm(Points ~ . - Avg - Games - Player, 
          data = final_table %>% 
            drop_na())

Best_Subset <-
  regsubsets(Points ~ . - Avg - Games - Player,
             data = final_table,
             nbest = 1,      # 1 best model for each number of predictors
             nvmax = NULL,    # NULL for no limit on number of variables
             force.in = NULL, force.out = NULL,
             method = "exhaustive")

sum_bs <- summary(Best_Subset)
as.data.frame(sum_bs$outmat)

sum_bs$which[12,]


mod2 <- lm(Points ~ Yds + TD + `TD%` + Int + `AY/A` + `Y/C` +
             `Y/G` + QBR + sack_yards + `ANY/A` + `Sk%`,
           data = final_table)

summary(mod2)



modd <- step(mod)



table21$est <- predict(modd, newdata = table21)
table21$est2 <- predict(mod2, newdata = table21)
table21 <- table21 %>% 
  filter(Att > 200)

table_final <- left_join(table21, proj, by = "Player")

table_final$fpts <- as.numeric(table_final$fpts)

table_final %>% 
  ggplot(aes(est, fpts, label = Player)) +
  geom_point() +
  geom_text(aes(label = Player)) +
  geom_smooth(method = "lm")

link <- "https://www.fantasypros.com/nfl/adp/qb.php"
webpage <- read_html(link)

table <- webpage %>% html_table()
table <- table[[1]]
table <- table %>% 
  select("player" = `Player Team (Bye)`, AVG)

names <- table$player
nnn <- c()
for(i in 1:length(names)){
  nnn <- c(nnn, format_name2(names[i]))
}

final <- c()
for(i in 1:length(nnn)){
  final <- c(final, format_name3(nnn[i]))
}

final[final == "P. Mahomes II"] <- "P. Mahomes"

table$player <- final


table_final <- table_final %>% 
  left_join(table, by = c("Player" = "player"))


write_csv(table_final %>% 
  select(Player, est, est2, fpts, "adp" = AVG), 
  "qb_rank.csv")


