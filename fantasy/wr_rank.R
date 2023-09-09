### wide receivers

##################################################### 2019 ##############################################

link <- "https://www.pro-football-reference.com/years/2019/rushing.htm"
webpage <- read_html(link)

table <- webpage %>% html_table()
rushtable <- table[[1]] %>% 
  janitor::row_to_names(1) %>% 
  select(-c(Rk, Tm, Age, Pos, G, GS, Fmb, Lng))

link <- "https://www.pro-football-reference.com/years/2019/receiving.htm"
webpage <- read_html(link)

table <- webpage %>% html_table()
rectable <- table[[1]] %>% 
  select(-c(Lng))


rush_rec_table <- left_join(rectable, rushtable,
                            by = "Player") %>% 
  filter(Player != "Player") %>% 
  rename("rec_yards" = Yds.x,
         "rec_TD" = TD.x,
         "rec_1D" = `1D.x`,
         "rec_y/g" = `Y/G.x`,
         "rush_yards" = Yds.y,
         "rush_TD" = TD.y,
         "rush_1D" = `1D.y`,
         "rush_y/g" = `Y/G.y`) %>% 
  mutate(`Ctch%` = parse_number(`Ctch%`))

table <- rush_rec_table %>% 
  filter(Pos == "wr" | Pos == "WR")
table <- table %>% 
  select(-c(Rk, Tm, Pos, G, GS, Age))

names <- table$Player
nnn <- c()
for(i in 1:length(names)){
  nnn <- c(nnn, format_name(names[i]))
}

nnn <- str_replace_all(nnn, "\\*", "")
nnn <- str_replace_all(nnn, "\\+", "")

table$Player <- nnn

# GET FANTASY POINTS ------------------------------------

link <- "https://www.fantasypros.com/nfl/reports/leaders/wr.php?year=2020"
page <- read_html(link)

fan_pts <- html_table(page)
fan_pts <- fan_pts[[1]]

names <- fan_pts$Player
nnn <- c()
for(i in 1:length(names)){
  nnn <- c(nnn, format_name2(names[i]))
}

fan_pts$Player <- nnn

link <- "https://www.cbssports.com/fantasy/football/stats/WR/2020/season/stats/ppr/"
webpage <- read_html(link)

table2 <- webpage %>% html_table()
table2 <- table2[[1]]
table2[1,1] <- "Player\n"

for(i in 1:ncol(table2)){
  table2[1, i] <- substring(table2[1, i], first = 1, last = (gregexpr("\n", table2[i, 1])[[1]][1] - 1))
  table2[1, i] <- stringi::stri_replace_all_charclass(table2[1, i], "\\p{WHITE_SPACE}", "")
}

colnames(table2) <- table2[1,]
table2 <- table2[-1,]

for(i in 1:nrow(table2)){
  table2[i, 1] <- substring(table2[i, 1], first = 1, last = (gregexpr("\n", table2[i, 1])[[1]][1] - 1))
}

table2[,2:ncol(table2)] <- lapply(table2[,2:ncol(table2)], as.numeric)

colnames(table2)[1] <- "player"
colnames(table2)[3] <- "pass_att"
colnames(table2)[6] <- "avg_pass_yds"
colnames(table2)[5] <- "pass_yds"
colnames(table2)[7] <- "pass_td"
colnames(table2)[10] <- "rush_att"
colnames(table2)[11] <- "rush_yds"

fan_pts <- left_join(fan_pts, table2 %>% 
                       select(player, fpts),
                     by = c("Player" = "player"))

# RE-FORMAT NAMES

wr2019 <- table %>% 
  filter(Player %in% fan_pts$Player)

wr.stats <- left_join(wr2019, fan_pts, by = "Player")

wr.stats <- wr.stats %>% 
  select(-c(Team, Rank))

final <- wr.stats %>% 
  mutate(fpts = (fpts + Points)/2)

##################################################### 2020 ##############################################

link <- "https://www.pro-football-reference.com/years/2020/rushing.htm"
webpage <- read_html(link)

table <- webpage %>% html_table()
rushtable <- table[[1]] %>% 
  janitor::row_to_names(1) %>% 
  select(-c(Rk, Tm, Age, Pos, G, GS, Fmb, Lng))

link <- "https://www.pro-football-reference.com/years/2020/receiving.htm"
webpage <- read_html(link)

table <- webpage %>% html_table()
rectable <- table[[1]] %>% 
  select(-c(Lng))


rush_rec_table <- left_join(rectable, rushtable,
                            by = "Player") %>% 
  filter(Player != "Player") %>% 
  rename("rec_yards" = Yds.x,
         "rec_TD" = TD.x,
         "rec_1D" = `1D.x`,
         "rec_y/g" = `Y/G.x`,
         "rush_yards" = Yds.y,
         "rush_TD" = TD.y,
         "rush_1D" = `1D.y`,
         "rush_y/g" = `Y/G.y`) %>% 
  mutate(`Ctch%` = parse_number(`Ctch%`))

table <- rush_rec_table %>% 
  filter(Pos == "wr" | Pos == "WR")
table <- table %>% 
  select(-c(Rk, Tm, Pos, G, GS, Age))

names <- table$Player
nnn <- c()
for(i in 1:length(names)){
  nnn <- c(nnn, format_name(names[i]))
}

nnn <- str_replace_all(nnn, "\\*", "")
nnn <- str_replace_all(nnn, "\\+", "")

table$Player <- nnn

# GET FANTASY POINTS ------------------------------------

link <- "https://www.fantasypros.com/nfl/reports/leaders/wr.php?year=2021"
page <- read_html(link)

fan_pts <- html_table(page)
fan_pts <- fan_pts[[1]]

names <- fan_pts$Player
nnn <- c()
for(i in 1:length(names)){
  nnn <- c(nnn, format_name2(names[i]))
}

fan_pts$Player <- nnn

link <- "https://www.cbssports.com/fantasy/football/stats/WR/2021/season/stats/ppr/"
webpage <- read_html(link)

table2 <- webpage %>% html_table()
table2 <- table2[[1]]
table2[1,1] <- "Player\n"

for(i in 1:ncol(table2)){
  table2[1, i] <- substring(table2[1, i], first = 1, last = (gregexpr("\n", table2[i, 1])[[1]][1] - 1))
  table2[1, i] <- stringi::stri_replace_all_charclass(table2[1, i], "\\p{WHITE_SPACE}", "")
}

colnames(table2) <- table2[1,]
table2 <- table2[-1,]

for(i in 1:nrow(table2)){
  table2[i, 1] <- substring(table2[i, 1], first = 1, last = (gregexpr("\n", table2[i, 1])[[1]][1] - 1))
}

table2[,2:ncol(table2)] <- lapply(table2[,2:ncol(table2)], as.numeric)

colnames(table2)[1] <- "player"
colnames(table2)[3] <- "pass_att"
colnames(table2)[6] <- "avg_pass_yds"
colnames(table2)[5] <- "pass_yds"
colnames(table2)[7] <- "pass_td"
colnames(table2)[10] <- "rush_att"
colnames(table2)[11] <- "rush_yds"

fan_pts <- left_join(fan_pts, table2 %>% 
                       select(player, fpts),
                     by = c("Player" = "player"))

# RE-FORMAT NAMES

wr2020 <- table %>% 
  filter(Player %in% fan_pts$Player)

wr.stats <- left_join(wr2020, fan_pts, by = "Player")

wr.stats <- wr.stats %>% 
  select(-c(Team, Rank)) %>% 
  mutate(fpts = (fpts + Points)/2)

final <- rbind(final, wr.stats)

#################################################

# Reformat -------------------------------------------------------------------------------------

final_table <- final

final_table[,2:ncol(final_table)] <- lapply(final_table[,2:ncol(final_table)], as.numeric)

# Get 2021 Data -------------------------------------------------------------------------------

link <- "https://www.pro-football-reference.com/years/2021/rushing.htm"
webpage <- read_html(link)

table <- webpage %>% html_table()
rushtable <- table[[1]] %>% 
  janitor::row_to_names(1) %>% 
  select(-c(Rk, Tm, Age, Pos, G, GS, Fmb, Lng))

link <- "https://www.pro-football-reference.com/years/2021/receiving.htm"
webpage <- read_html(link)

table <- webpage %>% html_table()
rectable <- table[[1]] %>% 
  select(-c(Lng))


rush_rec_table <- left_join(rectable, rushtable,
                            by = "Player") %>% 
  filter(Player != "Player") %>% 
  rename("rec_yards" = Yds.x,
         "rec_TD" = TD.x,
         "rec_1D" = `1D.x`,
         "rec_y/g" = `Y/G.x`,
         "rush_yards" = Yds.y,
         "rush_TD" = TD.y,
         "rush_1D" = `1D.y`,
         "rush_y/g" = `Y/G.y`) %>% 
  mutate(`Ctch%` = parse_number(`Ctch%`))

table <- rush_rec_table %>% 
  filter(Pos == "wr" | Pos == "WR")
table <- table %>% 
  select(-c(Rk, Tm, Pos, G, GS, Age))

names <- table$Player
names[14] <- "Amon-Ra StBrown"
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

link <- "https://www.cbssports.com/fantasy/football/stats/WR/2022/restofseason/projections/nonppr/?sortcol=misc_fpts&sortdir=descending"
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

proj[proj == "A. St. Brown"] = "A. StBrown"

## MODEL CREATION -----------------------------------------------------------------------------

mod <- lm(fpts ~ . - Att - rush_yards - rush_TD -
            rush_1D - `Y/A` - `rush_y/g` - Avg - Games - Player -
            Points - Fmb, 
          data = final_table %>% 
            drop_na())


mod2 <- lm(Points ~ rec_yards + rec_TD + `Y/Tgt` +
             `rec_y/g`,
           data = final_table)



modd <- step(mod)



table21$est <- predict(modd, newdata = table21)
table21$est2 <- predict(mod2, newdata = table21)

table_final <- left_join(table21, proj, by = "Player")

table_final$fpts <- as.numeric(table_final$fpts)

table_final %>% 
  ggplot(aes(est2, fpts, label = Player)) +
  geom_point() +
  geom_text(aes(label = Player)) +
  geom_smooth(method = "lm")

link <- "https://www.fantasypros.com/nfl/adp/wr.php"
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

table$player <- final

table[table == "A. St. Brown"] <- "A. StBrown"
table[table == "M. Pittman Jr."] <- "M. Pittman"
table[table == "M. Jones Jr."] <- "M. Jones"
table[table == "L. Shenault Jr."] <- "L. Shenault"
table[table == "C. Wilson Jr."] <- "C. Wilson"
table[table == "A. Robinson II"] <- "A. Robinson"

table_final <- table_final %>% 
  left_join(table, by = c("Player" = "player"))

write_csv(table_final %>% 
            select(Player, est, est2, fpts, "adp" = AVG), 
          "wr_rank.csv")
