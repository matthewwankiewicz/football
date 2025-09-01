library(ffanalytics)
library(tidyverse)

## scrape
data <- ffanalytics::scrape_data(pos = c("QB", "WR", "TE", "RB"), week = 0)

## customize scoring
custom <- custom_scoring(pass_int = -2, fumbles_lost = -2, rec = 0.5)

## generate
projections <- projections_table(data)


## filter for robust
robust_projections <- projections %>% 
  filter(avg_type == "robust")


## add in ecr
robust_projections <- ffanalytics::add_ecr(robust_projections)

## add names
final <- ffanalytics::add_player_info(robust_projections)

final <- final %>% 
  mutate(player = paste(first_name, last_name)) %>% 
  data.table::data.table()

## edit dj moore
final[player == "D.J. Moore", player := "DJ Moore"]

## save
data.table::fwrite(final, "draft25/projections_r.csv")

## convert adp to csv
adp <- data.table::fread("draft25/adp.tsv")

data.table::setnames(adp, old = c("Player", "Position(s)"), 
                     new = c("player", "pos"))

## edit player name
adp <- adp %>% 
  janitor::clean_names() %>% 
  mutate(player_clean = sub("(.*), (.*)", "\\2 \\1", player),
         player = player_clean) %>% 
  select(-c("team_2", "team_pick", "player_clean")) 

data.table::fwrite(adp, "draft25/adp.csv")

## clean up boom sheet
boom <- data.table::fread("draft25/boom_sheet.csv")

boom <- boom %>% 
  janitor::row_to_names(7) %>% 
  janitor::clean_names()

## save
data.table::fwrite(boom, "draft25/boom_sheet.csv")
