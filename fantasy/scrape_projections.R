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
  mutate(player = paste(first_name, last_name))

## save
data.table::fwrite(final, "draft25/projections_r.csv")
