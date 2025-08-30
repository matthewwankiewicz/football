### create draft list for fantasy
library(googlesheets4)

x <- read_sheet("https://docs.google.com/spreadsheets/d/1g2XQSOLwIxve96XX11yejfjDaKmgUCtbOJm4e3lX_UE/edit?gid=0#gid=0")
boom_sheet <- x[-c(1:5),] %>% 
  janitor::row_to_names(1) %>%
  janitor::clean_names() %>% 
  select(player_name, "rank" = ppr_5_rk, team,
         o_line_total, "sos" = full_so_s_by_position,
         qb_wr_te_boom, team_depth_chart, boom_factor) %>% 
  mutate(rank = as.numeric(rank),
         qb_wr_te_boom = ifelse(as.character(qb_wr_te_boom) == "NULL", NA, 
                                as.character(qb_wr_te_boom)),
         boom_factor = ifelse(as.character(boom_factor) == "NULL", NA, 
                                as.character(boom_factor)),
         player_name = str_replace(player_name, " Jr.| Sr.| II| III", ""))


## read in nfc adp
nfc <- read_tsv("/Users/matthew/Downloads/ADP.tsv")

nfc <- nfc %>% 
  janitor::clean_names() %>% 
  separate(player, sep = ", ", into = c("last_name", "first_name")) %>% 
  mutate(player_name = paste(first_name, last_name, sep = " ")) %>% 
  select(rank, player_name, "team" = team_4, "pos" = position_s, adp)


matched_values <- fuzzyjoin::stringdist_left_join(nfc, boom_sheet, 
                                by = "player_name") %>%
  filter(player_name.x == player_name.y | is.na(player_name.y)) %>% 
  select(-c(rank.y, player_name.y, team.y)) %>% 
  rename("rank" = "rank.x",
         "player_name" = "player_name.x",
         "team" = "team.x")


## read in vols sheet
vols <- read_sheet("https://docs.google.com/spreadsheets/d/1YTnYDn1j7Go0HM_OsotTY9n8NKH8ueBTX-YuOnZUTpE/edit?gid=0#gid=0")

vols <- vols %>% 
  janitor::clean_names() %>% 
  separate(tm_bw, sep = "/", into = c("team", "bye")) %>% 
  select("player_name" = name, bye, val, vrk) %>% 
  mutate(vrk = as.character(vrk),
         player_name = str_replace(player_name, " Jr.| Sr.| II| III", ""))


## merge vols with merged table
final_list <- fuzzyjoin::stringdist_left_join(matched_values, vols,
                                by = "player_name") %>% 
  filter(player_name.x == player_name.y | is.na(player_name.y)) %>% 
  select(-c(player_name.y)) %>% 
  rename("player_name" = "player_name.x")


### add in projections
proj_table_halfppr_filter <- proj_table_halfppr %>% 
  filter(avg_type == "weighted") %>% 
  select("player_name" = name, pos, tier, pos_rank, points, floor_vor) %>% 
  mutate(player_name = str_replace(player_name, " Jr.| Sr.| II| III", ""),
         player_name = str_replace(player_name, "Marquise Brown", "Hollywood Brown"))


## merge with final list
final <- fuzzyjoin::stringdist_left_join(final_list,
                                proj_table_halfppr_filter,
                                by = "player_name") %>%
  filter(player_name.x == player_name.y | is.na(player_name.y)) %>% 
  select(-c(player_name.y, pos.y)) %>% 
  rename("player_name" = "player_name.x",
         "pos" = "pos.x")

## factor in juicebox rankings
juicebox <- read_sheet("https://docs.google.com/spreadsheets/d/1s_nCVLI0KNZyKUe7z55u1QqfkPeykSMgHOmazc56HCw/edit?gid=1821016264#gid=1821016264",
                       sheet = "NFL")

juicebox <- juicebox %>% 
  janitor::clean_names() %>% 
  select("player_name" = name, fantasy_pros, nfl, 
         "nfl_vs_adp" = nf_lv_adp, "nfl_vs_fp" = nf_lv_fp)


##merge again
final_2 <- fuzzyjoin::stringdist_left_join(final,
                                           juicebox,
                                           by = "player_name") %>% 
  filter(player_name.x == player_name.y | is.na(player_name.y)) %>% 
  select(-c(player_name.y)) %>% 
  rename("player_name" = "player_name.x")

## add sheet to google sheets
write_sheet(final_2, ss= "https://docs.google.com/spreadsheets/d/18Gz0iotLXf5YsiZbxCATFEu7ecT-L8LZnKPl7sIZsio/edit?gid=2000154093#gid=2000154093",
            sheet = "Draft List")


