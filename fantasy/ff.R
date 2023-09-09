library(ffanalytics)
library(tidyverse)
library(leaps)
library(rvest)
library(janitor)

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

filter_empties <- function(x){
  x[nchar(x)>0]
}

select_names <- function(x){
  paste(x, collapse = " ")
}

select_names.new <- function(x){
  x[1:length(x)-1]
}

nchar_apply <- function(x, limit){
  nchar(x) <= limit
}

select_names4 <- function(x){
  x <- x[-c(1:5, length(x), length(x)-1, length(x)-2)]
  x <- paste(x, collapse = " ")
  trimws(x)
}

format_name4 <- function(name){
  names <- str_split(name, " ")
  names <- lapply(names, filter_empties)
  names <- lapply(names, select_names4)
  unlist(names)
}

format_name.new <- function(name){
  names <- str_split(name, " ")
  names <- lapply(names, select_names.new)
  names <- lapply(names, select_names)
  unlist(names)
}

select_team <- function(x){
  x[5]
}


##################################################### 2020 ##############################################

link <- "https://www.fantasypros.com/nfl/stats/qb.php?year=2019&scoring=HALF&range=full"
webpage <- read_html(link)

table <- webpage %>% html_table()
table <- table[[1]]
table <- table %>% 
  row_to_names(2) %>% 
  clean_names()


table$rost <- parse_number(table$rost)
table$player <- format_name.new(table$player)

table %>% 
  mutate_at(vars(-player), ~ as.numeric(gsub(",", "", .))) %>% 
  mutate_at(vars(-player), ~ . / g) %>% 
  select(-rank) -> table_qb


# GET FANTASY POINTS ------------------------------------

link <- "https://www.fantasypros.com/nfl/stats/qb.php?year=2020&scoring=HALF&range=full"
page <- read_html(link)

fan_pts <- html_table(page)
fan_pts <- fan_pts[[1]]

fan_pts <- fan_pts %>% 
  row_to_names(2) %>% 
  janitor::clean_names()

fan_pts$rost <- parse_number(fan_pts$rost)
fan_pts$player <- format_name.new(fan_pts$player)

fan_pts <- fan_pts %>% 
  mutate_at(vars(-player), ~ as.numeric(gsub(",", "", .))) %>% 
  mutate_at(vars(-player), ~ . / g) %>% 
  select(-rank)



# RE-FORMAT NAMES

qb2020 <- table_qb %>% 
  filter(player %in% fan_pts$player)

qb.stats <- left_join(qb2020, fan_pts %>% select(player, fpts),
                      by = c("player"))

final <- qb.stats

##################################################### 2021 ##############################################
## use fan pts as starting table

table_qb <- fan_pts

# GET FANTASY POINTS ------------------------------------

link <- "https://www.fantasypros.com/nfl/stats/qb.php?year=2021&scoring=HALF&range=full"
page <- read_html(link)

fan_pts <- html_table(page)
fan_pts <- fan_pts[[1]]

fan_pts <- fan_pts %>% 
  row_to_names(2) %>% 
  janitor::clean_names()

fan_pts$rost <- parse_number(fan_pts$rost)
fan_pts$player <- format_name.new(fan_pts$player)

fan_pts <- fan_pts %>% 
  mutate_at(vars(-player), ~ as.numeric(gsub(",", "", .))) %>% 
  mutate_at(vars(-player), ~ . / g) %>% 
  select(-rank)

# RE-FORMAT NAMES

qb2021 <- table_qb %>% 
  filter(player %in% fan_pts$player)

qb.stats <- left_join(qb2021, fan_pts %>% select(player, fpts), by = "player")

final <- rbind(final, qb.stats)

####################################### 2022 ############################################


table_qb <- fan_pts


# GET FANTASY POINTS ------------------------------------

link <- "https://www.fantasypros.com/nfl/stats/qb.php?year=2022&scoring=HALF&range=full"
page <- read_html(link)

fan_pts <- html_table(page)
fan_pts <- fan_pts[[1]]

fan_pts <- fan_pts %>% 
  row_to_names(2) %>% 
  janitor::clean_names()

fan_pts$rost <- parse_number(fan_pts$rost)
fan_pts$player <- format_name.new(fan_pts$player)

fan_pts <- fan_pts %>% 
  mutate_at(vars(-player), ~ as.numeric(gsub(",", "", .))) %>% 
  mutate_at(vars(-player), ~ . / g) %>% 
  select(-rank)

## combine

qb2022 <- table_qb %>% 
  filter(player %in% fan_pts$player)

qb.stats <- left_join(qb2022, fan_pts %>% select(player, fpts), by = "player")

final <- rbind(final, qb.stats)

## MODEL CREATION -----------------------------------------------------------------------------
# final <- final %>% 
#   group_by(player) %>% 
#   filter(n() >= 2) %>% 
#   ungroup() %>% 
#   select(-tm)

set.seed(123)

# Determine the proportion of data for training (e.g., 70%)
train_proportion <- 0.7

# Create a vector of random indices for the train set
train_indices <- sample(nrow(final), floor(train_proportion * nrow(final)))

# Create the train and test sets using the indices
train_data <- final[train_indices, ]
test_data <- final[-train_indices, ]

train_data_2 <- train_data %>% drop_na() %>% select(-player)
mod <- lm(fpts.y ~ cmp + yds + td + td_2, 
          data = train_data_2)

null_mod <- lm(fpts.y ~ (. - player - rost - fpts.x - fpts.y) + 
                 (. - player - rost - fpts.x - fpts.y)^2,
               data = train_data)

aic_mod <- step(null_mod)

bic_mod <- step(null_mod, k = log(nrow(train_data)))



final$est_aic <- predict(aic_mod, newdata = final)
final$est_bic <- predict(bic_mod, newdata = final)
final$est_spec <- predict(mod, newdata = final)



final %>% 
  ggplot(aes(est_aic, fpts.y)) +
  geom_point()



## load in data from 2022

table <- fan_pts %>% 
  rename("fpts.x" = fpts)



table$projected_pts.aic <- predict(aic_mod, newdata = table)
table$projected_pts.bic <- predict(bic_mod, newdata = table)
table$projected_pts.null <- predict(mod, newdata = table)


### select actual values and projected


table %>% 
  mutate(aic_rank = rank(-projected_pts.aic),
         bic_rank = rank(-projected_pts.bic),
         spec_rank = rank(-projected_pts.null)) %>% 
  rowwise() %>% 
  mutate(avg_rank = mean(c(aic_rank, bic_rank, spec_rank))) %>% 
  select(player, aic_rank, bic_rank, spec_rank, avg_rank) -> table



### scrape cbs projections

link <- "https://www.rotoballer.com/half-ppr-fantasy-football-rankings-2023-september-1st/1194057"
page <- read_html(link)

cbs_proj <- html_table(page)
cbs_proj <- cbs_proj[[1]]

cbs_proj <- cbs_proj %>% 
  row_to_names(1) %>% 
  janitor::clean_names()

cbs_proj <- cbs_proj %>% 
  mutate_at(.vars = c(colnames(cbs_proj[-c(3:4)])), as.numeric)

cbs_proj[is.na(cbs_proj)] <- 0

cbs_proj %>% 
  filter(pos == "QB") %>% 
  select(player_name, rank, tier) %>% 
  mutate(proj_rank = rank(rank)) %>% 
  select(-rank) -> cbs_proj



## combine cbs proj and model estimates

table <- left_join(table, cbs_proj, by = c("player" = "player_name"))


link <- "https://fantasyfootballcalculator.com/rankings/half-ppr/qb"
page <- read_html(link)

html_table(page) %>% 
  .[[1]] -> adp

adp %>% 
  mutate(adp = rank(Rank)) %>% 
  clean_names() %>% 
  select(name, adp) -> adp_qb

adp_qb[adp_qb == "Patrick Mahomes"] <- "Patrick Mahomes II"

table %>% 
  left_join(adp_qb, by = c("player" = "name")) -> table


table <- table %>% 
  rowwise() %>% 
  mutate(avg_rank = .3*proj_rank + .4*adp + .2*spec_rank) %>% 
  ungroup() %>% 
  mutate(avg_rank = rank(avg_rank)) %>% 
  relocate(avg_rank)


# Assuming you have your player data in a data frame named 'player_data'
# Exclude the 'player' column, as it contains non-numeric data
data_for_clustering <- table %>% select(-c(aic_rank, bic_rank, player))

# Perform hierarchical clustering using complete linkage method
# You can change the linkage method based on your preferences
# 'euclidean' can be replaced with other distance metrics (e.g., 'manhattan')
hc <- hclust(dist(data_for_clustering, method = "manhattan"), method = "complete")

# Determine the number of clusters (you can adjust this based on your analysis)
num_clusters <- 30  # Change this number as needed

# Cut the dendrogram to obtain clusters
clusters <- cutree(hc, k = num_clusters)

# Add the cluster assignment to your player_data data frame
table$cluster <- clusters


table %>% 
  rowwise() %>% 
  mutate(rnk = mean(avg_rank, cluster)) %>% 
  ungroup() %>%
  mutate(avg_rank = rnk) %>%
  select(-rnk) -> table

#### write table

write_csv(table, "qb_rankings.csv")

