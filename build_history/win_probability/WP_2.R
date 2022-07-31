library(StatsBombR)
library(tidyverse)
library(devtools)
library(lubridate)

library(nnet)

source("functions_etc/scrape_StatsBomb.R")
source("app/app_functions/timestamp_to_seconds.R")
source("app/app_functions/bombViz.R")

#i have gathered the data i'm going to use in WP_adding_data3.R
#i'm going to fit a multinomial regression model to obtain probability
#distributions of future goals given time bin, score differential, and ball location
fg_list <- list(
  fg1 = read.csv("win_probability/wp_data/futureGoals_16_1.csv"),
  fg2 = read.csv("win_probability/wp_data/futureGoals_37_2.csv"),
  fg3 = read.csv("win_probability/wp_data/futureGoals_43_3.csv"),
  fg4 = read.csv("win_probability/wp_data/futureGoals_11_4.csv"),
  fg5 = read.csv("win_probability/wp_data/futureGoals_49_5.csv"),
  fg6 = read.csv("win_probability/wp_data/futureGoals_2_6.csv"),
  fg8 = read.csv("win_probability/wp_data/futureGoals_72_8.csv"))
#fg7 is the UEFA Euro 2020 dataset, so i'll leave it out of training

#taking out dummy rows
fg_list <- lapply(fg_list, function(x) x[-1,])

#binding dataframes into one
d1 <- rbind(fg_list[[1]], fg_list[[2]]) %>% rbind(fg_list[[3]]) %>% 
  rbind(fg_list[[4]]) %>% rbind(fg_list[[5]]) %>% 
  rbind(fg_list[[6]]) %>% rbind(fg_list[[7]])

#nullify old sets to free up memory
fg_list <- NULL

#clean dataset a little
d1 <- d1 %>% 
  mutate(id = paste0(competition_id, "_", season_id, "_", match_id),
         cur_score_diff = cur_home_score - cur_away_score) %>%
  select(-all_of(c("X","competition_id","season_id","match_id", 
                   "cur_home_score", "cur_away_score"))) %>%
  relocate(id)

#different datasets for home and away, to build two different 
#models for future goals
d1.home <- d1 %>% select(-future_goals_away)
d1.away <- d1 %>% select(-future_goals_home)

#how many unique matches in set?
length(unique(d1.home$id))
#1045

#hold out n matches for testing
test_n <- 20
set.seed(22); test_ids <- sample(unique(d1.home$id), test_n, replace = F)

#use same matches to train both models?  
#for now i am...

# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
#   Home Team Model 
#--------------------------------------------------------------------

#split training and testing sets
d1.h.tr <- d1.home %>% filter(!(id %in% test_ids))
d1.h.te <- d1.home %>% filter(id %in% test_ids)

#fitting multinomial regression model from nnet package
mod1.h <- multinom(future_goals_home ~ time_bin + cur_score_diff + start_loc_x + start_loc_y + end_loc_x + end_loc_y, d1.h.tr)
#saveRDS(mod1.h, "app/app_functions/goals_model_home.rds")

#--------------------------------------------------

# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
#   Away Team Model 
#--------------------------------------------------------------------

#split training and testing sets
d1.a.tr <- d1.away %>% filter(!(id %in% test_ids))
d1.a.te <- d1.away %>% filter(id %in% test_ids)

#fitting multinomial regression model from nnet package
mod1.a <- multinom(future_goals_away ~ time_bin + cur_score_diff + start_loc_x + start_loc_y + end_loc_x + end_loc_y, d1.a.tr)
#saveRDS(mod1.a, "app/app_functions/goals_model_away.rds")

#--------------------------------------------------

#producing fg matrix, by holding locations constant
#--------------------------------------------------
#when score differential is 0:
nd.sd0 <- data.frame(
  time_bin = 0:89,
  cur_score_diff = rep.int(0, 90),
  start_loc_x = rep.int(60,90),
  start_loc_y = rep.int(40,90),
  end_loc_x = rep.int(60,90),
  end_loc_y = rep.int(40,90)
)
#when score differential is 1:
nd.sd1 <- data.frame(
  time_bin = 0:89,
  cur_score_diff = rep.int(1, 90),
  start_loc_x = rep.int(60,90),
  start_loc_y = rep.int(40,90),
  end_loc_x = rep.int(60,90),
  end_loc_y = rep.int(40,90)
)
#and so on...
pred.h.sd0 <- predict(mod1.h, nd.sd0, type = "probs")
row.names(pred.h.sd0) <- paste0("tb_", 0:89)

barplot(pred.h.sd0["tb_0",])
barplot(pred.h.sd0["tb_20",])
barplot(pred.h.sd0["tb_44",])
#see shiny app!



