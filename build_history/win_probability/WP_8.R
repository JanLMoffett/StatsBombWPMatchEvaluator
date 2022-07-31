library(tidyverse)
library(devtools)
library(lubridate)

library(nnet)
library(caret)
library(scales)

#source("functions_etc/scrape_StatsBomb.R")
source("app/app_functions/functions_and_constants_time.R")
source("app/app_functions/functions_and_constants_visual.R")

#~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))
#                             training the model
#~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))

#i'm going to fit a multinomial regression model to obtain probability
#distributions of future goals given time bin, score differential, and ball location

#i have gathered the data i'm going to use in WP_adding_data3.R
fg_list <- list(
  fg1 = read.csv("win_probability/wp_data/futureGoals_16_1.csv"),
  fg2 = read.csv("win_probability/wp_data/futureGoals_37_2.csv"),
  fg3 = read.csv("win_probability/wp_data/futureGoals_43_3.csv"),
  fg4 = read.csv("win_probability/wp_data/futureGoals_11_4.csv"),
  fg5 = read.csv("win_probability/wp_data/futureGoals_49_5.csv"),
  fg6 = read.csv("win_probability/wp_data/futureGoals_2_6.csv"),
  fg8 = read.csv("win_probability/wp_data/futureGoals_72_8.csv"))
#fg7 is the UEFA Euro 2020 dataset, so i'll leave it out of training
fg7 <- read.csv("win_probability/wp_data/futureGoals_55_7.csv")

#taking out dummy rows
fg_list <- lapply(fg_list, function(x) x[-1,])
fg7 <- fg7[-1,]

#binding dataframes into one
d <- rbind(fg_list[[1]], fg_list[[2]]) %>% rbind(fg_list[[3]]) %>% 
  rbind(fg_list[[4]]) %>% rbind(fg_list[[5]]) %>% 
  rbind(fg_list[[6]]) %>% rbind(fg_list[[7]])

#nullify old sets to free up memory
fg_list <- NULL

#clean dataset a little
d1 <- d %>% 
  mutate(id = paste0(competition_id, "_", season_id, "_", match_id),
         cur_score_diff = cur_home_score - cur_away_score) %>%
  select(-all_of(c("X","competition_id","season_id","match_id", 
                   "cur_home_score", "cur_away_score"))) %>%
  relocate(id)

#scale data to between 0 and 1
d1_scaled <- as_tibble(rescale(as.matrix(d1[,-1]))) %>%
  mutate(id = d1$id) %>%
  relocate(id)

#different datasets for home and away, to build two different 
#models for future goals
d1s.home <- d1_scaled %>% select(-future_goals_away)
d1.home <- d1 %>% select(-future_goals_away)

d1s.away <- d1_scaled %>% select(-future_goals_home)
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
#scaled versions
d1s.h.te <- d1s.home %>% filter(id %in% test_ids)
d1s.h.tr <- d1s.home %>% filter(!(id %in% test_ids))

#non-scaled versions for reference
d1.h.tr <- d1.home %>% filter(!(id %in% test_ids))
d1.h.te <- d1.home %>% filter(id %in% test_ids)

#fitting multinomial regression model from nnet package
mod2.h <- multinom(future_goals_home ~ time_bin + cur_score_diff + start_loc_x + start_loc_y + end_loc_x + end_loc_y, d1s.h.tr)
saveRDS(mod2.h, "app/app_functions/goals_model_w_scaling_home.rds")

#--------------------------------------------------------------------

# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
#   Away Team Model 
#--------------------------------------------------------------------

#split training and testing sets
#scaled versions
d1s.a.tr <- d1s.away %>% filter(!(id %in% test_ids))
d1s.a.te <- d1s.away %>% filter(id %in% test_ids)

#non-scaled versions for reference
d1.a.tr <- d1.away %>% filter(!(id %in% test_ids))
d1.a.te <- d1.away %>% filter(id %in% test_ids)

#fitting multinomial regression model from nnet package
mod2.a <- multinom(future_goals_away ~ time_bin + cur_score_diff + start_loc_x + start_loc_y + end_loc_x + end_loc_y, d1s.a.tr)
saveRDS(mod2.a, "app/app_functions/goals_model_w_scaling_away.rds")

#--------------------------------------------------------------------


#~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))
#                       predicting UEFA 2020 matches
#~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))~))

mod1.a <- readRDS("app/app_functions/goals_model_away.rds")
mod1.h <- readRDS("app/app_functions/goals_model_home.rds")

#all the UEFA 2020 events
evc <- read.csv("app/app_data/events_corr_3.csv")
evp <- read.csv("win_probability/wp_data/events_pred_set.csv")

#evp_scaled <- rescale(as.matrix(evp %>% select(time_bin:end_loc_y)))
evp_scaled <- as_tibble(evp) %>%
  mutate(match_id = evp$match_id, id = evp$id) %>%
  relocate(match_id, id)

#some obs are in evc but not evp.  which obs from evc are going to be left out of prediction?
evc_event_ids <- sort(unique(evc$id))
length(evc_event_ids) #[1] 183481

evp_event_ids <- sort(unique(evp$id))
length(evp_event_ids) #[1] 182107

#1374 obs in evc will need to be flagged as not included in model
no_pred_ids <- setdiff(evc_event_ids, evp_event_ids)
evc <- evc %>% mutate(no_win_prediction = ifelse(id %in% no_pred_ids, 1, 0))
sum(evc$no_win_prediction) #1374

#save new variable to events set
#write.csv(evc, "app/app_data/events_corr_4.csv")
#evc <- read.csv("app/app_data/events_corr_4.csv")

match_ids <- unique(evp_scaled$match_id)

outer_wp_df <- data.frame(
  match_id = 0,
  id = "DELETE THIS ROW",
  reps = 0,
  wins = 0,
  draws = 0,
  losses = 0
)

i = 1
for(matchID in match_ids){
  print("------------------------------------")
  print(paste0("Calculating Match ",matchID, ": ", i, " of ", length(match_ids), " matches"))
  print("------------------------------------")
  
  #filter model input for selected match
  this.nd <- evp_scaled %>% filter(match_id == matchID)
  this.fg <- evc %>% filter(match_id == matchID, no_win_prediction == 0)
  
  #some time bins may be missing.  should i fill in?
  #length(unique(this.nd$time_bin))
  #length(unique(this.fg$time_bin))
  #i won't need to as long as i keep the time bin variable intact
  
  #get goal prediction matrices for match
  this.gpm.h <- predict(mod1.h, this.nd[,-1:-2], type = "probs")
  this.gpm.a <- predict(mod1.a, this.nd[,-1:-2], type = "probs")
  
  #renaming columns back to number of goals
  colnames(this.gpm.h) <- paste0("P_", as.character(0:13), "_Goals")
  colnames(this.gpm.a) <- paste0("P_", as.character(0:9), "_Goals")
  #naming rows with event ids
  row.names(this.gpm.h) <- this.nd$id
  row.names(this.gpm.a) <- this.nd$id
  
  #now, get win/draw/lose probabilities for match
  
  reps <- 10000
  
  #df to store wp results
  inner_wp_df <- data.frame(
    reps = 0,
    wins = 0,
    draws = 0,
    losses = 0, 
    id = "DELETE THIS ROW"
  )
  
  #win, lose, or draw predictions
  #iterate through each predicted event
  
  for(cur_event_id in this.nd$id){
    #for testing: cur_event_id <- "51c66f94-39ff-4dc1-9d6b-33c2cdaf09bf"
    
    rolls.home <- sample(0:13, reps, prob = this.gpm.h[cur_event_id,], replace = T)
    rolls.away <- sample(0:9, reps, prob = this.gpm.a[cur_event_id,], replace = T)
    
    
    #past goals scored as of selected event
    past_hg <- this.fg %>% filter(id == cur_event_id) %>% pull(cur_home_score)
    past_ag <- this.fg %>% filter(id == cur_event_id) %>% pull(cur_away_score)
    
    wld <- data.frame(
      pred_goals_home = rolls.home,
      pred_goals_away = rolls.away
    )
    
    wld <- wld %>% mutate(
      pred_final_home = pred_goals_home + past_hg,
      pred_final_away = pred_goals_away + past_ag) %>%
      mutate(
        win = ifelse(pred_final_home > pred_final_away, 1, 0),
        draw = ifelse(pred_final_home == pred_final_away, 1, 0),
        loss = ifelse(pred_final_home < pred_final_away, 1, 0)
      )
    
    #summarize the predictions for time bin into row of data
    wp_row <- wld %>% summarize(
      reps = n(),
      wins = sum(win),
      draws = sum(draw),
      losses = sum(loss)) %>%
      mutate(
        id = cur_event_id
      )
    
    #add row onto df
    inner_wp_df <- rbind(inner_wp_df, wp_row)
    
    
  }
  
  #here are the predictions for this match
  inner_wp_df <- inner_wp_df[-1,]
  inner_wp_df <- inner_wp_df %>% mutate(match_id = matchID) %>% relocate(match_id, id)
  outer_wp_df <- rbind(outer_wp_df, inner_wp_df)
  
  i = i+1
  
}

outer_wp_df <- outer_wp_df %>% mutate(
  p_win = wins/reps,
  p_draw = draws/reps,
  p_loss = losses/reps
)

#remove dummy row
outer_wp_df <- outer_wp_df[-1,]
#save all event-by-event win/draw/lose probabilities for uefa 2020
write.csv(outer_wp_df, "app/app_data/win_probabilities_events_redo.csv")
#outer_wp_df <- read.csv("app/app_data/win_probabilities_scaled.csv")

evc_w_pred <- left_join(evc, outer_wp_df %>% select(id, p_win, p_draw, p_loss), by = "id")
evc_w_pred <- evc_w_pred %>% arrange(match_id, index)

#joined predicted outcome probabilities with events
write.csv(evc_w_pred, "app/app_data/events_with_wdl2.csv")


ggplot(evc_w_pred %>% filter(match_id == match_ids[4])) + 
  geom_line(aes(x = cum_match_seconds, y = p_win), color = "orange") + 
  geom_line(aes(x = cum_match_seconds, y = p_draw), color = "green") + 
  geom_line(aes(x = cum_match_seconds, y = p_loss), color = "blue")
  
  
matchIDs[4]
  
  
  
  
