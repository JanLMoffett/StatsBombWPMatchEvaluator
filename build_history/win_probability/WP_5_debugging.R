
#library(StatsBombR)
library(tidyverse)
library(devtools)
library(lubridate)

library(nnet)

#source("functions_etc/scrape_StatsBomb.R")
source("app/app_functions/timestamp_to_seconds.R")
#source("app/app_functions/bombViz.R")


#i don't know if the win probability problem lies in the 
#predicted goals model, or the outcome simulations

#i think it may be the outcome simulations, so I'm checking here first


#how to apply model to matches from UEFA 2020?
#---------------------------------------------
fg_uefa2020 <- read.csv("win_probability/wp_data/futureGoals_55_7.csv")
#take out dummy row
fg_uefa2020 <- fg_uefa2020[-1,]

#does match data validate fg dataset?
m <- read.csv("app/app_data/dbb_matches.csv")
names(m)
m2 <- m %>% select(match_id, away_score, home_score) %>% 
  mutate(result = case_when(away_score < home_score ~ "home win",
                            away_score > home_score ~ "home lose",
                            away_score == home_score ~ "draw"))

fg <- left_join(fg_uefa2020, m2, by = "match_id")
fg_val <- fg %>% arrange(match_id, time_bin) %>% group_by(match_id) %>%
  summarize(away_score_ev = first(future_goals_away),
            home_score_ev = first(future_goals_home),
            away_score_m = first(away_score),
            home_score_m = first(home_score)) %>%
  mutate(flag_error_away = ifelse(away_score_ev != away_score_m, 1, 0),
         flag_error_home = ifelse(home_score_ev != home_score_m, 1, 0))

#there are some inconsistencies
#need to look at events, see if flags correlate with extra periods
ev <- read.csv("app/app_data/events.csv")
ev2 <- ev %>% group_by(match_id) %>%
  summarize(max_per = max(period, na.rm = T))

fg_val2 <- left_join(fg_val, ev2, by = "match_id")

#the inconsistencies do occur when there are tie-breaking periods
#need to amend the match data to reflect results after 2 periods for each match

#what other competitions had more than 2 periods in some of their matches?

#looking at the model data loop, i don't think it relies on the score from the matches dataset,
#just what it calculates from the events


  
#make data ready to put in the model
nd_uefa <- fg_uefa2020 %>% 
  mutate(cur_score_diff = cur_home_score - cur_away_score) %>%
  select(-all_of(c("X","competition_id","season_id",
                   "cur_home_score", "cur_away_score"))) %>%
  relocate(match_id, time_bin, cur_score_diff) %>%
  select(-starts_with("future_goals"))

#writing df to read into app
#write.csv(nd_uefa, "app/app_data/newdata_UEFA2020.csv")

#read in models for goals
mod1.h <- readRDS("app/app_functions/goals_model_home.rds")
mod1.a <- readRDS("app/app_functions/goals_model_away.rds")

#app gives a match id input and a time input

#matchID <- 3788744

match_ids <- unique(nd_uefa$match_id)

goal_probabilities <- data.frame(
  time_bin = 0, 
  goals = "0", 
  P_goals = 0, 
  match_id = 0, 
  team = "None"
  )

outer_wp_df <- data.frame(
  match_id = 0,
  reps = 0,
  wins = 0,
  draws = 0,
  losses = 0, 
  time_bin = 999
  )

#matchID = 3788744
for(matchID in match_ids){
  
  
  #filter model input for selected match
  this.nd <- nd_uefa %>% filter(match_id == matchID)
  #some time bins may be missing.  should i fill in?
  
  #get goal prediction matrices for match
  this.gpm.h <- predict(mod1.h, this.nd, type = "probs")
  this.gpm.a <- predict(mod1.a, this.nd, type = "probs")
  
  #how to turn goal prediction matrix into useable df?
  #each obs is a probability
  #columns: match_id, time_bin, team, goals, P_goals
  this.gp.h <- as_tibble(this.gpm.h) %>% 
    mutate(time_bin = 0:(dim(this.gpm.h)[1]-1)) %>%
    pivot_longer(1:14, names_to = "goals", values_to = "P_goals") %>%
    mutate(match_id = matchID,
           team = "Home")
  
  this.gp.a <- as_tibble(this.gpm.a) %>% 
    mutate(time_bin = 0:(dim(this.gpm.a)[1]-1)) %>%
    pivot_longer(1:10, names_to = "goals", values_to = "P_goals") %>%
    mutate(match_id = matchID,
           team = "Away")
  
  #finished df of goal probabilities for match
  this.gp <- rbind(this.gp.h, this.gp.a)
  goal_probabilities <- rbind(goal_probabilities, this.gp)
  
  
  #now, get win/draw/lose probabilities for match
  
  #for each time bin, run simulations to predict outcome
  #home team can score 0-13 goals
  #away team can score 0-9 goals
  #roll "weighted dice" for home and away reps times
  #this.tb <- 0
  reps <- 10000
  
  #df to store wp results
  inner_wp_df <- data.frame(
    reps = 0,
    wins = 0,
    draws = 0,
    losses = 0, 
    time_bin = 999
  )
  max_tb = max()
  
  #win, lose, or draw predictions
  for(this.tb in 0:(dim(this.gpm.a)[1]-1)){
    rolls.home <- sample(0:13, reps, prob = this.gpm.h[this.tb + 1,], replace = T)
    rolls.away <- sample(0:9, reps, prob = this.gpm.a[this.tb + 1,], replace = T)
    
    #past goals scored as of selected time bin
    #!!! failed to filter fg_uefa2020 down to selected match id !!!
    this.fg <- fg_uefa2020 %>% filter(match_id == matchID)
    past_hg <- this.fg$cur_home_score[this.tb + 1]
    past_ag <- this.fg$cur_away_score[this.tb + 1]
    
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
        time_bin = this.tb
      )
    
    #add row onto df
    inner_wp_df <- rbind(inner_wp_df, wp_row)
  }
  
  #here are the predictions for this match
  inner_wp_df <- inner_wp_df[-1,]
  inner_wp_df <- inner_wp_df %>% mutate(match_id = matchID) %>% relocate(match_id)
  outer_wp_df <- rbind(outer_wp_df, inner_wp_df)
  
}

outer_wp_df <- outer_wp_df %>% mutate(
  p_win = wins/reps,
  p_draw = draws/reps,
  p_loss = losses/reps
)

#all min-by-min goal probabilities for uefa 2020
write.csv(goal_probabilities, "app/app_data/goal_probabilities_UEFA2020_dbg.csv")

#all min-by-min win/draw/lose probabilities for uefa 2020
write.csv(outer_wp_df, "app/app_data/win_probabilities_UEFA2020_dbg.csv")





