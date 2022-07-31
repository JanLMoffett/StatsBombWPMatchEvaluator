
#library(StatsBombR)
library(tidyverse)
library(devtools)
library(lubridate)

library(nnet)

#source("functions_etc/scrape_StatsBomb.R")
#source("app/app_functions/timestamp_to_seconds.R")
#source("app/app_functions/bombViz.R")


#how to apply model to matches from UEFA 2020?
#---------------------------------------------
fg_uefa2020 <- read.csv("win_probability/wp_data/futureGoals_55_7.csv")
#take out dummy row
fg_uefa2020 <- fg_uefa2020[-1,]

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

#matchID <- 3795187

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


for(matchID in match_ids){
  
  
  #filter model input for selected match
  this.nd <- nd_uefa %>% filter(match_id == matchID)
  
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
  
  max_tb <- max(this.gp$time_bin, na.rm = T)
  #win, lose, or draw predictions
  for(this.tb in 0:(max_tb - 1)){
    rolls.home <- sample(0:13, reps, prob = this.gpm.h[this.tb + 1,], replace = T)
    rolls.away <- sample(0:9, reps, prob = this.gpm.a[this.tb + 1,], replace = T)
    
    #past goals scored as of selected time bin
    past_hg <- fg_uefa2020$cur_home_score[this.tb + 1]
    past_ag <- fg_uefa2020$cur_away_score[this.tb + 1]
    
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
write.csv(goal_probabilities, "app/app_data/goal_probabilities_UEFA2020.csv")

#all min-by-min win/draw/lose probabilities for uefa 2020
write.csv(outer_wp_df, "app/app_data/win_probabilities_UEFA2020.csv")





