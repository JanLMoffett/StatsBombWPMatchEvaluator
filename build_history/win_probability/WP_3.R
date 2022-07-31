library(StatsBombR)
library(tidyverse)
library(devtools)
library(lubridate)

library(nnet)

source("functions_etc/scrape_StatsBomb.R")
source("app/app_functions/timestamp_to_seconds.R")
source("app/app_functions/bombViz.R")


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

#~~~~~~~~~~~
#this code runs when the app starts up:
#Matches
m <- scrape_matches(compName = "UEFA Euro")
#Events (with nested vars)
ev_og <- scrape_events(m)
#Unnested starting lineups
lu <- get_startingXI(ev_og)
#events without nested vars
ev <- get_stripped_events(ev_og)
ev_og <- NULL
#when i get events set i'll need to chop off periods above 2
#~~~~~~~~~~~

#app gives a match id input and a time input

#will need to build a function to do this quickly
matchID <- 3795187

#filter model input for selected match
this.nd <- nd_uefa %>% filter(match_id == matchID)

#get goal prediction matrices for match
this.pmh <- predict(mod1.h, this.nd, type = "probs")
this.pma <- predict(mod1.a, this.nd, type = "probs")

#each row adds to 1
sum(this.pmh[3,])

#for each time bin, run simulations to predict outcome
#home team can score 0-13 goals
#away team can score 0-9 goals
#roll "weighted dice" for home and away reps times
#this.tb <- 0
reps <- 10000

#df to store wp results
wp_df <- data.frame(
  reps = 0,
  wins = 0,
  draws = 0,
  losses = 0, 
  time_bin = 999
)

#win, lose, or draw predictions
for(this.tb in 0:89){
  rolls.home <- sample(0:13, reps, prob = this.pmh[this.tb + 1,], replace = T)
  rolls.away <- sample(0:9, reps, prob = this.pma[this.tb + 1,], replace = T)
  
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
  wp_df <- rbind(wp_df, wp_row)
}

#here are the predictions for this match
wp_df <- wp_df[-1,]

wp_df <- wp_df %>% mutate(
  p_win = wins/reps,
  p_draw = draws/reps,
  p_loss = losses/reps
)

#plot the results!

ggplot(wp_df) + shUEFA_theme_icy +
  geom_line(aes(x = time_bin, y = p_win), color = "coral") + 
  geom_line(aes(x = time_bin, y = p_draw), color = "seagreen") + 
  geom_line(aes(x = time_bin, y = p_loss), color = "dodgerblue")














