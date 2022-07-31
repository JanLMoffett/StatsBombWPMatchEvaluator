
library(tidyverse)
library(devtools)
library(lubridate)

library(nnet)

#source("functions_etc/scrape_StatsBomb.R")
source("app/app_functions/functions_and_constants_visual.R")
source("app/app_functions/functions_and_constants_time.R")
#source("app/app_functions/bombViz.R")

mod1.a <- readRDS("app/app_functions/goals_model_away.rds")
mod1.h <- readRDS("app/app_functions/goals_model_home.rds")

#how to apply model to events and players from UEFA 2020?
#---------------------------------------------
fg_uefa2020 <- read.csv("win_probability/wp_data/futureGoals_55_7.csv")
#take out dummy row
fg_uefa2020 <- fg_uefa2020[-1,]

#does match data validate fg dataset?
m <- read.csv("app/app_data/dbb_matches.csv")
names(m)

ref_name <- m$referee.name

Encoding(m$referee.name) <- "UTF-8"
Encoding(m$stadium.name) <- "UTF-8"
#write.csv(m, "app/app_data/matches_corr.csv")

m1 <- m %>% select(match_id, away_score, home_score) %>% 
  mutate(result = case_when(away_score < home_score ~ "home win",
                            away_score > home_score ~ "home lose",
                            away_score == home_score ~ "draw"))

fg <- left_join(fg_uefa2020, m1, by = "match_id")
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

all_mp <- read.csv("app/app_data/all_match_players_corr.csv")
  
#make data ready to put in the model
nd_uefa <- fg_uefa2020 %>% 
  mutate(cur_score_diff = cur_home_score - cur_away_score) %>%
  select(-all_of(c("X","competition_id","season_id",
                   "cur_home_score", "cur_away_score"))) %>%
  relocate(match_id, time_bin, cur_score_diff) %>%
  select(-starts_with("future_goals"))

#these are the vars I need for each event to feed into the model
names(nd_uefa)

names(ev)
ev %>% filter(is.na(match_id))


sort(unique(m$match_id))
sort(unique(ev$match_id))

ev2 <- left_join(ev, m, by = "match_id")

#here i make sure I'm only including first and second periods
ev2 <- ev2 %>% filter(period < 3) %>%
  mutate(ts_seconds = timestamp_to_seconds(timestampStr = timestamp))

#flag score changes
ev2 <- ev2 %>% 
  mutate(is_goal = ifelse(type.name == "Shot" & shot.outcome.name == "Goal", 1, 0)) %>%
  mutate(score_change = case_when(is_goal == 1 ~ 1,
                                  type.name == "Own Goal For" ~ 1,
                                  TRUE ~ 0)) %>%
  mutate(is_home_team = ifelse(team.name == home_team.home_team_name, 1, 0),
         is_poss_team = ifelse(team.name == possession_team.name, 1, 0)) %>%
  mutate(home_score_change = ifelse(is_home_team == 1, score_change, 0),
         away_score_change = ifelse(is_home_team == 0, score_change, 0)) %>%
  group_by(match_id) %>% arrange(match_id, index) %>%
  mutate(cur_home_score = cumsum(home_score_change),
         cur_away_score = cumsum(away_score_change)) %>% 
  ungroup() %>%
  mutate(cur_score_diff = cur_home_score - cur_away_score)

#calculate future goals
ev3 <- ev2 %>% arrange(match_id, desc(index)) %>% group_by(match_id) %>%
  mutate(future_goals_home = cumsum(home_score_change), 
         future_goals_away = cumsum(away_score_change)) %>%
  mutate(future_goals_home = future_goals_home - home_score_change,
         future_goals_away = future_goals_away - away_score_change) %>%
  ungroup() %>%
  relocate(match_id)

ev3 <- ev3 %>% group_by(match_id) %>%
  mutate(final_goals_home = max(cur_home_score, na.rm = T),
         final_goals_away = max(cur_away_score, na.rm = T)) %>%
  ungroup()

names(ev3)

ev3 <- ev3 %>% select(-all_of(c("X.y", "X.1.y", "X.x", "X.1.x")))



write.csv(ev3, "app/app_data/events_corr_1.csv")
#will have to get variables match-by-match and add to the events set

#matchID <- 3788744
match_ids <- unique(nd_uefa$match_id)

evc_df <- read.csv("events_corr_3788741.csv")
evc_df <- evc_df[1,]
evc_df <- evc_df[,-1]
evc_df$id[1] <- "REMOVE_THIS_ROW"

#matchID = 3788744
for(matchID in match_ids){
  
  this.ev <- ev3 %>% filter(match_id == matchID)
  mp <- all_mp %>% filter(match_id == matchID)
  
  homeName <- unique(this.ev$home_team.home_team_name)
  awayName <- unique(this.ev$away_team.away_team_name)
  
  this.ev <- get_cumulative_match_seconds(this.ev)
  this.ev <- get_time_bins_match_events(this.ev)
  
  #standardize location coordinates
  this.ev <- this.ev %>%
    select(-player.name) %>%
    mutate(is_home_team = ifelse(team.name == homeName, 1, 0)) %>%
    mutate(std_location_x = ifelse(is_home_team == 0, 120 - location.x, location.x),
           std_location_y = ifelse(is_home_team == 0, 80 - location.y, location.y))
  
  #add timeline_y values to events df
  tly <- mp %>% select(player.id, player.name, timeline_y)
  this.ev <- this.ev %>% left_join(tly, by = "player.id")
  
  
  evc_df <- rbind(evc_df, this.ev)
  
  
}

evc_df <- evc_df[-1,]
#write.csv(evc_df, "app/app_data/events_corr_2.csv")

evc <- read.csv("app/app_data/events_corr_2.csv")

#need these vars to predict future goals:
#[1] "match_id"       "time_bin"       "cur_score_diff" "start_loc_x"   
#[5] "start_loc_y"    "end_loc_x"      "end_loc_y" 
names(evc)

#"carry.end_location.x"            
#"carry.end_location.y"            
#"pass.end_location.x"             
#"pass.end_location.y"             
#"shot.end_location.x"             
#"shot.end_location.y"             
#"shot.end_location.z"
#"location.x.GK"                   
#"location.y.GK"

evc %>% filter(!is.na(carry.end_location.x)) %>% group_by(is_home_team) %>% tally()
#there are other location coords i need to standardize and turn into end location
evc <- evc %>% 
  mutate(
    std_carry.end_location.x = ifelse(is_home_team == 0, 120 - carry.end_location.x, carry.end_location.x),
    std_carry.end_location.y = ifelse(is_home_team == 0, 80 - carry.end_location.y, carry.end_location.y)) %>%
  mutate(
    std_pass.end_location.x = ifelse(is_home_team == 0, 120 - pass.end_location.x, pass.end_location.x),
    std_pass.end_location.y = ifelse(is_home_team == 0, 80 - pass.end_location.y, pass.end_location.y)) %>%
  mutate(
    std_shot.end_location.x = ifelse(is_home_team == 0, 120 - shot.end_location.x, shot.end_location.x),
    std_shot.end_location.y = ifelse(is_home_team == 0, 80 - shot.end_location.y, shot.end_location.y)) %>%
  mutate(
    std_GK_location.x = ifelse(is_home_team == 0, 120 - location.x.GK, location.x.GK),
    std_GK_location.y = ifelse(is_home_team == 0, 80 - location.y.GK, location.y.GK))
    
#write.csv(evc, "app/app_data/events_corr_3.csv")

#groups of variables for each major event type
names(evc)
shots <- evc %>% filter(type.name == "Shot") %>% 
  select(match_id, id, type.name, starts_with("shot"))
passes <- evc %>% filter(type.name == "Pass") %>%
  select(match_id, id, type.name, starts_with("pass"))
carries <- evc %>% filter(type.name == "Carry") %>%
  select(match_id, id, type.name, starts_with("carry"))
clearances <- evc %>% filter(type.name == "Clearance") %>%
  select(match_id, id, type.name, starts_with("clearance"))

evp <- evc %>% 
  select(match_id, id, type.name, time_bin, cur_score_diff, starts_with("std"))

evp <- evp %>% 
  rename(start_loc_x = std_location_x,
         start_loc_y = std_location_y) %>%
  mutate(end_loc_x = case_when(
    type.name == "Pass" ~ std_pass.end_location.x,
    type.name == "Shot" ~ std_shot.end_location.x,
    type.name == "Carry" ~ std_carry.end_location.x,
    TRUE ~ start_loc_x)) %>%
  mutate(end_loc_y = case_when(
    type.name == "Pass" ~ std_pass.end_location.y,
    type.name == "Shot" ~ std_shot.end_location.y,
    type.name == "Carry" ~ std_carry.end_location.y,
    TRUE ~ start_loc_y))

evp <- evp %>% 
  select(match_id, id, type.name, time_bin, cur_score_diff, 
         start_loc_x, start_loc_y, end_loc_x, end_loc_y)

evp <- na.omit(evp)
write.csv(evp, "win_probability/wp_data/events_pred_set.csv")
