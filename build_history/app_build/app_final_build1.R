
library(tidyverse)

#source("app_functions/scrape_StatsBomb.R")
source("app_functions/bombViz.R")
source("app_functions/timestamp_to_seconds.R")
source("app_functions/get_match_players2.R")
source("app_functions/get_match_info_table.R")

#position abbreviations and display coordinates
posAb <- read.csv("app_data/positionDisplay.csv")

#future goals and other model input data, min-by-min
fg <- read.csv("app_data/futureGoals_55_7.csv")
#all min-by-min goal probabilities for uefa 2020
gp <- read.csv("app_data/goal_probabilities_UEFA2020.csv")
#all min-by-min win/draw/lose probabilities for uefa 2020
wld <- read.csv("app_data/win_probabilities_UEFA2020.csv")


#Matches
m <- read.csv("app_data/dbb_matches.csv")
#Starting lineups
lu <- read.csv("app_data/dbb_events_startingXI.csv")
#Events without nested vars
ev <- read.csv("app_data/events.csv")

#list of unique match_id values for selectInput
matchIDs <- unique(m$match_id)
names(matchIDs) <- m$menu_text

#randomly select a match id
selected_match <- sample(matchIDs, 1)

#I want to go ahead and generate this data and save it so app works faster
#then instead of running the code every time a match is selected, I can just filter for match id

#all match players for every match
all_mp <- data.frame(
  player.id = 9999,
  id = "delete_this_row",
  match_id = 999999,
  team.id = 9999,
  team.name = "",
  jersey_number = 99,
  player.name = "",
  position.id = 99,
  position.name = "",
  period_on = 9,
  timestamp_on = "",
  type.name = "",
  period_off = 9,
  timestamp_off = "",
  home_or_away = "",
  ts_on_seconds = 9,
  ts_off_seconds = 9,
  ts_on_cum_seconds = 9,
  ts_off_cum_seconds = 9,
  timeline_y = 0,
  position_abbr = ""
)

for(selected_match in matchIDs){
  
  this.m <- m %>% filter(match_id == selected_match)
  this.ev <- ev %>% filter(match_id == selected_match)
  
  #df of players in match
  mp <- get_match_players(selected_match, m, ev, lu)
  
  #period and time information
  pers <- get_period_summary(this.ev)
  #table of match info to display
  mit <- get_match_info_table(this.m)
  
  #get match-specific information
  #team names
  awayName <- this.m %>% pull(away_team.away_team_name)
  homeName <- this.m %>% pull(home_team.home_team_name)
  
  #add a var to sep home and away
  mp <- mp %>% mutate(home_or_away = ifelse(team.name == awayName, "away", "home"))
  
  #transform timestamps into cumulative seconds
  this.ev <- get_cumulative_match_seconds(this.ev)
  mp <- get_cumulative_ts_on_off(mp, pers)
  
  #assign a y value to each player in match
  mp <- mp %>% arrange(home_or_away, position.id, ts_on_cum_seconds) %>%
    mutate(timeline_y = seq(dim(mp)[1], 1, -1)) %>%
    mutate(timeline_y = ifelse(home_or_away == "away", timeline_y +1, timeline_y))
  #join position abbreviations
  mp <- mp %>% left_join(posAb %>% select(position_abbr, position.id), by = "position.id")
  
  #match timeline_y values to events based on player id
  this.ev <- this.ev %>% left_join(mp %>% select(player.id, timeline_y), by = "player.id")
  
  #add to dataset
  all_mp <- rbind(all_mp, mp)
}

all_mp <- all_mp[-1,]
write.csv(all_mp, "app_data/match_players_UEFA2020.csv")

#now i have all match players

