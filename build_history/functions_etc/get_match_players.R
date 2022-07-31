
#this is a function to take a match id and get a full list of players on each team

#matches dataset
#MatchesDF <- read.csv("big_data/dbb_matches.csv")

#events dataset
#EventsDF <- read.csv("big_data/dbb_events.csv")

#unnested starting XI dataframes
#LineupsDF <- read.csv("big_data/dbb_events_startingXI.csv")

get_match_players <- function(matchID, MatchesDF, EventsDF, LineupsDF){
  
  require(tidyverse)
  require(lubridate)
  
  #filter sets to chosen match id
  this.match <- MatchesDF %>% filter(match_id == matchID)
  this.ev <- EventsDF %>% filter(match_id == matchID)
  
  #end period and timestamp for the match
  maxPeriod <- max(this.ev$period, na.rm = T)
  maxTimestamp <- max(this.ev$timestamp[which(this.ev$period == maxPeriod)])
  
  #Starters for match
  starters <- LineupsDF %>% filter(match_id == matchID)
  
  #Substitutions for match
  subs <- this.ev %>% filter(type.name == "Substitution")
  #Player On
  playerOn <- this.ev %>% filter(type.name == "Player On")
  #Player Off
  playerOff <- this.ev %>% filter(type.name == "Player Off")
  
  
  #building match_players df, starting with starters
  match_players <- starters %>% mutate(period_on = 1, timestamp_on = "00:00:00.000", X = NULL, type.name = "Starting XI")
  #variables i want in the match_players df
  mpVars <- names(match_players)
  match_players <- match_players %>% select(all_of(mpVars))
  
  #add players who came in as substitutions and players on to the df
  subOn <- subs %>% select(id, match_id, team.id, team.name, 
                           substitution.replacement.id, substitution.replacement.name, 
                           position.id, position.name, period, timestamp, type.name) %>%
    rename(period_on = period,
           timestamp_on = timestamp,
           player.id = substitution.replacement.id,
           player.name = substitution.replacement.name) %>%
    mutate(jersey_number = NA) %>% 
    select(all_of(mpVars))
  
  plyrOn <- playerOn %>% select(id, match_id, team.id, team.name, 
                                player.id, player.name, 
                                position.id, position.name, period, timestamp, type.name) %>%
    rename(period_on = period, timestamp_on = timestamp) %>%
    mutate(jersey_number = NA) %>% 
    select(all_of(mpVars))
  
  #combine into df of all players
  match_players <- union(match_players, subOn) %>% union(plyrOn)
  
  #fill in period off and timestamp_off variables
  subOff <- subs %>% select(player.id, period, timestamp) %>%
    rename(period_off = period,
           timestamp_off = timestamp)
  
  plyrOff <- playerOff %>% select(player.id, period, timestamp) %>%
    rename(period_off = period,
           timestamp_off = timestamp)
  
  subOff <- union(subOff, plyrOff)
  
  match_players <- left_join(match_players, subOff, by = "player.id")
  
  match_players <- match_players %>% 
    mutate(period_off = ifelse(is.na(period_off), maxPeriod, period_off),
           timestamp_off = ifelse(is.na(timestamp_off), maxTimestamp, timestamp_off))
  
  return(match_players)
  
  
}




