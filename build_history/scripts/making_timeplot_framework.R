
library(tidyverse)
library(devtools)
library(lubridate)


#importing colors and themes for plots
#github.com/JanLMoffett/datavizExtras
#source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
#source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")
#source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/datavizExtras.R")


source("functions_etc/bombViz.R")
source("functions_etc/timestamp_to_seconds.R")
source("functions_etc/get_match_players.R")


posAb <- read.csv("functions_etc/positionDisplay.csv")                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 

m <- read.csv("big_data/dbb_matches.csv", encoding = "UTF-8")
ev <- read.csv("big_data/dbb_events.csv", encoding = "latin1")
#unnested starting XI dataframes
lu <- read.csv("big_data/dbb_events_startingXI.csv", encoding = "latin1")

rel_ev <- read.csv("big_data/dbb_events_relatedEvents.csv")


#transforming timestamp var so i can plot it on x axis
ev <- ev %>% mutate(timestamp_seconds = timestamp_to_seconds(timestamp))
ev <- get_cumulative_match_seconds(ev)

matchIDs <- unique(m$match_id)

#select a match
thisMatchID <- matchIDs[sample(1:length(matchIDs), 1)]

#------below is the loop contents

#filter all datasets to this matchID (testMatch)
this.m <- m %>% filter(match_id == thisMatchID)
this.ev <- ev %>% filter(match_id == thisMatchID)

#get away and home team names
awayName <- this.m$away_team.away_team_name
homeName <- this.m$home_team.home_team_name

#get players for the match
mp <- get_match_players(thisMatchID, m, ev, lu)
#join position abbreviations
mp <- mp %>% left_join(posAb %>% select(position.id, position_abbr), by = "position.id")


#get time summary for match
pers <- get_period_summary(this.ev)
pers <- pers %>% mutate(cum_seconds = cumsum(total_seconds))

mp <- get_cumulative_ts_on_off(mp, pers)

#number of away and home players
nA <- dim(mp %>% filter(team.name == awayName))[1]
nH <- dim(mp %>% filter(team.name == homeName))[1]

plA <- mp %>% filter(team.name == awayName) %>% arrange(position.id, ts_on_seconds)
plH <- mp %>% filter(team.name == homeName) %>% arrange(position.id, ts_on_seconds)

playerName_x = -1000
jerseyNum_x = -1300
position_x = -1600

ggplot() + shUEFA_theme + 
  #big rectangle
  geom_rect(aes(xmin = -2000, xmax = max(pers$cum_seconds), ymin = 0, ymax = (nA + nH + 2)),
            color = shUEFA["blueMed"], fill = NA) + 
  #player box
  geom_rect(aes(xmin = -2000, xmax = 0, ymin = 0, ymax = (nA + nH + 2)),
            color = shUEFA["blueMed"], fill = NA) + 
  #timeline box
  geom_rect(aes(xmin = 0, xmax = max(pers$cum_seconds), ymin = 0, ymax = (nA + nH + 3)),
            color = shUEFA["blueMed"], fill = NA) + 
  
  #away player text
  annotate("text", x = rep.int(playerName_x, nA), y = (nA+nH+1):(nH+2), 
           label = str_trunc(plA$player.name, 20, side = "right"), hjust = 0, color = shUEFA["blueLt"]) + 
  #jersey number
  annotate("text", x = rep.int(jerseyNum_x, nA), y = (nA+nH+1):(nH+2), label = plA$jersey_number, hjust = 0.5, color = shUEFA["blueLt"]) + 
  #position
  annotate("text", x = rep.int(position_x, nA), y = (nA+nH+1):(nH+2), label = plA$position_abbr, hjust = 1, color = shUEFA["blueLt"]) + 
  
  #home player text
  annotate("text", x = rep.int(playerName_x, nH), y = nH:1, 
           label = str_trunc(plH$player.name, 20, side = "right"), hjust = 0, color = shUEFA["blueLt"]) + 
  #jersey number
  annotate("text", x = rep.int(jerseyNum_x, nH), y = nH:1, label = plH$jersey_number, hjust = 0.5, color = shUEFA["blueLt"]) + 
  #position
  annotate("text", x = rep.int(position_x, nH), y = nH:1, label = plH$position_abbr, hjust = 1, color = shUEFA["blueLt"]) + 
  
  #line between teams
  geom_segment(aes(x = -2000, xend = max(pers$cum_seconds), y = nH+1, yend = nH+1), color = shUEFA["blueMed"]) + 
  
  #player lines
  geom_segment(aes(x = rep.int(position_x, (nA+nH+1)), xend = rep.int(max(pers$cum_seconds), (nA+nH+1)), y = 1:(nA+nH+1), yend = 1:(nA+nH+1)), 
               linetype = 3, color = shUEFA["purple"]) + 
  
  #on pitch lines - tneed to work off of index of df to keep names and numbers together
  geom_segment(aes(x = plH$ts_on_cum_seconds, xend = plH$ts_off_cum_seconds, y = nH:1, yend = nH:1), 
               linetype = 1, color = shUEFA["blueLt"]) +

  geom_segment(aes(x = plA$ts_on_cum_seconds, xend = plA$ts_off_cum_seconds, y = (nH+1+nA):(nH+2), yend = (nH+1+nA):(nH+2)), 
               linetype = 1, color = shUEFA["blueLt"]) + 
  
  #period markers
  geom_segment(aes(x = pers$cum_seconds, xend = pers$cum_seconds, y = rep.int(0, length(pers$cum_seconds)), 
                   yend = rep.int((nH+2+nA), length(pers$cum_seconds))), 
               color = shUEFA["blueMed"])




























