
#this is a function to take a match id and get a full list of players on each
#team, along with their transformed timestamps, ready to plot on timeline

#this code originated in exploring_data__time_markers.R

library(tidyverse)
library(devtools)
library(lubridate)
library(caret)


#importing colors and themes for plots
#github.com/JanLMoffett/datavizExtras
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")

source("functions_etc/bombViz.R")

#subsets of events dataset
tm <- read.csv("data/events_timeMarkers.csv")
pi <- read.csv("data/events_playerInfo.csv")
shots <- read.csv("data/events_shot.csv")
#put together the events i want to plot
ev1 <- rbind(tm, pi, shots)

#event types in the set
unique(ev1$type.name)
#[1] "Half Start"        "Half End"          "Injury Stoppage"   "Referee Ball-Drop" "Starting XI"      
#[6] "Substitution"      "Player Off"        "Player On"         "Shot"             


this.match = matchIDs[34]

t(m %>% filter(match_id == this.match))



#a function to take a timestamp string in "00:00:00.000" format and covert to numeric seconds
timestamp_to_seconds <- function(timestampStr){
  
  r1 <- as.numeric(seconds(hms(timestampStr)))
  
  return(r1)
  
}

#transforming timestamp var so i can plot it on x axis
ev1 <- ev1 %>% mutate(timestamp_seconds = timestamp_to_seconds(timestamp))
nzv <- nearZeroVar(ev1, saveMetrics = T)
remove <- which(nzv$zeroVar == T)
ev1 <- ev1 %>% select(-all_of(remove))


set.seed(3); this.match = matchIDs[sample(1:length(matchIDs), 1)]

#filter down to the match in question----------------------------------------------
ev2 <- ev1 %>% filter(match_id == this.match)

#info about this match (df with 1 row)
match_info <- m %>% filter(match_id == this.match)

#info about the periods in this match
per_info <- ev2 %>% group_by(period) %>%
  summarize(per_end = max(timestamp_seconds, na.rm = T)
            ) %>%
  mutate(per_div = cumsum(per_end))

#team names
teamAway <- match_info$away_team.away_team_name
teamHome <- match_info$home_team.home_team_name

#info about the starting xi for each team
starters_info <- lu %>% filter(match_id == this.match)
starters_info <- starters_info %>% left_join(posAbbr[,2:3], by = c("position.name" = "position_name"))
starterIDs <- starters_info %>% pull(player.id)

subs_info <- ev2 %>% filter(type.name == "Substitution")

#players who came in as subs
subbedOnIDs <- subs_info %>% pull(substitution.replacement.id)


#dfs of starters for each team
startersAway <- starters_info %>% filter(team.name == teamAway)
startersHome <- starters_info %>% filter(team.name == teamHome)

#the nonstarting players that appeared in the game for each team
#Substitution
subsAway <- subs_info %>% filter(team.name == teamAway)
subsHome <- subs_info %>% filter(team.name == teamHome)


subsHome <- pi.this %>% 
  filter(type.name == "Substitution", team.name == teamHome) %>% 
  select(match_id, team.name, substitution.replacement.id, substitution.replacement.name, position.name, type.name) %>%
  rename(player.id = substitution.replacement.id,
         player.name = substitution.replacement.name)

#Player On
playeronAway <- pi.this %>% 
  filter(type.name == "Player On", team.name == teamAway) %>%
  select(match_id, team.name, player.id, player.name, position.name, type.name)

playeronHome <- pi.this %>% 
  filter(type.name == "Player On", team.name == teamHome) %>%
  select(match_id, team.name, player.id, player.name, position.name, type.name)

nonstartersAway <- union(subsAway, playeronAway)
nonstartersHome <- union(subsHome, playeronHome)

#putting starters and nonstarters together to get df of all players in the match for each team
playersAway <- union(startersAway, nstartersAway)
playersHome <- union(startersHome, nstartersHome)

matchPlayers <- union(playersAway, playersHome)

matchPlayersAll <- union(matchPlayersAll, matchPlayers)

#empty df for all players in match
matchPlayersAll <- data.frame(
  "match_id" = 9999999,
  "team.name" = "dummy",
  "player.id" = 9999,
  "player.name" = "dummy",
  "position.name" = "dummy",
  "type.name" = "dummy"
)


#removing dummy row and saving
matchPlayersAll <- matchPlayersAll[-1,]

matchPlayersAll


max_ts <- max(ev1 %>% filter(match_id == this.match) %>% pull(timestamp_seconds))
max_per <- max(ev1 %>% filter(match_id == this.match) %>% pull(period))

#see if any players in the match have "player off" type
playerOffIDs <- ev1 %>% filter(match_id == this.match, type.name == "Player Off") %>% pull(player.id)
subbedOffIDs <- ev1 %>% filter(match_id == this.match, type.name == "Substitution") %>% pull(player.id)

mpa <- matchPlayersAll %>% mutate(is_playerOff = ifelse(player.id %in% playerOffIDs, 1, 0)) %>% 
  mutate(is_subbedOff = ifelse(player.id %in% subbedOffIDs, 1, 0))



