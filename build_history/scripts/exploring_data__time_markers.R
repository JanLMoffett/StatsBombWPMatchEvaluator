
library(tidyverse)
library(devtools)
library(caret)
library(lubridate)

source("functions_etc/bombViz.R")
#importing colors and themes for plots
#github.com/JanLMoffett/datavizExtras
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")


tm <- read.csv("data/events_timeMarkers.csv")
pi <- read.csv("data/events_playerInfo.csv")
m <- read.csv("data/matches.csv")
matchIDs <- m %>% arrange(match_date, kick_off) %>% pull(match_id)
lu <- read.csv("data/unnested_startingLineups.csv")

#Q's:
#how do i know what game an event is from?    match_id

#paring down vars
nzv <- nearZeroVar(tm, names = T, saveMetrics = T)
zv <- row.names(nzv[nzv$zeroVar == T,])

tm <- tm %>% select(-all_of(zv))

nzv <- nearZeroVar(pi, names = T, saveMetrics = T)
zv <- row.names(nzv[nzv$zeroVar == T,])

pi <- pi %>% select(-all_of(zv))

names(tm)
#[1] "X.1"                  "X"                    "id"                  
#[4] "index"                "period"               "timestamp"           
#[7] "minute"               "second"               "possession"          
#[10] "type.id"              "type.name"            "possession_team.id"  
#[13] "possession_team.name" "play_pattern.id"      "play_pattern.name"   
#[16] "team.id"              "team.name"            "player.id"           
#[19] "player.name"          "position.id"          "position.name"       
#[22] "match_id"             "location.x"           "location.y"          
#[25] "milliseconds"         "ElapsedTime"          "StartOfPossession"   
#[28] "TimeInPoss"           "TimeToPossEnd"        "OpposingTeam"        
#[31] "OpposingTeam.id"      "location_x"           "location_y"  

names(pi)
#[1] "X.1"                           "X"                            
#[3] "id"                            "index"                        
#[5] "period"                        "timestamp"                    
#[7] "minute"                        "second"                       
#[9] "possession"                    "type.id"                      
#[11] "type.name"                     "possession_team.id"           
#[13] "possession_team.name"          "play_pattern.id"              
#[15] "play_pattern.name"             "team.id"                      
#[17] "team.name"                     "tactics.formation"            
#[19] "player.id"                     "player.name"                  
#[21] "position.id"                   "position.name"                
#[23] "substitution.outcome.id"       "substitution.outcome.name"    
#[25] "substitution.replacement.id"   "substitution.replacement.name"
#[27] "match_id"                      "milliseconds"                 
#[29] "ElapsedTime"                   "StartOfPossession"            
#[31] "TimeInPoss"                    "TimeToPossEnd"                
#[33] "OpposingTeam"                  "OpposingTeam.id"


setdiff(names(pi), names(tm))
setdiff(names(tm), names(pi))
#[1] "tactics.formation"             "substitution.outcome.id"      
#[3] "substitution.outcome.name"     "substitution.replacement.id"  
#[5] "substitution.replacement.name"
setdiff(names(tm), names(pi))
#[1] "location.x" "location.y" "location_x" "location_y"

#what has locations?
unique(tm$type.name[which(!is.na(tm$location.x))])
#Referee Ball-Drop

setdiff(tm$location.x, tm$location_x)
setdiff(tm$location.y, tm$location_y)
setdiff(tm$location_x, tm$location.x)
setdiff(tm$location_y, tm$location.y)
#these vars are the same

tm <- tm %>% rename(ballDropLoc_x = location.x,
                    ballDropLoc_y = location.y) %>%
  mutate(location_x = NULL,
         location_y = NULL)

#a plot of all the ball drop locations on the field
plot_pitch(tm %>% filter(type.name == "Referee Ball-Drop"), lineColor = jmbn["mint"]) + bombTurf + 
  geom_point(aes(x = ballDropLoc_x, y = ballDropLoc_y), color = jmbn["rose"]) + 
  labs(title = "Referee Ball-Drop Locations")
#by match

plot_pitch(tm %>% filter(match_id == 3788742), lineColor = jmbn["mint"]) + bombTurf + 
  geom_point(aes(x = ballDropLoc_x, y = ballDropLoc_y, color = factor(match_id)))

sort(head(m$kick_off))
head(tm$timestamp)

#transforming timestamp var so i can plot it on x axis
tm <- tm %>% mutate(timestamp_seconds = seconds(hms(timestamp))) %>% 
  mutate(timestamp_seconds = str_remove(timestamp_seconds, "S")) %>%
  mutate(timestamp_seconds = as.numeric(timestamp_seconds))

#rudimentary matchtime_plot
ggplot(tm) + bombTime + 
  geom_point(aes(x = timestamp_seconds, y = factor(type.name)), 
             shape = "|", size = 4, color = jmbn["periwinkle"]) + 
  facet_wrap(vars(period)) + 
  labs(title = "Distribution of Timestamps by Period")


#i'm trying to figure out how time is structured in the game
tm %>% group_by(period) %>%
  summarize(
    min.ts = min(timestamp_seconds, na.rm = T),
    max.ts = max(timestamp_seconds, na.rm = T)) %>%
  mutate(
    max.tm = max.ts/60
  )
#  period min.ts max.ts max.tm
#1      1      0  3073.   51.2 
#2      2      0  3188.   53.1 
#3      3      0  1219.   20.3 
#4      4      0  1167.   19.4 
#5      5      0   513.   8.54

#first and second half are supposed to be 45 min long, but can be up to ~55 min
#the third and fourth periods are 15-20 min 
#the fifth period is up to 10

#timestamps revert to zero at the start of each new period

#which games have more than 2 periods?
matches_extraTime <- tm %>% group_by(match_id) %>%
  summarize(
    periods = n_distinct(period)
            ) %>%
  filter(periods > 2) %>%
  left_join(m, by = "match_id") %>%
  select(match_id, periods, match_date, 
         home_team.home_team_name, away_team.away_team_name,
         home_score, away_score,
         match_week
         )


ggplot(tm) + bombTime + 
  coord_cartesian(xlim = c(0,3300), ylim = c(0,2000)) + 
  geom_rect(aes(xmin = 0, xmax = 3300, ymin = 0, ymax = 2000), fill = NA, color = jmbn["thistle"])

#getting all the players that appeared in each match
#i need a example match id for testing
matchId_index = 36

#empty df to save data
matchPlayersAll <- data.frame(
  "match_id" = 9999999,
  "team.name" = "dummy",
  "player.id" = 9999,
  "player.name" = "dummy",
  "position.name" = "dummy",
  "type.name" = "dummy"
)

for(this.match in matchIDs){
  #rows from matches, player info, and lineups from this match
  m.this <- m %>% filter(match_id == this.match)
  pi.this <- pi %>% filter(match_id == this.match)
  lu.this <- lu %>% filter(match_id == this.match)
  
  #team names
  teamAway <- m.this$away_team.away_team_name
  teamHome <- m.this$home_team.home_team_name
  
  #dfs of starters for each team
  startersAway <- lu.this %>% 
    filter(team.name == teamAway) %>%
    select(match_id, team.name, player.id, player.name, position.name) %>% 
    mutate(type.name = "Starting XI")
  
  startersHome <- lu.this %>% 
    filter(team.name == teamHome) %>%
    select(match_id, team.name, player.id, player.name, position.name) %>%
    mutate(type.name = "Starting XI")
  
  
  #the nonstarting players that appeared in the game for each team
  #Substitution
  subsAway <- pi.this %>% 
    filter(type.name == "Substitution", team.name == teamAway) %>% 
    select(match_id, team.name, substitution.replacement.id, substitution.replacement.name, position.name, type.name) %>%
    rename(player.id = substitution.replacement.id,
           player.name = substitution.replacement.name)
  
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
  
  nstartersAway <- union(subsAway, playeronAway)
  nstartersHome <- union(subsHome, playeronHome)
  
  #putting starters and nonstarters together to get df of all players in the match for each team
  playersAway <- union(startersAway, nstartersAway)
  playersHome <- union(startersHome, nstartersHome)
  
  matchPlayers <- union(playersAway, playersHome)
  
  matchPlayersAll <- union(matchPlayersAll, matchPlayers)
  
  
}

#removing dummy row and saving
matchPlayersAll <- matchPlayersAll[-1,]
write.csv(matchPlayersAll, "data/matchPlayers.csv")


#I'd like to build a table that has each timestamp in and out for each player, and var for total time on the field
#goal is to build a kind of gantt chart that has time on the x axis and players on the y axis

#want to build a dataset of player appearances 
#with a unique row for each time a player appears in a game, timestamps in and out, stats for the appearance



