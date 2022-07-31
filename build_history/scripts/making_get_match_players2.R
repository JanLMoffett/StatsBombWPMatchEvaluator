
#this is a function to take a match id and get a full list of players on each
#team

library(tidyverse)
library(devtools)
library(lubridate)
library(caret)


#importing colors and themes for plots
#github.com/JanLMoffett/datavizExtras
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")

source("functions_etc/bombViz.R")
source("functions_etc/timestamp_to_seconds.R")

#matches dataset
m <- read.csv("big_data/dbb_matches.csv")
#list of unique match ID's from UEFA CL 2020
matchIDs <- m %>% arrange(match_date, kick_off) %>% pull(match_id)

#events dataset
ev <- read.csv("big_data/dbb_events.csv")

#unnested starting XI dataframes
lu <- read.csv("big_data/dbb_events_startingXI.csv")

#position display table i made in making_plot_pos_on_pitch.R
pd <- read.csv("functions_etc/positionDisplay.csv")


# ~ ~ ~ ~ ~ ~ ~ # Pick a Match ID # ~ ~ ~ ~ ~ ~ ~ #
#b <- sample(1:51, 1)
b = 26
thisMatchID = matchIDs[b]
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ #

#filter sets to chosen match id
this.match <- m %>% filter(match_id == thisMatchID)
this.ev <- ev %>% filter(match_id == thisMatchID)

#move to another file 
# |
# v
#narrow down variables to display
matchDisplayVars <- c(
  "competition.competition_name",
  "season.season_name",
  "competition_stage.name",
  "stadium.name","stadium.country.name",
  "referee.name",
  "match_date", "kick_off", 
  "away_team.away_team_name", "away_team.away_team_group",
  "away_score", 
  "home_team.home_team_name", "home_team.home_team_group",
   "home_score")

#display info about the match
t(this.match %>% select(all_of(matchDisplayVars)))
# ^
# |

#Starters for match
starters <- lu %>% filter(match_id == thisMatchID)

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
#[1] "id"            "match_id"      "team.id"       "team.name"     "jersey_number"
#[6] "player.id"     "player.name"   "position.id"   "position.name" "period_on"   timestamp_on" 
#[11] "period_off"  "timestamp_off"   "type.name"
match_players <- match_players %>% select(all_of(mpVars))

#add players who came in as substitutions to the df
subOn <- subs %>% select(id, match_id, team.id, team.name, 
                         substitution.replacement.id, substitution.replacement.name, 
                         position.id, position.name, period, timestamp, type.name) %>%
  rename(period_on = period,
         timestamp_on = timestamp,
         player.id = substitution.replacement.id,
         player.name = substitution.replacement.name) %>%
  mutate(jersey_number = NA)

subOn <- subOn %>% select(all_of(mpVars))

plyrOn <- playerOn %>% select(id, match_id, team.id, team.name, 
                              player.id, player.name, 
                              position.id, position.name, period, timestamp, type.name) %>%
  rename(period_on = period, timestamp_on = timestamp) %>%
  mutate(jersey_number = NA)

plyrOn <- plyrOn %>% select(all_of(mpVars))

#combine into df of all players
match_players <- union(match_players, subOn) %>% union(plyrOn)

match_players


#fill in period off and timestamp_off variables

subOff <- subs %>% select(player.id, period, timestamp) %>%
  rename(period_off = period,
         timestamp_off = timestamp)

plyrOff <- playerOff %>% select(player.id, period, timestamp) %>%
  rename(period_off = period,
         timestamp_off = timestamp)

subOff <- union(subOff, plyrOff)

match_players <- left_join(match_players, subOff, by = "player.id")

maxPeriod <- max(this.ev$period, na.rm = T)
maxTimestamp <- max(this.ev$timestamp[which(this.ev$period == maxPeriod)])

match_players <- match_players %>% 
  mutate(period_off = ifelse(is.na(period_off), maxPeriod, period_off),
         timestamp_off = ifelse(is.na(timestamp_off), maxTimestamp, timestamp_off))


