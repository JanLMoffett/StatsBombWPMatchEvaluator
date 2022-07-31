

library(tidyverse)
library(devtools)
library(caret)
library(lubridate)

source("functions_etc/bombViz.R")
#importing colors and themes for plots
#github.com/JanLMoffett/datavizExtras
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/datavizExtras.R")

m <- read.csv("data/matches.csv")

#this dataset was made in scripts/exploring_data__time_markers.R
#it contains all plays appearing in each match, not just starters
mp <- read.csv("data/matchPlayers.csv")

tm <- read.csv("data/events_timeMarkers.csv")
pi <- read.csv("data/events_playerInfo.csv")
posAbbr <- read.csv("data/positionAbbrev.csv")
names(posAbbr)[1] <- "position_number"

#join abbreviations for position names to player dfs
mp <- mp %>% left_join(posAbbr, by = c("position.name" = "position_name"))
pi <- pi %>% left_join(posAbbr, by = c("position.name" = "position_name"))


#transforming timestamp var so i can plot it on x axis
tm <- tm %>% mutate(timestamp_seconds = seconds(hms(timestamp))) %>% 
  mutate(timestamp_seconds = str_remove(timestamp_seconds, "S")) %>%
  mutate(timestamp_seconds = as.numeric(timestamp_seconds))

nzv <- nearZeroVar(tm, names = T, saveMetrics = T)
remove <- row.names(nzv[nzv$zeroVar == T,])
tm <- tm %>% select(-all_of(remove)) %>% arrange(period, timestamp_seconds)

pi <- pi %>% mutate(timestamp_seconds = seconds(hms(timestamp))) %>%
  mutate(timestamp_seconds = str_remove(timestamp_seconds, "S")) %>%
  mutate(timestamp_seconds = as.numeric(timestamp_seconds))

nzv <- nearZeroVar(pi, names = T, saveMetrics = T)
remove <- row.names(nzv[nzv$zeroVar == T,])
pi <- pi %>% select(-all_of(remove)) %>% arrange(period, timestamp_seconds)


#i need a match with more than 2 periods to use as an example
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
matches_extraTime %>% left_join(m, by = "match_id")

#id of match i'm testing code with
testMatch <- matches_extraTime$match_id[8]

#------below is the loop contents

#filter all datasets to this matchID (testMatch)
this.match <- m %>% filter(match_id == testMatch)
these.players <- mp %>% filter(match_id == testMatch)
these.timeMarkers <- tm %>% filter(match_id == testMatch)
these.playersOnOff <- pi %>% filter(match_id == testMatch)

#get away and home team names
awayTmName <- this.match$away_team.away_team_name
homeTmName <- this.match$home_team.home_team_name
#get a df of players for each team
awayPlayers <- these.players %>% filter(team.name == awayTmName)
homePlayers <- these.players %>% filter(team.name == homeTmName)

#number of away and home players
nA <- dim(awayPlayers)[1]
nH <- dim(homePlayers)[1]

#number of periods
#end ts (seconds) of each period
max(these.timeMarkers$period)

perEnds <- these.timeMarkers %>% filter(type.name == "Half End") %>%
  group_by(period) %>%
  summarize(
    per_end_seconds = first(timestamp_seconds),
  ) %>% 
  pull(per_end_seconds)

#-----PLAYER BOX-----
#player box acts as an index to the timeplot, is to the left of the timeplot in display

#x coordinates for data column justifications
positionX = 37 # adj = 1
jerseyX = 44 # adj = 0.5
nameX = 50 # adj = 0

#player box
ggplot() + shUEFA_theme +
  coord_fixed(xlim = c(0,80), ylim = c(17, 0), ratio = 5) +
  geom_rect(aes(xmin = 0, xmax = 80, ymin = 0, ymax = nA+1), 
            color = shUEFA["blueLt"], fill = NA) + 
  geom_segment(aes(x = rep.int(positionX, nA), xend = rep.int(80, nA), y = 1:nA, yend = 1:nA), color = shUEFA["purple"], linetype = 3, size = 1) +
  annotate("text", x = rep.int(nameX, nA), y = 1:nA, label = awayPlayers$player.name, color = shUEFA["yellow"], adj = 0) +
  annotate("text", x = rep.int(positionX, nA), y = 1:nA, label = awayPlayers$position.name, color = shUEFA["yellow"], adj = 1) + 
  annotate("text", x = rep.int(jerseyX, nA), y = 1:nA, label = as.character(1:nA), color = shUEFA["yellow"], adj = 0.5)

#-----TIMEPLOT BOX-----


pi.per1 <- these.playersOnOff %>% filter(period == 1)
pi.per2 <- these.playersOnOff %>% filter(period == 2)
pi.per3 <- these.playersOnOff %>% filter(period == 3)
pi.per4 <- these.playersOnOff %>% filter(period == 4)
pi.per5 <- these.playersOnOff %>% filter(period == 5)


subs <- pi.per1 %>% filter(type.name == "Substitution", team.name == awayTmName)
p_off <- pi.per1 %>% filter(type.name == "Player Off", team.name == awayTmName)
p_on <- pi.per1 %>% filter(type.name == "Player On", team.name == awayTmName)

dim(subs)[1] > 0
dim(p_off)[1] > 0
dim(p_on)[1] > 0
#timeplot box for first period
ggplot() + 
  shUEFA_theme + 
  coord_cartesian(xlim = c(0, perEnds[1]), ylim = c(0, nA+1)) + 
  geom_rect(aes(xmin = 0, xmax = perEnds[1], ymin = 0, ymax = nA+1), fill = NA, color = shUEFA["yellow"]) + 
  geom_segment(aes(x = rep.int(0, nA), xend = rep.int(perEnds[1], nA), y = 1:nA, yend = 1:nA), color = transpa(shUEFA["yellow"],120), linetype = 2) + 
  annotate("text", y = c(-0.5, -0.5), x = c(0, perEnds[1]), label = c("0", as.character(perEnds[1])), color = shUEFA["yellow"], size = 3)


#timeplot box for first period with player box

#left boundary of the plot
leftBound <- -2500

#position text
positionX <- -2100
jerseyX <- -1800
nameX <- -1500

perDivs <- c(0, perEnds[1], sum(perEnds[1:2]), sum(perEnds[1:3]), sum(perEnds[1:4]), sum(perEnds[1:5]))


awayBase <- ggplot() + shUEFA_theme + 
  theme(axis.title = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(), 
        axis.text = element_blank()) + 
  
  coord_cartesian(xlim = c(leftBound, sum(perEnds)), ylim = c(-(nH+4), nA+1)) + 
  
  
  #away player box
  geom_rect(aes(xmin = leftBound, xmax = 0, ymin = 0, ymax = nA+1), fill = NA, color = shUEFA["yellow"]) + 
  geom_segment(aes(x = rep.int(positionX, nA), xend = rep.int(0, nA), 
                   y = 1:nA, yend = 1:nA), 
               color = shUEFA["blueMed"], linetype = 3, size = 1) +
  #names
  annotate("text", x = rep.int(nameX, nA), y = 1:nA, label = awayPlayers$player.name, 
           color = shUEFA["yellow"], adj = 0, size = 3.25) +
  #positions
  annotate("text", x = rep.int(positionX, nA), y = 1:nA, label = awayPlayers$position_abbr, 
           color = shUEFA["yellow"], adj = 1, size = 3.25) + 
  #jersey numbers
  annotate("text", x = rep.int(jerseyX, nA), y = 1:nA, label = as.character(1:nA), color = shUEFA["yellow"], adj = 0.5, size = 3.25) +
  
  #home player box
  geom_rect(aes(xmin = leftBound, xmax = 0, ymin = -(nH+1), ymax = 0), fill = NA, color = shUEFA["yellow"]) + 
  geom_segment(aes(x = rep.int(positionX, nH), xend = rep.int(0, nH), 
                   y = -1:-nH, yend = -1:-nH), 
               color = shUEFA["blueMed"], linetype = 3, size = 1) +
  #names
  annotate("text", x = rep.int(nameX, nH), y = -1:-nH, label = homePlayers$player.name, 
           color = shUEFA["yellow"], adj = 0, size = 3.25) +
  #positions
  annotate("text", x = rep.int(positionX, nH), y = -1:-nH, label = homePlayers$position_abbr, 
           color = shUEFA["yellow"], adj = 1, size = 3.25) + 
  #jersey numbers
  annotate("text", x = rep.int(jerseyX, nH), y = -1:-nH, label = as.character(1:nH), color = shUEFA["yellow"], adj = 0.5, size = 3.25) +
  
  #away time box
  geom_rect(aes(xmin = 0, xmax = sum(perEnds), ymin = 0, ymax = nA+1), fill = NA, color = shUEFA["yellow"]) + 
  geom_segment(aes(x = rep.int(0, nA), xend = rep.int(sum(perEnds), nA), y = 1:nA, yend = 1:nA), color = shUEFA["blueMed"], linetype = 2) + 
  
  #home time box
  geom_rect(aes(xmin = 0, xmax = sum(perEnds), ymin = -(nH+1), ymax = 0), fill = NA, color = shUEFA["yellow"]) + 
  geom_segment(aes(x = rep.int(0, nH), xend = rep.int(sum(perEnds), nH), y = -1:-nH, yend = -1:-nH), color = shUEFA["blueMed"], linetype = 2) + 
  
  #period dividers
  geom_segment(aes(x = perDivs, xend = perDivs, y = nA+1, yend = -(nH+1)), color = shUEFA["yellow"]) +
  
  #period boxes
  geom_rect(aes(xmin = perDivs[1], xmax = perDivs[2], ymin = -(nH+3), ymax = -(nH+1)), color = shUEFA["yellow"], fill = shUEFA["purple"]) + 
  geom_rect(aes(xmin = perDivs[2], xmax = perDivs[3], ymin = -(nH+3), ymax = -(nH+1)), color = shUEFA["yellow"], fill = shUEFA["blueDk"]) + 
  geom_rect(aes(xmin = perDivs[3], xmax = perDivs[4], ymin = -(nH+3), ymax = -(nH+1)), color = shUEFA["yellow"], fill = shUEFA["purple"]) + 
  geom_rect(aes(xmin = perDivs[4], xmax = perDivs[5], ymin = -(nH+3), ymax = -(nH+1)), color = shUEFA["yellow"], fill = shUEFA["blueDk"]) + 
  geom_rect(aes(xmin = perDivs[5], xmax = perDivs[6], ymin = -(nH+3), ymax = -(nH+1)), color = shUEFA["yellow"], fill = shUEFA["purple"]) + 
  
  #period box labels
  annotate("text", x = perDivs[2]-(perDivs[2]-perDivs[1])/2, y = -(nH+2), label = "1", color = shUEFA["yellow"]) +
  annotate("text", x = perDivs[3]-(perDivs[3]-perDivs[2])/2, y = -(nH+2), label = "2", color = shUEFA["yellow"]) +
  annotate("text", x = perDivs[4]-(perDivs[4]-perDivs[3])/2, y = -(nH+2), label = "3", color = shUEFA["yellow"]) +
  annotate("text", x = perDivs[5]-(perDivs[5]-perDivs[4])/2, y = -(nH+2), label = "4", color = shUEFA["yellow"]) +
  annotate("text", x = perDivs[6]-(perDivs[6]-perDivs[5])/2, y = -(nH+2), label = "5", color = shUEFA["yellow"]) +
  
  #team name boxes
  geom_rect(aes(xmin = leftBound - 300, xmax = leftBound, ymin = 0, ymax = nA+1), color = shUEFA["yellow"], fill = shUEFA["blueDk"]) + 
  geom_rect(aes(xmin = leftBound - 300, xmax = leftBound, ymin = -(nH+1), ymax = 0), color = shUEFA["yellow"], fill = shUEFA["blueDk"]) +
  
  #team name labels
  annotate("text", x = leftBound - 150, y = -(nH/2), angle = 90, label = paste0("Home | ", homeTmName), size = 2.9, color = shUEFA["yellow"]) + 
  annotate("text", x = leftBound - 150, y = nA/2, angle = 90, label = paste0("Away | ", awayTmName), size = 2.9, color = shUEFA["yellow"])
  
awayBase

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





