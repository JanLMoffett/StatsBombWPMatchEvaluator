

library(tidyverse)
library(devtools)
library(lubridate)


#importing colors and themes for plots
#github.com/JanLMoffett/datavizExtras
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/datavizExtras.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")


source("functions_etc/bombViz.R")
source("functions_etc/timestamp_to_seconds.R")
source("functions_etc/get_match_players.R")
#reading in table i just made above
posAb <- read.csv("functions_etc/positionDisplay.csv")

m <- read.csv("big_data/dbb_matches.csv", encoding = "UTF-8")
ev <- read.csv("big_data/dbb_events.csv", encoding = "latin1")
#unnested starting XI dataframes
lu <- read.csv("big_data/dbb_events_startingXI.csv", encoding = "latin1")

rel_ev <- read.csv("big_data/dbb_events_relatedEvents.csv")


plot_colors(shUEFA)

#match ID's
matchIDs <- unique(m$match_id)

#event types
eventTypes <- unique(ev$type.name)
#positions
posTypes <- unique(ev$position.name)


# @>->->- <{{ @ }}> -<-<-<@ @>->->- <{{ @ }}> -<-<-<@ @>->->- <{{ @ }}> -<-<-<@
#             what types of events are related to which positions?
#===============================================================================
# @>->->- <{{ @ }}> -<-<-<@ @>->->- <{{ @ }}> -<-<-<@ @>->->- <{{ @ }}> -<-<-<@
pos_x_events <- table(ev$type.name, ev$position.name)

pos_x_events["50/50",]
pos_x_events["Clearance",c("Center Forward", "Left Center Forward", "Right Center Forward")]

posSum <- ev %>% group_by(type.name, position.name) %>% tally() %>% arrange(desc(n))

unique(posSum$type.name)
print(posSum, n = 50)
print(posSum %>% filter(type.name == "Shot") %>% arrange(type.name, desc(n)), n = 50)
print(posSum %>% filter(type.name == "Pass") %>% arrange(type.name, desc(n)), n = 50)
print(posSum %>% filter(type.name == "Interception") %>% arrange(type.name, desc(n)), n = 50)
print(posSum %>% filter(type.name == "Miscontrol") %>% arrange(type.name, desc(n)), n = 50)

#i also need to know what types of events have the most related events, and what event types
# are related
rel_ev %>% select(id, num_related_events) %>% arrange(desc(num_related_events)) %>% slice_head(n = 20)

plot_pos_on_pitch()

# Bar chart of event types in events dataset
ggplot(ev) + 
  shUEFA_theme + 
  theme(plot.title = element_text(size = 18), 
        axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_bar(aes(type.name), fill = transpa(shUEFA["orange"], 80), color = shUEFA["orange"]) + 
  labs(title = "Frequency of Event Types")

# Bar chart of positions in events dataset
ggplot(ev) + 
  shUEFA_theme + 
  theme(plot.title = element_text(size = 18), 
        axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_bar(aes(position.name), fill = transpa(shUEFA["orange"], 50), color = shUEFA["orange"]) + 
  labs(title = "Frequency of Positions")


# Bar Charts of Event Types for each Position
plotForPos <- function(posName){
  ggplot(ev %>% filter(position.name == posName)) + 
    shUEFA_theme + 
    theme(plot.title = element_text(size = 18), 
          axis.text.x = element_text(angle = 90, hjust = 1)) + 
    geom_bar(aes(type.name), fill = transpa(shUEFA["blueLt"], 50), color = shUEFA["blueLt"]) + 
    labs(title = paste0(posName, ": Frequency of Event Types"))
}

posAbs <- posAb %>% select(position.name, position_abbr) %>% arrange(position_abbr)

posAbs <- matrix(posAbs$position.name, ncol = 1)
row.names(posAbs) <- posAbs$position_abbr

#center forwards
plotForPos(posAbs["CF",1])
plotForPos(posAbs["LCF", 1])
plotForPos(posAbs["RCF", 1])

#

# 2D Binned Plots

ggplot(ev) + shUEFA_theme + theme(plot.title = element_text(size = 18), axis.text.x = element_text(angle = 90)) + 
  stat_bin_2d(aes(x = type.name, y = position.name)) + 
  scale_fill_binned(type = "viridis") +
  #scale_fill_steps(low = shUEFA["blueNavy"], high = shUEFA["blueSky"]) + 
  labs(title = "Frequency of Event Types by Position")
#this plot is getting thrown off by imbalance in frequencies of some events 

samePlot_diffData <- function(plotData){
  
  ggplot(plotData) + shUEFA_theme + theme(plot.title = element_text(size = 18), axis.text.x = element_text(angle = 90)) + 
    stat_bin_2d(aes(x = type.name, y = position.name)) + 
    scale_fill_steps(low = shUEFA["blueNavy"], high = shUEFA["blueSky"]) + 
    labs(title = "Frequency of Event Types by Position")
  
}

#need to group events by frequency, so heatmap is more informative
#Ball Receipt, Carry, and Pass need to be peeled off first
samePlot_diffData(ev %>% filter(type.name %in% c("Pass", "Ball Receipt*", "Carry"))) + 
  labs(title = "Frequency of Event Types by Position")
#Right and Left Center Backs do the most of all three
#what does it look like without them?
samePlot_diffData(ev %>% filter(type.name %in% c("Pass","Ball Receipt*","Carry"), !(position.name %in% c("Right Center Back", "Left Center Back"))))


# @>->->- <{{ @ }}> -<-<-<@ @>->->- <{{ @ }}> -<-<-<@ @>->->- <{{ @ }}> -<-<-<@


#want to look at interceptions and related events
interceptions <- ev %>% filter(type.name == "Interception")
#join to related events
ic <- interceptions %>% left_join(rel_ev, by = "id")

ic <- ic %>% arrange(match_id) %>% group_by(match_id) %>% 
  mutate(match_interceptions = n()) %>% ungroup()

not_all_na <- function(x) any(!is.na(x))
not_any_na <- function(x) all(!is.na(x))

ic <- ic %>% select(where(not_all_na))
ic <- ic %>% mutate(timestamp_seconds = timestamp_to_seconds(timestamp))

ic <- get_cumulative_match_seconds(ic)

#picking a match to focus on
thisMatchID <- unique(ic$match_id)[25]

this.ic <- ic %>% filter(match_id == thisMatchID)
this.m <- m %>% filter(match_id == thisMatchID)

#info about this match
t(this.m)

this.mp <- get_match_players(thisMatchID, m, ev, lu)
this.mp

#-----PLAYER BOX-----

this.mp <- this.mp %>% group_by(team.name) %>% arrange()

plot_player_box <- function(matchPlayersDF){
  
  #x coordinates for data column justifications
  positionX = 37 # adj = 1
  jerseyX = 44 # adj = 0.5
  nameX = 50 # adj = 0
  
  ggp <- ggplot() + shUEFA_theme + theme(axis.text = element_blank(), axis.ticks = element_blank(),
                                  axis.line = element_blank(), axis.title = element_blank()) +
    
    geom_rect(aes(xmin = 0, xmax = 80, ymin = 0, ymax = dim(matchPlayersDF)[1]+1), 
              color = shUEFA["blueLt"], fill = NA) + 
    
    geom_segment(aes(x = rep.int(positionX, dim(matchPlayersDF)[1]), 
                     xend = rep.int(80, dim(matchPlayersDF)[1]), 
                     y = 1:dim(matchPlayersDF)[1], 
                     yend = 1:dim(matchPlayersDF)[1]), 
                 color = shUEFA["purple"], linetype = 3, size = 1) +
    
    annotate("text", x = rep.int(nameX, dim(matchPlayersDF)[1]), y = 1:dim(matchPlayersDF)[1], 
             label = matchPlayersDF$player.name, color = shUEFA["yellow"], adj = 0) +
    
    annotate("text", x = rep.int(positionX, dim(matchPlayersDF)[1]), y = 1:dim(matchPlayersDF)[1], 
             label = matchPlayersDF$position.name, color = shUEFA["yellow"], adj = 1) + 
    
    annotate("text", x = rep.int(jerseyX, dim(matchPlayersDF)[1]), y = 1:dim(matchPlayersDF)[1], 
             label = as.character(1:dim(matchPlayersDF)[1]), color = shUEFA["yellow"], adj = 0.5)
  
  return(ggp)
  
}

away_tm <- this.m$away_team.away_team_name
home_tm <- this.m$home_team.home_team_name
#player box
plot_player_box(this.mp %>% filter(team.name == away_tm)) + labs(title = away_tm)
plot_player_box(this.mp %>% filter(team.name == home_tm)) + labs(title = home_tm)

plot_pitch(this.ic, lineColor = shUEFA["blueMed"]) + shUEFA_theme + 
  geom_point(aes(location.x, location.y, color = team.name))

#pick an interception
icx <- this.ic[22,]

icx.re1 <- ev %>% filter(id == icx$related_event_1) %>% select(where(not_all_na))
icx.re2 <- ev %>% filter(id == icx$related_event_2) %>% select(where(not_all_na))
#no second related event


#since they're on different teams, their coord are flipped
#need to transform opposing location coord

transform_opp_loc_x <- function(locX) 120-locX
transform_opp_loc_y <- function(locY) 80-locY

transform_opp_loc_x(icx.re1$pass.end_location.x)

pass.x1 <- transform_opp_loc_x(icx.re1$location.x)
pass.y1 <- transform_opp_loc_y(icx.re1$location.x)

pass.x2 <- transform_opp_loc_x(icx.re1$pass.end_location.x)
pass.y2 <- transform_opp_loc_y(icx.re1$pass.end_location.y)

#related event was a pass
#plot of pass interception
plot_pitch(lineColor = shUEFA["blueMed"]) + shUEFA_theme + 
  theme(axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(title = "Pass Interception") + 
  annotate("point", x = c(pass.x1, pass.x2), y = c(pass.y1, pass.y2), color = shUEFA["blueLt"], shape = 3, size = 3) +
  annotate("text", x = pass.x1, y = pass.y1-3, color = shUEFA["blueLt"], label = icx.re1$player.name) +
  annotate("segment", x = pass.x1, xend = pass.x2, y = pass.y1, yend = pass.y2, color = shUEFA["blueLt"], linetype = 2) +
  annotate("point", x = icx$location.x, y = icx$location.y, color = shUEFA["orangeDk"], shape = 13, size = 5) + 
  annotate("text", x = icx$location.x, y = icx$location.y + 4, label = icx$player.name, color = shUEFA["orangeDk"])
  
