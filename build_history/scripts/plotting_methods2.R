
library(StatsBombR)
library(tidyverse)
library(devtools)
library(lubridate)

source("app/app_functions/scrape_StatsBomb.R")
source("app/app_functions/bombViz.R")
source("app/app_functions/timestamp_to_seconds.R")
source("app/app_functions/get_match_players2.R")
source("app/app_functions/get_match_info_table.R")

#position abbreviations and display coordinates
posAb <- read.csv("app/app_functions/positionDisplay.csv")


# ||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=
#                               Get Data
# ||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=

#since there is only one competition used in hackathon data, pulling data from
#api isn't reactive, just once at the beginning.  ideally, dashboard will be
#usable with any available StatsBomb360 competition dataset

#Matches
m <- scrape_matches(compName = "UEFA Euro")
#m <- read.csv("app_data/dbb_matches.csv")

#Events (with nested vars)
ev_og <- scrape_events(m)

#Unnested starting lineups
lu <- get_startingXI(ev_og)
#lu <- read.csv("app_data/dbb_events_startingXI.csv")

#related events
rel_ev <- get_related_events(ev_og)
#write.csv(rel_ev, "big_data/relatedEventsLong.csv")

#events without nested vars
ev <- get_stripped_events(ev_og)
#ev <- read.csv("app_data/dbb_events.csv")

ev_og <- NULL

#list of unique match_id values 
matchIDs <- unique(m$match_id)
#all data used in app will be filtered by input match_id value 
#each time a new match id is selected

#event types are standing in for statviews, which will be groups of event types tbd
eventTypes <- unique(ev$type.name)

#transforming timestamp var so i can plot it on x axis
ev <- ev %>% mutate(timestamp_seconds = timestamp_to_seconds(timestamp))


#-------------------------------------------------------------------------


# i need to look at the relationship between possession and location

#picking a match example to build with
matchIDs
names(m)
m %>% select(match_id, match_date, away_team.away_team_name, away_score, home_score, home_team.home_team_name)

thisMatchID <- sample(matchIDs, 1)
#3788755

#filter data for current match
this.lu <- lu %>% filter(match_id == thisMatchID)
this.m <- m %>% filter(match_id == thisMatchID)
this.ev <- ev %>% filter(match_id == thisMatchID)
this.rel_ev <- rel_ev %>% filter(match_id == thisMatchID)

#get match-specific information
#team names
awayName <- this.m %>% pull(away_team.away_team_name)
homeName <- this.m %>% pull(home_team.home_team_name)

#match players
mp <- get_match_players(thisMatchID, m, ev, lu)
mp <- mp %>% mutate(home_or_away = ifelse(team.name == awayName, "away", "home"))
#i need to debug this re: players going off and coming back on

#period start and end times
pers <- get_period_summary(this.ev)

#transform timestamps into cumulative seconds
this.ev <- get_cumulative_match_seconds(this.ev)
mp <- get_cumulative_ts_on_off(mp, pers)

#assign a y value to each player in match
mp <- mp %>% arrange(home_or_away, position.id, ts_on_cum_seconds) %>%
  mutate(timeline_y = seq(dim(mp)[1], 1, -1)) %>%
  mutate(timeline_y = ifelse(home_or_away == "away", timeline_y +1, timeline_y))
#join position abbreviations
mp <- mp %>% left_join(posAb %>% select(position_abbr, position.id), by = "position.id")

#number of players on each team
nA <- dim(mp %>% filter(team.name == awayName))[1]
nH <- dim(mp %>% filter(team.name == homeName))[1]

#match timeline_y values to events based on player id
this.ev <- this.ev %>% left_join(mp %>% select(player.id, timeline_y), by = "player.id")
#make NA values for duration into zeros
this.ev <- this.ev %>% mutate(duration = ifelse(is.na(duration), 0, duration))


base_tl <- ggplot(mp) + shUEFA_theme + 
  coord_cartesian(xlim = c(-3000, max(pers$cum_total_seconds))) +
  annotate("segment", x = rep.int(0, dim(mp)[1]), xend = rep.int(max(pers$cum_total_seconds), dim(mp)[1]), y = mp$timeline_y, yend = mp$timeline_y, 
           color = shUEFA["blueMed"], linetype = 2) + 
  annotate("rect", xmin = 0, xmax = max(pers$cum_total_seconds), ymin = 0, ymax = nH+1, fill = NA, color = shUEFA["blueMed"]) + 
  annotate("rect", xmin = 0, xmax = max(pers$cum_total_seconds), ymin = nH+1, ymax = nH+nA+2, fill = NA, color = shUEFA["blueMed"]) + 
  geom_segment(aes(x = ts_on_cum_seconds, xend = ts_off_cum_seconds, y = timeline_y, yend = timeline_y), color = shUEFA["blueLt"]) + 
  annotate("text", x = rep.int(-650, dim(mp)[1]), y = mp$timeline_y, label = mp$player.name, hjust = 1, color = shUEFA["blueLt"]) + 
  annotate("text", x = rep.int(-250, dim(mp)[1]), y = mp$timeline_y, label = mp$position_abbr, hjust = 0.5, color = shUEFA["blueLt"])


base_tl

# == | | == | | == | | == | | ==



#pick a player and plot his events on the timeline
myPlayer <- 7097
#Christian Norgaard's events
cn.ev <- this.ev %>% filter(player.id == myPlayer)
#his y-value
cn.y <- mp %>% filter(player.id == myPlayer) %>% pull(timeline_y)
cn.ev$timeline_y = cn.y

eventTypes

names(cn.ev)
base_tl + 
  geom_point(data = cn.ev %>% filter(type.name == "Pass"), 
             aes(y = timeline_y, x = cum_match_seconds), color = shUEFA["orangeDk"])


#take one of christian's passes and get its related events

cn.passes <- cn.ev %>% filter(type.name == "Pass") %>% pull(id)

cn.pass1.id <- cn.passes[1]
cn.pass1 <- this.ev %>% filter(id == cn.pass1.id)

cn.pass1.rel.id <- this.rel_ev %>% filter(id == cn.pass1.id) %>% pull(related_event_id)
cn.pass1.rel <- this.ev %>% filter(id == cn.pass1.rel.id)

id1 <- cn.pass1$player.id
id2 <- cn.pass1.rel$player.id

y1 <- mp %>% filter(player.id == id1) %>% pull(timeline_y)
y2 <- mp %>% filter(player.id == id2) %>% pull(timeline_y)

base_tl + 
  annotate("point", x = cn.pass1$cum_match_seconds, 
           y = y1, color = shUEFA["yellow"]) + 
  annotate("point", x = cn.pass1.rel$cum_match_seconds, 
           y = y2, color = shUEFA["orange"]) + 
  annotate("segment", y = y1, yend = y2, 
           x = cn.pass1$cum_match_seconds, xend = cn.pass1.rel$cum_match_seconds, 
           linetype = 4, color = shUEFA["orange"])


#want to be able to pick a range on the x-axis and plot all the events that fall into it
this.second <- 5000
buffer <- 3

now.ev <- this.ev %>% filter(between(cum_match_seconds, this.second - buffer, this.second + buffer))

base_tl + annotate("point", x = now.ev$cum_match_seconds, y = now.ev$timeline_y, color = shUEFA["yellow"])

#need to get the related events
re_ids <- this.rel_ev %>% filter(id %in% now.ev$id) %>% pull(related_event_id)
  
now.re <- this.ev %>% filter(id %in% re_ids)

#plot again with related events added
base_tl + 
  annotate("point", x = now.re$cum_match_seconds, y = now.re$timeline_y, color = shUEFA["orangeDk"], shape = 4) +
  annotate("point", x = now.ev$cum_match_seconds, y = now.ev$timeline_y, color = shUEFA["yellow"])


#if the event is a pass, it has a pass.recipient.id and a bunch of other stuff

#need to make a separate df with pass information for match
passes <- this.ev %>% filter(type.name == "Pass")

names(this.ev)

#possession!
this.ev %>% select(possession, possession_team.name, cum_match_seconds, 
                   StartOfPossession, TimeInPoss, TimeToPossEnd)

ggplot(this.ev) + 
  geom_point(aes(x = cum_match_seconds, 
                 y = possession_team.name))

#possessions df
poss <- this.ev %>% group_by(possession) %>%
  summarize(
    possession_team = first(possession_team.name),
    start_of_poss = first(cum_match_seconds)) %>% 
  mutate(end_of_poss = lead(start_of_poss, n = 1))

#to make a manual color scale, need vector same length as num of levels,
#and to rename colors to match levels
clrs <- shUEFA[c("blueMed","purpleLt")]
names(clrs) <- c(homeName,awayName)

#plot of possessions throughout the match
ggplot(poss) + shUEFA_theme_icy + 
  geom_rect(aes(xmin = start_of_poss, xmax = end_of_poss,
                ymin = 1, ymax = 2, 
                color = possession_team, fill = possession_team)) + 
  scale_color_manual(values = clrs) + 
  scale_fill_manual(values = clrs)

clickX <- 1050

pers
#timeline
ggplot() + shUEFA_theme_icy +
  coord_cartesian(xlim = c(0, max(pers$cum_total_seconds)), ylim = c(-20,100)) + 
  #big rectangle
  geom_rect(aes(xmin = 0, xmax = max(pers$cum_total_seconds), ymin = 0, ymax = 100), 
            fill = icyUEFA["ice2"], color = icyUEFA["ice4"], size = 1) +
  #period divider
  geom_segment(aes(x = pers$cum_total_seconds, xend = pers$cum_total_seconds, 
                   y = rep.int(-15, dim(pers)[1]), yend = rep.int(100, dim(pers)[1])), 
               color = shUEFA["blueLt"], size = 1) + 
  #time line
  geom_segment(aes(x = 0, xend = max(pers$cum_total_seconds), 
                   y = 50, yend = 50), color = icyUEFA["ice5"], size = 1.5) + 
  #time line ends
  geom_segment(aes(x = c(0, max(pers$cum_total_seconds)), 
                   xend = c(0, max(pers$cum_total_seconds)), 
                   y = c(40, 40), yend = c(60,60)), color = icyUEFA["ice5"], size = 1.5) + 
  #time labels
  annotate("text", y = rep.int(-10, dim(pers)[1]), x = pers$cum_total_seconds-50, 
           label = pers$max_ts, color = shUEFA["blueLt"], hjust = 1) + 
  
  annotate("segment", x = clickX, xend = clickX, y = 30, yend = 70, color = shUEFA["orangeDk"], size = 1.5) + 
  annotate("text", x = clickX, y = 25, label = as.character(clickX), color = icyUEFA["ice5"])






