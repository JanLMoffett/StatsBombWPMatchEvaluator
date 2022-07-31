
library(StatsBombR)
library(tidyverse)


#with code from StatsBomb tutorial:
#----
#https://statsbomb.com/articles/soccer/statsbomb-announce-the-release-of-free-statsbomb-360-data-euro-2020-available-now/
Comp <- FreeCompetitions()

#the Matches dataset
Matches <- FreeMatches(Comp)
Matches <- Matches %>% filter(competition.competition_name=="UEFA Euro")

matches <- Matches %>% select(-all_of(c("home_team.managers", "away_team.managers")))

#nested variable, home_team.managers from Matches
managers <- Matches %>% select(home_team.managers, away_team.managers)

str(matches)
unique(matches$home_team.home_team_name)

matches_simple <- matches %>% 
  select(match_id,
         away_team.away_team_name,
         away_team.away_team_group,
         home_team.home_team_name,
         home_team.home_team_group,
         competition_stage.name,
         match_week,
         match_date,
         kick_off,
         stadium.name,
         stadium.country.name
         ) %>%
  arrange(match_date, kick_off)

#write.csv(matches_simple, "data/matches_simple.csv")
#write.csv(matches, "data/matches.csv")
#write.csv(managers, "data/managers.csv")

data360 <- StatsBombFree360Events(MatchesDF = Matches, Parallel = T)

events <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = T)
events <- allclean(events)
events <- get.opposingteam(events)

data360 = data360 %>% rename(id = event_uuid)

events = events %>% left_join(data360, by = c("id" = "id"))
events = events %>% rename(match_id = match_id.x) %>% select(-match_id.y)


#events is the most important dataset
#what's in the dataset?
for(i in seq_along(events)){
  print(paste0(names(events)[i], " : ", class(events[[i]])))
}

listVars <- vector()
for(i in seq_along(events)){
  if(class(events[[i]]) == "list"){
    listVars <- c(listVars, names(events)[i])
  }
}

#vars that are lists:
listVars

#taking out nested variables
events.noNest <- events %>% 
  select(-all_of(listVars))

#write.csv(events.noNest, "data/events_noNestedVars.csv")

#unnest each of the nested vars and make them joinable by event id

events.location <- events %>% 
  select(id, location) %>%
  unnest(cols = location) %>%
  group_by(id) %>%
  mutate(location_x = first(location),
         location_y = last(location)) %>%
  summarize(
    id = first(id),
    location_x = first(location_x),
    location_y = first(location_y)
  ) %>% ungroup()

events.related_events <- events %>% 
  select(id, type.id, type.name, related_events) %>% 
  unnest(cols = related_events)
#write.csv(events.related_events, "data/unnested_relatedEvents.csv")

events.lineup <- events %>% filter(type.name == "Starting XI") %>%
  select(id, match_id, team.id, team.name, tactics.lineup) %>%
  rename(lineup = tactics.lineup) %>%
  unnest(cols = lineup) %>%
  select(-id)
  
#write.csv(events.lineup, "data/unnested_startingLineups.csv")
names(events.noNest)
events.passEndLoc <- events %>% select(id, pass.end_loc)
events %>% pull(pass.end_location)


#what are the different types of events?
unique(events$type.name)

#filtering down to passes only and reducing variables (statsbomb tutorial)
events.passes = events %>%
  group_by(team.name) %>%
  filter(type.name=="Pass") %>%
  select(id, match_id, team.name, OpposingTeam, player.name, type.name, minute, second, location.x, location.y, pass.end_location.x, pass.end_location.y, pass.type.name, pass.cross, freeze_frame)

events.passes = events.passes %>% unnest(freeze_frame) %>%
  mutate(ff_location.x = (map(location, 1)), 
         ff_location.y = (map(location, 2))) %>%
  select(-location) %>%
  mutate(ff_location.x = as.numeric(ifelse(ff_location.x == "NULL", NA, ff_location.x)), 
         ff_location.y = as.numeric(ifelse(ff_location.y == "NULL", NA, ff_location.y)))



#this is me:
ggplot(ffs %>% filter(match_id == 3788741)) +
  geom_point(aes(x = location.x, y = location.y)) + 
  geom_point(aes(x = ff_location.x, y = ff_location.y), color = "blue")

length(unique(ffs$id))
thisEvent <- unique(ffs$id)[651]



t <- ffs %>% filter(id == thisEvent)

ggplot(t) +
  geom_point(aes(x = location.x, y = location.y), color = "purple", shape = 22) +
  geom_point(aes(x = pass.end_location.x, y = pass.end_location.y), color = "red", shape = 15) +
  geom_segment(aes(x = location.x, xend = pass.end_location.x, y = location.y, yend = pass.end_location.y), color = "purple") +
  geom_point(aes(x = ff_location.x, y = ff_location.y), color = "blue")


#----


#what's in the data?

#Comp
str(Comp) #this is to look up id for UEFA Euro 2020

#str function produces huge output
for(i in seq_along(ffs)){
  print(paste0(names(ffs)[i], " : ", class(ffs[[i]])))
}


#data360----
names(data360)
#str function produces huge output
for(i in seq_along(data360)){
  print(paste0(names(data360)[i], " : ", class(data360[[i]])))
}

#visible area and freeze frame are big lists
visible_area <- data360$visible_area
#list of numeric vectors with about 12 entries for each observation

freeze_frame <- data360$freeze_frame
freeze_frame[[]]

#id
data360[[1]]

#visible_area
data360[[2]]

#end data360----