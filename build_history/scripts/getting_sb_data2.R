
library(StatsBombR)
library(tidyverse)


#with code from StatsBomb tutorial:
#----
#https://statsbomb.com/articles/soccer/statsbomb-announce-the-release-of-free-statsbomb-360-data-euro-2020-available-now/
Comp <- FreeCompetitions()

#the Matches dataset
Matches <- FreeMatches(Comp)
Matches <- Matches %>% filter(competition.competition_name=="UEFA Euro")

Matches2 <- Matches %>% select(-all_of(c("home_team.managers", "away_team.managers")))
#write.csv(Matches2, "big_data/dbb_matches.csv")

#nested variable, home_team.managers from Matches
managers <- Matches %>% select(home_team.managers, away_team.managers)

data360 <- StatsBombFree360Events(MatchesDF = Matches, Parallel = T)

events <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = T)
events <- allclean(events)
events <- get.opposingteam(events)

data360 = data360 %>% rename(id = event_uuid)

#events = events %>% left_join(data360, by = c("id" = "id"))
#events = events %>% rename(match_id = match_id.x) %>% select(-match_id.y)

#what's in the dataset?

look_at_df <- function(aDataframe){
  
  for(i in seq_along(aDataframe)){
    print(paste0(names(aDataframe)[i], " : ", class(aDataframe[[i]])))
  }
  
  lv <- vector()
  
  print("")
  print("~ Lists:")
  for(i in seq_along(aDataframe)){
    if(class(aDataframe[[i]]) == "list"){
      print(paste0(names(aDataframe)[i]))
      lv <- c(lv, names(aDataframe)[i])
    }
  }
  
  return(lv)
  
}

#unnest vars from statsBomb360 dataset:
#----
lv1 <- look_at_df(data360)

#visible_area
data360$visible_area[1]
#this is annoying bc the inner vectors are unnamed
#they alternate x1, y1, x2, y2, ...
data360_visibleArea <- data360 %>% select(id, visible_area) %>%
  unnest(cols = visible_area)

coord_type <- rep(c("x","y"), dim(data360_visibleArea)[1]/2)
data360_visibleArea$coord_type <- coord_type


tx <- data360_visibleArea %>% filter(coord_type == "x")
ty <- data360_visibleArea %>% filter(coord_type == "y")

tx <- tx %>% mutate(coord_order = 1:dim(tx)[1])
ty <- ty %>% mutate(coord_order = 1:dim(ty)[1])

data360_visibleArea <- full_join(tx, ty, by = c("id", "coord_order")) %>%
  select(id, visible_area.x, visible_area.y) %>%
  rename(visible_area_x = visible_area.x, 
         visible_area_y = visible_area.y)

data360_visibleArea <- data360_visibleArea %>% 
  mutate(one = 1) %>%
  group_by(id) %>%
  mutate(coord_seq = cumsum(one)) %>%
  ungroup() %>% mutate(one = NULL)

va1 <- data360_visibleArea %>% filter(coord_seq == 1) %>%
  rename(va_x_1 = visible_area_x,
         va_y_1 = visible_area_y)
va2 <- data360_visibleArea %>% filter(coord_seq == 2) %>%
  rename(va_x_2 = visible_area_x,
         va_y_2 = visible_area_y)
va3 <- data360_visibleArea %>% filter(coord_seq == 3) %>%
  rename(va_x_3 = visible_area_x,
         va_y_3 = visible_area_y)
va4 <- data360_visibleArea %>% filter(coord_seq == 4) %>%
  rename(va_x_4 = visible_area_x,
         va_y_4 = visible_area_y)
va5 <- data360_visibleArea %>% filter(coord_seq == 5) %>%
  rename(va_x_5 = visible_area_x,
         va_y_5 = visible_area_y)
va6 <- data360_visibleArea %>% filter(coord_seq == 6) %>%
  rename(va_x_6 = visible_area_x,
         va_y_6 = visible_area_y)
va7 <- data360_visibleArea %>% filter(coord_seq == 7) %>%
  rename(va_x_7 = visible_area_x,
         va_y_7 = visible_area_y)
va8 <- data360_visibleArea %>% filter(coord_seq == 8) %>%
  rename(va_x_8 = visible_area_x,
         va_y_8 = visible_area_y)

#this is how many values I should end up with:
length(unique(data360_visibleArea$id))
#172081

data360_visibleArea <- full_join(va1, va2, by = "id") %>%
  full_join(va3, by = "id") %>% full_join(va4, by = "id") %>%
  full_join(va5, by = "id") %>% full_join(va6, by = "id") %>%
  full_join(va7, by = "id") %>% full_join(va8, by = "id") %>%
  select(-starts_with("coord_seq"))

#freeze_frame
class(data360$freeze_frame[[1]])
#these are data frames

data360_ff <- data360 %>% select(id, freeze_frame) %>%
  unnest(cols = freeze_frame) %>% 
  unnest(cols = location) %>%
  mutate(coord_type = rep(c("x","y"), dim(data360_ff)[1]/2))

data360_ff.x <- data360_ff %>% filter(coord_type == "x") %>%
  rename(location_x = location) %>%
  mutate(one = 1) %>%
  group_by(id) %>%
  mutate(ff_player = cumsum(one)) %>%
  mutate(coord_type = NULL,
         one = NULL)
  
data360_ff.y <- data360_ff %>% filter(coord_type == "y") %>%
  rename(location_y = location) %>%
  mutate(one = 1) %>%
  group_by(id) %>%
  mutate(ff_player = cumsum(one)) %>%
  select(-all_of(c("teammate", "actor", "keeper", "one", "coord_type")))

data360_ff <- inner_join(data360_ff.x, data360_ff.y, by = c("id", "ff_player"))

#write.csv(data360_visibleArea, "big_data/dbb_data360_visibleArea.csv")
#write.csv(data360_ff, "big_data/dbb_data360_freezeFrames.csv")

data360 <- data360 %>% select(-all_of(lv1))
#this is just id and match_id now
##write.csv(data360, "big_data/dbb_data360_events_matches.csv")

#----

lv2 <- look_at_df(events)
#unnest vars:

#"~ Lists:"
#[1] "related_events"   X
#[1] "location"         X
#[1] "tactics.lineup"   X  
#[1] "pass.end_location"    X
#[1] "carry.end_location"   X
#[1] "shot.end_location"    X
#[1] "shot.freeze_frame"    X
#[1] "goalkeeper.end_location" X

unqEvents <- unique(events$id)
eventTypes <- unique(events$type.name)
eventTypes
#"related_events"
#----
relEvents <- events %>% 
  select(id, related_events) %>% 
  unnest(cols = "related_events") %>%
  mutate(one = 1) %>%
  group_by(id) %>%
  mutate(related_event_seq = cumsum(one),
         num_related_events = sum(one)) %>%
  select(-one)

max(relEvents$related_event_seq)
re1 <- relEvents %>% filter(related_event_seq == 1) %>% rename(related_event_1 = related_events)
re2 <- relEvents %>% filter(related_event_seq == 2) %>% rename(related_event_2 = related_events) %>% select(1:2)
re3 <- relEvents %>% filter(related_event_seq == 3) %>% rename(related_event_3 = related_events) %>% select(1:2)
re4 <- relEvents %>% filter(related_event_seq == 4) %>% rename(related_event_4 = related_events) %>% select(1:2)
re5 <- relEvents %>% filter(related_event_seq == 5) %>% rename(related_event_5 = related_events) %>% select(1:2)
re6 <- relEvents %>% filter(related_event_seq == 6) %>% rename(related_event_6 = related_events) %>% select(1:2)
re7 <- relEvents %>% filter(related_event_seq == 7) %>% rename(related_event_7 = related_events) %>% select(1:2)
re8 <- relEvents %>% filter(related_event_seq == 8) %>% rename(related_event_8 = related_events) %>% select(1:2)
re9 <- relEvents %>% filter(related_event_seq == 9) %>% rename(related_event_9 = related_events) %>% select(1:2)

re <- re1 %>% left_join(re2, by = "id") %>% left_join(re3, by = "id") %>%
  left_join(re4, by = "id") %>% left_join(re5, by = "id") %>%
  left_join(re6, by = "id") %>% left_join(re7, by = "id") %>% 
  left_join(re8, by = "id") %>% left_join(re9, by = "id") %>% 
  select(-related_event_seq)

#write.csv(re, "big_data/dbb_events_relatedEvents.csv")
#----

#"tactics.lineup"
View(events %>% filter(type.name == "Starting XI") %>% select(id, tactics.lineup))
lu <- events %>% filter(type.name == "Starting XI") %>% select(id, match_id, team.id, team.name, tactics.lineup) %>%
  unnest(cols = tactics.lineup)

#write.csv(lu, "big_data/dbb_events_startingXI.csv")

#"shot.freeze_frame"
#----
View(events %>% filter(type.name == "Shot") %>% select(id, shot.freeze_frame))

sff <- events %>% filter(type.name == "Shot") %>% select(id, match_id, shot.freeze_frame)
sff2 <- sff %>% unnest(cols = shot.freeze_frame)
sff2 <- sff2 %>% mutate(one = 1) %>%
  group_by(id) %>%
  mutate(shotFF_player_seq = cumsum(one),
         shotFF_num_players = sum(one)) %>% ungroup()

sff2.loc <- sff2 %>% select(id, shotFF_player_seq, location) %>% unnest(cols = location)
sff2.loc$coord_type <- rep(c("x", "y"), dim(sff2.loc)[1]/2)
sff2.loc.x <- sff2.loc %>% filter(coord_type == "x") %>% select(id, shotFF_player_seq, location) %>% rename(shotFF_player_location_x = location)
sff2.loc.y <- sff2.loc %>% filter(coord_type == "y") %>% select(id, shotFF_player_seq, location) %>% rename(shotFF_player_location_y = location)
sff2.loc <- inner_join(sff2.loc.x, sff2.loc.y, by = c("id", "shotFF_player_seq"))

sff2 <- sff2 %>% select(-all_of(c("location", "one")))
sff3 <- sff2 %>% inner_join(sff2.loc, by = c("id", "shotFF_player_seq"))

#write.csv(sff3, "big_data/dbb_events_shotFreezeFrames.csv")
#----

#"goalkeeper.end_location"
#----
View(events %>% filter(str_detect(type.name, "Goal")) %>% select(id, type.name, goalkeeper.end_location))
gk <- events %>% filter(str_detect(type.name, "Goal")) %>% select(id, type.name, goalkeeper.end_location)

gk <- gk %>% unnest(cols = goalkeeper.end_location)
coord_type <- rep(c("x", "y"), dim(gk)[1]/2)
gk$coord_type <- coord_type
gk.x <- gk %>% filter(coord_type == "x") %>% rename(gk.end_location_x = goalkeeper.end_location) %>% select(-all_of(c("coord_type", "type.name")))
gk.y <- gk %>% filter(coord_type == "y") %>% rename(gk.end_location_y = goalkeeper.end_location) %>% select(-all_of(c("coord_type", "type.name")))
gk <- inner_join(gk.x, gk.y, by = "id")

#write.csv(gk, "big_data/dbb_events_gkEndLocation.csv")
#----

#already unnested into vars in the table:

#"location"
View(events %>% select(id, location, location.x, location.y))

#"pass.end_location"
View(events %>% select(id, pass.end_location, pass.end_location.x, pass.end_location.y))

#"carry.end_location"
View(events %>% select(id, carry.end_location, carry.end_location.x, carry.end_location.y))

#"shot.end_location"
View(events %>% select(id, shot.end_location, shot.end_location.x, shot.end_location.y, shot.end_location.z))

#events set without nested variables
events.noNest <- events %>% 
  select(-all_of(lv2))
#write.csv(events.noNest, "big_data/dbb_events.csv")

