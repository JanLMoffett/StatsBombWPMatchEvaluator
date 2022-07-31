
library(tidyverse)
library(devtools)
library(lubridate)

library(igraph)
library(sna)

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
eventTypes <- ev %>% group_by(type.id) %>%
  summarize(type.name = first(type.name))

#want to match an event type to each related event id in rel events dataset
#skip to read.csv
#-------------------------------------------------------------------------------
names(rel_ev)

ev.type <- ev %>% select(id, type.id)
#id's are unique?
length(unique(ev.type$id))
length(ev.type$id)
#yes

rel_ev1 <- rel_ev %>% left_join(ev.type, by = "id") %>%
  left_join(ev.type, by = c("related_event_1" = "id"), suffix = c("", "_re1")) %>%
  left_join(ev.type, by = c("related_event_2" = "id"), suffix = c("", "_re2")) %>%
  left_join(ev.type, by = c("related_event_3" = "id"), suffix = c("", "_re3")) %>%
  left_join(ev.type, by = c("related_event_4" = "id"), suffix = c("", "_re4")) %>%
  left_join(ev.type, by = c("related_event_5" = "id"), suffix = c("", "_re5")) %>%
  left_join(ev.type, by = c("related_event_6" = "id"), suffix = c("", "_re6")) %>%
  left_join(ev.type, by = c("related_event_7" = "id"), suffix = c("", "_re7")) %>%
  left_join(ev.type, by = c("related_event_8" = "id"), suffix = c("", "_re8")) %>%
  left_join(ev.type, by = c("related_event_9" = "id"), suffix = c("", "_re9"))

#write.csv(rel_ev1, "big_data/dbb_relatedEvents_with_typeIDs.csv")

#-------------------------------------------------------------------------------

#START HERE:
re <- read.csv("big_data/dbb_relatedEvents_with_typeIDs.csv")

re2 <- re %>% select(starts_with("type.id"), num_related_events)


re2.1 <- re2 %>% filter(num_related_events == 1)
re2.2 <- re2 %>% filter(num_related_events == 2)
re2.3 <- re2 %>% filter(num_related_events == 3)
re2.4 <- re2 %>% filter(num_related_events == 4)
re2.5 <- re2 %>% filter(num_related_events == 5)
re2.6 <- re2 %>% filter(num_related_events == 6)
re2.7 <- re2 %>% filter(num_related_events == 7)
re2.8 <- re2 %>% filter(num_related_events == 8)
re2.9 <- re2 %>% filter(num_related_events == 9)


#groups of two related events
re2.1 <- re2.1 %>% select(1:2)
re2.1 <- re2.1 %>% group_by(type.id, type.id_re1) %>% 
  tally() %>%
  rename(from = type.id, to = type.id_re1, frequency = n)

plot(graph_from_data_frame(re2.1))



#groups of three related events
re2.2 <- re2.2 %>% select(1:3)


re2.2.1 <- re2.2 %>% select(1:2) %>% rename(from = type.id, to = type.id_re1)
re2.2.2 <- re2.2 %>% select(2:3) %>% rename(from = type.id_re1, to = type.id_re2)
re2.2.3 <- re2.2 %>% select(1,3) %>% rename(from = type.id, to = type.id_re2)

re2.2 <- rbind(re2.2.1, re2.2.2) %>% rbind(re2.2.3)
re2.2 <- re2.2 %>% distinct()

plot(graph_from_data_frame(re2.2))

edgeLst <- matrix(c(re2.2$to, re2.2$from), ncol = 2)
head(edgeLst)

#go through each row
for(r in 1:dim(edgeLst)[1]){
  #pull out row as vector, sort, put back in
  edgeLst[r,] <- sort(edgeLst[r,])
}

colnames(edgeLst) <- c("from", "to")
eldf <- data.frame(edgeLst)

eldf2 <- distinct(eldf)

plot(graph_from_data_frame(eldf2, directed = F))

eldf3 <- eldf2 %>% left_join(eventTypes, by = c("to" = "type.id")) %>% rename(to.name = type.name)
eldf3 <- eldf3 %>% left_join(eventTypes, by = c("from" = "type.id")) %>% rename(from.name = type.name)

eldf4 <- eldf3 %>% select(3:4) %>% rename(from = from.name, to  = to.name)
plot(graph_from_data_frame(eldf4, directed = F))
