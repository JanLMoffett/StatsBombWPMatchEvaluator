
#this file follows getting_sb_data.R

library(tidyverse)
library(devtools)

#importing colors and themes for plots
#github.com/JanLMoffett/datavizExtras
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")

#statsbomb event data
ev <- read.csv("data/events_noNestedVars.csv")
dim(ev)

#this is a really big csv, so i want to break it down by event type
m  <- matrix(sort(table(ev$type.name)), ncol = 1)
row.names(m) <- names(sort(table(ev$type.name)))
colnames(m) <- "n"

par(las = 1, mar = c(7,10,5,2))
barplot(height = m[,1], names.arg = row.names(m), horiz = T, xlab = "Count",
        main = "Event Types in Events Dataset")

#are there any rows with na for type.name?
sum(is.na(ev$type.name))
#no

length(unique(ev$type.name))
#33 unique event types

#want to classify different types

#~Player Info~~~~~~~~~~~~~~
#   Starting XI         102
#   Player On            21
#   Player Off           22
#   Substitution        454
#~time markers~~~~~~~~~~~~~
#   Referee Ball-Drop   121
#   Half Start          244
#   Half End            244
#   Injury Stoppage     288
#~Fouls~~~~~~~~~~~~~~~~~~~~
#   Bad Behaviour        26
#   Foul Committed     1394
#~~Offensive Fails~~~~~~~~~
#   Own Goal Against     11
#   Dispossessed       1087
#   Miscontrol         1273
#   Offside               9
#   Error                24
#~~Defensive Fails~~~~~~~~~
#   Dribbled Past       894
#~Freebies ~~~~~~~~~~~~~~~~
#   Own Goal For         11
#   Foul Won           1337
#~Offensive Plays~~~~~~~~~~
#   Ball Recovery      4445
#   Shot               1289
#   Ball Receipt*     52722 (may be successful or not)
#   Dribble            1476
#   Carry             43802
#   Pass              54820
#~Defensive Plays~~~~~~~~~~
#   Goal Keeper        1514
#   Clearance          2283
#   Shield               58
#   Interception       1460
#   Block              1770
#   Pressure          15958
#~Special Circumstances~~~~
#   50/50                56 - this is when the ball is in the air and two guys are trying to get it
#   Tactical Shift      125
#   Duel               3355 - this is actually a defensive play

evs_playerInfo <- c("Starting XI", "Player On", "Player Off", "Substitution")
evs_timeMarkers <- c("Referee Ball-Drop", "Half Start", "Half End", "Injury Stoppage")
evs_fouls <- c("Bad Behaviour", "Foul Committed")
evs_failsOffense <- c("Own Goal Against", "Dispossessed", "Miscontrol", "Offside", "Error")
evs_failsDefense <- "Dribbled Past"
evs_freebies <- c("Own Goal For", "Foul Won")
evs_playsOffense <- c("Ball Recovery", "Shot", "Ball Receipt*", "Dribble", "Carry", "Pass")
evs_playsDefense <- c("Goal Keeper", "Clearance", "Shield", "Interception", "Block", "Pressure")
evs_specialCirc <- c("50/50", "Tactical Shift", "Duel")

#split data by event category
ev <- ev %>%
  mutate(event_type_category = case_when(
    type.name %in% evs_playerInfo ~ "Player Info",
    type.name %in% evs_timeMarkers ~ "Time Marker",
    type.name %in% evs_fouls ~ "Foul",
    type.name %in% evs_failsOffense ~ "Offensive Fail",
    type.name %in% evs_failsDefense ~ "Defensive Fail",
    type.name %in% evs_freebies ~ "Freebie",
    type.name %in% evs_playsOffense ~ "Offensive Play",
    type.name %in% evs_playsDefense ~ "Defensive Play",
    type.name %in% evs_specialCirc ~ "Special Circumstance")
    )

events.playerInfo <- ev %>% filter(event_type_category == "Player Info")
events.timeMarkers <- ev %>% filter(event_type_category == "Time Marker")
events.fouls <- ev %>% filter(event_type_category %in% c("Foul", "Freebie"))
events.special <- ev %>% filter(event_type_category == "Special Circumstance")
events.defense <- ev %>% filter(event_type_category %in% c("Defensive Play", "Defensive Fail"))
events.offenseMinus <- ev %>% filter(event_type_category == "Offensive Fail")
#this set is big and needs further breakdown:
events.offensePlus <- ev %>% filter(event_type_category == "Offensive Play")

#saving data
write.csv(events.playerInfo, "data/events_playerInfo.csv")
write.csv(events.timeMarkers, "data/events_timeMarkers.csv")
write.csv(events.fouls, "data/events_fouls.csv")
write.csv(events.special, "data/events_special.csv")
write.csv(events.defense, "data/events_defense.csv")
write.csv(events.offenseMinus, "data/events_offenseMinus.csv")

#Breaking down offensePlus set by event type
m  <- matrix(sort(table(events.offensePlus$type.name)), ncol = 1)
row.names(m) <- names(sort(table(events.offensePlus$type.name)))
colnames(m) <- "n"

par(las = 1, mar = c(7,10,5,2))
barplot(height = m[,1], names.arg = row.names(m), horiz = T, xlab = "Count",
        main = "Event Types in Offensive Plays Dataset")

#later i'm going to join these to the freeze frames (?)
events.pass <- ev %>% filter(type.name == "Pass")
events.receipt <- ev %>% filter(type.name == "Ball Receipt*")
events.carry <- ev %>% filter(type.name == "Carry")
events.recovery <- ev %>% filter(type.name == "Ball Recovery")
events.dribble <- ev %>% filter(type.name == "Dribble")
events.shot <- ev %>% filter(type.name == "Shot")

write.csv(events.pass, "data/events_pass.csv")
write.csv(events.receipt, "data/events_receipt.csv")
write.csv(events.carry, "data/events_carry.csv")
write.csv(events.recovery, "data/events_recovery.csv")
write.csv(events.dribble, "data/events_dribble.csv")
write.csv(events.shot, "data/events_shot.csv")

#i need to make sure all my new datasets add up to the number of obs in the original
dim(ev)[1]
#[1] 192695
sum(
  dim(events.carry)[1],
  dim(events.defense)[1],
  dim(events.dribble)[1],
  dim(events.fouls)[1],
  dim(events.offenseMinus)[1],
  dim(events.pass)[1],
  dim(events.playerInfo)[1],
  dim(events.receipt)[1],
  dim(events.recovery)[1],
  dim(events.shot)[1],
  dim(events.special)[1],
  dim(events.timeMarkers)[1]
  )
#[1] 192695 huzzah!




