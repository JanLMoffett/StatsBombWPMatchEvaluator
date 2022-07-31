
library(tidyverse)
library(devtools)
library(caret)

#see getting_sb_data.R and storing_sb_data.R for how i got this dataset:
special <- read.csv("data/events_special.csv")
defense <- read.csv("data/events_defense.csv")

#taking out varibles that are all NA
nzv1 <- nearZeroVar(special, saveMetrics = T, names = T)
remove <- row.names(nzv1[nzv1$zeroVar == T,])
special <- special %>% select(-c(all_of(remove)))

#what event types are in the df?
table(special$type.name)
#50/50           Duel Tactical Shift 
#   56           3355            125

#what are Duels like?
duels <- special %>% filter(type.name == "Duel")

#there might be more NA vars now
nzv2 <- nearZeroVar(duels, saveMetrics = T, names = T)
remove <- row.names(nzv2[nzv2$zeroVar == T,])[3:5]
duels <- duels %>% select(-c(all_of(remove)))

summary(duels)
#so duels belong with defense
#the aerial lost type of duel is here
#the aerial won is classified as a clearance







