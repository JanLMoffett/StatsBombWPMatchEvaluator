
library(tidyverse)
library(lubridate)

source("functions_etc/get_match_players.R")
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
b <- sample(1:51, 1)

thisMatchID = matchIDs[b]
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ #

this.ev <- ev %>% filter(match_id == thisMatchID)

#testing get_match_players function
this.mp <- get_match_players(thisMatchID, m, ev, lu)

this.mp <- this.mp %>% mutate(ts_on_seconds = timestamp_to_seconds(timestamp_on),
                              ts_off_seconds = timestamp_to_seconds(timestamp_off)) 

this.mp <- get_cumulative_ts_on_off(this.mp, this.ev)
