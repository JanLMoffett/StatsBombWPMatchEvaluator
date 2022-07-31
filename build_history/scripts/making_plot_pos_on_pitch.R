

library(tidyverse)
library(devtools)
library(lubridate)
library(caret)


#importing colors and themes for plots
#github.com/JanLMoffett/datavizExtras
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/colorConstants.R")
source_url("https://raw.githubusercontent.com/JanLMoffett/datavizExtras/master/extraThemes.R")

source("functions_etc/bombViz.R")

#m <- read.csv("data/matches.csv")
#pi <- read.csv("data/events_playerInfo.csv")

m <- read.csv("big_data/dbb_matches.csv")
ev <- read.csv("big_data/dbb_events.csv")
#unnested starting XI dataframes
lu <- read.csv("big_data/dbb_events_startingXI.csv")

#all event types
unique(ev$type.name)

#events relating to lineup information
pi <- ev %>% filter(type.name %in% c("Starting XI", "Substitution", "Player On", "Player Off"))


#creating an index of all unique pos from the dataset, with nums, abbrs, and coord for display
#-------------------------------------------------------------------------------

#all possible positions
sort(unique(pi$position.name))
sort(unique(lu$position.name))
sort(unique(ev$position.name))
#there are 23 positions in the uefa 2020 data
posNames <- sort(unique(pi$position.name))

#ids for all 51 uefa CL 2020 matches
matchIDs <- m %>% arrange(match_date, kick_off) %>% pull(match_id)

#this is the position abbreviation table i made based on the open data documentation
posAbbr <- read.csv("data/positionAbbrev.csv")
names(posAbbr)[1] <- "position_number"
#i need to modify this because the UEFA 2020 positions are slightly different

#appearing in abbr set but not events:
setdiff(posAbbr$position_name, posNames)
#[1] "Center Midfield"   "Striker"  "Secondary Striker"

#appearing in events but not abbr:
setdiff(posNames, posAbbr$position_name)
#nothing 

#revised table of position abbreviations
posAb <- posAbbr %>% filter(position_name %in% posNames) %>% select(-position_number)

posAb <- pi %>% group_by(position.id) %>%
  summarize(position.name = first(position.name)) %>%
  left_join(posAb, by = c("position.name" = "position_name")) %>%
  filter(!is.na(position.id))

#add display coordinates to each position
#assign rows and columns first, so coord can be changed easily
display_rows <- c(3, 
                  1, 2, 3, 4, 5,
                  1, 5, 2, 3, 4,
                  1, 2, 4, 5,
                  1, 2, 3, 4, 5,
                  2, 3, 4)
display_cols <- c(1, 
                  rep.int(2, 5), 
                  rep.int(3, 5), 
                  rep.int(4, 4),
                  rep.int(5, 5), 
                  rep.int(6, 3))

posAb <- posAb %>% mutate(display_row = display_rows,
                 display_col = display_cols)
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
#set x and y coordinates for the rows and columns where positions appear on pitch
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
position_column_1 = 9
position_column_2 = 25
position_column_3 = 42
position_column_4 = 60
position_column_5 = 78
position_column_6 = 95

position_row_1 = 13
position_row_2 = 26
position_row_3 = 40
position_row_4 = 53
position_row_5 = 66
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

#add coordinates to df
posAb <- posAb %>% 
  mutate(display_x = case_when(display_col == 1 ~ position_column_1,
                               display_col == 2 ~ position_column_2,
                               display_col == 3 ~ position_column_3,
                               display_col == 4 ~ position_column_4,
                               display_col == 5 ~ position_column_5,
                               display_col == 6 ~ position_column_6
                               ),
         display_y = case_when(display_row == 1 ~ position_row_1,
                               display_row == 2 ~ position_row_2,
                               display_row == 3 ~ position_row_3,
                               display_row == 4 ~ position_row_4,
                               display_row == 5 ~ position_row_5))

write.csv(posAb, "functions_etc/positionDisplay.csv")
#------------------------------------------------------------------------------

#reading in table i just made above
posAb <- read.csv("functions_etc/positionDisplay.csv")


plot_pos_on_pitch <- function(){
  
  plot_pitch(lineColor = shUEFA["blueLt"]) + shUEFA_theme + 
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.line = element_blank()) + 
    
    labs(title = "") + 
    
    annotate("text",
             x = posAb$display_x,
             y = posAb$display_y,
             label = paste0(posAb$position.id, "   ", posAb$position_abbr), 
             color = shUEFA["orange"])


}

plot_pos_on_pitch()


  
  
