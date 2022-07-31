
library(tidyverse)
library(lubridate)

#source("app_functions/scrape_StatsBomb.R")
source("app_functions/bombViz.R")
source("app_functions/timestamp_to_seconds.R")
source("app_functions/get_match_players2.R")
source("app_functions/get_match_info_table.R")

#future goals and other model input data, min-by-min
fg <- read.csv("app_data/futureGoals_55_7.csv")
#all min-by-min goal probabilities for uefa 2020
gp <- read.csv("app_data/goal_probabilities_UEFA2020.csv")
#all min-by-min win/draw/lose probabilities for uefa 2020
wdl <- read.csv("app_data/win_probabilities_UEFA2020.csv")

#Matches
m <- read.csv("app_data/dbb_matches.csv")
#all match players
all_mp <- read.csv("app_data/match_players_UEFA2020.csv")
#Events without nested vars
ev <- read.csv("app_data/events.csv")
ev <- ev %>% select(-1:-2)

#list of unique match_id values for selectInput
matchIDs <- unique(m$match_id)
names(matchIDs) <- m$menu_text


# # # # # INPUT # # # # #
#randomly select a match id
selected_match <- sample(matchIDs, 1)

#select a time in seconds to stand in for slider input
selected_time <- 3200


# Match-ID-Reactive Data
this.m <- m %>% filter(match_id == selected_match)
this.ev <- ev %>% filter(match_id == selected_match, period < 3)
this.ev <- get_cumulative_match_seconds(this.ev)
mp <- all_mp %>% filter(match_id == selected_match)
pers <- get_period_summary(this.ev)
poss <- get_possession_summary(this.ev)
this.fg <- fg %>% filter(match_id == selected_match)
this.gp <- gp %>% filter(match_id == selected_match)
this.wdl <- wdl %>% filter(match_id == selected_match)

#get names of teams
awayName <- this.m %>% pull(away_team.away_team_name)
homeName <- this.m %>% pull(home_team.home_team_name)

#i need to calculate the time bin for events, so i can connect them to the model data
#get end ts_seconds of match (2 periods only)
max_sec <- max(this.ev$cum_match_seconds)
#divide it by 90 bins
bin_width <- max_sec/90

#assign bins to events
this.ev <- this.ev %>% mutate(time_bin = floor(cum_match_seconds/bin_width))

#write.csv(m_events, "win_probability/wp_data/m_events_test.csv")
#View(m_events %>% select(time_bin, is_home_team, location.x, location.y))

#standardize location coordinates
this.ev <- this.ev %>%
  mutate(is_home_team = ifelse(team.name == homeName, 1, 0)) %>%
  mutate(std_location_x = ifelse(is_home_team == 0, 120 - location.x, location.x),
         std_location_y = ifelse(is_home_team == 0, 80 - location.y, location.y))

#add timeline_y values to events df
tly <- mp %>% select(player.id, timeline_y)
this.ev <- this.ev %>% left_join(tly, by = "player.id")


#this match data
tmd <- list(this_m = this.m,
            this_ev = this.ev,
            mp = mp,
            pers = pers,
            poss = poss,
            this_fg = this.fg,
            this_gp = this.gp,
            this_wdl = this.wdl,
            awayName = awayName,
            homeName = homeName,
            max_sec = max_sec,
            bin_width = bin_width)

#access elements of the list
tmd[["awayName"]]


#win probability plot
winColor = shUEFA["orangeDk"]
drawColor = shUEFA["purple"]
lossColor = shUEFA["blueMed"]

ggplot(this.wdl) + shUEFA_theme_icy +
  theme(plot.margin = unit(c(2,2,0,0), unit="pt"), 
        plot.title = element_text(size = 12, hjust = 0, color = shUEFA["blueLt"])) +
  
  labs(title = "Match Outcome Probability") + 
  
  coord_cartesian(xlim = c(0, 90), ylim = c(0,1), expand = 0) + 
  
  annotate("rect", xmin = seq(0, 88, 2), xmax = seq(1, 89, 2), ymin = 0, ymax = 1, fill = "white", alpha = 0.5) + 
  
  geom_line(aes(x = time_bin, y = p_win), color = winColor, size = 1) + 
  geom_line(aes(x = time_bin, y = p_draw), color = drawColor, size = 1) + 
  
  #legend
  annotate("text", x = 1, y = 0.95, label = paste0(homeName, " Win"), color = winColor, hjust = 0, size = 8) + 
  annotate("text", x = 1, y = 0.85, label = "Draw", color = drawColor, hjust = 0, size = 8)


#plot of score changes
max_as = max(this.fg$cur_away_score, na.rm = T)
max_hs = max(this.fg$cur_home_score, na.rm = T)

#away team
ggplot(this.fg) + 
  shUEFA_theme_icy + 
  theme(plot.margin = unit(c(2,2,0,0), unit="pt"), 
        plot.title = element_text(size = 12, hjust = 0, color = shUEFA["blueLt"])) +
  
  coord_cartesian(xlim = c(0, 90), ylim = c(0, max(c(max_as, max_hs))), expand = 0) +
  
  labs(title = "Score") + 
  annotate("rect", xmin = seq(0, 87, 1), xmax = seq(1, 88, 1), ymin = 0, ymax = this.fg$cur_away_score, fill = lossColor, alpha = 0.5) +
 
  annotate("rect", xmin = seq(0, 87, 1), xmax = seq(1, 88, 1), ymin = 0, ymax = this.fg$cur_home_score, fill = winColor, alpha = 0.5)




#pick a time bin
selected_tb <- floor(selected_time/bin_width)

#time-bin-reactive data
tb_wdl <- this.wdl %>% filter(time_bin == selected_tb)
tb_fg <- this.fg %>% filter(time_bin == selected_tb)
tb_ev <- this.ev %>% filter(time_bin == selected_tb)
#when a time bin is selected, i want to highlight the players that are involved
tb_players <- unique(tb_ev$player.id)
tb_mp <- mp %>% filter(player.id %in% tb_players)

#plot of W-D-L probability for a time bin
ggplot() + 
  shUEFA_theme_icy + 
  coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = 0) + 
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = tb_wdl$p_win, fill = shUEFA["orange"]) + 
  annotate("rect", xmin = 0, xmax = 1, ymin = tb_wdl$p_win, ymax = tb_wdl$p_win + tb_wdl$p_draw, fill = shUEFA["purpleLt"]) + 
  annotate("rect", xmin = 0, xmax = 1, ymin = tb_wdl$p_win + tb_wdl$p_draw, ymax = tb_wdl$p_win + tb_wdl$p_draw + tb_wdl$p_loss, fill = shUEFA["yellow"])

#plot of W-D-L probabilities for all time bins
n_bins = dim(this.wdl)[1]
ggplot() + 
  shUEFA_theme_icy + 
  coord_cartesian(xlim = c(0, n_bins), ylim = c(0,1), expand = 0) + 
  annotate("rect", xmin = 0:(n_bins-1), xmax = 1:n_bins, ymin = rep.int(0, n_bins), ymax = this.wdl$p_win, fill = shUEFA["orange"]) + 
  annotate("rect", xmin = 0:(n_bins-1), xmax = 1:n_bins, ymin = this.wdl$p_win, ymax = this.wdl$p_win + this.wdl$p_draw, fill = shUEFA["purpleLt"]) + 
  annotate("rect", xmin = 0:(n_bins-1), xmax = 1:n_bins, ymin = this.wdl$p_win + this.wdl$p_draw, ymax = this.wdl$p_win + this.wdl$p_draw + this.wdl$p_loss, fill = shUEFA["yellow"])

#plot of start and end location of "ball" for time bin
plot_pitch() + shUEFA_theme_icy +
  annotate("point", x = tb_fg$start_loc_x, y = tb_fg$start_loc_y, color = shUEFA["blueMed"], shape = 10, size = 5) +
  annotate("point", x = tb_fg$end_loc_x, y = tb_fg$end_loc_y, color = shUEFA["blueSky"], shape = 10, size = 5) +
  annotate("segment", x = tb_fg$start_loc_x, xend = tb_fg$end_loc_x, y = tb_fg$start_loc_y, yend = tb_fg$end_loc_y, arrow = my_arrow, color = shUEFA["blueMed"])
  

#add on locations of events that happened in the time bin
plot_pitch(tb_ev) + shUEFA_theme_icy +
  geom_point(aes(x = std_location_x, y = std_location_y, shape = type.name), color = shUEFA["orange"], size = 5) + 
  annotate("point", x = tb_fg$start_loc_x, y = tb_fg$start_loc_y, color = shUEFA["blueMed"], shape = 10, size = 5) +
  annotate("point", x = tb_fg$end_loc_x, y = tb_fg$end_loc_y, color = shUEFA["blueSky"], shape = 10, size = 5) +
  annotate("segment", x = tb_fg$start_loc_x, xend = tb_fg$end_loc_x, y = tb_fg$start_loc_y, yend = tb_fg$end_loc_y, arrow = my_arrow, color = shUEFA["blueMed"])
#i want these to have tool tips when I hover over them

#what kind of event types are showing up and what types do i want to plot?
names(this.ev)










#timeline plot
#position adjustments for the text that appears on the plot
this.hjust <- ifelse(selected_time > (max(pers$cum_total_seconds)/2), 1, 0)
this.posAdj <- ifelse(selected_time > (max(pers$cum_total_seconds)/2), -100, 100)

#current period - need to make sure all matches are cut off after 2 periods
cp <- ifelse(selected_time < pers$cum_total_seconds[1], 1, 2)

#display the input time in timestamp form
display_ts <- seconds_to_timestamp(pers, selected_time)
#-----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot() + shUEFA_theme_icy + 
  theme(plot.margin = unit(c(2,2,0,0), unit="pt"), 
        plot.title = element_text(size = 12, hjust = 0, color = shUEFA["blueLt"])) +
  
  labs(title = "Timestamp") + 
  
  coord_cartesian(xlim = c(0, max(pers$cum_total_seconds)), ylim = c(-20,100), expand = 0) + 
  #big rectangle
  geom_rect(aes(xmin = 0, xmax = max(pers$cum_total_seconds), ymin = 0, ymax = 100), 
            fill = icyUEFA["ice2"], color = icyUEFA["ice4"], size = 1) +
  #period divider
  geom_segment(aes(x = pers$cum_total_seconds, xend = pers$cum_total_seconds, 
                   y = rep.int(-15, dim(pers)[1]), yend = rep.int(100, dim(pers)[1])), 
               color = icyUEFA["ice4"], size = 1) + 
  #time line
  geom_segment(aes(x = 0, xend = max(pers$cum_total_seconds), 
                   y = 50, yend = 50), color = icyUEFA["ice5"], size = 1.5) + 
  #time line ends
  geom_segment(aes(x = c(0, max(pers$cum_total_seconds)), 
                   xend = c(0, max(pers$cum_total_seconds)), 
                   y = c(40, 40), yend = c(60,60)), color = icyUEFA["ice5"], size = 2) + 
  #period end-time labels
  annotate("text", y = rep.int(-10, dim(pers)[1]), x = pers$cum_total_seconds - 50, 
           label = pers$max_ts, color = shUEFA["blueLt"], hjust = 1) +
  
  #selected time marker
  annotate("segment", x = selected_time, xend = selected_time, y = 0, yend = 100, 
           color = shUEFA["orangeDk"], size = 1) + 
  
  #selected time labels
  #timestamp
  annotate("text", x = selected_time + this.posAdj, y = 80, 
           label = display_ts, hjust = this.hjust,
           color = icyUEFA["ice5"], size = 6) + 
  #period
  annotate("text", x = selected_time + this.posAdj, y = 20, 
           label = paste0("Period ",cp), hjust = this.hjust, 
           color = icyUEFA["ice5"], size = 6) + 
  
  
  

#-----

#possession plot
#~~~~~~~~~~~~~~~~~~~~
#to make a manual color scale, need vector same length as num of levels,
#and to rename colors to match levels
clrs <- c(icyUEFA["ice3"], icyUEFA["ice5"])
names(clrs) <- c(homeName,awayName)

#selected possession & team
cur_poss <- poss %>% filter(start_of_poss < selected_time, end_of_poss >= selected_time)
selected_team <- cur_poss$possession_team

this.hjust <- ifelse(input$slider_time > (max(pers$cum_total_seconds)/2), 1, 0)
this.posAdj <- ifelse(input$slider_time > (max(pers$cum_total_seconds)/2), -100, 100)
#----
#plot of possessions throughout the match
ggplot(poss) + shUEFA_theme_icy + 
  theme(plot.margin = unit(c(2,2,0,0), unit="pt"),
        plot.title = element_text(size = 12, hjust = 0, color = shUEFA["blueLt"])
  ) +
  labs(title = "Possession") + 
  
  coord_cartesian(xlim = c(0, max(pers$cum_total_seconds)), ylim = c(0, 3), expand = 0) +
  geom_rect(aes(xmin = start_of_poss, xmax = end_of_poss,
                ymin = 0, ymax = 2, 
                color = possession_team, fill = possession_team)) + 
  
  #colors
  scale_color_manual(values = clrs) + 
  scale_fill_manual(values = clrs) + 
  
  #period divider
  annotate("segment", x = pers$cum_total_seconds, xend = pers$cum_total_seconds, 
           y = rep.int(0, dim(pers)[1]), yend = rep.int(3, dim(pers)[1]), 
           color = icyUEFA["ice4"], size = 1) + 
  
  #selected time marker
  annotate("segment", x = selected_time, xend = selected_time, 
           y = 0, yend = 3, color = shUEFA["orangeDk"], size = 1) + 
  
  #possession team label
  annotate("text", x = selected_time + this.posAdj, y = 2.5, 
           label = as.character(selected_team), hjust = this.hjust,
           color = icyUEFA["ice5"], size = 8)

#----


#player events plot (frmly roster timeline plot)
#~~~~~~~~~~~~~~~~~~~~

#get match-specific information

#number of players on each team
nA <- dim(mp %>% filter(home_or_away == "away"))[1]
nH <- dim(mp %>% filter(home_or_away == "home"))[1]


#----
ggplot(mp) + shUEFA_theme_icy + 
  
  theme(plot.margin = unit(c(2,2,0,0), unit="pt"),
        plot.title = element_text(size = 12, hjust = 0, color = shUEFA["blueLt"])) +
  
  labs(title = "Player Events") + 
  coord_cartesian(xlim = c(0, max(pers$cum_total_seconds)), ylim = c(0, nH+nA+2), expand = 0) +
  
  #dotted lines for each player
  annotate("segment", x = rep.int(0, dim(mp)[1]), xend = rep.int(max(pers$cum_total_seconds), dim(mp)[1]), y = mp$timeline_y, yend = mp$timeline_y, 
           color = icyUEFA["ice3"], linetype = 2, size = 1) + 
  
  #rectangles around each team
  annotate("rect", xmin = 0, xmax = max(pers$cum_total_seconds), ymin = 0, ymax = nH+1, fill = NA, color = icyUEFA["ice2"]) + 
  annotate("rect", xmin = 0, xmax = max(pers$cum_total_seconds), ymin = nH+1, ymax = nH+nA+2, fill = NA, color = icyUEFA["ice2"]) + 
  
  #on-field timelines for each player
  geom_segment(aes(x = ts_on_cum_seconds, xend = ts_off_cum_seconds, y = timeline_y, yend = timeline_y), color = icyUEFA["ice5"], size = 1) + 
  
  #period divider
  annotate("segment", x = pers$cum_total_seconds, xend = pers$cum_total_seconds, 
           y = rep.int(0, dim(pers)[1]), yend = rep.int((nH+nA+2), dim(pers)[1]), 
           color = icyUEFA["ice4"], size = 1) + 
  
  #selected time marker
  annotate("segment", x = selected_time, xend = selected_time, 
           y = 0, yend = nH+nA+2, color = shUEFA["orangeDk"], size = 1) + 
  
  #events -- going to need to assign shape and color scales depending on event types
  geom_point(data = this.ev, aes(x = cum_match_seconds, y = timeline_y), color = shUEFA["orange"], shape = "|", size = 5)


#closer look at this.ev to visualize events better ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

eventTypes <- unique(ev$type.name)
#all possible event types:

#[1] "Starting XI"       "Half Start"        "Pass"             
#[4] "Ball Receipt*"     "Carry"             "Pressure"         
#[7] "Miscontrol"        "Interception"      "Dribble"          
#[10] "Duel"              "Ball Recovery"     "Block"            
#[13] "Shot"              "Goal Keeper"       "Clearance"        
#[16] "Dribbled Past"     "Foul Committed"    "Foul Won"         
#[19] "Dispossessed"      "Offside"           "Shield"           
#[22] "Half End"          "Substitution"      "Tactical Shift"   
#[25] "Injury Stoppage"   "Own Goal Against"  "Own Goal For"     
#[28] "Referee Ball-Drop" "Error"             "Player Off"       
#[31] "Player On"         "50/50"             "Bad Behaviour"

#to make a manual color scale, need vector same length as num of levels,
#and to rename colors to match levels
clrs <- c(icyUEFA["ice3"], icyUEFA["ice5"])
names(clrs) <- c(homeName,awayName)
length(eventTypes)

events_scale_shapes <- c(NA, NA, 19,
                         19, 15, 6,
                         13, 8, 15,
                         17, 19, 5, 
                         19, 18, 1,
                         0, 4, 4,
                         13, 4, 5, 
                         NA, NA, NA,
                         NA, 19, 19,
                         3, 4, NA,
                         NA, 17, 4)
names(events_scale_shapes) <- eventTypes

events_scale_colors <- c(NA, NA, shUEFA["orange"],
                         shUEFA["yellow"], shUEFA["yellow"], shUEFA["purpleLt"],
                         shUEFA["orangeDk"], shUEFA["purple"], shUEFA["yellow"],
                         shUEFA["orange"], shUEFA["orange"], shUEFA["purpleLt"],
                         shUEFA["orangeDk"], shUEFA["blueMed"], shUEFA["purple"],
                         shUEFA["purpleLt"], shUEFA["blueNavy"], shUEFA["blueSky"],
                         shUEFA["orangeDk"], shUEFA["blueNavy"], shUEFA["purpleLt"],
                         NA, NA, NA, 
                         NA, shUEFA["blueNavy"], shUEFA["orangeDk"],
                         shUEFA["blueMed"], shUEFA["blueNavy"], NA,
                         NA, shUEFA["yellow"], shUEFA["blueNavy"])
names(events_scale_colors) <- eventTypes


ggplot(mp) + shUEFA_theme_icy + 
  
  theme(plot.margin = unit(c(2,2,0,0), unit="pt"),
        plot.title = element_text(size = 12, hjust = 0, color = shUEFA["blueLt"])) +
  
  labs(title = "Player Events") + 
  coord_cartesian(xlim = c(0, max(pers$cum_total_seconds)), ylim = c(0, nH+nA+2), expand = 0) +
  
  #dotted lines for each player
  annotate("segment", x = rep.int(0, dim(mp)[1]), xend = rep.int(max(pers$cum_total_seconds), dim(mp)[1]), y = mp$timeline_y, yend = mp$timeline_y, 
           color = icyUEFA["ice3"], linetype = 2, size = 1) + 
  
  #rectangles around each team
  annotate("rect", xmin = 0, xmax = max(pers$cum_total_seconds), ymin = 0, ymax = nH+1, fill = NA, color = icyUEFA["ice2"]) + 
  annotate("rect", xmin = 0, xmax = max(pers$cum_total_seconds), ymin = nH+1, ymax = nH+nA+2, fill = NA, color = icyUEFA["ice2"]) + 
  
  #on-field timelines for each player
  geom_segment(aes(x = ts_on_cum_seconds, xend = ts_off_cum_seconds, y = timeline_y, yend = timeline_y), color = icyUEFA["ice5"], size = 1) + 
  
  #period divider
  annotate("segment", x = pers$cum_total_seconds, xend = pers$cum_total_seconds, 
           y = rep.int(0, dim(pers)[1]), yend = rep.int((nH+nA+2), dim(pers)[1]), 
           color = icyUEFA["ice4"], size = 1) + 
  
  #selected time marker
  annotate("segment", x = selected_time, xend = selected_time, 
           y = 0, yend = nH+nA+2, color = shUEFA["orangeDk"], size = 1) + 
  
  #events
  geom_point(data = this.ev, aes(x = cum_match_seconds, y = timeline_y, shape = type.name, color = type.name), size = 3) + 
  #shape and color scales depending on event types
  scale_color_manual(values = events_scale_colors) + 
  scale_shape_manual(values = events_scale_shapes)
  












#----


#roster plot
#~~~~~~~~~~~~~~~~~~~~
#require match id input and time input 

ggplot(mp) + shUEFA_theme_icy + 
  
  theme(plot.margin = unit(c(2,2,0,0), unit="pt"),
        plot.title = element_text(size = 12, hjust = 0, color = shUEFA["blueLt"])) +
  
  labs(title = "Players") + 
  coord_cartesian(xlim = c(0, 500), ylim = c(0, nH+nA+2), expand = 0) +
  
  #rectangles around each team
  annotate("rect", xmin = 0, xmax = 500, ymin = 0, ymax = nH+1, fill = NA, color = icyUEFA["ice2"]) + 
  annotate("rect", xmin = 0, xmax = 500, ymin = nH+1, ymax = nH+nA+2, fill = NA, color = icyUEFA["ice2"]) + 
  
  #player name
  annotate("text", x = rep.int(10, dim(mp)[1]), y = mp$timeline_y, label = str_trunc(mp$player.name, width = 20, side = "right"), hjust = 0, color = shUEFA["blueLt"], size = 5) + 
  #position
  annotate("text", x = rep.int(300, dim(mp)[1]), y = mp$timeline_y, label = mp$position_abbr, hjust = 0.5, color = shUEFA["blueLt"], size = 5) + 
  #team
  annotate("text", x = rep.int(490, dim(mp)[1]), y = mp$timeline_y, label = mp$team.name, hjust = 1, color = shUEFA["blueLt"], size = 5) +

  #highlighted player name
  annotate("rect", xmin = rep.int(0, dim(tb_mp)[1]), xmax = rep.int(500, dim(tb_mp)[1]), ymin = tb_mp$timeline_y - 0.5, ymax = tb_mp$timeline_y + 0.5, fill = icyUEFA["ice2"]) +
  annotate("text", x = rep.int(10, dim(tb_mp)[1]), y = tb_mp$timeline_y, label = str_trunc(tb_mp$player.name, width = 20, side = "right"), hjust = 0, color = shUEFA["blueMed"], size = 5) + 
  #position
  annotate("text", x = rep.int(300, dim(tb_mp)[1]), y = tb_mp$timeline_y, label = tb_mp$position_abbr, hjust = 0.5, color = shUEFA["blueMed"], size = 5) + 
  #team
  annotate("text", x = rep.int(490, dim(tb_mp)[1]), y = tb_mp$timeline_y, label = tb_mp$team.name, hjust = 1, color = shUEFA["blueMed"], size = 5)


#when a player is selected:



