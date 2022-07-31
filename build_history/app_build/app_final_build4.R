

library(tidyverse)
library(lubridate)

source("app_functions/functions_and_constants_visual2.R")
source("app_functions/functions_and_constants_time2.R")

m <- read.csv("app_data/matches_corr.csv")
ev <- read.csv("app_data/events_with_wdl2.csv")
all_mp <- read.csv("app_data/all_match_players_corr.csv")

matchIDs <- unique(m$match_id)
names(matchIDs) <- m$menu_text

#|~||~||~||~||~||~||~||~| this_match_data() |~||~||~||~||~||~||~||~|
#selected_match <- input$match_select
selected_match <- matchIDs[4]

this.m <- m %>% filter(match_id == selected_match)
this.ev <- ev %>% filter(match_id == selected_match)

mp <- all_mp %>% filter(match_id == selected_match)
pers <- get_period_summary(this.ev)
poss <- get_possession_summary(this.ev)

this.tb <- this.ev %>% arrange(index) %>% group_by(time_bin) %>%
  summarize(start_cur_away_score = first(cur_away_score),
            end_cur_away_score = last(cur_away_score),
            start_cur_home_score = first(cur_home_score),
            end_cur_home_score = last(cur_home_score),
            start_p_win = first(p_win),
            start_p_draw = first(p_draw),
            start_p_loss = first(p_loss),
            end_p_win = last(p_win),
            end_p_draw = last(p_draw),
            end_p_loss = last(p_loss)) %>%
  mutate(away_goal = end_cur_away_score - start_cur_away_score,
         home_goal = end_cur_home_score - start_cur_home_score,
         delta_p_win = end_p_win - start_p_win,
         delta_p_draw = end_p_draw - start_p_draw,
         delta_p_loss = end_p_loss - start_p_loss)

#get names of teams
awayName <- this.m %>% pull(away_team.away_team_name)
homeName <- this.m %>% pull(home_team.home_team_name)

#i need to calculate the time bin for events, so i can connect them to the model data
#get end ts_seconds of match (2 periods only)
max_sec <- max(this.ev$cum_match_seconds)
#divide it by 90 bins
bin_width <- max_sec/90

tmd <- list(this_m = this.m,
              this_ev = this.ev,
              this_tb = this.tb,
              mp = mp,
              pers = pers,
              poss = poss,
              awayName = awayName,
              homeName = homeName,
              max_sec = max_sec,
              bin_width = bin_width)

#|~||~||~||~||~||~||~||~| score_plot |~||~||~||~||~||~||~||~|

cas <- this.tb$end_cur_away_score
chs <- this.tb$end_cur_home_score

cas2 <- c(cas, rep.int(max(cas, na.rm = T), ifelse(length(cas) < 90, (90 - length(cas)), 0)))[1:90]
chs2 <- c(chs, rep.int(max(chs, na.rm = T), ifelse(length(chs) < 90, (90 - length(chs)), 0)))[1:90]

max_score = max(max(cas, na.rm = T), max(chs, na.rm = T))

#selected_time <- input$slider_time
selected_time <- 400
selected_tb <- get_time_bin(selected_time, tmd[["max_sec"]])

this.hjust <- ifelse(selected_tb > 45, 1, 0)
this.posAdj <- ifelse(selected_tb > 45, -1, 1)

tb_as <- this.tb %>% filter(time_bin == selected_tb) %>% pull(end_cur_away_score)
tb_hs <- this.tb %>% filter(time_bin == selected_tb) %>% pull(end_cur_home_score)

lossColor <- shUEFA["ibm_blue"]
winColor <- shUEFA["ibm_pink"]

ggplot(this.tb) + 
  shUEFA_theme_icy + 
  theme(plot.margin = unit(c(2,2,0,0), unit="pt"), 
        plot.title = element_text(size = 12, hjust = 0, color = shUEFA["blueLt"])) +
  coord_cartesian(xlim = c(0, 90), ylim = c(-0.05, max_score+1.5), expand = 0) +
  labs(title = "Score") + 
  #goal markers
  annotate("segment", x = rep.int(0, (max_score+1)), xend = rep.int(90, (max_score+1)), y = 0:max_score, yend = 0:max_score, linetype = 1, color = icyUEFA["ice3"], size = 0.5) +
  annotate("text", x = rep.int(89, (max_score+1)), y = 0:max_score + 0.5, label = as.character(0:max_score), size = 4, color = icyUEFA["ice5"], hjust = 1) +
  
  #away team score
  annotate("rect", xmin = seq(0, 89, 1), xmax = seq(1, 90, 1), ymin = 0, ymax = cas2, fill = lossColor, alpha = 0.7) +
  #home team score
  annotate("rect", xmin = seq(0, 89, 1), xmax = seq(1, 90, 1), ymin = 0, ymax = chs2, fill = winColor, alpha = 0.4) + 
  
  #away team score
  annotate("segment", x = seq(0, 89, 1), xend = seq(1, 90, 1), y = cas2, yend = cas2, color = lossColor, size = 1.1, linetype = 1) +
  #home team score
  annotate("segment", x = seq(0, 89, 1), xend = seq(1, 90, 1), y = chs2, yend = chs2, color = winColor, size = 1, linetype = 3) + 
  
  #selected time marker
  annotate("segment", x = selected_time/tmd[["bin_width"]], xend = selected_time/tmd[["bin_width"]], 
           y = 0, yend = max_score+1.5, color = shUEFA["orangeDk"], size = 1) + 
  #selected time score away
  annotate("text", x = selected_time/tmd[["bin_width"]] + this.posAdj, y = max_score + 0.4, label = paste0(tmd[["awayName"]], " ", tb_as), hjust = this.hjust, color = lossColor, size = 6) +
  #selected time score home
  annotate("text", x = selected_time/tmd[["bin_width"]] + this.posAdj, y = max_score + 1.2, label = paste0(tmd[["homeName"]], " ", tb_hs), hjust = this.hjust, color = winColor, size = 6)


#|~||~||~||~||~||~||~||~| wp_plot |~||~||~||~||~||~||~||~|


#selected_time <- input$slider_time
#selected_tb <- floor(selected_time/tmd[["bin_width"]])
selected_time <- 400
selected_tb <- get_time_bin(selected_time, tmd[["max_sec"]])

winColor = shUEFA["ibm_pink"]
drawColor = shUEFA["ibm_yellow"]
lossColor = shUEFA["ibm_blue"]

pWin <- this.tb %>% filter(time_bin == selected_tb) %>% pull(start_p_win)
pDraw <- this.tb %>% filter(time_bin == selected_tb) %>% pull(start_p_draw)
pLoss <- this.tb %>% filter(time_bin == selected_tb) %>% pull(start_p_loss)

#pWin <- round(median(p_Win, na.rm = T), digits = 3)
#pDraw <- round(median(p_Draw, na.rm = T), digits = 3)
#pLoss <- round(median(p_Loss, na.rm = T), digits = 3)

ggplot(this.ev) + shUEFA_theme_icy +
  theme(plot.margin = unit(c(2,2,0,0), unit="pt"), 
        plot.title = element_text(size = 12, hjust = 0, color = shUEFA["blueLt"])) +
  coord_cartesian(xlim = c(0, max_sec), ylim = c(0,1), expand = 0) + 
  
  labs(title = "Match Outcome Probability") + 
  
  #probability lines
  geom_line(aes(x = cum_match_seconds, y = p_win), color = winColor) + 
  geom_line(aes(x = cum_match_seconds, y = p_draw), color = drawColor) + 
  geom_line(aes(x = cum_match_seconds, y = p_loss), color = lossColor) +
  
  geom_smooth(aes(x = cum_match_seconds, y = p_win), color = winColor) + 
  geom_smooth(aes(x = cum_match_seconds, y = p_draw), color = drawColor) + 
  geom_smooth(aes(x = cum_match_seconds, y = p_loss), color = lossColor) +
  
  #legend
  annotate("rect", xmin = 0, xmax = 34, ymin = 0.68, ymax = 1, fill = "white", alpha = 0.5) + 
  annotate("text", x = 1, y = 0.95, label = paste0("P(", homeName, " Win) = ", pWin), color = winColor, hjust = 0, size = 6) + 
  annotate("text", x = 1, y = 0.85, label = paste0("P(Draw) = ", pDraw), color = drawColor, hjust = 0, size = 6) +
  annotate("text", x = 1, y = 0.75, label = paste0("P(", awayName, " Win) = ", pLoss), color = lossColor, hjust = 0, size = 6) +
  
  #selected time marker
  annotate("segment", x = selected_time, xend = selected_time, 
           y = 0, yend = 1, color = shUEFA["orangeDk"], size = 1)




