

library(shiny)
library(tidyverse)
library(lubridate)

source("app_functions/functions_and_constants_visual.R")
source("app_functions/functions_and_constants_time.R")

# ||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=
#                               Get Data
# ||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=

m <- read.csv("app_data/dbb_matches.csv")
ev <- read.csv("app_data/events.csv")
all_mp <- read.csv("app_data/all_match_players_corr.csv")
#future goals and other model input data, min-by-min
fg <- read.csv("app_data/futureGoals_55_7.csv")
#all min-by-min goal probabilities for uefa 2020
gp <- read.csv("app_data/goal_probabilities_UEFA2020_dbg.csv")
#all min-by-min win/draw/lose probabilities for uefa 2020
wdl <- read.csv("app_data/win_probabilities_UEFA2020_dbg.csv")


#list of unique match_id values for selectInput
matchIDs <- unique(m$match_id)
names(matchIDs) <- m$menu_text


#UI-----

ui <- fluidPage(
  
  tags$head(
    
    tags$title("StatsBomb360 Win Probability Dashboard"),
    
    #app styling
    #----
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Dosis&display=swap');
      body {
        background-color: #ccebff;
        color: #223fb3;
      }
      
      h1, h2 {
        font-family: 'Dosis', sans-serif;
        color: #223fb3;
      }
      
      .col-sm-1, .col-sm-2, .col-sm-3, .col-sm-4, .col-sm-5, .col-sm-6, .col-sm-7, .col-sm-8, .col-sm-9, .col-sm-10, .col-sm-11, .col-sm-12 {
        padding: 10px;
        margin: 0px;
      
      }
      
      .row {
        padding: 0px;
        margin: 0px;
      
      }
      
      .shiny-html-output {
        
        width: 100%;
        background-color: #a3daff;
        color: #223fb3;
      }
      
      #match_select_wrapper {
        
        padding: 10px; 
        background-color: #ffe6cc;
        border: 2px solid #ff4d00;
        color: #d64202;
        
      
      }
      
      #match_info_wrapper {
        padding: 10px;
        margin: 10px;
      }
      
      #event_type_select_wrapper {
        padding: 10px; 
        background-color: #ffe6cc;
        border: 2px solid #ff4d00;
      
      }
      
      #timeline_slider_wrapper {
        background-color: #ffe6cc;
        border: 1px solid #ff4d00;
      
      }
      
      .shiny-input-container {
        
        background-color: #ffe6cc;
        color: #d64202;
        
      }"))
    #----
  ),
  
  #header
  fluidRow(
    column(4, 
           tags$img(src = "SB_CoreWordmark_ColourPositive.png", width = "80%"),
           tags$img(src = "SB_BrandIcon_ColourPositive.png", width = "15%")
           ),
    column(8,
           tags$h1("Win Probability Evaluator")
           )
    
  ),
  
  
  #first main row - match selection input
  fluidRow(
    
    tags$div(id = "match_select_wrapper",
     
        tags$p("Welcome to the StatsBomb Match Evaluator! This app uses StatsBomb soccer events data and Win Probability to provide and evidence-based approach to reviewing the events of a match and accurately evaluating player contributions to the outcome."),
        #match selection input
        selectInput("match_select", "Select a match from the UEFA Euro 2020 Competition:", matchIDs, selectize = F, width = "500px")
        
      )
      
    ),
    

  
  #second main row - plots and other content
  fluidRow(
    #first column, timeline plots
    column(7,
           plotOutput("score_plot", height = 125),
           plotOutput("possession_plot", height = 100),
           plotOutput("wp_plot", height = 250),
           plotOutput("timeline_plot", height = 100),
           
           div(id = "timeline_slider_wrapper",
              uiOutput("timeline_slider")
           )
           
           
           
           ),
    #second column - text and tables, pitch plot
    column(5,
           htmlOutput("match_info", height = 100),
           plotOutput("pitch_plot", height = 350, click = "pitch_click"),
           verbatimTextOutput("pitch_event_data")
           
           )
    
  ),
  
  #event type checkbox input row
  fluidRow(
    div(id = "event_type_select_wrapper",
           checkboxGroupInput("event_select", "Include event types:", choices = eventTypes2, selected = eventTypes2,
                              inline = T, width = "100%")
           )
    
    
    
  ),
  
  #player event timeline and roster row
  fluidRow(
    column(7,
           plotOutput("roster_timeline_plot", height = 600, brush = "timeline_brush"),
           verbatimTextOutput("event_data")
           ),
    
    column(5,
           plotOutput("roster_plot", height = 600)
           )
    
    
  )
)

#-----    


server <- function(input, output){
  
  #update data according to match id input
  this_match_data <- reactive({
    
    req(input$match_select)
    selected_match <- input$match_select
    #row of general info for match
    # Match ID Reactive Data
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
    #standardize location coordinates
    this.ev <- this.ev %>%
      mutate(is_home_team = ifelse(team.name == homeName, 1, 0)) %>%
      mutate(std_location_x = ifelse(is_home_team == 0, 120 - location.x, location.x),
             std_location_y = ifelse(is_home_team == 0, 80 - location.y, location.y))
    
    #add timeline_y values to events df
    tly <- mp %>% select(player.id, timeline_y)
    this.ev <- this.ev %>% left_join(tly, by = "player.id")
    
    
    return(#this match data
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
                  bin_width = bin_width))
  })
  
  
  this_time_bin_data <- reactive({
    
    tmd <- this_match_data()
    
    req(input$slider_time)
    selected_tb <- get_time_bin(input$slider_time, tmd[["max_sec"]])
    
    #time-bin-reactive data
    tb_wdl <- tmd[["this_wdl"]] %>% filter(time_bin == selected_tb)
    tb_fg <- tmd[["this_fg"]] %>% filter(time_bin == selected_tb)
    tb_ev <- tmd[["this_ev"]] %>% filter(time_bin == selected_tb)
    #when a time bin is selected, i want to highlight the players that are involved
    tb_players <- unique(tb_ev$player.id)
    tb_mp <- tmd[["mp"]] %>% filter(player.id %in% tb_players)
    
    return(list(
      tb_wdl = tb_wdl,
      tb_fg = tb_fg,
      tb_ev = tb_ev,
      tb_mp = tb_mp
    ))
    
  })
  
  output$match_info <- renderUI({
    
    tmd <- this_match_data()
    this.m <- tmd[["this_m"]]
    
    tags$div(id = "match_info_wrapper",
             tags$tr(tags$td(paste0(tmd[["awayName"]], " at ", tmd[["homeName"]])),
                     tags$td(paste0(this.m$away_score, " - ", this.m$home_score)),
                     tags$td(paste0(this.m$competition.competition_name, " ", this.m$season.season_name)), 
                     tags$td(paste0("Week ", this.m$match_week)),
                     tags$td(paste0(this.m$stadium.name, " ", this.m$stadium.country.name))))
    
    
    
  })
  
  output$score_plot <- renderPlot({
    tmd <- this_match_data()
    this.fg <- tmd[["this_fg"]]
    
    cas <- this.fg$cur_away_score
    chs <- this.fg$cur_home_score
    
    cas2 <- c(cas, rep.int(max(cas, na.rm = T), (90 - length(cas))))
    chs2 <- c(chs, rep.int(max(chs, na.rm = T), (90 - length(chs))))
    
    max_score = max(max(cas, na.rm = T), max(chs, na.rm = T))
    
    req(input$slider_time)
    selected_time <- input$slider_time
    selected_tb <- get_time_bin(selected_time, tmd[["max_sec"]])
    
    this.hjust <- ifelse(selected_time/tmd[["bin_width"]] > 45, 1, 0)
    this.posAdj <- ifelse(selected_time/tmd[["bin_width"]] > 45, -1, 1)
    
    tb_as <- this.fg %>% filter(time_bin == selected_tb) %>% pull(cur_away_score)
    tb_hs <- this.fg %>% filter(time_bin == selected_tb) %>% pull(cur_home_score)
    
    lossColor <- shUEFA["ibm_blue"]
    winColor <- shUEFA["ibm_pink"]
    
    ggplot(this.fg) + 
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
      annotate("segment", x = seq(0, 89, 1), xend = seq(1, 90, 1), y = chs2, yend = chs2, color = winColor, size = 1, linetype = 2) + 
      
      #selected time marker
      annotate("segment", x = selected_time/tmd[["bin_width"]], xend = selected_time/tmd[["bin_width"]], 
               y = 0, yend = max_score+1.5, color = shUEFA["orangeDk"], size = 1) + 
      #selected time score away
      annotate("text", x = selected_time/tmd[["bin_width"]] + this.posAdj, y = max_score + 0.4, label = paste0(tmd[["awayName"]], " ", tb_as), hjust = this.hjust, color = lossColor, size = 6) +
      #selected time score home
      annotate("text", x = selected_time/tmd[["bin_width"]] + this.posAdj, y = max_score + 1.2, label = paste0(tmd[["homeName"]], " ", tb_hs), hjust = this.hjust, color = winColor, size = 6)
    
    
  })
 
  #win probability plot
  output$wp_plot <- renderPlot({
    
    tmd <- this_match_data()
    this.wdl <- tmd[["this_wdl"]]
    this.fg <- tmd[["this_fg"]]
    awayName <- tmd[["awayName"]]
    homeName <- tmd[["homeName"]]
    
    req(input$slider_time)
    selected_time <- input$slider_time
    selected_tb <- floor(selected_time/tmd[["bin_width"]])
    
    winColor = shUEFA["ibm_pink"]
    drawColor = shUEFA["ibm_yellow"]
    lossColor = shUEFA["ibm_blue"]
    
    pWin <- this.wdl %>% filter(time_bin == selected_tb) %>% pull(p_win)
    pDraw <- this.wdl %>% filter(time_bin == selected_tb) %>% pull(p_draw)
    pLoss <- this.wdl %>% filter(time_bin == selected_tb) %>% pull(p_loss)
    
    ggplot(this.wdl) + shUEFA_theme_icy +
      theme(plot.margin = unit(c(2,2,0,0), unit="pt"), 
            plot.title = element_text(size = 12, hjust = 0, color = shUEFA["blueLt"])) +
      
      labs(title = "Match Outcome Probability") + 
      
      coord_cartesian(xlim = c(0, 90), ylim = c(0,1), expand = 0) + 
      
      annotate("rect", xmin = seq(0, 88, 2), xmax = seq(1, 89, 2), ymin = 0, ymax = 1, fill = "white", alpha = 0.5) + 
      
      #probability lines
      geom_line(aes(x = time_bin, y = p_win), color = winColor, size = 1) + 
      geom_line(aes(x = time_bin, y = p_draw), color = drawColor, size = 1) + 
      geom_line(aes(x = time_bin, y = p_loss), color = lossColor, size = 1) +
      
      #legend
      annotate("rect", xmin = 0, xmax = 34, ymin = 0.68, ymax = 1, fill = "white", alpha = 0.5) + 
      annotate("text", x = 1, y = 0.95, label = paste0("P(", homeName, " Win) = ", pWin), color = winColor, hjust = 0, size = 6) + 
      annotate("text", x = 1, y = 0.85, label = paste0("P(Draw) = ", pDraw), color = drawColor, hjust = 0, size = 6) +
      annotate("text", x = 1, y = 0.75, label = paste0("P(", awayName, " Win) = ", pLoss), color = lossColor, hjust = 0, size = 6) +
      
      #selected time marker
      annotate("segment", x = selected_time/tmd[["bin_width"]], xend = selected_time/tmd[["bin_width"]], 
               y = 0, yend = 1, color = shUEFA["orangeDk"], size = 1)
    
    
  })
  
  #time slider input
  #~~~~~~~~~~~~~~~~~~~~
  output$timeline_slider <- renderUI({
    
    tmd <- this_match_data()
    pers <- tmd[["pers"]]
    
    sliderInput("slider_time", "Select time:", min = 0, max = max(pers$cum_total_seconds), 
                value = 0, width = "100%")
    
  })
  
  
  
  #pitch plot
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$pitch_plot <- renderPlot({
    
    ttbd <- this_time_bin_data()
    tb_ev <- ttbd[["tb_ev"]]
    tb_fg <- ttbd[["tb_fg"]]
    
    req(input$event_select)
    selected_events <- input$event_select
    
    #add on locations of events that happened in the time bin
    plot_pitch(tb_ev %>% filter(type.name %in% selected_events), icyUEFA["ice5"]) + shUEFA_theme_icy +
      geom_point(aes(x = std_location_x, y = std_location_y, shape = type.name, color = type.name), size = 5) + 
      scale_color_manual(values = events_scale_colors) + 
      scale_shape_manual(values = events_scale_shapes) +
      annotate("point", x = tb_fg$start_loc_x, y = tb_fg$start_loc_y, color = shUEFA["blueMed"], shape = 10, size = 5) +
      annotate("point", x = tb_fg$end_loc_x, y = tb_fg$end_loc_y, color = shUEFA["blueSky"], shape = 10, size = 5) +
      annotate("segment", x = tb_fg$start_loc_x, xend = tb_fg$end_loc_x, y = tb_fg$start_loc_y, yend = tb_fg$end_loc_y, arrow = my_arrow, color = shUEFA["blueMed"])
    #i want these to have tool tips when I hover over them
    
  })
  
  output$pitch_event_data <- renderPrint({
    
    ttbd <- this_time_bin_data()
    tb_ev <- ttbd[["tb_ev"]]
    
    req(input$pitch_click)
    
    np <- nearPoints(tb_ev, input$pitch_click, threshold = 10, maxpoints = 10, xvar = "std_location_x", yvar = "std_location_y")
    np %>% arrange(index) %>% select(timestamp, type.name, player.name, team.name)
    
  })
  
  
  #timeline plot
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$timeline_plot <- renderPlot({
    
    tmd <- this_match_data()
    pers <- tmd[["pers"]]
    
    req(input$slider_time)
    selected_time <- input$slider_time
    
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
               label = paste0("Period ", cp), hjust = this.hjust, 
               color = icyUEFA["ice5"], size = 6)
    
    #-----
    
  })
  
  
  output$possession_plot <- renderPlot({
    tmd <- this_match_data()
    pers <- tmd[["pers"]]
    poss <- tmd[["poss"]]
    
    homeColor = shUEFA["ibm_pink"]
    awayColor = shUEFA["ibm_blue"]
    
    #to make a manual color scale, need vector same length as num of levels,
    #and to rename colors to match levels
    clrs <- c(homeColor, awayColor)
    names(clrs) <- c(tmd[["homeName"]], tmd[["awayName"]])
    
    req(input$slider_time)
    selected_time <- input$slider_time
    
    #selected possession & team
    cur_poss <- poss %>% filter(start_of_poss < selected_time, end_of_poss >= selected_time)
    selected_team <- cur_poss$possession_team
    
    this.hjust <- ifelse(selected_time > (max(pers$cum_total_seconds)/2), 1, 0)
    this.posAdj <- ifelse(selected_time > (max(pers$cum_total_seconds)/2), -100, 100)
    
    labelColor <- ifelse(selected_team == tmd[["awayName"]], awayColor, homeColor)
    
    #plot of possessions throughout the match
    ggplot(poss) + shUEFA_theme_icy + 
      theme(plot.margin = unit(c(2,2,0,0), unit="pt"),
            plot.title = element_text(size = 12, hjust = 0, color = shUEFA["blueLt"])) +
      
      labs(title = "Possession") + 
      
      coord_cartesian(xlim = c(0, max(pers$cum_total_seconds)), ylim = c(0, 3), expand = 0) +
      geom_rect(aes(xmin = start_of_poss, xmax = end_of_poss, ymin = 0, ymax = 2, 
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
               color = labelColor, size = 6)
    
    
    
    
  })
  

  #roster timeline plot
  #~~~~~~~~~~~~~~~~~~~~
  output$roster_timeline_plot <- renderPlot({
    
    tmd <- this_match_data()
    mp <- tmd[["mp"]]
    pers <- tmd[["pers"]]
    this.ev <- tmd[["this_ev"]]
    
    #number of players on each team
    nA <- dim(mp %>% filter(home_or_away == "away"))[1]
    nH <- dim(mp %>% filter(home_or_away == "home"))[1]
    
    req(input$slider_time)
    selected_time <- input$slider_time
    
    req(input$event_select)
    selected_events <- input$event_select
    
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
      geom_point(data = this.ev %>% filter(type.name %in% selected_events), aes(x = cum_match_seconds, y = timeline_y, shape = type.name, color = type.name), size = 3) + 
      #shape and color scales depending on event types
      scale_color_manual(values = events_scale_colors) + 
      scale_shape_manual(values = events_scale_shapes)
    
    
  })
  
  #roster plot
  #~~~~~~~~~~~~~~~~~~~~
  output$roster_plot <- renderPlot({
    
    tmd <- this_match_data()
    mp <- tmd[["mp"]]
    
    ttbd <- this_time_bin_data()
    tb_mp <- ttbd[["tb_mp"]]
    
    #number of players on each team
    nA <- dim(mp %>% filter(home_or_away == "away"))[1]
    nH <- dim(mp %>% filter(home_or_away == "home"))[1]
    
    
    
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
      
  })
  
  output$event_data <- renderPrint({
    
    tmd <- this_match_data()
    
    req(input$timeline_brush)
    
    bp <- brushedPoints(tmd[["this_ev"]], input$timeline_brush, xvar = "cum_match_seconds", yvar = "timeline_y")
    
    bp %>% select(function(x) !all(is.na(x)))
  })
  
  
  
  
  
}


shinyApp(ui, server)









