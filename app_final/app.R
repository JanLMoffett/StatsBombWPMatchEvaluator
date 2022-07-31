

library(shiny)
library(tidyverse)
library(lubridate)
library(DT)

source("app_functions/functions_and_constants_visual2.R")
source("app_functions/functions_and_constants_time2.R")

# ||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=
#                               Get Data
# ||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=

m <- read.csv("app_data/matches_corr.csv")
ev <- read.csv("app_data/events_with_wdl2.csv")
all_mp <- read.csv("app_data/all_match_players_corr.csv")

#list of unique match_id values for selectInput
matchIDs <- unique(m$match_id)
names(matchIDs) <- m$menu_text

# calculating delta win probability
ev <- ev %>% 
  arrange(match_id, index) %>%
  mutate(delta_p_win = p_win - lag(p_win, n = 1),
         delta_p_draw = p_draw - lag(p_draw, n = 1),
         delta_p_loss = p_loss - lag(p_loss, n = 1)) %>%
  mutate(delta_p_win_adj = ifelse(is_home_team == 1, delta_p_win, -1*delta_p_win),
         delta_p_draw_adj = ifelse(is_home_team == 1, delta_p_draw, -1*delta_p_draw),
         delta_p_loss_adj = ifelse(is_home_team == 1, delta_p_loss, -1*delta_p_loss))

#event WPA averages for competition
event_WPV_table <- ev %>% group_by(type.name) %>%
  summarize(
    count = n(),
    win_probability_value = mean(delta_p_win_adj, na.rm = T)) %>%
  rename(event_type = type.name) %>%
  arrange(desc(win_probability_value))

#UI-----

ui <- fluidPage(
  
  tags$head(
    
    tags$title("StatsBomb360 Win Probability Match Evaluator"),
    
    #app styling
    #----
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Dosis&display=swap');
      body {
        background-color: #ccebff;
        color: #223fb3;
      }
      
      h1, h2, h3 {
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
      
      .help-text {
        font-size: 0.9em;
        color: #d64202;
        
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
      
      .mi {
        margin: 0px;
        font-size: 1.1em;
      
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
      
      .slider_text_wrapper {
        padding: 10px;
      
      }
      
      .help_text_wrapper {
        padding: 10px; 
        background-color: #ffe6cc;
        border: 2px solid #ff4d00;
        color: #d64202;
        
      }
      
      .shiny-input-container {
        background-color: #ffe6cc;
        color: #d64202;
        
      }"))
    #----
    #----
  ),
  
  #header
  fluidRow(
    column(4, 
           tags$img(src = "SB_CoreWordmark_ColourPositive.png", width = "80%"),
           tags$img(src = "SB_BrandIcon_ColourPositive.png", width = "15%")
           ),
    column(8,
           tags$h1("Win Probability Match Evaluator"),
           tags$p("Designed and built by ", tags$a(href = "https://github.com/JanLMoffett/", "Jan Moffett"))
           )),
  
  #first main row - match selection input
  fluidRow(
    tags$div(id = "match_select_wrapper",
        
        tags$p(class = "help-text", "Welcome to the StatsBomb Win Probability Match Evaluator! This app uses StatsBomb soccer events data and Win Probability to provide an evidence-based approach to reviewing the events of a match and accurately evaluating player contributions to the outcome."),
        #match selection input
        selectInput("match_select", "Select a match from the UEFA Euro 2020 Competition:", matchIDs, selectize = F, width = "500px"),
        #hide help text checkbox
        checkboxInput("show_help", "Show Help Text", value = T)
        )),
  
  #second main row - plots and other content
  fluidRow(
    #first column, timeline plots
    column(7,
           plotOutput("timeline_plot", height = 100),
           
           div(id = "timeline_slider_wrapper",
               div(
                 class = "slider_text_wrapper",
                 conditionalPanel(
                   condition = "input.show_help",
                   tags$p(class = "help-text", 
                          "Use this slider to select a single second of time in the match. The timestamp and period will display above, the score and possession will display below.")
                 )
                 
               ),
               
               uiOutput("timeline_slider"),
               conditionalPanel(
                 condition = "input.show_help",
               
                 div(
                   class = "slider_text_wrapper",
                   tags$p(class = "help-text", 
                          "Click and drag on the Match Outcome Probability Plot below to select a time span. A detail of the Outcome Probability Plot and the events contained in the time span will display to the right.")
                 )
               ),
           ),
           plotOutput("wp_plot", height = 250,
                      brush = brushOpts("wp_brush", direction = "x")),
           plotOutput("score_plot", height = 125),
           plotOutput("possession_plot", height = 100),
           tags$br(),
           conditionalPanel(
             condition = "input.show_help",
             
             tags$div(class = "help_text_wrapper",
                      tags$p(class = "help-text",
                             "Each Player's events throughout the match are displayed below.  Scroll down for tables of Win Probability values for events and players."))),
           
           tags$br(),
           div(id = "event_type_select_wrapper",
               checkboxGroupInput("event_select", "Include event types:", choices = eventTypes2, selected = eventTypes2,
                                  inline = T, width = "100%")),
           plotOutput("events_legend", height = 120)
            
         ),
    #second column - text and tables, pitch plot
    column(5,
           tags$h3("Match Information"),
           htmlOutput("match_info", height = 100),
           
           conditionalPanel(
             condition = "input.show_help",
             tags$div(
               class = "help_text_wrapper",
               tags$p(class = "help-text",
                      "Click and drag on the plots below to refine your selection. Click again to clear your selection."
               )
             )
             
           ),
           
          plotOutput("wp_zoom_plot", height = 250,
                      brush = brushOpts("wp_zoom_brush", direction = "x")),
           
           
          plotOutput("pitch_plot", height = 350, 
                      brush = brushOpts("pitch_brush")),
           
             
          DT::dataTableOutput("pitch_event_data")
             
           
    
  )),
  
  
  #player event timeline and roster row
  fluidRow(
    column(7,
           plotOutput("roster_timeline_plot", height = 600, click = "timeline_click"),
           tags$h3("Match Win Probability Leaders"),
           dataTableOutput("player_WPA")
           
           ),
    
    column(5,
           plotOutput("roster_plot", height = 600),
           tags$h3("UEFA 2020 Win Probability Values for Event Types"),
           dataTableOutput("event_WPV")
           )
    
    
  )
)

#-----    


server <- function(input, output){
  
  # Match ID Reactive Data
  this_match_data <- reactive({
    
    req(input$match_select)
    selected_match <- input$match_select
    
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
  })
  
  wp_brushed_data <- reactive({
    tmd <- this_match_data()
    this.ev <- tmd[["this_ev"]]
    
    req(input$wp_brush)
    return(brushedPoints(this.ev, input$wp_brush, xvar = "cum_match_seconds") %>% arrange(desc(cum_match_seconds)))
    
  })
  
  wp_brushed_time <- reactive({
    tmd <- this_match_data()
    this.ev <- tmd[["this_ev"]]
    
    req(input$wp_brush)
    bp <- brushedPoints(this.ev, input$wp_brush, xvar = "cum_match_seconds")
    brushed_time_min <- min(bp$cum_match_seconds, na.rm = T)
    brushed_time_max <- max(bp$cum_match_seconds, na.rm = T)
    
    return(list(min = brushed_time_min, max = brushed_time_max))
    
  })
  
  wp_zoom_brushed_data <- reactive({
    
    tmd <- this_match_data()
    
    #default to events data from first minute
    rd <- tmd[["this_ev"]] %>% filter(between(cum_match_seconds, 0, 60), !is.na(player.name))
    
    if(!is.null(input$wp_brush)){
      rd <- wp_brushed_data()
    }
    
    req(input$wp_zoom_brush)
    bp <- brushedPoints(rd, input$wp_zoom_brush, xvar = "cum_match_seconds")
    
    return(bp)
    
  })
  
  pitch_brushed_data <- reactive({
    tmd <- this_match_data()
    
    #default to events data from first minute
    rd <- tmd[["this_ev"]] %>% filter(between(cum_match_seconds, 0, 60), !is.na(player.name))
    
    #if zoom is brushed, use zoom data
    if(!is.null(input$wp_zoom_brush)){
      rd <- wp_zoom_brushed_data()
    }else{
      if(!is.null(input$wp_brush)){
        rd <- wp_brushed_data()
        
      }else{
        rd <- tmd[["this_ev"]] %>% filter(between(cum_match_seconds, 0, 60), !is.na(player.name))
        
      }
    }
    
    req(input$pitch_brush)
    return(brushedPoints(rd, input$pitch_brush, xvar = "std_location_x", yvar = "std_location_y") %>% arrange(cum_match_seconds))
    
  })
  
  
  output$match_info <- renderUI({
    
    tmd <- this_match_data()
    this.m <- tmd[["this_m"]]
    
    tags$div(id = "match_info_wrapper",
             tags$tr(tags$td(tags$p(class = "mi", paste0(tmd[["awayName"]], " at ", tmd[["homeName"]], " | Final Score: ", this.m$away_score, " - ", this.m$home_score))),
                     tags$td(tags$p(class = "mi", paste0(this.m$competition.competition_name, " ", this.m$season.season_name, " | Week ", this.m$match_week))), 
                     tags$td(tags$p(class = "mi", paste0(this.m$stadium.name, " ", this.m$stadium.country.name, " | Referee: ", this.m$referee.name)))))
    
    
    
  })

  
  output$score_plot <- renderPlot({
    tmd <- this_match_data()
    this.tb <- tmd[["this_tb"]]
    pers <- tmd[["pers"]]
    
    cas <- this.tb$end_cur_away_score
    chs <- this.tb$end_cur_home_score
    
    cas2 <- c(cas, rep.int(max(cas, na.rm = T), ifelse(length(cas) < 90, (90 - length(cas)), 0)))[1:90]
    chs2 <- c(chs, rep.int(max(chs, na.rm = T), ifelse(length(chs) < 90, (90 - length(chs)), 0)))[1:90]
    
    max_score = max(max(cas, na.rm = T), max(chs, na.rm = T))
    
    req(input$slider_time)
    selected_time <- input$slider_time
    selected_tb <- get_time_bin(selected_time, tmd[["max_sec"]])
    
    this.hjust <- ifelse(selected_tb > 45, 1, 0)
    this.posAdj <- ifelse(selected_tb > 45, -1, 1)
    
    tb_as <- this.tb %>% filter(time_bin == selected_tb) %>% pull(end_cur_away_score)
    tb_hs <- this.tb %>% filter(time_bin == selected_tb) %>% pull(end_cur_home_score)
    
    winColor <- shUEFA["ibm_pink"]
    lossColor <- shUEFA["ibm_purple"]
    
    brushTime <- list(min = 0, max = 0)
    if(!is.null(input$wp_brush)){brushTime <- wp_brushed_time()} 
    
    ggplot(this.tb) + 
      shUEFA_theme_icy + 
      theme(plot.margin = unit(c(7,7,0,0), unit="pt"), 
            plot.title = element_text(size = 12, hjust = 0, color = shUEFA["blueLt"])) +
      coord_cartesian(xlim = c(0, 90), ylim = c(-0.05, max_score+1.5), expand = 0) +
      labs(title = "Score") + 
      
      #period divider
      annotate("segment", x = pers$cum_total_seconds[1]/tmd[["bin_width"]], xend = pers$cum_total_seconds[1]/tmd[["bin_width"]], 
               y = -0.05, yend = max_score+1.5, 
               color = icyUEFA["ice4"], size = 1) + 
      
      #goal markers
      annotate("segment", x = rep.int(0, (max_score+1)), xend = rep.int(90, (max_score+1)), y = 0:max_score, yend = 0:max_score, linetype = 1, color = icyUEFA["ice3"], size = 0.5) +
      annotate("text", x = rep.int(89, (max_score+1)), y = 0:max_score + 0.5, label = as.character(0:max_score), size = 4, color = shUEFA["blueLt"], hjust = 1) +
      
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
      annotate("text", x = selected_time/tmd[["bin_width"]] + this.posAdj, y = max_score + 1.2, label = paste0(tmd[["homeName"]], " ", tb_hs), hjust = this.hjust, color = winColor, size = 6) + 
      
      #selected time brush marker
      annotate("rect", xmin = brushTime[["min"]]/tmd[["bin_width"]], xmax = brushTime[["max"]]/tmd[["bin_width"]], ymin = 0, ymax = max_score + 1.5, fill = transpa(shUEFA["blueLt"], 50))
    
    
  })
 
  #win probability plot
  output$wp_plot <- renderPlot({
    
    tmd <- this_match_data()
    this.ev <- tmd[["this_ev"]]
    this.tb <- tmd[["this_tb"]]
    pers <- tmd[["pers"]]
    awayName <- tmd[["awayName"]]
    homeName <- tmd[["homeName"]]
    
    req(input$slider_time)
    selected_time <- input$slider_time
    selected_tb <- get_time_bin(selected_time, tmd[["max_sec"]])
    
    winColor = shUEFA["ibm_pink"]
    drawColor = shUEFA["ibm_yellow"]
    lossColor = shUEFA["ibm_purple"]
    
    pWin <- this.tb %>% filter(time_bin == selected_tb) %>% pull(end_p_win)
    pDraw <- this.tb %>% filter(time_bin == selected_tb) %>% pull(end_p_draw)
    pLoss <- this.tb %>% filter(time_bin == selected_tb) %>% pull(end_p_loss)
    
    ggplot(this.ev) + shUEFA_theme_icy +
      theme(plot.margin = unit(c(7,7,0,0), unit="pt"), 
            plot.title = element_text(size = 12, hjust = 0, color = shUEFA["blueLt"])) +
      coord_cartesian(xlim = c(0, tmd[["max_sec"]]), ylim = c(-0.1,1.1), expand = 0) + 
      
      labs(title = "Match Outcome Probability") + 
      
      #horizontal lines
      geom_hline(aes(yintercept = 0), color = icyUEFA["ice3"]) +
      geom_hline(aes(yintercept = 0.5), color = icyUEFA["ice3"]) +
      geom_hline(aes(yintercept = 1), color = icyUEFA["ice3"]) + 
      annotate("text", x = tmd[["max_sec"]], y = -0.05, label = "Probability = 0.0", color = shUEFA["blueLt"], size = 4, hjust = 1) +
      annotate("text", x = tmd[["max_sec"]], y = 0.55, label = "Probability = 0.5", color = shUEFA["blueLt"], size = 4, hjust = 1) +
      annotate("text", x = tmd[["max_sec"]], y = 1.05, label = "Probability = 1.0", color = shUEFA["blueLt"], size = 4, hjust = 1) +
      
      #period divider
      annotate("segment", x = pers$cum_total_seconds[1], xend = pers$cum_total_seconds[1], 
               y = 0, yend = 1, 
               color = icyUEFA["ice4"], size = 1) + 
      
      #probability lines
      geom_line(aes(x = cum_match_seconds, y = p_win), color = winColor) + 
      geom_line(aes(x = cum_match_seconds, y = p_draw), color = drawColor) + 
      geom_line(aes(x = cum_match_seconds, y = p_loss), color = lossColor) +
      
      geom_smooth(aes(x = cum_match_seconds, y = p_win), color = winColor) + 
      geom_smooth(aes(x = cum_match_seconds, y = p_draw), color = drawColor) + 
      geom_smooth(aes(x = cum_match_seconds, y = p_loss), color = lossColor) +
      
      #selected time marker
      annotate("segment", x = selected_time, xend = selected_time, y = -0.1, yend = 1.1, color = shUEFA["orangeDk"], size = 1) +
      
      #legend
      annotate("rect", xmin = 0, xmax = tmd[["max_sec"]]/2, ymin = 0.78, ymax = 1.1, fill = "white", alpha = 0.5) + 
      annotate("text", x = 90, y = 1.05, label = paste0("P(", homeName, " Win) = ", pWin), color = winColor, hjust = 0, size = 6) + 
      annotate("text", x = 90, y = 0.95, label = paste0("P(Draw) = ", pDraw), color = drawColor, hjust = 0, size = 6) +
      annotate("text", x = 90, y = 0.85, label = paste0("P(", awayName, " Win) = ", pLoss), color = lossColor, hjust = 0, size = 6) 
      
      
    
    
  })
  
  output$wp_zoom_plot <- renderPlot({
    
    tmd <- this_match_data()
    this.ev <- tmd[["this_ev"]]
    this.tb <- tmd[["this_tb"]]
    pers <- tmd[["pers"]]
    awayName <- tmd[["awayName"]]
    homeName <- tmd[["homeName"]]
    
    brushTime <- list(min = 0, max = 60)
    if(!is.null(input$wp_brush)){brushTime <- wp_brushed_time()}
    
    winColor = shUEFA["ibm_pink"]
    drawColor = shUEFA["ibm_yellow"]
    lossColor = shUEFA["ibm_purple"]
    
    pWin_max <- max(this.ev %>% filter(between(cum_match_seconds, brushTime[["min"]], brushTime[["max"]])) %>% pull(p_win), na.rm = T)
    pDraw_max <- max(this.ev %>% filter(between(cum_match_seconds, brushTime[["min"]], brushTime[["max"]])) %>% pull(p_draw), na.rm = T)
    pLoss_max <- max(this.ev %>% filter(between(cum_match_seconds, brushTime[["min"]], brushTime[["max"]])) %>% pull(p_loss), na.rm = T)
    
    pWin_min <- min(this.ev %>% filter(between(cum_match_seconds, brushTime[["min"]], brushTime[["max"]])) %>% pull(p_win), na.rm = T)
    pDraw_min <- min(this.ev %>% filter(between(cum_match_seconds, brushTime[["min"]], brushTime[["max"]])) %>% pull(p_draw), na.rm = T)
    pLoss_min <- min(this.ev %>% filter(between(cum_match_seconds, brushTime[["min"]], brushTime[["max"]])) %>% pull(p_loss), na.rm = T)
    
    ggplot(this.ev) + shUEFA_theme_icy +
      theme(plot.margin = unit(c(7,7,0,0), unit="pt"), 
            plot.title = element_text(size = 12, hjust = 0, color = shUEFA["blueLt"])) +
      
      coord_cartesian(xlim = c(brushTime[["min"]], brushTime[["max"]]), ylim = c(-0.4,1.1), expand = 0) + 
      
      labs(title = "Match Outcome Probability Detail") + 
      
      #horizontal lines
      geom_hline(aes(yintercept = 0), color = icyUEFA["ice3"]) +
      geom_hline(aes(yintercept = 0.5), color = icyUEFA["ice3"]) +
      geom_hline(aes(yintercept = 1), color = icyUEFA["ice3"]) + 
      annotate("text", x = brushTime[["max"]], y = -0.05, label = "Probability = 0.0", color = shUEFA["blueLt"], size = 4, hjust = 1) +
      annotate("text", x = brushTime[["max"]], y = 0.55, label = "Probability = 0.5", color = shUEFA["blueLt"], size = 4, hjust = 1) +
      annotate("text", x = brushTime[["max"]], y = 1.05, label = "Probability = 1.0", color = shUEFA["blueLt"], size = 4, hjust = 1) +
      
      #time label
      annotate("text", x = brushTime[["min"]], y = -0.3, label = paste0(" Start Time: ", seconds_to_timestamp(pers, brushTime[["min"]])), color = icyUEFA["ice5"], hjust = 0, size = 5) + 
      annotate("text", x = brushTime[["max"]], y = -0.3, label = paste0("End Time: ", seconds_to_timestamp(pers, brushTime[["max"]]), " "), color = icyUEFA["ice5"], hjust = 1, size = 5) +
      
      #period divider
      annotate("segment", x = pers$cum_total_seconds[1], xend = pers$cum_total_seconds[1], 
               y = 0, yend = 1, 
               color = icyUEFA["ice4"], size = 1) + 
      
      #probability lines
      geom_line(aes(x = cum_match_seconds, y = p_win), color = winColor) + 
      geom_line(aes(x = cum_match_seconds, y = p_draw), color = drawColor) + 
      geom_line(aes(x = cum_match_seconds, y = p_loss), color = lossColor) +
      
      #legend
      annotate("rect", xmin = 0, xmax = brushTime[["min"]] + ((brushTime[["max"]] - brushTime[["min"]])/2), ymin = 0.78, ymax = 1.1, fill = "white", alpha = 0.5) + 
      annotate("text", x = brushTime[["min"]] + ((brushTime[["max"]] - brushTime[["min"]])/50), y = 1.05, label = paste0("P(", homeName, " Win) = [", pWin_min, ", ", pWin_max, "]"), color = winColor, hjust = 0, size = 5) + 
      annotate("text", x = brushTime[["min"]] + ((brushTime[["max"]] - brushTime[["min"]])/50), y = 0.95, label = paste0("P(Draw) = [", pDraw_min, ", ", pDraw_max, "]"), color = drawColor, hjust = 0, size = 5) +
      annotate("text", x = brushTime[["min"]] + ((brushTime[["max"]] - brushTime[["min"]])/50), y = 0.85, label = paste0("P(", awayName, " Win) = [", pLoss_min, ", ", pLoss_max, "]"), color = lossColor, hjust = 0, size = 5)
    
    
    
  })
  
  
  output$timeline_slider <- renderUI({
    
    tmd <- this_match_data()
    pers <- tmd[["pers"]]
    
    sliderInput("slider_time", "Select time:", min = 0, max = max(pers$cum_total_seconds), 
                value = 0, width = "100%")
    
  })
  
  output$pitch_plot <- renderPlot({
    tmd <- this_match_data()
    this.ev <- tmd[["this_ev"]]
    
    winColor <- shUEFA["ibm_pink"]
    lossColor <- shUEFA["ibm_purple"]
    
    pd <- this.ev %>% filter(between(cum_match_seconds, 0, 60), !is.na(player.name))
    
    if(!is.null(input$wp_zoom_brush)){
      pd <- wp_zoom_brushed_data()
    }else{
      if(!is.null(input$wp_brush)){
        pd <- wp_brushed_data()
      }
      
    }
    
    #add on locations of events that happened in the time bin
    plot_pitch(pd, icyUEFA["ice5"]) + shUEFA_theme_icy +
      
      #showing legend
      #theme(legend.position = "top") +
      #team name labels
      annotate("text", x = 0, y = 83, size = 5, color = winColor, label = tmd[["homeName"]], hjust = 0) +
      annotate("text", x = 120, y = 83, size = 5, color = lossColor, label = tmd[["awayName"]], hjust = 1) +
      
      #goal shading
      annotate("rect", xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = transpa(winColor, 100)) + 
      annotate("rect", xmin = 114, xmax = 120, ymin = 30, ymax = 50, fill = transpa(lossColor, 100)) + 
      
      geom_point(aes(x = std_location_x, y = std_location_y, shape = type.name, color = type.name), size = 5) + 
      scale_color_manual(values = events_scale_colors) + 
      scale_shape_manual(values = events_scale_shapes) 
    
  })
  
  output$pitch_event_data <- renderDataTable({
    
    tmd <- this_match_data()
    this.ev <- tmd[["this_ev"]]
    
    dtd <- this.ev %>% filter(between(cum_match_seconds, 0, 60), !is.na(player.name))
    
    #pitch_brush not null
    if(!is.null(input$pitch_brush)){
      dtd <- pitch_brushed_data()
      
    #pitch brush null  
    }else{
      #pitch brush null and wp_zoom_brush not null
      if(!is.null(input$wp_zoom_brush)){
        dtd <- wp_zoom_brushed_data()
        
      #pitch brush null and wp_zoom_brush null  
      }else{
        
        #pitch brush null and wp_zoom brush null and wp_brush not null
        if(!is.null(input$wp_brush)){
          
          dtd <- wp_brushed_data()
        }else{
          dtd <- this.ev %>% filter(between(cum_match_seconds, 0, 60), !is.na(player.name))
          
        }
        
      }
      
    }
    
    #if(!is.null(input$wp_brush)){dtd <- wp_brushed_data()}
    #if(!is.null(input$wp_zoom_brush)){dtd <- wp_zoom_brushed_data()}
    #if(!is.null(input$pitch_brush)){dtd <- pitch_brushed_data()}
    
    DT::datatable(dtd %>% select(player.name, type.name, team.name, timestamp),
                  options = list(pageLength = 5,
                                 lengthMenu = list(c(5,5,-1), c("5","5","All"))))
  })

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
    
    brushTime <- list(min = 0, max = 0)
    if(!is.null(input$wp_brush)){brushTime <- wp_brushed_time()} 
    
    ggplot() + shUEFA_theme_icy + 
      theme(plot.margin = unit(c(7,7,0,0), unit="pt"), 
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
               color = icyUEFA["ice5"], size = 6) + 
      
      #selected time brush marker
      annotate("rect", xmin = brushTime[["min"]], xmax = brushTime[["max"]], ymin = -20, ymax = 100, fill = transpa(shUEFA["blueLt"], 50))
    
    
    #-----
    
  })
  
  output$possession_plot <- renderPlot({
    tmd <- this_match_data()
    pers <- tmd[["pers"]]
    poss <- tmd[["poss"]]
    
    homeColor = shUEFA["ibm_pink"]
    awayColor = shUEFA["ibm_purple"]
    
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
    
    brushTime <- list(min = 0, max = 0)
    if(!is.null(input$wp_brush)){brushTime <- wp_brushed_time()} 
    
    #plot of possessions throughout the match
    ggplot(poss) + shUEFA_theme_icy + 
      theme(plot.margin = unit(c(7,7,0,0), unit="pt"),
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
               color = labelColor, size = 6) + 
      #selected time brush marker
      annotate("rect", xmin = brushTime[["min"]], xmax = brushTime[["max"]], ymin = 0, ymax = 3, fill = transpa(shUEFA["blueLt"], 50))
    
  })
  
  output$events_legend <- renderPlot({
    ggplot(eventTypes) + shUEFA_theme_icy +
      theme(plot.margin = unit(c(7,7,0,0), unit="pt"),
            plot.title = element_text(size = 12, hjust = 0, color = shUEFA["blueLt"])) +
      coord_cartesian(ylim = c(0,7), xlim = c(0.5,11.5), expand = 0) +
      labs(title = "Event Types Legend") +
      geom_point(aes(x = plot_x, y = plot_y), color = eventTypes$color, shape = eventTypes$shape, size = 3) + 
      annotate("text", x = eventTypes$plot_x, y = eventTypes$plot_y + 0.8, label = eventTypes$event_type, size = 3)
    
  })

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
      
      theme(plot.margin = unit(c(7,7,0,0), unit="pt"),
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
      geom_point(data = this.ev %>% filter(type.name %in% selected_events), aes(x = cum_match_seconds, y = timeline_y, shape = type.name, color = type.name), size = 3.5, alpha = 0.8) + 
      #shape and color scales depending on event types
      scale_color_manual(values = events_scale_colors) + 
      scale_shape_manual(values = events_scale_shapes)
    
    
  })
  
  output$roster_plot <- renderPlot({
    
    tmd <- this_match_data()
    this.ev <- tmd[["this_ev"]]
    mp <- tmd[["mp"]]
    
    
    cd <- this.ev %>% filter(between(cum_match_seconds, 0, 60), !is.na(player.name))
    
    if(all(!is.null(input$pitch_brush), !is.null(input$wp_zoom_brush), !is.null(input$wp_brush))){
      cd <- pitch_brushed_data()
    }else{
      if(all(!is.null(input$wp_zoom_brush), !is.null(input$wp_zoom))){
        cd <- wp_zoom_brushed_data()
        
      }else{
        if(!is.null(input$wp_brush)){
          cd <- wp_brushed_data()
        }
      }
    }
    
    req(input$event_select)
    selected_events <- input$event_select
    
    cd_player_ids <- unique(cd$player.id)
    tb_mp <- mp %>% filter(player.id %in% cd_player_ids)
    
    #number of players on each team
    nA <- dim(mp %>% filter(home_or_away == "away"))[1]
    nH <- dim(mp %>% filter(home_or_away == "home"))[1]
    
    ggplot(mp) + shUEFA_theme_icy + 
        
        theme(plot.margin = unit(c(7,7,0,0), unit="pt"),
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
  
  output$event_WPV <- renderDataTable({
    #event WPA averages for competition
    DT::datatable(event_WPV_table)
  })
  
  output$player_WPA <- renderDataTable({
    
    tmd <- this_match_data()
    this.ev <- tmd[["this_ev"]]
    
    wpa <- this.ev %>% group_by(player.name) %>%
      summarize(win_probability_added = sum(delta_p_win_adj, na.rm = T),
                team = first(team.name)) %>%
      arrange(desc(win_probability_added))
    
    DT::datatable(wpa)
    
  })
  
}


shinyApp(ui, server)









