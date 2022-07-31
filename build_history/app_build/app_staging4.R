
#this version of the app is debugged square one


#library(StatsBombR)
library(shiny)
library(tidyverse)
library(lubridate)

#source("app_functions/scrape_StatsBomb.R")
source("app_functions/bombViz.R")
source("app_functions/timestamp_to_seconds.R")
source("app_functions/get_match_players2.R")
source("app_functions/get_match_info_table.R")

# ||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=
#                               Get Data
# ||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=

#since there is only one competition used in hackathon data, pulling data from
#api isn't reactive, just once at the beginning.  ideally, dashboard will be
#usable with any available StatsBomb360 competition dataset

#Matches
#m <- scrape_matches(compName = "UEFA Euro")
m <- read.csv("app_data/dbb_matches.csv")

#Events (with nested vars)
#ev_og <- scrape_events(m)

#related events
#rel_ev <- get_related_events(ev_og)

#events without nested vars
#ev <- get_stripped_events(ev_og)
ev <- read.csv("app_data/dbb_events.csv")

#ev_og <- NULL

all_mp <- read.csv("app_data/match_players_UEFA2020.csv")


#list of unique match_id values for selectInput
matchIDs <- unique(m$match_id)
names(matchIDs) <- m$menu_text

#UI-----

ui <- fluidPage(
  
  tags$head(
    
    tags$title("StatsBomb360 Win Probability Dashboard"),
    
    #app styling
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
        padding-left: 10px; 
      
      }
      
      .shiny-input-container {
        color: #223fb3;
      }"))
  ),
  
  #header
  fluidRow(
    column(4, 
           tags$img(src = "SB_CoreWordmark_ColourPositive.png", width = "80%"),
           tags$img(src = "SB_BrandIcon_ColourPositive.png", width = "15%")
           ),
    column(8,
           tags$h1("Win Probability Calculator")
           )
    
  ),
  
  
  #first main row - match selection input
  fluidRow(
    
    div(id = "match_select_wrapper",
      #match selection input
      selectInput("match_select", "Select Match:", matchIDs, selectize = F)
    )
    
    
  ),
  
  #second main row - plots and other content
  fluidRow(
    #first column, timeline plots
    column(9,
           plotOutput("wp_plot", height = 300),
           uiOutput("timeline_slider"),
           plotOutput("timeline_plot", height = 100),
           plotOutput("possession_plot", height = 100)
           
           ),
    #second column - text and tables, pitch plot
    column(3,
           tableOutput("match_info_table")
           
           )
    
  ),
  
  fluidRow(
    column(9,
           plotOutput("roster_timeline_plot", height = 600, 
                      hover = hoverOpts("hover_player", nullOutside = F))
           ),
    
    column(3,
           plotOutput("roster_plot", height = 600)
           )
    
    
  )
)

#-----    


server <- function(input, output){
  
  #update data according to match id input
  this_match_data <- reactive({
    
    req(input$match_select)
    #row of general info for match
    this.m <- m %>% filter(match_id == input$match_select)
    
    #all events in match
    this.ev <- ev %>% filter(match_id == input$match_select)
    this.ev <- get_cumulative_match_seconds(this.ev)
    
    #all players in match
    mp <- all_mp %>% filter(match_id == input$match_select)
    #get period & time info for match
    pers <- get_period_summary(this.ev)
    #get possession info for match
    poss <- get_possession_summary(this.ev)
    
    #get names of teams
    awayName <- this.m %>% pull(away_team.away_team_name)
    homeName <- this.m %>% pull(home_team.home_team_name)
    
    return(list(this_m = this.m,
                this_ev = this.ev,
                mp = mp,
                pers = pers,
                poss = poss,
                awayName = awayName,
                homeName = homeName))
  })
  
  
 
  #win probability plot
  output$wp_plot <- renderPlot({
    
    
    
  })
  
  #time slider input
  #~~~~~~~~~~~~~~~~~~~~
  output$timeline_slider <- renderUI({
    
    tmd <- this_match_data()
    pers <- tmd[["pers"]]
    
    sliderInput("slider_time", "Select time:", min = 0, max = max(pers$cum_total_seconds), 
                value = 0, width = "100%")
    
  })
  
  #info about selected match
  #~~~~~~~~~~~~~~~~~~~~
  output$match_info_table <- renderTable({
    
    
    
    
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
               label = paste0("Period ",cp), hjust = this.hjust, 
               color = icyUEFA["ice5"], size = 6)
    
    #-----
    
  })
  
  
  output$possession_plot <- renderPlot({
    tmd <- this_match_data()
    pers <- tmd[["pers"]]
    poss <- tmd[["poss"]]
    
    #to make a manual color scale, need vector same length as num of levels,
    #and to rename colors to match levels
    clrs <- c(icyUEFA["ice3"], icyUEFA["ice5"])
    names(clrs) <- c(tmd[["homeName"]], tmd[["awayName"]])
    
    req(input$slider_time)
    selected_time <- input$slider_time
    
    #selected possession & team
    cur_poss <- poss %>% filter(start_of_poss < selected_time, end_of_poss >= selected_time)
    selected_team <- cur_poss$possession_team
    
    this.hjust <- ifelse(selected_time > (max(pers$cum_total_seconds)/2), 1, 0)
    this.posAdj <- ifelse(selected_time > (max(pers$cum_total_seconds)/2), -100, 100)
    
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
               color = icyUEFA["ice5"], size = 8)
    
    
    
    
  })
  

  #roster timeline plot
  #~~~~~~~~~~~~~~~~~~~~
  output$roster_timeline_plot <- renderPlot({
    
    tmd <- this_match_data()
    mp <- tmd[["mp"]]
    pers <- tmd[["pers"]]
    
    #number of players on each team
    nA <- dim(mp %>% filter(home_or_away == "away"))[1]
    nH <- dim(mp %>% filter(home_or_away == "home"))[1]
    
    req(input$slider_time)
    selected_time <- input$slider_time
    
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
               y = 0, yend = nH+nA+2, color = shUEFA["orangeDk"], size = 1)
    
    
  })
  
  #roster plot
  #~~~~~~~~~~~~~~~~~~~~
  output$roster_plot <- renderPlot({
    
    tmd <- this_match_data()
    mp <- tmd[["mp"]]
    
    #number of players on each team
    nA <- dim(mp %>% filter(home_or_away == "away"))[1]
    nH <- dim(mp %>% filter(home_or_away == "home"))[1]
    
    
    req(input$hover_player)
    cur_y <- input$hover_player$y
    
    hov_player_tly <- round(cur_y)
    hov_player_name <- mp %>% filter(timeline_y == hov_player_tly) %>% pull(player.name)
    hov_position_abbr <- mp %>% filter(timeline_y == hov_player_tly) %>% pull(position_abbr)
    hov_team <- mp %>% filter(timeline_y == hov_player_tly) %>% pull(team.name)
    
    ggplot(mp) + shUEFA_theme_icy + 
      
      theme(plot.margin = unit(c(2,2,0,0), unit="pt"),
            plot.title = element_text(size = 12, hjust = 0, color = shUEFA["blueLt"])
      ) +
      
      labs(title = "Players") + 
      coord_cartesian(xlim = c(0, 500), ylim = c(0, nH+nA+2), expand = 0) +
      
      #rectangles around each team
      annotate("rect", xmin = 0, xmax = 500, ymin = 0, ymax = nH+1, fill = NA, color = icyUEFA["ice2"]) + 
      annotate("rect", xmin = 0, xmax = 500, ymin = nH+1, ymax = nH+nA+2, fill = NA, color = icyUEFA["ice2"]) + 
      
      #player names 
      annotate("text", x = rep.int(10, dim(mp)[1]), y = mp$timeline_y, label = str_trunc(mp$player.name, width = 20, side = "right"), hjust = 0, color = shUEFA["blueLt"], size = 5) + 
      #positions
      annotate("text", x = rep.int(300, dim(mp)[1]), y = mp$timeline_y, label = mp$position_abbr, hjust = 0.5, color = shUEFA["blueLt"], size = 5) + 
      #team
      annotate("text", x = rep.int(490, dim(mp)[1]), y = mp$timeline_y, label = mp$team.name, hjust = 1, color = shUEFA["blueLt"], size = 5) +
      
      
      #hovered player
      annotate("rect", xmin = 0, xmax = 500, ymin = hov_player_tly - 0.5, ymax = hov_player_tly + 0.5, fill = icyUEFA["ice5"], color = NA) + 
      #name
      annotate("text", x = 10, y = hov_player_tly, label = hov_player_name, hjust = 0, color = icyUEFA["ice1"], size = 5) + 
      #position
      annotate("text", x = 300, y = hov_player_tly, label = hov_position_abbr, hjust = 0.5, color = icyUEFA["ice1"], size = 5) +
      #team  
      annotate("text", x = 490, y = hov_player_tly, label = hov_team, hjust = 1, color = icyUEFA["ice1"], size = 5)
    
    
    
    
  })
  
  
  
}


shinyApp(ui, server)









