
#library(StatsBombR)
library(shinydashboard)
library(shiny)
library(tidyverse)
library(devtools)
library(lubridate)

#source("app_functions/scrape_StatsBomb.R")
source("app_functions/bombViz.R")
source("app_functions/timestamp_to_seconds.R")
source("app_functions/get_match_players2.R")
source("app_functions/get_match_info_table.R")

#position abbreviations and display coordinates
posAb <- read.csv("app_functions/positionDisplay.csv")


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

#Unnested starting lineups
#lu <- get_startingXI(ev_og)
lu <- read.csv("app_data/dbb_events_startingXI.csv")

#related events
#rel_ev <- get_related_events(ev_og)

#events without nested vars
#ev <- get_stripped_events(ev_og)
ev <- read.csv("app_data/dbb_events.csv")

#ev_og <- NULL

#list of unique match_id values 
matchIDs <- unique(m$match_id)
#all data used in app will be filtered by input match_id value 
#each time a new match id is selected

#event types are standing in for statviews, which will be groups of event types tbd
eventTypes <- unique(ev$type.name)

#transforming timestamp var so i can plot it on x axis
ev <- ev %>% mutate(timestamp_seconds = timestamp_to_seconds(timestamp))
ev <- get_cumulative_match_seconds(ev)

#functions
get_match_player_table <- function(matchPlayersDF){
  
  mptVars <- c("team.name", "jersey_number", "player.name", "position.id", 
               "position.name", "type.name", "period_on", "timestamp_on", 
               "period_off", "timestamp_off")
  
  this.mpt <- matchPlayersDF %>% 
    arrange(team.name, position.id, period_on, timestamp_on) %>% 
    select(all_of(mptVars))
  
  return(this.mpt)

}


ui <- fluidPage(
  
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Dosis&display=swap');
      body {
        background-color: #473c85;
        color: white;
      }
      h2 {
        font-family: 'Dosis', sans-serif;
      }
      .shiny-html-output {
        background-color: #10006b;
      }
      .shiny-input-container {
        color: #341cbd;
        
      }"))
  ),
  
  titlePanel("StatsBomb 360"
             
             ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      #match selection input
      selectInput("match_select", "Select Match:", matchIDs, selectize = F),
      
      #match info output
      tableOutput("match_info"),
      
      #statview selection input
      #selectInput("event_select", "Select Event:", eventTypes, selectize = F)
      selectInput("event_select", "Select Event:", 
                  c("Pass","Interception","Shot","Carry","Dribble","Miscontrol","Clearance"), 
                  selectize = F)
      
    ),
    
    mainPanel(
      
      fluidRow(
        plotOutput("pitch_plot")
        
      ),
      
      fluidRow(
        plotOutput("timeline_plot", height = 600)
        
      )
    )
  )
)

    
    
  
  



server <- function(input, output){
  
  #reactive function to update working datasets when new match id is selected by user
  cur_data <- reactive({
    
    #get dataframe of players in match
    mp <- get_match_players(input$match_select, m, ev, lu)
    
    #get time summary for match
    pers <- get_period_summary(ev %>% filter(match_id == input$match_select))
    pers <- pers %>% mutate(cum_seconds = cumsum(total_seconds))
    #transform timestamps and join position abbreviations
    mp <- get_cumulative_ts_on_off(mp, pers)
    mp <- mp %>% left_join(posAb %>% select(position.id, position_abbr), by = "position.id")
    
    #filter matches and events datasets
    this.m <- m %>% filter(match_id == input$match_select)
    this.ev <- ev %>% filter(match_id == input$match_select)
    
    #period start and end times
    pers <- get_period_summary(this.ev)
    
    #transform timestamps into cumulative seconds
    this.ev <- get_cumulative_match_seconds(this.ev)
    
    
    #assign a y value to each player in match
    mp <- mp %>% arrange(home_or_away, position.id, ts_on_cum_seconds) %>%
      mutate(timeline_y = seq(dim(mp)[1], 1, -1)) %>%
      mutate(timeline_y = ifelse(home_or_away == "away", timeline_y +1, timeline_y))
    #join position abbreviations
    mp <- mp %>% left_join(posAb %>% select(position_abbr, position.id), by = "position.id")
    
    #number of players on each team
    nA <- dim(mp %>% filter(team.name == awayName))[1]
    nH <- dim(mp %>% filter(team.name == homeName))[1]
    
    #match timeline_y values to events based on player id
    this.ev <- this.ev %>% left_join(mp %>% select(player.id, timeline_y), by = "player.id")
    #make NA values for duration into zeros
    this.ev <- this.ev %>% mutate(duration = ifelse(is.na(duration), 0, duration))
    
    
    #put in a list to call from rendering functions
    list("mp" = mp, "pers" = pers, "cur_m" = this.m, "cur_ev" = this.ev)
    
  })
  
  
  output$pitch_plot <- renderPlot({
    
    this_data <- cur_data()
    
    cur_ev <- this_data[["cur_ev"]]
    cur_ev <- cur_ev %>% filter(type.name == input$event_select)
    
    #right now a blank pitch plot
    #will eventually react to time slider input and statview input
    plot_pitch(cur_ev, lineColor = shUEFA["blueMed"]) + shUEFA_theme  + 
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank()) + 
      geom_point(aes(x = location.x, y = location.y), color = shUEFA["yellow"]) + 
      labs(title = as.character(input$event_select))
      
  })
  
  output$timeline_plot <- renderPlot({
    
    this_data <- cur_data()
    
    mp <- this_data[["mp"]] 
    pers <- this_data[["pers"]]
    cur_m <- this_data[["cur_m"]]
    cur_ev <- this_data[["cur_ev"]]
    
    #get away and home team names
    awayName <- cur_m$away_team.away_team_name
    homeName <- cur_m$home_team.home_team_name
    
    #number of away and home players
    nA <- dim(mp %>% filter(team.name == awayName))[1]
    nH <- dim(mp %>% filter(team.name == homeName))[1]
    
    plA <- mp %>% filter(team.name == awayName) %>% arrange(position.id, ts_on_seconds)
    plH <- mp %>% filter(team.name == homeName) %>% arrange(position.id, ts_on_seconds)
    
    playerName_x = -1000
    jerseyNum_x = -1300
    position_x = -1600
    
    ggplot() + shUEFA_theme + theme(axis.text = element_blank(),
                                    axis.title = element_blank(),
                                    axis.ticks = element_blank(),
                                    axis.line = element_blank()) + 
      #big rectangle
      geom_rect(aes(xmin = -2000, xmax = max(pers$cum_seconds), ymin = 0, ymax = (nA + nH + 2)),
                color = shUEFA["blueMed"], fill = NA) + 
      #player box
      geom_rect(aes(xmin = -2000, xmax = 0, ymin = 0, ymax = (nA + nH + 2)),
                color = shUEFA["blueMed"], fill = NA) + 
      #timeline box
      geom_rect(aes(xmin = 0, xmax = max(pers$cum_seconds), ymin = 0, ymax = (nA + nH + 3)),
                color = shUEFA["blueMed"], fill = NA) + 
      
      #away player text
      annotate("text", x = rep.int(playerName_x, nA), y = (nA+nH+1):(nH+2), 
               label = str_trunc(plA$player.name, 20, side = "right"), hjust = 0, color = shUEFA["blueLt"]) + 
      #jersey number
      annotate("text", x = rep.int(jerseyNum_x, nA), y = (nA+nH+1):(nH+2), label = plA$jersey_number, hjust = 0.5, color = shUEFA["blueLt"]) + 
      #position
      annotate("text", x = rep.int(position_x, nA), y = (nA+nH+1):(nH+2), label = plA$position_abbr, hjust = 1, color = shUEFA["blueLt"]) + 
      
      #home player text
      annotate("text", x = rep.int(playerName_x, nH), y = nH:1, 
               label = str_trunc(plH$player.name, 20, side = "right"), hjust = 0, color = shUEFA["blueLt"]) + 
      #jersey number
      annotate("text", x = rep.int(jerseyNum_x, nH), y = nH:1, label = plH$jersey_number, hjust = 0.5, color = shUEFA["blueLt"]) + 
      #position
      annotate("text", x = rep.int(position_x, nH), y = nH:1, label = plH$position_abbr, hjust = 1, color = shUEFA["blueLt"]) + 
      
      #line between teams
      geom_segment(aes(x = -2000, xend = max(pers$cum_seconds), y = nH+1, yend = nH+1), color = shUEFA["blueMed"]) + 
      
      #player lines
      geom_segment(aes(x = rep.int(position_x, (nA+nH+1)), xend = rep.int(max(pers$cum_seconds), (nA+nH+1)), y = 1:(nA+nH+1), yend = 1:(nA+nH+1)), 
                   linetype = 3, color = shUEFA["purple"]) + 
      
      #on pitch lines - tneed to work off of index of df to keep names and numbers together
      geom_segment(aes(x = plH$ts_on_cum_seconds, xend = plH$ts_off_cum_seconds, y = nH:1, yend = nH:1), 
                   linetype = 1, color = shUEFA["blueLt"]) +
      
      geom_segment(aes(x = plA$ts_on_cum_seconds, xend = plA$ts_off_cum_seconds, y = (nH+1+nA):(nH+2), yend = (nH+1+nA):(nH+2)), 
                   linetype = 1, color = shUEFA["blueLt"]) + 
      
      #period markers
      geom_segment(aes(x = pers$cum_seconds, xend = pers$cum_seconds, y = rep.int(0, length(pers$cum_seconds)), 
                       yend = rep.int((nH+2+nA), length(pers$cum_seconds))), 
                   color = shUEFA["blueMed"])
    
  })
  
  #match info output
  output$match_info <- renderTable({
    
    this_data <- cur_data()
    
    mit <- get_match_info_table(this_data[["cur_m"]])
    mit
    
  })
  
}


shinyApp(ui, server)









