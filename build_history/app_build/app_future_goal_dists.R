
library(shiny)
library(tidyverse)
library(nnet)

#read in models for goals
mod1.h <- readRDS("app_functions/goals_model_home.rds")
mod1.a <- readRDS("app_functions/goals_model_away.rds")


ui <- fluidPage(
  fluidRow(
    column(4,
      #input for score differential
      selectInput("scoreDiff", "Select score differential:", -5:5, selected = 0),
      
      #input for time bin
      sliderInput("timebin", "Select time bin:", 0, 89, 0, step = 1),
      
      #input for start location x
      sliderInput("startLocationX", "Select start x coordinate:", 0, 120, 60),
      
      #input for start location y
      sliderInput("startLocationY", "Select start y coordinate:", 0, 80, 40),
      
      #input for end location x
      sliderInput("endLocationX", "Select end x coordinate:", 0, 120, 60),
      
      #input for end location y
      sliderInput("endLocationY", "Select end y coordinate:", 0, 80, 40)
      
      
    ),
    
    column(8,
      plotOutput("home_goals_plot"),
      plotOutput("away_goals_plot")
       
    )
  )
)


server <- function(input, output){
  
  cur_model_results <- reactive({
    
    nd <- data.frame(
      time_bin = 0:89,
      cur_score_diff = rep.int(as.numeric(input$scoreDiff), 90),
      
      start_loc_x = rep.int(input$startLocationX, 90),
      start_loc_y = rep.int(input$startLocationY, 90),
      
      end_loc_x = rep.int(input$endLocationX, 90),
      end_loc_y = rep.int(input$endLocationY, 90)
      )
    
    pm.h <- predict(mod1.h, nd, type = "probs")
    pm.a <- predict(mod1.a, nd, type = "probs")
    
    return(list("home" = pm.h, "away" = pm.a))
    
  })
  
  output$home_goals_plot <- renderPlot({
    
    cmr <- cur_model_results()
    pmh <- cmr[["home"]]
    cur_tb <- as.numeric(input$timebin) + 1
    
    barplot(pmh[cur_tb,], main = "Home Team Goal Expectancy", ylim = c(0,1), col = "coral")
    
  })
  
  output$away_goals_plot <- renderPlot({
    
    cmr <- cur_model_results()
    pma <- cmr[["away"]]
    cur_tb <- as.numeric(input$timebin) + 1
    
    barplot(pma[cur_tb,], main = "Away Team Goal Expectancy", ylim = c(0,1), col = "dodgerblue")
    
  })
  
}

shinyApp(ui, server)



