
library(tidyverse)

source("functions_etc/bombViz.R")
posAb <- read.csv("functions_etc/positionDisplay.csv")


plot_pos_on_pitch <- function(){
  require(ggplot2)
  
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
