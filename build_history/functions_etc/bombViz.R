
library(tidyverse)
library(devtools)

hex <- function(hexCode){
  
  u <- as.character(hexCode)
  #make sure input is six digits long, letters and numbers only
  u <- str_remove(u, "[:punct:]")
  u <- str_sub(u, 1, 6)
  u <- paste0("#", u)
  
  return(rgb(t(col2rgb(u)), maxColorValue = 255))
}

transpa <- function(colorObject, alpha1to255){
  return(rgb(t(col2rgb(colorObject)), maxColorValue = 255, alpha = alpha1to255))
}

#blank soccer pitch!!! from statsbomb pdf on UEFA 2020 data
blank_pitch <- function(){
  ggplot() +
    annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+
    annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
    annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)
  
}
  
#my function for plotting a blank pitch
plot_pitch <- function(data = NULL, lineColor = "black"){
  
  ggp <- ggplot(data = data) +
    annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = lineColor, size = 0.6) +
    annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = lineColor, size = 0.6) +
    annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = lineColor, size = 0.6) +
    annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = lineColor, size = 0.6) +
    annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = lineColor, size = 0.6) +
    annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = lineColor, size = 0.6) +
    annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = lineColor, size = 0.6) +
    annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = lineColor, size = 0.6) +
    annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = lineColor, size = 0.6)+
    annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = lineColor, size = 0.6)+
    annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = lineColor, size = 0.6)
  
  return(ggp)
}

# colors from my ShinyUEFA color scheme
shUEFA <- c(
  "blueNavy" = hex("09003d"),
  "blueDk" = hex("10006b"),
  "blueMed" = hex("341cbd"),
  "blueLt" = hex("0077ff"),
  "blueSky" = hex("00ccff"),
  "bluePale" = hex("ccebff"),
  "purple" = hex("473c85"),
  "purpleLt" = hex("a08fff"),
  "orangeLt" = hex("ffce99"),
  "orange" = hex("ff9421"),
  "orangeDk" = hex("ff4d00"),
  "yellow" = hex("ffbf00")
)

shUEFA_theme <- theme(
  
  line = element_line(color = shUEFA["blueSky"], linetype = "solid", lineend = "square", size = 0.5),
  rect = element_rect(fill = shUEFA["blueDk"]),
  
  text = element_text(family = "sans", color = shUEFA["purpleLt"]),
  title = element_text(family = "sans", color = shUEFA["purpleLt"]),
  
  axis.title = element_text(face = "bold", color = shUEFA["purpleLt"], size = 16, margin = margin(2,2,2,2,"pt")),
  axis.text = element_text(color = shUEFA["blueSky"], size = 11),
  axis.ticks = element_line(size = 0.5, lineend = "square", color = shUEFA["blueSky"]),
  axis.ticks.length = unit(2, "mm"),
  axis.line = element_line(color = shUEFA["blueSky"], size = 0.5),
  
  legend.background = element_rect(fill = shUEFA["blueNavy"], linetype = "blank"),
  legend.margin = margin(10,10,10,10, unit = "pt"),
  legend.key = element_rect(fill = shUEFA["blueNavy"], linetype = "blank"),
  legend.text = element_text(color = shUEFA["blueSky"], size = 11),
  legend.text.align = 0.5,
  legend.title = element_text(color = shUEFA["purpleLt"], face = "bold", size = 16),
  legend.title.align = 0.5,
  legend.position = "right",
  legend.direction = "vertical",
  legend.justification = "top",
  legend.box = "vertical",
  legend.box.just = "center",
  legend.box.margin = margin(6,6,6,6),
  legend.box.background = element_rect(fill = shUEFA["blueDk"], color = shUEFA["blueNavy"], size = 1),
  
  panel.background = element_rect(fill = shUEFA["blueNavy"]),
  panel.border = element_rect(fill = NA, color = shUEFA["blueDk"], size = 0.5),
  panel.spacing = unit(10, "pt"),
  panel.grid.major = element_line(color = shUEFA["blueNavy"], size = 0.5),
  panel.grid.minor = element_line(color = shUEFA["blueNavy"], size = 0.5),
  
  plot.background = element_rect(fill = shUEFA["blueDk"]),
  plot.title = element_text(face = "bold", size = 24, color = shUEFA["purpleLt"], hjust = 0.5),
  plot.subtitle = element_text(family = "sans", face = "italic", size = 12, color = shUEFA["purpleLt"], hjust = 0.5),
  plot.caption = element_text(family = "sans", color = shUEFA["blueSky"], size = 11),
  plot.margin = margin(15,15,15,15),
  plot.tag = element_text(family = "sans", color = shUEFA["blueSky"], size = 16),
  plot.tag.position = c(0.95,1),
  
  strip.background = element_rect(fill = shUEFA["blueDk"], shUEFA["blueSky"], size = 0.5),
  strip.text = element_text(family = "sans", face = "bold", size = 13, color = shUEFA["blueSky"]),
  
  complete = FALSE,
  validate = TRUE
)






