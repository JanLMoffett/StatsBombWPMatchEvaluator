
library(tidyverse)
library(devtools)

my_arrow <- arrow(angle = 35, length = unit(0.1, "inches"), ends = "last", type = "closed")
#}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}
#                                 Color Functions
#}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}

#turn hex codes into colors
hex <- function(hexCode){
  
  u <- as.character(hexCode)
  #make sure input is six digits long, letters and numbers only
  u <- str_remove(u, "[:punct:]")
  u <- str_sub(u, 1, 6)
  u <- paste0("#", u)
  
  return(rgb(t(col2rgb(u)), maxColorValue = 255))
}

#make an opaque color transparent
transpa <- function(colorObject, alpha1to255){
  return(rgb(t(col2rgb(colorObject)), maxColorValue = 255, alpha = alpha1to255))
}


#}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}
#                             Color Constants & Themes
#}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}

# colors from my ShinyUEFA color scheme
shUEFA <- c(
  "blueNavy" = hex("09003d"),
  "blueDk" = hex("10006b"),
  "blueMed" = hex("341cbd"),
  "blueLt" = hex("0077ff"),
  "blueSky" = hex("00ccff"),
  "bluePale" = hex("ccebff"),
  "purple" = hex("760ee6"),
  "purpleLt" = hex("9e53ed"),
  "orangeLt" = hex("ffce99"),
  "orange" = hex("ff9421"),
  "orangeDk" = hex("ff4d00"),
  "orangeDkr" = hex("d64202"),
  "yellow" = hex("ffbf00"),
  "red" = hex("f50505"),
  "salmon" = hex("f25252"),
  "ibm_blue" = hex("648fff"),
  "ibm_purple" = hex("785ef0"),
  "ibm_pink" = hex("dc267f"),
  "ibm_orange" = hex("fe6100"),
  "ibm_yellow" = hex("ffb000")
)



icyUEFA <- c(
  "ice1" = hex("e6f5ff"),
  "ice2" = hex("ccebff"),
  "ice3" = hex("A3DAFF"),
  "ice4" = hex("81b1eb"),
  "ice5" = hex("223fb3")
)

#an arrow to add to line segments, when needed
my_arrow <- arrow(angle = 35, length = unit(0.1, "inches"), ends = "last", type = "closed")

#pale blue theme used for plots in app
shUEFA_theme_icy <- theme(
  
  line = element_line(color = icyUEFA["ice3"], linetype = "solid", lineend = "square", size = 0.5),
  rect = element_rect(fill = icyUEFA["ice1"]),
  
  text = element_text(family = "sans", color = icyUEFA["ice5"]),
  title = element_text(family = "sans", color = icyUEFA["ice5"]),
  
  axis.title = element_blank(), #element_text(face = "bold", color = shUEFA["blueDk"], size = 16, margin = margin(2,2,2,2,"pt")),
  axis.text = element_blank(), #element_text(color = shUEFA["blueDk"], size = 11),
  axis.ticks = element_blank(), #element_line(size = 0.5, lineend = "square", color = shUEFA["blueDk"]),
  axis.ticks.length = unit(2, "mm"),
  axis.line = element_blank(), #element_line(color = shUEFA["blueSky"], size = 0.5),
  
  legend.background = element_rect(fill = icyUEFA["ice2"], linetype = "blank"),
  legend.margin = margin(10,10,10,10, unit = "pt"),
  legend.key = element_rect(fill = icyUEFA["ice1"], linetype = "blank"),
  legend.text = element_text(color = icyUEFA["ice5"], size = 11),
  legend.text.align = 0.5,
  legend.title = element_text(color = icyUEFA["ice5"], face = "bold", size = 16),
  legend.title.align = 0.5,
  legend.position = "none", #"right",
  legend.direction = "vertical",
  legend.justification = "top",
  legend.box = "vertical",
  legend.box.just = "center",
  legend.box.margin = margin(6,6,6,6),
  legend.box.background = element_rect(fill = icyUEFA["ice1"], color = icyUEFA["ice3"], size = 1),
  
  panel.background = element_rect(fill = icyUEFA["ice1"]),
  panel.border = element_rect(fill = NA, color = icyUEFA["ice3"], size = 0.5),
  panel.spacing = unit(10, "pt"),
  panel.grid.major = element_blank(),#element_line(color = shUEFA["blueNavy"], size = 0.5),
  panel.grid.minor = element_blank(),#element_line(color = shUEFA["blueNavy"], size = 0.5),
  
  plot.background = element_rect(fill = icyUEFA["ice1"]),
  plot.title = element_text(face = "bold", size = 18, color = icyUEFA["ice5"], hjust = 0.5),
  plot.subtitle = element_text(family = "sans", face = "italic", size = 12, color = icyUEFA["ice5"], hjust = 0.5),
  plot.caption = element_text(family = "sans", color = icyUEFA["ice5"], size = 11),
  plot.margin = margin(15,15,15,15),
  plot.tag = element_text(family = "sans", color = icyUEFA["ice5"], size = 16),
  plot.tag.position = c(0.95,1),
  
  strip.background = element_rect(fill = icyUEFA["ice2"], color = icyUEFA["ice3"], size = 0.5),
  strip.text = element_text(family = "sans", face = "bold", size = 13, color = icyUEFA["ice5"]),
  
  complete = FALSE,
  validate = TRUE
)


#}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}
#                                 Plotting Functions
#}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}|{=@=}

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




# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
#scales for visualizing event types:
eventTypes <- c("Starting XI", "Half Start", "Pass", 
                "Ball Receipt*", "Carry", "Pressure", 
                "Miscontrol", "Interception", "Dribble", 
                "Duel", "Ball Recovery", "Block", 
                "Shot", "Goal Keeper", "Clearance", 
                "Dribbled Past", "Foul Committed", "Foul Won", 
                "Dispossessed", "Offside", "Shield", 
                "Half End", "Substitution", "Tactical Shift",
                "Injury Stoppage", "Own Goal Against", "Own Goal For", 
                "Referee Ball-Drop", "Error", "Player Off", 
                "Player On", "50/50", "Bad Behaviour")

eventColorGroups <- c("Roster", "Time", "Offense",
                      "Offense", "Offense", "Defense",
                      "Offense", "Defense", "Offense",
                      "Defense", "Offense", "Defense",
                      "Offense", "Defense", "Defense",
                      "Defense", "Foul", "Foul", 
                      "Offense", "Foul", "Offense", 
                      "Time", "Roster", "Roster", 
                      "Time", "Offense", "Defense",
                      "Time", "Defense", "Roster", 
                      "Roster", "Offense", "Foul")

eventShapeGroups <- c("Marker", "Marker", "Ball Plus", 
                "Ball Neutral", "Ball Plus", "No Ball", 
                "Ball Negative", "Ball Plus", "Ball Plus", 
                "No Ball", "Ball Plus", "No Ball", 
                "Ball Plus", "No Ball", "Ball Plus", 
                "No Ball", "Foul", "Freebie", 
                "Ball Negative", "No Ball", "No Ball", 
                "Marker", "Marker", "Marker",
                "Marker", "Ball Negative", "Ball Plus", 
                "Marker", "Ball Negative", "Marker", 
                "Marker", "Ball Neutral", "Foul")

eventTypes <- data.frame(event_type = eventTypes, 
                         color_group = eventColorGroups,
                         shape_group = eventShapeGroups)


ecg <- data.frame(
  group = unique(eventColorGroups),
  color = c(shUEFA["blueDk"], shUEFA["blueNavy"], shUEFA["orange"], shUEFA["purple"], shUEFA["red"])
  
)

esg <- data.frame(
  group = unique(eventShapeGroups),
  shape = c(3, 19, 1, 15, 13, 4, 18)
)

eventTypes <- left_join(eventTypes, ecg, by = c("color_group" = "group")) %>%
  left_join(esg, by = c("shape_group" = "group"))


events_scale_shapes <- eventTypes$shape
names(events_scale_shapes) <- eventTypes$event_type

events_scale_colors <- eventTypes$color
names(events_scale_colors) <- eventTypes$event_type

#event types included in checkbox input

eventTypes2 <- unique(eventTypes$event_type)[c(3:21,26:29,32:33)]
