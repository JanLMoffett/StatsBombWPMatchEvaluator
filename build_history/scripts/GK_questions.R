

#does the same goalie switch sides on the pitch (in the data) during the match?
shots <- read.csv("data/events_shot.csv")
dfns <- read.csv("data/events_defense.csv")

#number of events by position
shots %>% group_by(position.name) %>%
  summarize(n = n())

dfns %>% group_by(position.name) %>%
  summarize(n = n())


gk <- dfns %>% filter(position.id == 1)

this.match <- matchIDs[21]
m %>% filter(match_id == this.match)

gk.1 <- gk %>% filter(match_id == this.match)

plot_pitch(gk.1) + 
  shUEFA_theme +
  geom_point(aes(x = location.x, y = location.y, color = factor(period), shape = factor(player.id)))


#both goalies stay on the left, somehow.


