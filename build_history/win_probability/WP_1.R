
library(StatsBombR)
library(tidyverse)
library(devtools)
library(lubridate)

source("app/app_functions/scrape_StatsBomb.R")
source("app/app_functions/bombViz.R")
source("app/app_functions/timestamp_to_seconds.R")
source("app/app_functions/get_match_players2.R")
source("app/app_functions/get_match_info_table.R")

#position abbreviations and display coordinates
posAb <- read.csv("app/app_functions/positionDisplay.csv")


# ||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=
#                               Get Data
# ||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=||=||=||@||=||=||=

#since there is only one competition used in hackathon data, pulling data from
#api isn't reactive, just once at the beginning.  ideally, dashboard will be
#usable with any available StatsBomb360 competition dataset

#Matches
m <- scrape_matches(compName = "UEFA Euro")
#m <- read.csv("app_data/dbb_matches.csv")

#Events (with nested vars)
ev_og <- scrape_events(m)

#Unnested starting lineups
lu <- get_startingXI(ev_og)
#lu <- read.csv("app_data/dbb_events_startingXI.csv")

#related events
rel_ev <- get_related_events(ev_og)
#write.csv(rel_ev, "big_data/relatedEventsLong.csv")

#events without nested vars
ev <- get_stripped_events(ev_og)
#ev <- read.csv("app_data/dbb_events.csv")

ev_og <- NULL

#list of unique match_id values 
matchIDs <- unique(m$match_id)
#all data used in app will be filtered by input match_id value 
#each time a new match id is selected

#event types are standing in for statviews, which will be groups of event types tbd
eventTypes <- unique(ev$type.name)

#transforming timestamp var so i can plot it on x axis
ev <- ev %>% mutate(timestamp_seconds = timestamp_to_seconds(timestamp))

eventTypes

names(ev)

#how do "Own Goal For" and "Own Goal Against" relate?
ev %>% filter(type.name %in% c("Own Goal For", "Own Goal Against"))
#for each own goal has two events - one for each team

#how do i find goals?
unique(ev$shot.outcome.name)
unique(ev$shot.type.name)

ev <- ev %>% mutate(is_goal = ifelse(shot.outcome.name == "Goal" & shot.type.name != "Penalty", 1, 0),
                    is_penalty_goal = ifelse(shot.outcome.name == "Goal" & shot.type.name == "Penalty", 1, 0))


goals <- ev %>% filter(is_goal == 1)

#work on match df a little before joining it to events
names(m)

m2 <- m %>% 
  select(
    match_id, 
    match_date,
    kick_off,
    home_score,
    away_score,
    match_week,
    home_team.home_team_name,
    away_team.away_team_name,
    competition_stage.name,
    stadium.name,
    stadium.country.name,
    referee.name,
    referee.country.name) %>%
  rename(
    home_team = home_team.home_team_name,
    away_team = away_team.away_team_name,
    competition_stage = competition_stage.name,
    stadium = stadium.name,
    stadium_country = stadium.country.name,
    referee = referee.name,
    referee_country = referee.country.name
  )


m2 <- m2 %>% 
  mutate(
    match_result = case_when(home_score > away_score ~ "home win",
                             home_score < away_score ~ "away win",
                             TRUE ~ "draw"),
    final_score_differential = home_score - away_score
    )

names(ev)

ev <- ev %>% left_join(m2 %>% select(match_id, home_team, away_team), by = "match_id")

#match-specific vars data from the events df
m3 <- ev %>% 
  mutate(is_own_goal = ifelse(type.name == "Own Goal Against", 1, 0),
         is_goal_home = ifelse(is_goal == 1 & team.name == home_team, 1, 0),
         is_goal_away = ifelse(is_goal == 1 & team.name == away_team, 1, 0)
         ) %>%
  mutate(is_own_goal_home = ifelse(is_own_goal == 1 & team.name == home_team, 1, 0),
         is_own_goal_away = ifelse(is_own_goal == 1 & team.name == away_team, 1, 0)
    
  ) %>%
  group_by(match_id) %>% 
  summarize(total_goals = sum(is_goal, na.rm = T),
            total_goals_away = sum(is_goal_away, na.rm = T),
            total_goals_home = sum(is_goal_home, na.rm = T),
            
            total_own_goals = sum(is_own_goal, na.rm = T),
            total_own_goals_home = sum(is_own_goal_home, na.rm = T),
            total_own_goals_away = sum(is_own_goal_away, na.rm = T)
            )
  
  
m4 <- m2 %>% left_join(m3, by = "match_id")
write.csv(m4, "win_probability/wp_data/matches_UEFA2020.csv")

#"home" teams in matches are usually in neutral sites, difference is home team
#starts with possession?
ev %>% filter(index < 10) %>% select(possession_team.name, team.name)

#i need matches grouped and events in chrono order
ev %>% group_by(match_id) %>% arrange(index)

debug_match <- ev %>% filter(match_id == 3795506)
write.csv(debug_match, "win_probability/wp_data/match3795506.csv")
