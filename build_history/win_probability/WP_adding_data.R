library(StatsBombR)
library(tidyverse)

source("functions_etc/scrape_StatsBomb.R")


#with code from StatsBomb tutorial:
#----
#https://statsbomb.com/articles/soccer/statsbomb-announce-the-release-of-free-statsbomb-360-data-euro-2020-available-now/
Comp <- FreeCompetitions()

#how many other matches can i scrape from statsbomb?
#looks like a lot

#the Matches dataset
Matches <- FreeMatches(Comp)

#what is in this big matches set before i filter for uefa?
#Matches <- Matches %>% filter(competition.competition_name=="UEFA Euro")

#there are 1096 matches in the set
Matches <- Matches %>% select(-all_of(c("home_team.managers", "away_team.managers")))
#write.csv(Matches, "win_probability/wp_data/dbb_matches.csv")

compIDs <- unique(Matches$competition.competition_id)
#16 37 43 11 49  2 55 72

#dataframe to store results
wp_df <- data.frame(
  "period" = 0,
  "is_home_team" = 0,
  "is_poss_team" = 0,
  "cur_score_diff" = 0,
  "n" = 0,
  "n_home_win" = 0,
  "n_draw" = 0,
  "n_home_loss" = 0,
  "comp_id" = 0)

for(cmp in compIDs){
  #these datasets are going to get huge unless i filter out events i don't need
  #going to figure things out using premiere league matches
  m1 <- Matches %>% filter(competition.competition_id == cmp)
  
  events1 <- StatsBombFreeEvents(MatchesDF = m1, Parallel = F)
  events1 <- allclean(events1)
  events1 <- get.opposingteam(events1)
  
  #get rid of nested vars
  events1 <- get_stripped_events(events1)
  
  #determining what variables and events types i need for win prob calculations
  names(events1)
  
  keep_vars <- c(
    "id", "type.id", "type.name", "team.id", "team.name" ,"player.id", "player.name", "out",
    "index", "match_id", "competition_id", "season_id",
    "period", "timestamp", "minute", "second", "duration",
    "possession", "possession_team.id", "possession_team.name",
    "tactics.formation", "injury_stoppage.in_chain",
    "play_pattern.id", "play_pattern.name",
    "shot.type.id", "shot.type.name",
    "shot.outcome.id", "shot.outcome.name")
  
  unique(events1$type.name)
  
  keep_types <- c("Starting XI", "Half Start", 
                  "Foul Committed", "Foul Won", 
                  "Shot", "Bad Behaviour", "Half End",
                  "Substitution", 
                  "Offside", "Error", "Tactical Shift",
                  "Injury Stoppage", "Player Off", "Player On",
                  "Own Goal Against", "Referee Ball-Drop")
  
  events2 <- events1 %>% select(all_of(keep_vars)) %>% 
    filter(type.name %in% keep_types)
  
  #write.csv(events2, "win_probability/wp_data/match_example.csv")
  
  #join events and match data
  names(m1)
  m2 <- m1 %>% select(-all_of(c(
    "metadata.shot_fidelity_version", "metadata.xy_fidelity_version",
    "metadata.data_version", "match_status", "match_status_360", "last_updated",
    "last_updated_360")))
  
  m2 <- m2 %>% mutate(
    final_score_diff = home_score - away_score) %>%
    rename(final_home_score = home_score,
           final_away_score = away_score) %>%
    mutate(home_result = case_when(final_score_diff > 0 ~ 1, 
                                   final_score_diff < 0 ~ -1,
                                   TRUE ~ 0)) %>%
    mutate(is_home_win = ifelse(final_score_diff > 0, 1, 0),
           is_draw = ifelse(final_score_diff == 0, 1, 0),
           is_home_loss = ifelse(final_score_diff < 0, 1, 0))
  
  events3 <- left_join(events2, m2, by = "match_id")
  
  #flag score changes
  events3 <- events3 %>% 
    mutate(is_goal = ifelse(type.name == "Shot" & shot.outcome.name == "Goal" & shot.type.name != "Penalty", 1, 0)) %>%
    mutate(score_change = case_when(is_goal == 1 ~ 1,
                                    type.name == "Own Goal Against" ~ -1,
                                    TRUE ~ 0)) %>%
    mutate(is_home_team = ifelse(team.name == home_team.home_team_name, 1, 0),
           is_poss_team = ifelse(team.name == possession_team.name, 1, 0)) %>%
    mutate(home_score_change = ifelse(is_home_team == 1, score_change, 0),
           away_score_change = ifelse(is_home_team == 0, score_change, 0)) %>%
    group_by(match_id) %>% arrange(match_id, index) %>%
    mutate(cur_home_score = cumsum(home_score_change),
           cur_away_score = cumsum(away_score_change)) %>% 
    ungroup() %>%
    mutate(cur_score_diff = cur_home_score - cur_away_score)
  
  #check to see if code is working right
  #write.csv(events3, "win_probability/wp_data/events_test.csv")
  
  #names(events3)
  wp_calc <- events3 %>% group_by(period, is_home_team, 
                                  is_poss_team, cur_score_diff) %>%
    summarize(n = n(),
              n_home_win = sum(is_home_win, na.rm = T),
              n_draw = sum(is_draw, na.rm = T),
              n_home_loss = sum(is_home_loss, na.rm = T),
              comp_id = first(competition.competition_id))
  
  wp_df <- rbind(wp_df, wp_calc)
  
  
}


#remove dummy row of wp_df
wp_df <- wp_df[-1,]

write.csv(wp_df, "win_probability/wp_data/WP_calculations_1.csv")

