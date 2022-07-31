library(StatsBombR)
library(tidyverse)

source("functions_etc/scrape_StatsBomb.R")


#with code from StatsBomb tutorial:
#https://statsbomb.com/articles/soccer/statsbomb-announce-the-release-of-free-statsbomb-360-data-euro-2020-available-now/
Comp <- FreeCompetitions()

#the Matches dataset
Matches <- FreeMatches(Comp)

Matches <- Matches %>% select(-all_of(c("home_team.managers", "away_team.managers")))
#write.csv(Matches, "win_probability/wp_data/dbb_matches.csv")

m <- Matches

compIDs <- unique(Matches$competition.competition_id)

#dataframe to store results
home_goalX_df <- data.frame(
  "minute" = 0,
  "cur_score_diff" = 0,
  "n" = 0,
  "n_future_goals" = 0,
  "n_home_win" = 0,
  "n_draw" = 0,
  "n_home_loss" = 0,
  "comp_id" = 0,
  "season_id" = 0)

away_goalX_df <- data.frame(
  "minute" = 0,
  "cur_score_diff" = 0,
  "n" = 0,
  "n_future_goals" = 0,
  "n_home_win" = 0,
  "n_draw" = 0,
  "n_home_loss" = 0,
  "comp_id" = 0,
  "season_id" = 0)

for(cmp in compIDs){
  
  sids <- unique(m %>% filter(competition.competition_id == cmp) %>% pull(season.season_id))
  
  for(sid in sids){
    
    print(paste0("Calculating Comp ID ", cmp, ", Season ID ", sid))
    
    #these datasets are going to get huge unless i filter out events i don't need
    #going to figure things out using premiere league matches
    m1 <- m %>% filter(competition.competition_id == cmp, season.season_id == sid)
    
    events1 <- StatsBombFreeEvents(MatchesDF = m1, Parallel = F)
    events1 <- allclean(events1)
    events1 <- get.opposingteam(events1)
    
    #get rid of nested vars
    events1 <- get_stripped_events(events1)
    
    #determining what variables and events types i need for win prob calculations
    names(events1)
    
    keep_vars <- c(
      "id", "type.id", "type.name", "team.id", "team.name" ,"player.id", "player.name",
      "index", "match_id", "competition_id", "season_id",
      "period", "timestamp", "minute", "second", "duration",
      "possession", "possession_team.id", "possession_team.name",
      "tactics.formation", #"injury_stoppage.in_chain",
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
                                      type.name == "Own Goal For" ~ 1,
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
    
    #calculate future goals
    events3 <- events3 %>% group_by(match_id) %>% arrange(desc(index)) %>%
      mutate(future_goals_home = cumsum(home_score_change), 
             future_goals_away = cumsum(away_score_change)) %>%
      mutate(future_goals_home = future_goals_home - home_score_change,
             future_goals_away = future_goals_away - away_score_change)
      
    
    #names(events3)
    home_goalX_calc <- events3 %>% 
      filter(is_home_team == 1) %>%
      group_by(minute, cur_score_diff) %>%
      summarize(n = n(),
                n_future_goals = sum(future_goals_home, na.rm = T),
                n_home_win = sum(is_home_win, na.rm = T),
                n_draw = sum(is_draw, na.rm = T),
                n_home_loss = sum(is_home_loss, na.rm = T),
                comp_id = first(competition.competition_id),
                season_id = first(season.season_id))
    
    home_goalX_calc <- events3 %>% 
      filter(is_home_team == 1) %>%
      group_by(minute, cur_score_diff) %>%
      summarize(n = n(),
                n_future_goals = sum(future_goals_home, na.rm = T),
                n_home_win = sum(is_home_win, na.rm = T),
                n_draw = sum(is_draw, na.rm = T),
                n_home_loss = sum(is_home_loss, na.rm = T),
                comp_id = first(competition.competition_id),
                season_id = first(season.season_id))
    
    away_goalX_calc <- events3 %>% 
      filter(is_home_team == 0) %>%
      group_by(minute, cur_score_diff) %>%
      summarize(n = n(),
                n_future_goals = sum(future_goals_away, na.rm = T),
                n_home_win = sum(is_home_win, na.rm = T),
                n_draw = sum(is_draw, na.rm = T),
                n_home_loss = sum(is_home_loss, na.rm = T),
                comp_id = first(competition.competition_id),
                season_id = first(season.season_id))
    
    home_goalX_df <- rbind(home_goalX_df, home_goalX_calc)
    away_goalX_df <- rbind(away_goalX_df, away_goalX_calc)
    
    
  }
  
  
  
}


#remove dummy row of wp_df
home_goalX_df <- home_goalX_df[-1,]
away_goalX_df <- away_goalX_df[-1,]

write.csv(home_goalX_df, "win_probability/wp_data/home_goalX_calculations_1.csv")
write.csv(away_goalX_df, "win_probability/wp_data/away_goalX_calculations_1.csv")

