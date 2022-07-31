
library(StatsBombR)
library(tidyverse)

source("functions_etc/scrape_StatsBomb.R")
source("app/app_functions/timestamp_to_seconds.R")

#with code from StatsBomb tutorial:
#https://statsbomb.com/articles/soccer/statsbomb-announce-the-release-of-free-statsbomb-360-data-euro-2020-available-now/
Comp <- FreeCompetitions()

#the Matches dataset
Matches <- FreeMatches(Comp)

Matches <- Matches %>% select(-all_of(c("home_team.managers", "away_team.managers")))
#write.csv(Matches, "win_probability/wp_data/dbb_matches.csv")

m <- Matches

compIDs <- unique(Matches$competition.competition_id)
#how many matches in each season?
#View(m %>% group_by(competition.competition_id, season.season_id) %>% tally())


#cmp = 11
#sid = 37

i = 1
for(cmp in compIDs){
  j = 1
  #dataframe to store results
  fg_df <- data.frame(
    "competition_id" = 0,
    "season_id" = 0,
    "match_id" = 0,
    "time_bin" = 0,
    "cur_home_score" = 0,
    "cur_away_score" = 0,
    "future_goals_home" = 0,
    "future_goals_away" = 0,
    "start_loc_x" = 0,
    "start_loc_y" = 0,
    "end_loc_x" = 0,
    "end_loc_y" = 0
  )
  
  sids <- unique(m %>% filter(competition.competition_id == cmp) %>% pull(season.season_id))
  
  for(sid in sids){
    
    print("")
    print(paste0("Calculating Comp ", i, " of ", length(compIDs), ", Season ", j, " of ", length(sids)))
    
    
    m1 <- m %>% filter(competition.competition_id == cmp, season.season_id == sid)
    
    events1 <- StatsBombFreeEvents(MatchesDF = m1, Parallel = F)
    events1 <- allclean(events1)
    events1 <- get.opposingteam(events1)
    
    #get rid of nested vars
    events1 <- get_stripped_events(events1)
    
    #determining what variables and events types i need for win prob calculations
    #names(events1)
    
    keep_vars <- c(
      "id", "type.id", "type.name", "team.id", "team.name" ,"player.id", "player.name",
      "index", "match_id", "competition_id", "season_id",
      "period", "timestamp", "minute", "second", "duration",
      "possession", "possession_team.id", "possession_team.name",
      "tactics.formation", #"injury_stoppage.in_chain",
      "play_pattern.id", "play_pattern.name",
      "shot.type.id", "shot.type.name",
      "shot.outcome.id", "shot.outcome.name", "location.x", "location.y")
    
    #unique(events1$type.name)
    
    #keep_types <- c("Starting XI", "Half Start", 
    #                "Foul Committed", "Foul Won", 
    #                "Shot", "Bad Behaviour", "Half End",
    #                "Substitution", 
    #                "Offside", "Error", "Tactical Shift",
    #                "Injury Stoppage", "Player Off", "Player On",
    #                "Own Goal Against", "Referee Ball-Drop")
    
    events2 <- events1 %>% select(all_of(keep_vars))
    
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
    
    #here i make sure I'm only included first and second periods
    events3 <- events3 %>% filter(period < 3) %>%
      mutate(ts_seconds = timestamp_to_seconds(timestampStr = timestamp))
    
    #flag score changes
    events3 <- events3 %>% 
      mutate(is_goal = ifelse(type.name == "Shot" & shot.outcome.name == "Goal", 1, 0)) %>%
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
    
    #calculate future goals
    events3 <- events3 %>% group_by(match_id) %>% arrange(desc(index)) %>%
      mutate(future_goals_home = cumsum(home_score_change), 
             future_goals_away = cumsum(away_score_change)) %>%
      mutate(future_goals_home = future_goals_home - home_score_change,
             future_goals_away = future_goals_away - away_score_change) %>%
      ungroup()
      
    
    #check to see if code is working right
    #write.csv(events3, "win_probability/wp_data/events_test.csv")
    
    #now i need to iterate through matches in comp-season
    matchIDs <- unique(events3$match_id)
    
    #mid = 22912
    for(mid in matchIDs){
      m_events <- events3 %>% filter(match_id == mid) %>% arrange(index)
      
      m_events <- get_cumulative_match_seconds(m_events)
      
      #get end ts_seconds of match (2 periods only)
      max_sec <- max(m_events$cum_match_seconds)
      #divide it by 90 bins
      bin_width <- max_sec/90
      
      #assign bins to events
      m_events <- m_events %>% mutate(time_bin = floor(cum_match_seconds/bin_width))
      
      #write.csv(m_events, "win_probability/wp_data/m_events_test.csv")
      #View(m_events %>% select(time_bin, is_home_team, location.x, location.y))
      
      #standardize location coordinates
      m_events <- m_events %>%
        mutate(std_location_x = ifelse(is_home_team == 0, 120 - location.x, location.x),
               std_location_y = ifelse(is_home_team == 0, 80 - location.y, location.y))
      
      
      #condense to 90 rows of data for match
      mtb <- m_events %>% 
        filter(!is.na(std_location_x), !is.na(std_location_y)) %>% 
        group_by(time_bin) %>%
        summarize(match_id = first(match_id),
                  cur_home_score = first(cur_home_score),
                  cur_away_score = first(cur_away_score),
                  future_goals_home = first(future_goals_home),
                  future_goals_away = first(future_goals_away),
                  start_loc_x = first(std_location_x),
                  start_loc_y = first(std_location_y),
                  end_loc_x = last(std_location_x),
                  end_loc_y = last(std_location_y)
                  ) %>%
        mutate(competition_id = cmp,
               season_id = sid) %>%
        relocate(competition_id, season_id, match_id)
      
      #append to df
      fg_df <- rbind(fg_df, mtb)
      
    }
    
    
    
    
    j = j+1
  }
  
  #save data so sets don't get too large
  fn = paste0("win_probability/wp_data/futureGoals_", cmp, "_", i, ".csv")
  write.csv(fg_df, fn)
  i = i+1
}



