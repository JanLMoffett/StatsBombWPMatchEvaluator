
#this file was built with code from "scripts/getting_sb_data2.R"


#helper function, returns vector of nested var names
find_nests <- function(aDataframe){
  
  lv <- vector()

  for(i in seq_along(aDataframe)){
    if(class(aDataframe[[i]]) == "list"){
      lv <- c(lv, names(aDataframe)[i])
    }
  }
  
  return(lv)
}



#get EUFA Euros 2020 matches from StatsBombR
scrape_matches <- function(compName = "UEFA Euro"){
  
  require(StatsBombR)
  require(tidyverse)
  
  #with code from StatsBomb tutorial:
  #https://statsbomb.com/articles/soccer/statsbomb-announce-the-release-of-free-statsbomb-360-data-euro-2020-available-now/
  Comp <- FreeCompetitions()
  
  #the Matches dataset, for the specified competition
  Matches <- FreeMatches(Comp)
  Matches <- Matches %>% filter(competition.competition_name==compName) %>% 
    #remove nested managers vars
    select(-all_of(c("home_team.managers", "away_team.managers")))
  
  #this df is stored as "big_data/dbb_matches.csv"
  return(Matches)
  
}


#test:  // // // // // // // // // // // // // // // // // // // // // // // // 
#m <- scrape_matches() #is there any way to make this faster?
# // // // // // // // // // // // // // // // // // // // // // // // // // // 

#get 360 Events dataset from StatsBombR
#i'm not worried about this dataset rn
#depends on output from scrape_matches()
scrape_events360 <- function(matches_df){
  
  require(StatsBombR)
  require(tidyverse)
  
  data360 <- StatsBombFree360Events(MatchesDF = matches_df, Parallel = T)
  return(data360)
}


#get Events dataset from StatsBombR
#depends on output from scrape_matches()
#this returns an events df that still contains nested vars (lists as obs)
scrape_events <- function(matches_df){
  
  require(StatsBombR)
  require(tidyverse)
  
  events <- StatsBombFreeEvents(MatchesDF = matches_df, Parallel = T)
  events <- allclean(events)
  events <- get.opposingteam(events)
  
  return(events)
  
}

#test: // // // // // // // // // // // // // // // // // // // // // // // // //
#ev_original <- scrape_events(m) #this is also pretty slow
# // // // // // // // // // // // // // // // // // // // // // // // // // // 

#after getting original events df, will need to strip off nested vars and unnest into df's

#  Nested vars:
#_____________________________
# "related_events"    
# "location"          
# "tactics.lineup"     
# "pass.end_location"     
# "carry.end_location"    
# "shot.end_location"     
# "shot.freeze_frame"     
# "goalkeeper.end_location"  

#all the df's produced by the following functions will be filter-able by match_id


#unnest related events variable and return df of related event pairs
#depends on output from scrape_events(), which depends on scrape_matches()
get_related_events <- function(events_df){
  
  require(tidyverse)
  
  relEvents <- events_df %>% 
    select(id, match_id, related_events) %>% 
    unnest(cols = "related_events") %>%
    mutate(one = 1) %>%
    group_by(id) %>%
    mutate(related_event_seq = cumsum(one),
           num_related_events = sum(one)) %>%
    select(-one) %>% ungroup() %>%
    rename(related_event_id = related_events) %>%
    mutate(event_relationship_id = paste0(id, "_", related_event_id, "_", 
                                          str_pad(related_event_seq, width = 2, 
                                                  side = "left", pad = "0")))
  
  return(relEvents)

}


#test:  // // // // // // // // // // // // // // // // // // // // // // // // 
#rel_ev <- get_related_events(ev_original)
# // // // // // // // // // // // // // // // // // // // // // // // // // // 

#unnest and return df of tactics.lineup variable
get_startingXI <- function(events_df){
  
  require(tidyverse)
  
  sXI <- events_df %>% filter(type.name == "Starting XI") %>% 
    select(id, match_id, team.id, team.name, tactics.formation, tactics.lineup) %>%
    unnest(cols = tactics.lineup)
  
  return(sXI)
  
}

 
#test:  // // // // // // // // // // // // // // // // // // // // // // // // 
#lu <- get_startingXI(ev_original)
# // // // // // // // // // // // // // // // // // // // // // // // // // // 

#unnest and return df of shot freeze frame variable
get_shotFF <- function(events_df){
  
  require(tidyverse)
  
  sff <- events_df %>% filter(type.name == "Shot") %>% select(id, match_id, shot.freeze_frame) %>% 
    unnest(cols = shot.freeze_frame) %>% 
    mutate(one = 1) %>%
    group_by(id) %>%
    mutate(shotFF_player_seq = cumsum(one),
           shotFF_num_players = sum(one)) %>% 
    ungroup() %>%
    mutate(shotFF_person_id = paste0(id, "_", str_pad(shotFF_player_seq, width = 2, side = "left", pad = "0")))
  
  sff.loc <- sff %>% select(id, shotFF_person_id, location) %>% unnest(cols = location)
  sff.loc$coord_type <- rep(c("x", "y"), dim(sff.loc)[1]/2)
  sff.loc.x <- sff.loc %>% filter(coord_type == "x") %>% select(id, shotFF_person_id, location) %>% rename(shotFF_player_location_x = location)
  sff.loc.y <- sff.loc %>% filter(coord_type == "y") %>% select(id, shotFF_person_id, location) %>% rename(shotFF_player_location_y = location)
  sff.loc <- inner_join(sff.loc.x, sff.loc.y, by = c("id", "shotFF_person_id"))
  
  
  sff <- sff %>% select(-all_of(c("location", "one"))) %>% 
    inner_join(sff.loc, by = c("id", "shotFF_person_id"))
  
  return(sff)
  
}


#test:  // // // // // // // // // // // // // // // // // // // // // // // // 
#shotFF <- get_shotFF(ev_original)
# // // // // // // // // // // // // // // // // // // // // // // // // // // 

#Unnest and return df of GK end location variable
get_gk_end_loc <- function(events_df){
  
  require(tidyverse)
  
  gk <- events_df %>% filter(type.name == "Goal Keeper") %>% 
    select(id, match_id, goalkeeper.end_location)  %>% 
    unnest(cols = goalkeeper.end_location)
  
  gk$coord_type <- rep(c("x", "y"), dim(gk)[1]/2)
  gk.x <- gk %>% filter(coord_type == "x") %>% rename(gk.end_location_x = goalkeeper.end_location) %>% select(-coord_type)
  gk.y <- gk %>% filter(coord_type == "y") %>% rename(gk.end_location_y = goalkeeper.end_location) %>% select(-coord_type)
  gk <- inner_join(gk.x, gk.y, by = c("id", "match_id"))
  
  return(gk)
  
}


#test:  // // // // // // // // // // // // // // // // // // // // // // // // 
#gkLoc <- get_gk_end_loc(ev_original)
# // // // // // // // // // // // // // // // // // // // // // // // // // //


#take nested variables out of events dataframe
get_stripped_events <- function(events_df){
  
  require(tidyverse)
  
  remove_vars <- find_nests(events_df)
  edf <- events_df %>% select(-all_of(remove_vars))
  
  return(edf)
  
}

#test:  // // // // // // // // // // // // // // // // // // // // // // // // 
#ev <- get_stripped_events(ev_original)
# // // // // // // // // // // // // // // // // // // // // // // // // // //










