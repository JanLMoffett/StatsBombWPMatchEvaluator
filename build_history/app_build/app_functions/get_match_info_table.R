
get_match_info_table <- function(matchDF){
  
  matchDisplayVars <- c(
    "competition.competition_name",
    "season.season_name",
    "competition_stage.name",
    "stadium.name","stadium.country.name",
    "referee.name",
    "match_date", "kick_off", 
    "away_team.away_team_name", "away_team.away_team_group",
    "away_score", 
    "home_team.home_team_name", "home_team.home_team_group",
    "home_score")
  
  info_type <- c(
    "Competition",
    "Season",
    "Stage",
    "Stadium",
    "Stadium Location",
    "Referee",
    "Date",
    "Kickoff Time",
    "Away Team",
    "Away Team Group",
    "Away Score Final",
    "Home Team",
    "Home Team Group",
    "Home Score Final")
  
  mt <- t(matchDF %>% select(all_of(matchDisplayVars)))
  mt <- data.frame("value" = mt, "info_type" = info_type)
  return(mt %>% select(info_type, value))
}

