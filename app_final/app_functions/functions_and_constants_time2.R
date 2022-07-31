
#takes a timestamp string in "00:00:00.000" format and returns numeric seconds
timestamp_to_seconds <- function(timestampStr){
  
  require(lubridate)
  return(as.numeric(seconds(hms(timestampStr))))
  
}

#turns non-cumulative seconds into a timestamp
seconds_to_timestamp <- function(persum, num_seconds){
  require(lubridate)
  require(stringr)
  
  curper = ifelse(num_seconds < persum$cum_total_seconds[1], 1, 2)
  num_sec = ifelse(curper > 1, num_seconds - persum$cum_total_seconds[1], num_seconds)
  
  num_min = floor(num_sec / 60)
  
  rem_seconds = round(num_sec - num_min*60, digits = 3)
  
  num_min = str_pad(as.character(num_min), width = 2, side = "left", pad = "0")
  rem_seconds = str_pad(as.character(rem_seconds), width = 2, side = "left", pad = "0")
  
  return(paste0("00:", num_min, ":", rem_seconds))
}


#takes match events df and returns a table of periods with end timestamps and total seconds
get_period_summary <- function(MatchEventsDF){
  require(dplyr)
  
  per <- MatchEventsDF %>% group_by(period) %>%
    summarize(max_ts = max(timestamp, na.rm = T)) %>% 
    mutate(total_seconds = timestamp_to_seconds(max_ts)) %>%
    mutate(cum_total_seconds = cumsum(total_seconds))
  
  return(per)
  
}

#takes match events df and returns a table of possessions
get_possession_summary <- function(MatchEventsDF){
  require(dplyr)
  
  max_sec <- max(MatchEventsDF$cum_match_seconds, na.rm = T)
  
  poss <- MatchEventsDF %>% arrange(index) %>% group_by(possession) %>%
    summarize(
      possession_team = first(possession_team.name),
      start_of_poss = first(cum_match_seconds)) %>% 
    mutate(end_of_poss = lead(start_of_poss, n = 1, default = max_sec))
  
  return(poss)
  
}

#takes match events df and returns same df with cum_match_seconds variable added
get_cumulative_match_seconds <- function(MatchEventsDF){
  require(dplyr)
  this.per <- get_period_summary(MatchEventsDF)
  
  mdf <- MatchEventsDF %>% mutate(ts_seconds = timestamp_to_seconds(timestamp)) %>%
    mutate(
      cum_match_seconds = case_when(
        period == 1 ~ ts_seconds,
        period == 2 ~ ts_seconds + this.per$total_seconds[1],
        period == 3 ~ ts_seconds + sum(this.per$total_seconds[1:2]),
        period == 4 ~ ts_seconds + sum(this.per$total_seconds[1:3]),
        period == 5 ~ ts_seconds + sum(this.per$total_seconds[1:4])
      ))
  
  return(mdf)
}

#takes match events df and returns same df with time_bins variable added
get_time_bins_match_events <- function(MatchEventsDF){
  require(dplyr)
  
  if(is.null(MatchEventsDF$cum_match_seconds)){
    print("Run get_cumulative_match_seconds() first!")
  }else{
    max_sec <- max(MatchEventsDF$cum_match_seconds, na.rm = T)
    #divide it by 90 bins
    bin_width <- max_sec/90
    
    #assign bins to events
    MatchEventsDF <- MatchEventsDF %>% mutate(time_bin = floor(cum_match_seconds/bin_width))
  }
  
  return(MatchEventsDF)
}

#takes an amount of seconds, max seconds, and returns a bin, 0-89
get_time_bin <- function(cumMatchSeconds, maxMatchSeconds){
  bin_width <- maxMatchSeconds/90
  return(floor(cumMatchSeconds/bin_width))
}
