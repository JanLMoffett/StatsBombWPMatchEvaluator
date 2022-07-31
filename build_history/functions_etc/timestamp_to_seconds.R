
#takes a timestamp string in "00:00:00.000" format and returns numeric seconds
timestamp_to_seconds <- function(timestampStr){
  require(lubridate)
  return(as.numeric(seconds(hms(timestampStr))))
  
}


#takes match events df and returns list containing "period" and "timestamp" for end of match
get_match_maxTime <- function(MatchEventsDF){
  
  #end period and timestamp for the match
  maxPeriod <- max(MatchEventsDF$period, na.rm = T)
  maxTimestamp <- max(MatchEventsDF$timestamp[which(MatchEventsDF$period == maxPeriod)], na.rm = T)
  
  return(list("period" = maxPeriod, "timestamp" = maxTimestamp))
}

#takes match events df and returns a table of periods with end timestamps and total seconds
get_period_summary <- function(MatchEventsDF){
  
  per <- MatchEventsDF %>% group_by(period) %>%
    summarize(max_ts = max(timestamp, na.rm = T)) %>% 
    mutate(total_seconds = timestamp_to_seconds(max_ts))
  
  return(per)
  
}

#takes events df and returns same df with cum_match_seconds variable added
get_cumulative_match_seconds <- function(MatchEventsDF){
  
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

#takes match players and match events df and returns match players df with 
#cumulative seconds on and off vars added
get_cumulative_ts_on_off <- function(MatchPlayersDF, periodSummary){
  
  ret.mp <- MatchPlayersDF %>% 
    mutate(ts_on_seconds = timestamp_to_seconds(timestamp_on),
           ts_off_seconds = timestamp_to_seconds(timestamp_off)) 
  
  ret.mp <- ret.mp %>% mutate(
    ts_on_cum_seconds = case_when(
      period_on == 1 ~ ts_on_seconds,
      period_on == 2 ~ ts_on_seconds + periodSummary$total_seconds[1],
      period_on == 3 ~ ts_on_seconds + sum(periodSummary$total_seconds[1:2]),
      period_on == 4 ~ ts_on_seconds + sum(periodSummary$total_seconds[1:3]),
      period_on == 5 ~ ts_on_seconds + sum(periodSummary$total_seconds[1:4])),
    ts_off_cum_seconds = case_when(
      period_off == 1 ~ ts_off_seconds,
      period_off == 2 ~ ts_off_seconds + periodSummary$total_seconds[1],
      period_off == 3 ~ ts_off_seconds + sum(periodSummary$total_seconds[1:2]),
      period_off == 4 ~ ts_off_seconds + sum(periodSummary$total_seconds[1:3]),
      period_off == 5 ~ ts_off_seconds + sum(periodSummary$total_seconds[1:4])))
  
  return(ret.mp)
  
}

