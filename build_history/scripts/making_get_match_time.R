

maxPeriod <- max(this.ev$period, na.rm = T)
maxTimestamp <- max(this.ev$timestamp[which(this.ev$period == maxPeriod)])

#info about the periods in this match
per_info <- this.ev %>% group_by(period) %>%
  summarize(per_end = max(timestamp_seconds, na.rm = T)
  ) %>%
  mutate(per_div = cumsum(per_end))