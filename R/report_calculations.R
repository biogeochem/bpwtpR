summarize_monthly <- function(labdat){
  
  df.monthly <- labdat %>%
    mutate(month = month(datetime_ymd.hms, label = T, abbr = T)) %>%
    group_by(station, unit, year, month, parm_tag, parameter) %>%
    summarize('Monthly Mean' = mean(result, na.rm = TRUE))
  
  return(as.data.frame(df.monthly))
    
}

historical_extremes <- function(df = "", sampling_station = "", 
                                grouping = "", by_month = T){
  if(by_month == T){
    df %>% 
      filter(parm_tag == grouping & station == sampling_station) %>%
      group_by(station, parameter, month) %>%
      summarize("Min" = min(result, na.rm = TRUE), "Max" = max(result, na.rm = TRUE))
    
  } else {
    
    df %>% 
      filter(parm_tag == grouping & station == sampling_station) %>%
      group_by(station, parameter) %>%
      summarize("Min" = min(result, na.rm = TRUE), "Max" = max(result, na.rm = TRUE))
  }
  
}


historical_percentiles <- function(df = "", percentiles = c(0.1, .25, .50, .75, .9), 
                                   sampling_station = "", grouping = "",
                                   by_month = T){
  
  p_names <- map_chr(percentiles, ~paste0(.x*100, "%"))
  p_funs <- map(percentiles, ~ partial(quantile, probs = .x, na.rm = TRUE)) %>% 
    set_names(nm = p_names)
  
  if(by_month == T){
    df %>%
      filter(parm_tag == grouping & station == sampling_station) %>%
      group_by(station, parameter, month) %>%
      summarize_at(vars(result), funs(!!!p_funs))
    
  } else {
    
    df %>%
      filter(parm_tag == grouping & station == sampling_station) %>%
      group_by(station, parameter) %>%
      summarize_at(vars(result), funs(!!!p_funs))  
  }
  
}


# this function will calculate the basic historical metrics by month or overall. Month must be specified by three letter abbr.

historical_basics <- function(df = "", sampling_station = "", grouping = "", by_month = T){
  
  if(by_month == T){
    
    df %>%
      filter(parm_tag == grouping & station == sampling_station) %>%
      group_by(station, parameter, month) %>%
      summarize(N = n(), Mean = mean(result, na.rm = T), 
                SD = sd(result, na.rm = T), CV = (SD/N)*100)
  } else{
    
    df %>%
      filter(parm_tag == grouping & station == sampling_station) %>%
      group_by(station, parameter) %>%
      summarize(N = n(), Mean = mean(result, na.rm = T), 
                SD = sd(result, na.rm = T), CV = (SD/N)*100)
  }
  
}
