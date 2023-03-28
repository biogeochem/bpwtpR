#' Build monthly summary table
#'
#' @param df dataframe. Data for table
#' @param grouping string. data grouping
#' @param sampling_station sampling location
#' @param year_select year of interest
#'
#' @param df dataframe. Data for table, Default: ''
#' @param grouping string. data grouping, Default:
#'  c("operation", "physical", "majorConstituents", "traceConstituents",
#'    "THM", "biological", "bacteriological")
#' @param sampling_station string. Sampling location, Default: ''
#' @param year_select numeric. Desired year, Default: NULL
#'
#' @return dataframe
#' @export
#'
#' @importFrom pander pander
#' @importFrom lubridate year
#' @importFrom dplyr filter select rowwise
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
summarize_monthly_table <- function(df = "",
                                    grouping = c("operations","physical",
                                                 "majorConstituents", "traceConstituents",
                                                 "THM", "biological","bacteriological"),
                                    sampling_station = "",
                                    year_select = NULL){

  if(is.null(year_select)){
    year_select = year(Sys.Date())
  }

  df <- df %>%
    filter(parm_tag == grouping & station == sampling_station,
           year(date_ymd) == year_select) %>%
    mutate(result = case_when(result == -999999 ~ NA,
                              TRUE ~ result))

  # These parameters do not contain numeric values that
  if (grouping == "operations") {
    df <- df %>%
      filter(!grepl("ON", parameter),
             !grepl("OFF", parameter),
             !grepl("Intake", parameter))
  }

  df_round <- df %>%
    rowwise() %>%
    mutate(decimal_places = decimal_places(result)) %>%
    group_by(parameter, station, unit, parm_tag) %>%
    summarise(round_val = max(decimal_places, na.rm = TRUE))

  table_values <- summarize_monthly(labdat = df) %>%
    left_join(df_round) %>%
    rowwise() %>%
    mutate(monthly_mean = case_when(!is.infinite(round_val) ~
                                      round(monthly_mean, round_val),
                                    TRUE ~
                                      monthly_mean)) %>%
    select(month, parameter, unit, monthly_mean)

  if (grouping == "biological"){
    table_values <- table_values %>%
      mutate(monthly_mean = formatC(monthly_mean, format = "e"))
  }

  table_values <- table_values %>%
    pivot_wider(id_cols = c(parameter, unit),
                names_from = month,
                values_from = monthly_mean)

  table_values <- as.matrix(table_values)
  table_values[is.na(table_values)] <- "--"
  table_values[table_values == "NaN"] <- "--"

  caption.text = paste(year_select, "monthly summary table at the ",
                       sampling_station, "sampling location.")

  pander(table_values,
         justify = c("left", "center", rep("right", ncol(table_values)-2)),
         style = "rmarkdown",
         split.table = Inf, use.hyphenation = T,
         caption = caption.text, graph.fontsize = 9)
}


#' Determine number of decimal places
#'
#' Determine the number of decimal places of a numeric value
#'
#' @param x numeric value
#'
#' @return numeric value. The number of decimal places of the value. Does not
#'  consider 1.00 to have 2 decimal places but 1
decimal_places <- function(x) {

  if (is.na(x)) {
    return(NA)
  } else {
    if ((x %% 1) != 0) {
      dec <- nchar(strsplit(as.character(x), ".", fixed = TRUE)[[1]][[2]])
      if (dec > 5) {
        # Sometimes there are issues with decimal places (ex R reads in 16 dec
        # places). This makes sure that we don't have a ridiculous number of decimal
        # places in final report
        dec <- 2
      }
    } else {
      return(0)
    }
  }

}

#' Build table with historical values
#'
#' @inheritParams summarize_monthly_table
#' @param percentiles percentiles of interest
#' @param by_month group by month (T) or by year (F)
#' @param month_select month of interest
#'
#' @return dataframe
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter left_join rowwise mutate_at
#' @importFrom pander pander
#' @importFrom lubridate month
#' @importFrom tidyselect matches
group_table_historical <- function(df = "", year_select = "", grouping = "",
                                   sampling_station = "", percentiles = "",
                                   by_month = T, month_select = NULL){

  df <- df %>%
    filter(parm_tag == grouping & station == sampling_station) %>%
    mutate(result = case_when(result == -999999 ~ NA,
                              TRUE ~ result))

  # These parameters do not contain numeric values that
  if (grouping == "operations") {
    df <- df %>%
      filter(!grepl("ON", parameter),
             !grepl("OFF", parameter),
             !grepl("Intake", parameter))
  }

  df_round <- df %>%
    rowwise() %>%
    mutate(decimal_places = decimal_places(result)) %>%
    group_by(parameter, station, unit, parm_tag) %>%
    summarise(round_val = max(decimal_places, na.rm = TRUE))

  # monthly mean
  month_values <- summarize_monthly(labdat = df) %>%
    left_join(df_round) %>%
    rowwise() %>%
    mutate(monthly_mean = case_when(!is.infinite(round_val) ~
                                      round(monthly_mean, round_val),
                                    TRUE ~
                                      monthly_mean)) %>%
    select(month, parameter, unit, monthly_mean)


  # historical extremes
  hist_ext <- historical_extremes(df = df, grouping = grouping,
                                  sampling_station = sampling_station,
                                  by_month = by_month) %>%
    left_join(df_round) %>%
    rowwise() %>%
    mutate_at(c("Min", "Max"),
              ~ case_when(!is.infinite(round_val) ~ round(.x, round_val),
                                       TRUE ~ Min))


  # historical percentiles
  hist_per <- historical_percentiles(df = df, grouping = grouping,
                                     sampling_station = sampling_station,
                                     percentiles = percentiles,
                                     by_month = by_month) %>%
    left_join(df_round) %>%
    mutate_at(vars(matches("%")),
              ~ case_when(!is.infinite(round_val) ~ round(.x, round_val),
                          TRUE ~ .x))


  # historical basics
  hist_bas <- historical_basics(df = df, grouping = grouping,
                                sampling_station = sampling_station,
                                by_month = by_month) %>%
    left_join(df_round) %>%
    mutate_at(c("Mean", "SD", "% CV"),
              ~ case_when(!is.infinite(round_val) ~ round(.x, round_val),
                          TRUE ~ .x))


  merged_values <- left_join(hist_bas, hist_ext) %>%
    left_join(hist_per) %>%
    select(station:N, Mean:"% CV", Min:Max, "10%":"90%")

  if(by_month == F){
    caption.text = paste0("Overall historical summary (", min(df$year), "-",
                          max(df$year), ") for ", sampling_station, " ",
                          grouping, " water quality characteristics.")
  } else {
    merged_values <- merged_values %>%
      mutate(month = factor(month, ordered = T, levels = month.abb)) %>%
      filter(month == month_select)

    caption.text = paste(sampling_station, grouping, "characterics for",
                         month_select, year_select, "with collated", month_select,
                         "historical data from", min(df$year), "-",
                         max(df$year), ".")
  }

  if (grouping == "biological") {
    merged_values <- merged_values %>%
      mutate_at(c("Mean", "SD", "% CV", "Min", "Max",
                  "10%", "25%", "50%", "75%", "90%"),
                ~ formatC(.x, format = "e"))
  }


  merged_values <- merged_values %>%
    mutate_at(c("Mean", "SD", "% CV", "Min", "Max",
                "10%", "25%", "50%", "75%", "90%"),
              ~ as.character(.x),
              c("Mean", "SD", "% CV", "Min", "Max",
                "10%", "25%", "50%", "75%", "90%"),
              ~ case_when(gre ~ round(.x, round_val),
                          TRUE ~ .x))


  replace_list <- c("NA","NaN","-Inf","Inf")

  ## stopiing::need to replace values
  for(i in 1:length(replace_list)){
    merged_values[merged_values == replace_list[i]] <- "--"
  }

  pander(merged_values[, c(2,3,5:ncol(merged_values))],
         justify = c("left", "center", rep("right", ncol(merged_values) - 4)),
         style = "rmarkdown", split.table = Inf,
         caption = caption.text, graph.fontsize = 9)

}




