#' Build monthly summary table
#'
#' @param df data for table
#' @param grouping data grouping
#' @param sampling_station sampling location
#' @param year_select year of interest
#'
#' @importFrom pander pander
#' @importFrom lubridate year
#' @importFrom dplyr filter select
#' @importFrom tidyr pivot_wider
#' @importFrom magrittr %>%
#'
#' @return
#' @export
#'
#' @examples
#' #
summarize_monthly_table <- function(df = "",
                                    grouping = c("operation","physical",
                                                 "majorConstituents", "traceConstituents",
                                                 "THM", "biological","bacteriological"),
                                    sampling_station = "", year_select = NULL){

  if(is.null(year_select)){
    year_select = year(Sys.Date())
  }

  df <- df %>%
    filter(parm_tag == grouping & station == sampling_station,
           year(datetime_ymd.hms) == year_select)

  table_values <- summarize_monthly(labdat = df)

  if(grouping == "biological"){
    num_format = "e"
  } else {
    num_format = "f"
  }


  parameter_digits <- read.csv("./data/parameter_digits.csv", fileEncoding = "ISO-8859-1")

  table_values <- table_values %>%
    select(month, parameter, unit, `Monthly Mean`) %>%
    left_join(parameter_digits)

  # correct precision
  for(i in 1:nrow(table_values)){
    table_values$format_value[i] <- formatC(table_values$`Monthly Mean`[i],
                                            digits = table_values$digits[i],
                                            format = num_format)
  }

  table_values <- table_values %>%
    pivot_wider(id_cols = c(parameter, unit), names_from = month, values_from = format_value)


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


#' Build table with historical values
#'
#' @param df dataframe
#' @param year_select year of interest
#' @param grouping operational grouping
#' @param sampling_station sampling location
#' @param percentiles percentiles of interest
#' @param by_month group by month (T) or by year (F)
#' @param month_select month of interest
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter left_join
#' @importFrom pander pander
#' @importFrom lubridate month
#'
#' @return
#' @export
group_table_historical <- function(df = "", year_select = "", grouping = "",
                                   sampling_station = "", percentiles = "",
                                   by_month = T, month_select = NULL){

  parameter_digits <- read.csv("./data/parameter_digits.csv", fileEncoding = "ISO-8859-1")

  if(grouping == "biological"){
    num_format = "e"
  } else {
    num_format = "f"
  }


  df <- df %>%
    filter(parm_tag == grouping & station == sampling_station)


  # monthly mean
  month_values <- summarize_monthly(labdat = df) %>%
    filter(month == month_select, year == year_select) %>%
    left_join(parameter_digits)

  for(i in 1:nrow(month_values)){
    month_values$`Monthly Mean`[i] <- formatC(month_values$`Monthly Mean`[i],
                                              digits = month_values$digits[i],
                                              format = num_format)
  }


  # historical extremes
  hist_ext <- historical_extremes(df = df, grouping = grouping,
                                  sampling_station = sampling_station,
                                  by_month = by_month) %>%
    left_join(parameter_digits) %>% data.frame()

  for(i in 1:nrow(hist_ext)){
    hist_ext$Min_format[i] <- formatC(hist_ext$Min[i],
                                      digits = hist_ext$digits[i],
                                      format = num_format)
    hist_ext$Max_format[i] <- formatC(hist_ext$Max[i],
                                      digits = hist_ext$digits[i],
                                      format = num_format)
  }

  # historical percentiles
  hist_per <- historical_percentiles(df = df, grouping = grouping,
                                     sampling_station = sampling_station,
                                     percentiles = percentiles,
                                     by_month = by_month)%>%
    left_join(parameter_digits) %>% data.frame()

  for(i in 1:nrow(hist_per)){
    hist_per$X10._format[i] <- formatC(hist_per$X10.[i] ,
                                       digits = hist_per$digits[i],
                                       format = num_format)
    hist_per$X25._format[i] <- formatC(hist_per$X25.[i] ,
                                       digits = hist_per$digits[i],
                                       format = num_format)
    hist_per$X50._format[i] <- formatC(hist_per$X50.[i] ,
                                       digits = hist_per$digits[i],
                                       format = num_format)
    hist_per$X75._format[i] <- formatC(hist_per$X75.[i] ,
                                       digits = hist_per$digits[i],
                                       format = num_format)
    hist_per$X90._format[i] <- formatC(hist_per$X90.[i] ,
                                       digits = hist_per$digits[i],
                                       format = num_format)
  }

  # historical basics
  hist_bas <- historical_basics(df = df, grouping = grouping,
                                sampling_station = sampling_station,
                                by_month = by_month) %>%
    left_join(parameter_digits) %>%
    data.frame() %>%
    mutate(Mean = as.numeric(Mean),
           SD = as.numeric(SD),
           X..CV = as.numeric(X..CV))

  for(i in 1:nrow(hist_bas)){
    hist_bas$Mean_format[i] <- formatC(hist_bas$Mean[i] ,
                                       digits = hist_bas$digits[i],
                                       format = num_format)
    hist_bas$SD_format[i] <- formatC(hist_bas$SD[i],
                                     digits = hist_bas$digits[i],
                                     format = num_format)
    hist_bas$`% CV_format`[i] <- formatC(hist_bas$X..CV[i],
                                         digits = hist_bas$digits[i],
                                         format = num_format)
  }


  merged_values <- left_join(hist_bas, hist_ext) %>%
    left_join(hist_per) %>%
    select(station:N, ends_with("_format"))

  if(by_month == F){
    names(merged_values) <- c(names(merged_values)[1:4],
                              gsub("_format", "", names(merged_values)[5:9]),
                              "10%", "25%", "50%", "75%", "90%")

    caption.text = paste0("Overall historical summary (", min(df$year), "-",
                          max(df$year), ") for ", sampling_station, " ",
                          grouping, " water quality characteristics.")
  } else {

    names(merged_values) <- c(names(merged_values)[1:5],
                              gsub("_format", "", names(merged_values)[6:10]),
                              "10%", "25%", "50%", "75%", "90%")

    merged_values <- merged_values %>%
      mutate(month = factor(month, ordered = T, levels = month.abb)) %>%
      filter(month == month_select)

    caption.text = paste(sampling_station, grouping, "characterics for",
                         month_select, year_select, "with collated", month_select,
                         "historical data from", min(df$year), "-",
                         max(df$year), ".")
  }

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




