#' Build monthly summary table
#'
#' @param df data for table
#' @param grouping data grouping
#' @param sampling_station
#' @param year_select
#'
#' @importFrom pander pander
#'
#' @return
#' @export
#'
#' @examples
#' #
summarize_monthly_table <- function(df = "", grouping = "",
                                    sampling_station = "", year_select = NULL){

  if(is.null(year_select)){
    year_select = year(Sys.Date())
  }

  df <- df %>%
    filter(parm_tag == grouping & station == sampling_station,
           year(datetime_ymd.hms) == year_select)

  table.values <- summarize_monthly(labdat = df)

  table.values <- table.values %>%
    select(month, parameter, unit, `Monthly Mean`) %>%
    spread(month, `Monthly Mean`)

  caption.text = paste(year_select, "monthly summary table at the ",
                       sampling_station, "location.")
  pander(table.values, justify = c("left", rep("center",
                                               ncol(table.values)-1)),
         style = "rmarkdown",
         split.table = Inf, use.hyphenation = T,
         caption = caption.text, graph.fontsize = 9, digits = 2)
}


#' Build table with historical values
#'
#' @param df
#' @param year_select
#' @param grouping
#' @param sampling_station
#' @param percentiles
#' @param by_month
#' @param month_select
#'
#' @return
#' @export
#'
#' @examples
group_table_historical <- function(df = "", year_select = "", grouping = "",
                                   sampling_station = "", percentiles = "",
                                   by_month = T, month_select = NULL){
  if(is.null(month_select)){
    month_select = month(Sys.Date(), label = T, abbr = T)}

  df <- df %>%
    filter(parm_tag == grouping & station == sampling_station)

  month_values <- summarize_monthly(labdat = df) %>%
    filter(month == month_select, year == year_select)

  hist_ext <- historical_extremes(df = df, grouping = grouping,
                                  sampling_station = sampling_station, by_month = by_month)
  hist_per <- historical_percentiles(df = df, grouping = grouping,
                                     sampling_station = sampling_station,
                                     percentiles = percentiles, by_month = by_month)
  hist_bas <- historical_basics(df = df, grouping = grouping,
                                sampling_station = sampling_station, by_month = by_month)

  if(by_month == T) {
    hist.values <- left_join(hist_bas, hist_ext) %>%
      left_join(hist_per) %>%
      filter(month == month_select)
    table.values <- left_join(month_values, hist.values)
    table.values <- as.data.frame(table.values)

    caption.text = paste(sampling_station, grouping, "characterics for",
                         month_select, "historical data.")

    pander(table.values[,5:ncol(table.values)],
           justify = c("left", rep("center",ncol(table.values)-5)),
           style = "rmarkdown", split.table = Inf,
           caption = caption.text, graph.fontsize = 9, digits = 2)

  } else {

    table.values <- left_join(hist_bas, hist_ext) %>% left_join(hist_per)
    caption.text = paste("Complete historical summary for the",
                         sampling_station, grouping, "characteristics.")

    pander(table.values[,2:ncol(table.values)],
           justify = c("left", rep("centre",ncol(table.values)-2)),
           style = "rmarkdown", split.table = Inf,
           caption = caption.text, graph.fontsize = 9, digits = 2)

  }

}




