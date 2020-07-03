#' Build monthly summary table
#'
#' @param df data for table
#' @param grouping data grouping
#' @param sampling_station sampling location
#' @param year_select year of interest
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


  table.values[,c(3:ncol(table.values))] <-
    signif(table.values[ , c(3:ncol(table.values))], digits = 3)
  table.values <- as.matrix(table.values)
  table.values[is.na(table.values)] <- "--"

  caption.text = paste(year_select, "monthly summary table at the ",
                       sampling_station, "location.")
  pander(table.values, justify = c("left", rep("center",
                                               ncol(table.values)-1)),
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
#' @return
#' @export
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
                                  sampling_station = sampling_station,
                                  by_month = by_month)
  hist_per <- historical_percentiles(df = df, grouping = grouping,
                                     sampling_station = sampling_station,
                                     percentiles = percentiles,
                                     by_month = by_month)
  hist_bas <- historical_basics(df = df, grouping = grouping,
                                sampling_station = sampling_station,
                                by_month = by_month)


  if(by_month == T) {
    hist.values <- left_join(hist_bas, hist_ext) %>%
      left_join(hist_per)
    hist.values$month <- factor(hist.values$month, ordered = T, levels = month.abb)
    hist.values <- hist.values[hist.values$month == month_select,]
    table.values <- left_join(month_values, hist.values)
    table.values <- as.data.frame(table.values)
    table.values[,c(7:ncol(table.values))] <-
      signif(table.values[,c(7:ncol(table.values))], digits = 2)
    table.values <- as.matrix(table.values)
    table.values[is.na(table.values)] <- "--"

    caption.text = paste(sampling_station, grouping, "characterics for",
                         month_select, year_select, "with collated", month_select,
                         "historical data from", min(df$year), "-",
                         max(df$year), ".")

    pander(table.values[, c(6, 2, 7:ncol(table.values))],
           justify = c("left", "center", rep("right", ncol(table.values) - 6)),
           style = "rmarkdown", split.table = Inf,
           caption = caption.text, graph.fontsize = 9)

  } else {

    table.values <- left_join(hist_bas, hist_ext) %>% left_join(hist_per)

    table.values[,c(5:ncol(table.values))] <-
      signif(table.values[,c(5:ncol(table.values))], digits = 2)
    table.values <- as.matrix(table.values)
    table.values[is.na(table.values)] <- "--"

    caption.text = paste0("Overall historical summary (", min(df$year), "-",
                          max(df$year), ") for ", sampling_station, " ",
                          grouping, " water quality characteristics.")

    pander(table.values[,2:ncol(table.values)],
           justify = c("left", "center", rep("right",ncol(table.values) - 3)),
           style = "rmarkdown", split.table = Inf,
           caption = caption.text, graph.fontsize = 9)

  }

}




