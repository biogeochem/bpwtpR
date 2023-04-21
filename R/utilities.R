#' Calculate percent yield
#'
#' Calculates the percent yield, equal to (pre-post)/pre*100
#'
#' @param pre numeric value. First measurement
#' @param post numeric value. Last measurement
#'
#' @return numeric value. Calculated percent yield
percent_yield <- function(pre, post){
  yield = ((pre - post)/ pre) * 100
}

#' Determine which calculated values will are valid
#'
#' Classifies values as NA, NaN, or 999 depending on whether or not the measured
#' values required for that calculation exist for none, some, or all of the
#' parameters.
#'
#' @param df dataframe containing measured values required for a specific calculation
#' @param col1 first parameter required for the calculation
#' @param col2 last parameter required for the calculation
#' @param include_between boolean value, defaults to TRUE
#'  Do there exist columns between col1 and col2 that must be examined for the
#'  existence of valid numbers?
#'  If FALSE, no columns other than col1 and col2 must be examined.
#' @param col_name string used to assign a name to the column that stores the
#'  result of `determine_NA_NC()`. If not specified, column is given the name
#'  `result`
#'
#' @return A dataframe identical to the input dataframe with one additional
#'  column containing the result of `determine_NA_NC()` and named using
#'  col_name. Values are assigned as follows:
#'  * When all of the parameters required for the calculation are missing
#'  values, assign NA
#'  * When one or more (but not all) of the parameters required for the
#'  calculation are missing values, assign NaN
#'  * When all of the parameters required for the calculation are present
#'  values, assign 999 as a placeholder to indicate that a value should be
#'  calculated
determine_NA_NC <- function(df, col1, col2, include_between = F, col_name) {

  if (isTRUE(include_between)) {
    df <- df %>%
      # If all of the parameters were not entered (all NA), calculated value = NA
      mutate(result = ifelse(apply(.[, c(col1:col2)], 1, function(x) all(is.na(x))),
                             NA,
                             # If at least 1 of the parameters was not entered (is NA), calculated value = NaN
                             ifelse(!complete.cases(.[, c(col1:col2)]),
                                    NaN,
                                    # Using 999 as a placeholder to indicate that all necessary parameters were entered
                                    # and we will therefore calculate the value
                                    999)),
             result = as.numeric(result))
  } else {
    df <- df %>%
      # If all of the parameters were not entered (all NA), calculated value = NA
      mutate(result = ifelse(apply(.[, c(col1, col2)], 1, function(x) all(is.na(x))),
                             NA,
                             # If at least 1 of the parameters was not entered (is NA), calculated value = NaN
                             ifelse(!complete.cases(.[, c(col1, col2)]),
                                    NaN,
                                    # Using 999 as a placeholder to indicate that all necessary parameters were entered
                                    # and we will therefore calculate the value
                                    999)),
             result = as.numeric(result))
  }

  if(!missing(col_name)) {
    colnames(df)[colnames(df) == "result"] <- col_name
  }

  return(df)

}

#' Add columns
#'
#' Add columns that are required for function processing in the case that
#' required columns are missing from the input dataframe.
#'
#' @param df dataframe
#' @param cols A vector containing the desired columns in the desired order,
#'  setup as c(colname = NA_real_, colname = NA_real_, ...)
#'
#' @return Dataframe with desired columns in the desired order
handle_missing_cols <- function(df, cols) {

  df <- df %>%
    # For cases where parameters are missing and df is therefore missing columns
    # from `cols`, add in those columns (fill with NA)
    add_column(!!!cols[setdiff(names(cols), names(.))]) %>%
    # If a column was added, columns must be reordered
    select(rownames(as.data.frame(cols)))

  return(df)

}


#' Calculate vector of desired columns
#'
#' Create a numeric vector to store the names of the columns required for the
#'  calculation of interest. Used to later reorganize data and to add columns
#'  that are missing from a dataset but that are required for a calculation.
#'
#' @param parms string vector. The names of the columns specific to the
#'  calculation of interest
#'
#' @return string vector. The names of all the columns of interest for a
#'  calculation of interest (including datasheet, sheet year, station, date, and
#'  whatever other columns are specific to the calculation of interest)
det_cols <- function(parms) {

  cols_ds_to_dt <- c("datasheet", "sheet_year", "station", "date_ymd")

  cols <- rep.int(NA_real_, length(parms) + length(cols_ds_to_dt))
  names(cols) <- c(cols_ds_to_dt, parms)

  return(cols)

}
