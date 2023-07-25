#' Add processed data to existing db file
#'
#' @param old_data dataframe. Output from read_db() (both db and db_dates)
#' @param new_data dataframe. Data to append to db
#' @param path_to_file string. Full path (from root directory) to the .csv
#'  database file if it exists, and desired path to a .csv database file if it
#'  does not exist
#'
#' @return result of write.table()
write_data <- function(old_data, new_data, path_to_file) {
  # write.table() allows us to append the new data, rather than re-writing all
  # of the data to the file
  if (is.null(old_data)) {
    suppressWarnings(write.table(new_data, path_to_file,
                                 append = TRUE,
                                 row.names = FALSE, col.names = TRUE,
                                 fileEncoding = "ISO-8859-1",
                                 sep = ","))
  } else {
    suppressWarnings(write.table(new_data, path_to_file,
                                 append = TRUE,
                                 row.names = FALSE, col.names = FALSE,
                                 fileEncoding = "ISO-8859-1",
                                 sep = ","))
  }
}
