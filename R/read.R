#' Read the csv containing the database
#'
#' Read the csv containing all of the processed labdat data, if it exists. If it
#' does not exist, it will later be created
#'
#' @param path_to_db_file character string. Path to the file that contains the
#'  database file, or if the file does not yet exist, the desired path to the
#'  file that will contain the database file
#'
#' @return dataframe containing the database, if it exists, or NULL if it does not
read_db <- function(path_to_db_file) {

  fpath <- file.path(path_to_db_file)

  # The DB does not yet exist
  if (file.exists(fpath) == FALSE) {
    labdat <- NULL
    print("Existing database was not input. Database will be newly created.")
  } else {
    labdat <- fpath %>%
      read.csv(fileEncoding = "ISO-8859-1") %>%
      mutate(tbl_date_ymd = as.Date(tbl_date_ymd,
                                    tryFormats = c("%Y-%m-%d", "%Y/%m/%d",
                                                   "%m/%d/%Y")))
  }

  return(labdat)

}
