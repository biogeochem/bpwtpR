#' Create database
#'
#' Access all of the lab data files contained in the specified directory.
#'  Process each of them and place them all into the specified database file
#'
#' @inheritParams prepare_labdat
#' @param path_to_labdat_dir string. Path to the directory that contains all
#'  the lab data files to be entered into the database file
#'
#' @return dataframe
#' @export
#'
#' @importFrom purrr map
create_database <- function(path_to_labdat_dir,
                            path_to_db_file,
                            path_to_parameters,
                            file_sheet_year = NULL) {

  # RShiny converts files into a Temp file so the original filename is lost.
  # The app will therefore calculate the year itself, and any user that needs
  # to use the script directly (ie filename is not lost) will have the sheet_year
  # calculated for them
  if (is.na(file_sheet_year) | is.null(file_sheet_year)) {
    file_sheet_year <- str_extract(last(unlist(str_split(path_to_labdat_file,
                                                         "/"))),
                                   "[12][0-9]{3}")
  }

  # Want files that don't start with ~ as these are not useable files
  path_to_labdat_files <- list.files(path = path_to_labdat_dir,
                                     pattern = "^[12(ROUTINE)].+xlsx",
                                     full.names = TRUE)

  map(path_to_labdat_files,
      prepare_labdat,
      path_to_db_file,
      path_to_parameters,
      file_sheet_year)

}

#' Update database with data contained in the specified labdat file
#'
#' @inheritParams prepare_labdat
#'
#' @return dataframe containing the processed lab data that has been newly
#'  inserted into the database file
#' @export
update_database <- function(path_to_labdat_file,
                            path_to_db_file,
                            path_to_parameters,
                            file_sheet_year = NULL) {

  # RShiny converts files into a Temp file so the original filename is lost.
  # The app will therefore calculate the year itself, and any user that needs
  # to use the script directly (ie filename is not lost) will have the sheet_year
  # calculated for them
  if (is.na(file_sheet_year) | is.null(file_sheet_year)) {
    file_sheet_year <- str_extract(last(unlist(str_split(path_to_labdat_file,
                                                         "/"))),
                                   "[12][0-9]{3}")
  }

  prepare_labdat(path_to_labdat_file,
                 path_to_db_file,
                 path_to_parameters,
                 file_sheet_year)

}
