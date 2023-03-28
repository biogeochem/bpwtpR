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
                            path_to_parameters) {

  # Want files that don't start with ~ as these are not useable files
  path_to_labdat_files <- list.files(path = path_to_labdat_dir,
                                     pattern = "^[12(ROUTINE)].+xlsx",
                                     full.names = TRUE)

  map(path_to_labdat_files,
      prepare_labdat,
      path_to_db_file,
      path_to_parameters)

}

#' Update database with data contained in the specified labdat file
#'
#' @inheritParams prepare_labdat
#'
#' @return dataframe containing the processed lab data that has been inserted
#'  into the database file
#' @export
update_database <- function(path_to_labdat_file,
                            path_to_db_file) {

  prepare_labdat(path_to_labdat_file,
                 path_to_db_file,
                 path_to_parameters)

}
