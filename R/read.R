#' Read the csv containing the database
#'
#' Read the csv containing all of the processed labdat data, if it exists. If it
#' does not exist, it will later be created
#'
#' @inheritParams prepare_labdat
#'
#' @return dataframe containing the database, if it exists, or NULL if it does not
read_db <- function(path_to_db_file) {

  options(scipen = 999)

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

#' Read Weekly Data sheet of lab data workbook
#'
#' @inheritParams prepare_labdat
#' @param skip_num integer. Number of rows to skip over when reading in data.
#'  Default = 7
#'
#' @return dataframe containing the raw weekly data
read_weekly <- function(path_to_labdat_file, skip_num = 7) {
  # Note that if the number of rows to skip changes, the skip value here must
  # be changed as well as the skip value and addition used to calculate the
  # row number within check_missing_params in utilities.R
  spreadsheet <- suppressMessages(read_excel(path_to_labdat_file,
                                             sheet = 1,
                                             col_names = TRUE, col_types = NULL,
                                             skip = skip_num))
}

#' Read WTP DOC Data sheet of lab data workbook
#'
#' @inheritParams prepare_labdat
#'
#' @return dataframe containing the DOC data with edited column headers
read_doc <- function(path_to_labdat_file) {
  doc_data <- suppressMessages(read_excel(path_to_labdat_file,
                                          sheet = "WTP DOC Profile"))

  # Read in the sheet again as text to be able to alter the excess header names
  doc_data <- suppressMessages(read_excel(path_to_labdat_file,
                                          sheet = "WTP DOC Profile",
                                          col_types = rep("text",
                                                          times = ncol(doc_data))))

  # Because of the merged cells and current header setup, read_excel yields many
  # empty header cells. This fills them as they should be filled
  doc_data[1,] <- t(as.data.frame((na.locf(as.character(doc_data[1,]),
                                           na.rm = FALSE))))

  # To check if there is an extra header row or not (3 header rows from 2021
  # onwards VS 2 header rows from 2004 to 2020)
  date_start <- which(sapply(doc_data,
                             function(x) grepl("date", x, ignore.case = TRUE)),
                      arr.ind = TRUE)

  # Either we are expecting an extra header row under the row that contains
  # "Sample Date" or the data starts immediately underneath
  if (is.na(excel_numeric_to_date(as.numeric(doc_data[date_start[1,1]+1,
                                                      date_start[1,2]])))) {

    # We only want to extend the cell values for Coagulant Dose values. All other
    # cells should contain the required info
    coag_dose_cells <- which(agrepl("Coagulant Dosage", doc_data[1,]))

    doc_data[2, min(coag_dose_cells):max(coag_dose_cells)] <-
      t(as.data.frame((na.locf(
        as.character(doc_data[2,min(coag_dose_cells):max(coag_dose_cells)]),
        na.rm = FALSE))))

    names(doc_data) <- mapply(create_doc_colnames,
                              doc_data[1,], doc_data[2,], doc_data[3,])

    # Remove all unnecessary rows and cols - start dataframe at the correct cell
    doc_data <- doc_data[date_start[1,1]+2:nrow(doc_data),
                         date_start[1,2]:ncol(doc_data)]

  } else {
    # We consider that only the first two rows are headers
    names(doc_data) <- mapply(create_doc_colnames,
                              doc_data[1,], doc_data[2,], NA)

    # Remove all unnecessary rows and cols - start dataframe at the correct cell
    doc_data <- doc_data[date_start[1,1]+1:nrow(doc_data),
                         date_start[1,2]:ncol(doc_data)]
  }

  # There is a typo in WTP DOC profile, where a date is entered as "29Jan-18"
  if (any(doc_data$`Sample Date` == "29Jan-18", na.rm = TRUE)) {
    error_spot <- which(doc_data$`Sample Date` == "29Jan-18")
    doc_data$`Sample Date`[error_spot] <- "43129"
  }

  return(doc_data)
}

#' Create DOC colnames
#'
#' Use all available header row info to create full column names for DOC profile
#' data
#'
#' @param val1 string. Value in 1st header row
#' @param val2 string. Value in 2nd header row
#' @param val3 string. Value in 3rd header row, if there are three header rows.
#'  NA otherwise
#'
#' @return string. Column name
create_doc_colnames <- function(val1, val2, val3) {
  colname <- c(val1, val2, val3)
  colname <- colname[!is.na(colname)]
  colname <- paste(colname, collapse = " ")
}

#' Read parameters.xlsx file
#'
#' @inheritParams prepare_labdat
#'
#' @return dataframe containing the parameters info
read_parameters <- function(path_to_parameters) {
  labdat_parameters <- read_xlsx(path_to_parameters) %>%
    mutate(tbl_parameter = as.character(tbl_parameter),
           tbl_unit = as.character(tbl_unit))

  # To simplify column names while the data frame is being used. Blair wanted
  # columns to start with _tbl
  colnames(labdat_parameters) <- str_remove(colnames(labdat_parameters), "tbl_")

  return(labdat_parameters)
}
