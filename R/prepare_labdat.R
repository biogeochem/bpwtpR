#' Prepare lab data
#'
#' Prepare lab data for incorporation into database. Extract the data that is
#'  not yet in the database, format it, and insert into the database file.
#'
#' @param path_to_labdat_file string. Path to the lab data file to be inserted
#'  into the database file
#' @param path_to_db_file string. Path to the .csv database file if it exists,
#'  and desired path to a .csv database file if it does not exist
#' @param path_to_processed_labdat_dir string. Path to the directory in which
#'  the data that was added into the database file will be stored
#' @param path_to_parameters string. Path to the .xlsx file that contains the
#'  desired parameters and associated info (station, units, corrected units...)
#'
#' @return dataframe containing the processed lab data that has been inserted
#'  into the database file
#' @importFrom magrittr "%>%" "%T>%"
#' @importFrom tibble add_column
#' @importFrom lubridate as_datetime year month day yday week
#' @importFrom dplyr case_when distinct filter first mutate rename row_number
#'  select mutate_if left_join group_by summarise arrange bind_rows last
#' @importFrom stats complete.cases
#' @importFrom janitor excel_numeric_to_date
#' @importFrom tidyr pivot_longer replace_na pivot_wider
#' @importFrom utils read.table read.csv write.table write.csv
#' @importFrom readxl read_excel excel_sheets read_xlsx
#' @importFrom data.table setnames
#' @importFrom stringr str_remove str_extract str_split
#' @importFrom stats sd
#' @importFrom tidyselect all_of starts_with everything contains
#' @importFrom rlang .data
#' @importFrom cellranger cell_limits
#'
prepare_labdat <- function(path_to_labdat_file,
                           path_to_db_file,
                           path_to_processed_labdat_dir,
                           path_to_parameters) {

  file_sheet_year <- str_extract(last(unlist(str_split(path_to_labdat_file,
                                                       "/"))),
                                 "[12][0-9]{3}")
  print(sprintf("File Year: %s", file_sheet_year))

  # Load data ------------------------------------------------------------------
  print("Loading data...")

  old_data <- read_db(path_to_db_file)

  new_data_weekly <- scrape_labdatxls(path_to_labdat_file, path_to_parameters) %>%
    mutate(sheet_year = as.factor(file_sheet_year))

  # One of the files has "<1 entered instead of <1 in one of the cells
  position_double_quotes <- which(grepl("\"<1", new_data_weekly$result))
  new_data_weekly$result[position_double_quotes] <-
    str_remove(new_data_weekly$result[position_double_quotes], pattern = "\"")

  # WTP DOC Profile sheet exists only in some sheets from 2001 onwards
  if ("WTP DOC Profile" %in% excel_sheets(path_to_labdat_file)) {
    new_data_doc <- scrape_docprofiles(path_to_labdat_file) %>%
      mutate(sheet_year = as.factor(file_sheet_year))
  } else {
    new_data_doc <- as.data.frame(NULL)
  }

  # Determining data to add ----------------------------------------------------
  if (is.null(old_data)) {
    database_dates_weekly <- NA
    database_dates_doc    <- NA
  } else {
    database_dates_weekly <-
      unique(select(filter(old_data, tbl_datasheet != "doc_profile"), "tbl_date_ymd"))

    database_dates_doc    <-
      unique(select(filter(old_data, tbl_datasheet == "doc_profile"), "tbl_date_ymd"))
  }

  current_dates_weekly  <- unique(new_data_weekly$date_ymd)
  current_dates_doc     <- unique(new_data_doc$date_ymd)

  print("Determining data to add...")
  # Need to consider weekly dates and WTP DOC dates separately because the dates
  # do not line up

  # Some consecutive files have the same end and start date (ex: last date in
  # 2019 file is 2020-01-06, first date in 2020 file is 2020-01-06). Only of
  # interest when a file is being read in for the first time and when there
  # exists database data (ie file is not empty)
  if (!is.null(old_data)) {
    if (!file_sheet_year %in% old_data$tbl_sheet_year) {
      if (last(database_dates_weekly)$tbl_date_ymd == current_dates_weekly[1]) {
        database_dates_weekly <-
          as.data.frame(database_dates_weekly[1:nrow(database_dates_weekly)-1,])
      }
    }
  }

  new_dates_weekly <- current_dates_weekly[which(!(current_dates_weekly %in%
                                                     database_dates_weekly[[1]]))]
  # We don't want to read in dates for which measurements have not yet been collected
  new_dates_weekly <- new_dates_weekly[new_dates_weekly < Sys.Date()]

  # If all of new_data_weekly is in old_data, new_data_weekly becomes a
  # data.frame with 0 rows
  new_data_weekly  <- filter(new_data_weekly, date_ymd %in% new_dates_weekly)

  new_dates_doc    <- current_dates_doc[which(!(current_dates_doc %in%
                                                  database_dates_doc[[1]]))]
  # If all of new_dates_doc is in old_data, new_dates_doc stays a
  # data.frame with 0 rows
  if (nrow(new_data_doc) != 0) {
    new_data_doc   <- filter(new_data_doc, date_ymd %in% new_dates_doc)
  }

  new_data <- rbind(new_data_weekly, new_data_doc)

  if (nrow(new_data) == 0) {
    stop("No data to add", call. = T)
  } else {
    print("There exists data to add to the database. Processing...")
  }

  # Processing data to add -----------------------------------------------------
  print("Updating parameters...")
  new_data <- update_parameters(new_data, file_sheet_year, path_to_parameters)

  print("Correcting detection limit, removing values calculated in-sheet...")

  new_data <- new_data %T>%
    # Warning arises when script converts certain results to numeric format
    # (ex "--" as an indicator for an incalculable value), but
    # suppressWarnings is not for use in pipes. {options} is the alternative
    {options(warn = -1)} %>%
    # Create a new column for the original result and force character in case of
    # non numeric values (ex: "uncalculable", "<1", "--", ...)
    mutate(result_org  = as.character(result),
           result_flag = "",
           result      = as.numeric(result)) %T>%
    {options(warn = 0)} %>%
    round_values() %>%
    replace_dl() %>%
    convert_biocounts() %>%
    convert_UV254()

  # DOC profile values and LSI RTW values are calculated_insheet but are NOT
  # recalculated by script, and are left in the DB as-is
  new_data_calc_insheet <- new_data %>%
    filter(parm_eval == "calculated_insheet" & datasheet != "doc_profile" &
             parameter != "Langelier Index (RTW)") %>%
    # To keep result_org to later combine with the results calculated by script
    select(!c("result", "parm_eval"))

  print("Applying calculations...")
  # join() and summarise() functions print messages to the console that we don't
  # need to see
  new_data_calcs <- suppressMessages(apply_calculations(new_data %>%
                                                          filter(datasheet != "doc_profile"),
                                                        file_sheet_year))

  # Combine values calculated in script with those calculated in sheet ---------

  # Script calculated values can be compared with those recorded in labdat files
  new_data_calcs_not_combo <- new_data_calcs %>%
    filter(datasheet != "Combined Sheets") %T>%
    {options(warn = -1)} %>%
    left_join(new_data_calc_insheet) %T>%
    {options(warn = 0)} %>%
    select("datasheet":"parm_unit", "parm_eval", "parm_tag",
           "result", "result_org", "result_flag")

  new_data_calcs_combo <- new_data_calcs %>%
    filter(datasheet == "Combined Sheets") %T>%
    {options(warn = -1)} %>%
    # Different join() statement needed compared to above because datasheet and
    # station values for these values differ from those listed in the yearly
    # labdat files (here: "Combined", in sheet = "Raw" | "Clearwell")
    left_join(select(new_data_calc_insheet, !c(datasheet, station))) %T>%
    {options(warn = 0)} %>%
    select(datasheet:parm_unit, parm_eval, parm_tag,
           result, result_org, result_flag)

  new_data_calcs <- rbind(new_data_calcs_not_combo, new_data_calcs_combo) %>%
    mutate(result_flag = replace_na(result_flag, ""))

  # Combining measured and calculated data -------------------------------------
  print("Appending new data...")
  new_data <- new_data %>%
    filter(!(parm_eval == "calculated_insheet" & datasheet != "doc_profile" &
               parameter != "Langelier Index (RTW)")) %>%
    rbind(new_data_calcs)

  new_data <- new_data %>%
    mutate(year  = lubridate::year(date_ymd),
           month = lubridate::month(date_ymd, label = TRUE),
           day   = lubridate::day(date_ymd),
           day_num   = lubridate::yday(date_ymd),
           week_num  = lubridate::week(date_ymd)) %>%
    select(datasheet:date_ymd, year:week_num, parameter:result_flag) %>%
    mutate_if(is.character, as.factor)

  # Putting new data in database -----------------------------------------------
  print("Updating file...")

  # Saving as csv converts NaN to NA. Setting NaN as -999999 for now in case if
  # Stephen (the DB expert) has some work around to this conversion for Access
  new_data <- new_data %>%
    mutate(result = replace(result, is.nan(result), -999999))

  # Blair would like DB colnames to start with "tbl_"
  colnames(new_data) <- paste("tbl", colnames(new_data), sep = "_")

  write.csv(new_data,
            path_to_processed_labdat_dir,
            row.names = FALSE,
            fileEncoding = "ISO-8859-1")

  # write.table() allows us to append the new data, rather than re-writing all
  # of the data to the file
  if (is.null(old_data)) {
    suppressWarnings(write.table(new_data, path_to_db_file,
                                 append = TRUE,
                                 row.names = FALSE, col.names = TRUE,
                                 fileEncoding = "ISO-8859-1",
                                 sep = ","))
  } else {
    suppressWarnings(write.table(new_data, path_to_db_file,
                                 append = TRUE,
                                 row.names = FALSE, col.names = FALSE,
                                 fileEncoding = "ISO-8859-1",
                                 sep = ","))
  }

  return(new_data)

  print("Database updated.")

}
