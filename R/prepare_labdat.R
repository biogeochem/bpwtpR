#' Prepare lab data
#'
#' Prepare lab data for incorporation into database. Extract the data that is
#'  not yet in the database, format it, and insert into the database file.
#'
#' @param path_to_labdat_file string. Full path (from root directory) to the lab
#'  data file to be inserted into the database file
#' @param path_to_db_file string. Full path (from root directory) to the .csv
#'  database file if it exists, and desired path to a .csv database file if it
#'  does not exist
#' @param path_to_db_dates_file string. Full path (from root directory) to the .csv
#'  database dates file if it exists, and desired path to a .csv database dates file if it
#'  does not exist
#' @param path_to_parameters string. Full path (from root directory) to the
#'  .xlsx file that contains the desired parameters and associated info
#'  (station, units, corrected units...)
#' @param file_sheet_year numeric. The year of the labdata file of interest
#'
#' @return list of length 3.
#'  1. boolean. Are the provided database files empty or not?
#'  2. dataframe containing the processed lab data (not dates)
#'  3. dataframe containing the processed lab data (dates)
#'  into the database file
#' @importFrom magrittr "%>%" "%T>%"
#' @importFrom tibble add_column
#' @importFrom lubridate as_datetime year month day yday week
#' @importFrom dplyr case_when distinct filter first mutate rename row_number
#'  select mutate_if left_join group_by summarise arrange bind_rows last nth
#'  ungroup full_join
#' @importFrom stats complete.cases
#' @importFrom janitor excel_numeric_to_date
#' @importFrom tidyr pivot_longer replace_na pivot_wider
#' @importFrom utils read.table read.csv write.table write.csv tail
#' @importFrom readxl read_excel excel_sheets read_xlsx
#' @importFrom data.table setnames as.data.table fsetdiff
#' @importFrom stringr str_remove str_extract str_split str_subset str_detect
#' @importFrom stats sd
#' @importFrom tidyselect all_of starts_with everything contains
#' @importFrom rlang .data is_empty
#' @importFrom cellranger cell_limits
#' @importFrom zoo na.locf
#' @importFrom writexl write_xlsx
#' @importFrom purrr map_df
#'
prepare_labdat <- function(path_to_labdat_file,
                           path_to_db_file,
                           path_to_db_dates_file,
                           path_to_parameters,
                           file_sheet_year) {

  # Have issues occasionally with R giving values in scientific notation. Not
  # desired for the sake of consistency
  options(scipen = 999)

  labdat_parameters <- path_to_parameters %>%
    read_parameters() %>%
    check_parameters_setup()

  print(sprintf("File Year: %s", file_sheet_year))

  # Load data ------------------------------------------------------------------
  print("Loading data...")

  old_data <- read_db(path_to_db_file) %>%
    rbind(read_db(path_to_db_dates_file))

  new_data_weekly <- path_to_labdat_file %>%
    read_weekly() %>%
    scrape_labdatxls(labdat_parameters) %>%
    check_scraped_data(labdat_parameters) %>%
    mutate(sheet_year = as.factor(file_sheet_year))

  # One of the files has "<1 entered instead of <1 in one of the cells, which
  # messes with all future use of the data
  position_double_quotes <- which(grepl("\"<1", new_data_weekly$result))
  new_data_weekly$result[position_double_quotes] <-
    str_remove(new_data_weekly$result[position_double_quotes], pattern = "\"")

  # WTP DOC Profile sheet exists only in some sheets from 2001 onwards
  if ("WTP DOC Profile" %in% excel_sheets(path_to_labdat_file)) {
    new_data_doc <- path_to_labdat_file %>%
      read_doc() %>%
      scrape_docprofiles(labdat_parameters) %>%
      check_scraped_data(labdat_parameters) %>%
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
      unique(select(filter(old_data, tbl_datasheet != "DOCProfile"), "tbl_date_ymd"))

    database_dates_doc    <-
      unique(select(filter(old_data, tbl_datasheet == "DOCProfile"), "tbl_date_ymd"))
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
    stop("Database file is already updated. There is no data to add. Cancelling update.",
         call. = FALSE)
  } else {
    print("There exists data to add to the database. Processing...")
  }

  # Processing data to add -----------------------------------------------------
  print("Updating parameters...")
  new_data <- update_parameters(new_data, file_sheet_year, labdat_parameters)

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

  print("Applying calculations...")
  # join() and summarise() functions print messages to the console that we don't
  # need to see
  new_data_calcs <- suppressMessages(apply_calculations(new_data %>%
                                                          filter(datasheet != "DOCProfile"),
                                                        file_sheet_year))

  new_data <- new_data %>%
    full_join(new_data_calcs, by = c("datasheet", "sheet_year", "station",
                                     "date_ymd", "parameter", "unit",
                                     "parm_tag")) %>%
    mutate(result    = case_when(!is.na(parm_eval.y) ~ result.y,
                                 TRUE ~ result.x),
           parm_eval = case_when(!is.na(parm_eval.y) ~ parm_eval.y,
                                 TRUE ~ parm_eval.x)) %>%
    mutate(year  = year(date_ymd),
           month = month(date_ymd, label = TRUE),
           day   = day(date_ymd),
           day_num   = yday(date_ymd),
           week_num  = week(date_ymd)) %>%
    select(datasheet, sheet_year, station, date_ymd,
           parameter, unit,	parm_eval, parm_tag,
           result, result_org, result_flag) %>%
    # Blair does not want any of the NA data in the final DB
    filter(!is.na(result)) %>%
    # in-script calculations yield NA result_org and _flag, but should be ""
    # to match with the sheet values
    mutate(result_org = ifelse(is.na(result_org), "", result_org),
           result_flag = ifelse(is.na(result_flag), "", result_flag))

  # Blair would like DB colnames to start with "tbl_"
  colnames(new_data) <- paste("tbl", colnames(new_data), sep = "_")

  # We must split all data away from dates data because R will store all the result
  # values as numeric, thereby losing the actual date for date data
  new_data_dates <- new_data %>%
    filter(tbl_unit == "ymd") %>%
    mutate(tbl_result = excel_numeric_to_date(tbl_result),
           tbl_result_org = excel_numeric_to_date(as.numeric(tbl_result_org)))

  new_data <- new_data %>%
    filter(tbl_unit != "ymd")

  print("Updated database available for download.")

  return(list(is.null(old_data), new_data, new_data_dates))

}
