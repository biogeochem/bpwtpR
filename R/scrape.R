# Notes from Megan
#
# 1. Files need to be saved as '.xlsx' for this code to work ('.XLSx' or other
#    will not work). Files in folder 'data/raw_data/yyyy_routine_labdat' match
#    the .xlsx pattern but were originally downloaded from OneDrive folder with
#    '.XLSx' extension.
#
# 2. According to as.Date() documentation (?as.Date), Excel is said to use
#    1900-01-01 as day 1 (Windows default), but this is complicated by Excel
#    incorrectly treating 1900 as a leap year. So for dates (post-1901) from
#    Windows Excel, use e.g., as.Date(29220, origin = "1899-12-30"), which
#    converts 29220 to its correct date (1979-12-31).
#       i. read_excel() converts sampling dates into numeric values (e.g.,
#          29220 for 1979-12-31) so they needed to be converted back to ISO
#          dates via the janitor package.

#' Scrape data from BPWTP routine lab data
#'
#' Read in all data contained in the Weekly (1st) sheet of the lab data file.
#'
#' @param weekly_data dataframe. Raw weekly data outputted from read_weekly()
#' @param labdat_parameters dataframe. Output from check_parameters_content
#'
#' @return data frame of all the data contained in the Weekly lab data file
scrape_labdatxls <- function(weekly_data, labdat_parameters) {

  if (colnames(weekly_data)[1] != "Parameters") {
    stop(paste("Issue with weekly data column names.",
               "Do column names start on row 8?",
               "Is the parameters column correctly named as \"Parameters\"?",
               "Check file requirements and weekly data."),
         call. = FALSE)
  }

  clearwell_start <- which(weekly_data$Parameters == "CLEAR WELL")

  if (is_empty(clearwell_start)) {
    stop(paste("Issue with the start of the Clear Well data.",
               "Is the start identified with the string \"CLEAR WELL\"?",
               "Check file requirements and weekly data."),
         call. = FALSE)
  }

  rawwater        <- scrape_rawwater(      weekly_data,
                                           clearwell_start,
                                           labdat_parameters)
  clearwell       <- scrape_clearwell(     weekly_data,
                                           clearwell_start,
                                           labdat_parameters)
  clearwell_THMs  <- scrape_clearwell_thms(weekly_data,
                                           clearwell_start,
                                           labdat_parameters)
  clearwell_al    <- scrape_clearwell_al(  weekly_data,
                                           clearwell_start,
                                           labdat_parameters)
  ion_values      <- scrape_ion_values(    weekly_data,
                                           labdat_parameters)

  new_data_weekly <- bind_rows(rawwater, clearwell, clearwell_THMs,
                               clearwell_al, ion_values)

  return(new_data_weekly)

}

#' Scrape raw water data
#'
#' Read in raw water data from the Weekly (1st) sheet of the lab data file.
#' Identify parameters to read in based on parameters.xlsx
#'
#' @param weekly_data dataframe. All data in the Weekly sheet of the lab data file
#' @param clearwell_start numeric value. The start of the Clearwell data as
#'  indicated by the phrase "CLEAR WELL" in the Parameters column of the Weekly
#'  data
#' @inheritParams scrape_labdatxls
#'
#' @return data frame of the raw water data
scrape_rawwater <- function(weekly_data, clearwell_start, labdat_parameters) {

  rw_parms_list <- labdat_parameters %>%
    filter(datasheet == "Raw") %>%
    as.data.table() %>%
    # Ion values are not read in at this point. They are read in together with
    # CW ion values using scrape_ion_values() because they are found together at
    # the bottom of the lab data sheet
    fsetdiff(filter_ions(.)) %>%
    as.data.frame()

  rw_thms <- rw_parms_list %>%
    as.data.table() %>%
    filter_thms(.) %>%
    as.data.frame()

  rawwater <- weekly_data %>%
    filter(row_number() < clearwell_start) %>%
    filter(Parameters %in% rw_parms_list$parameter) %>%
    select(!starts_with("...")) %>%
    pivot_longer(cols = -c(Parameters, Units),
                 names_to = "date_ymd", values_to = "result") %>%
    rename(parameter = Parameters, unit = Units) %>%
    mutate(date_ymd = excel_numeric_to_date(as.numeric(date_ymd)),
           # As directed by Blair. Raw water THMs are only measured at the
           # PreFM stage
           station = as.factor(case_when(parameter %in% rw_thms$parameter |
                                          grepl("PreFM", parameter, ignore.case = TRUE) ~
                                         "PreFM",
                                         grepl("Train A", parameter, ignore.case = TRUE) ~
                                         "Train A",
                                         grepl("Train B", parameter, ignore.case = TRUE) ~
                                         "Train B",
                                         TRUE ~ "Raw"))) %>%
    filter(!is.na(date_ymd)) %>%
    add_column(datasheet = "Raw") %>%
    select(datasheet, station, parameter, unit, date_ymd, result)

  return(rawwater)

}

#' Scrape Clearwell data
#'
#' Read in Clearwell data (excluding THM and Al data). Identify parameters to
#' read in based on parameters.xlsx
#'
#' @inheritParams scrape_rawwater
#'
#' @return data frame of the Clearwell data (excluding THMs and Al)
scrape_clearwell <- function(weekly_data, clearwell_start, labdat_parameters) {

  # Aluminum, THMs, and ion values are read in separately
  cw_parms_list <- labdat_parameters %>%
    filter(datasheet == "Clearwell") %>%
    as.data.table() %>%
    # Ion values are not read in at this point. They are read in together with
    # CW ion values using scrape_ion_values() because they are found together at
    # the bottom of the lab data sheet
    fsetdiff(filter_ions(.)) %>%
    fsetdiff(filter_thms(.)) %>%
    fsetdiff(filter_al(.)) %>%
    as.data.frame()

  # based on premise that the aluminum and THMs are NOT read in here
  clearwell <- weekly_data %>%
    filter(row_number() > clearwell_start) %>%
    filter(Parameters %in% cw_parms_list$parameter) %>%
    select(!starts_with("...")) %>%
    pivot_longer(cols = -c(Parameters, Units),
                 names_to = "date_ymd", values_to = "result") %>%
    rename(parameter = Parameters, unit = Units) %>%
    mutate(date_ymd = excel_numeric_to_date(as.numeric(date_ymd)),
           # These matches match those within checks.R. If one changes, change the other too
           station = case_when(grepl("PreGAC", parameter, ignore.case = TRUE)
                               ~ "PreGAC",
                               grepl("PreBAC", parameter, ignore.case = TRUE)
                               ~ "PreBAC",
                               grepl("BAC ?E", parameter, ignore.case = TRUE)
                               ~ "BAC E",
                               grepl("BAC ?M", parameter, ignore.case = TRUE)
                               ~ "BAC M",
                               grepl("MM ?F ?1", parameter, ignore.case = TRUE)
                               ~ "MMF1",
                               grepl("MM ?F ?12", parameter, ignore.case = TRUE)
                               ~ "MMF12",
                               grepl("MM ?F ?A", parameter, ignore.case = TRUE)
                               ~ "MMFA",
                               grepl("MM ?F ?L", parameter, ignore.case = TRUE)
                               ~ "MMFL",
                               grepl("Channel(?! ?[12])", parameter, ignore.case = TRUE,
                                     perl = TRUE)
                               ~ "Channel",
                               grepl("Channel ?1", parameter, ignore.case = TRUE,
                                     perl = TRUE)
                               ~ "Channel 1",
                               grepl("Channel ?2", parameter, ignore.case = TRUE)
                               ~ "Channel 2",
                               grepl("PreFM", parameter, ignore.case = TRUE)
                               ~ "PreFM",
                               grepl("(?<!Pre)FM", parameter, ignore.case = TRUE,
                                     perl = TRUE)
                               ~ "FM",
                               grepl("Removal", parameter, ignore.case = TRUE)
                               ~ "Combined Stations",
                               TRUE ~ "Clearwell"),
           station = as.factor(station)) %>%
    filter(!is.na(date_ymd)) %>%
    add_column(datasheet = "Clearwell") %>%
    select(datasheet, station, parameter, unit, date_ymd, result)

  return(clearwell)

}


#' Scrape Clearwell THM data
#'
#' Read in Clearwell THM data. Identify parameters to read in based on
#' parameters.xlsx
#'
#' @inheritParams scrape_rawwater
#'
#' @return data frame of the Clearwell THMs data
scrape_clearwell_thms <- function(weekly_data, clearwell_start, labdat_parameters) {

  # Expecting consistently 4 THM rows for each of Clearwell, Channel, PreGAC
  cw_thms_parms_list <- labdat_parameters %>%
    filter(datasheet == "Clearwell") %>%
    as.data.table() %>%
    filter_thms(.) %>%
    as.data.frame()

  clearwell_THMs <- weekly_data %>%
    filter(row_number() > clearwell_start) %>%
    select(!starts_with("...")) %>%
    filter(Parameters %in% cw_thms_parms_list$parameter) %>%
    mutate(station = case_when(row_number() >= 1 & row_number() <= 5
                               ~ "Clearwell",
                               row_number() >= 6 & row_number() <= 10
                               ~ "Channel",
                               row_number() >= 11 & row_number() <= 15
                               ~ "PreGAC"))

  if (nrow(clearwell_THMs) != 15) {
    stop(paste("Issue with Clear Well THMs.",
               "Too many rows were identified. There should be 15.",
               "Check file requirements and weekly data."),
         call. = FALSE)
  }

  clearwell_THMs <- clearwell_THMs %>%
    select(parameter = Parameters, unit = Units, station,
           everything()) %>%
    pivot_longer(cols = -c(parameter, unit, station),
                 names_to = "date_ymd", values_to = "result") %>%
    mutate(date_ymd = excel_numeric_to_date(as.numeric(date_ymd))) %>%
    filter(!is.na(date_ymd)) %>%
    add_column(datasheet = "Clearwell") %>%
    select(datasheet, station, parameter, unit, date_ymd, result)

  return(clearwell_THMs)

}


#' Scrape Clearwell aluminum data
#'
#' Read in Clearwell aluminum data. Identify parameters to read in based on
#' parameters.xlsx
#'
#' @inheritParams scrape_rawwater
#'
#' @return data frame of the Clearwell aluminum data
scrape_clearwell_al <- function(weekly_data, clearwell_start, labdat_parameters) {

  # Expecting parameters that look like:
  # "Aluminum (dissolved 0.45\u00b5)",
  # "Aluminum (total)",
  # "Aluminum (Total)",
  # "Aluminum (dissolved)",
  # "Aluminum (particulate)"
  # Using labdat_parameters instead of hard-coding to account for small typos
  cw_al_parms_list <- labdat_parameters %>%
    filter(datasheet == "Clearwell") %>%
    as.data.table() %>%
    filter_al(.) %>%
    as.data.frame()

  clearwell_Al <- weekly_data %>%
    filter(row_number() > clearwell_start) %>%
    select(!starts_with("...")) %>%
    filter(Parameters %in% cw_al_parms_list$parameter)

  if (nrow(clearwell_Al) != 7) {
    stop(paste("Issue with Clear Well Aluminum values.",
               "Too many rows were identified. There should be 7.",
               "Check file requirements and weekly data."),
         call. = FALSE)
  }

  clearwell_Al <- clearwell_Al %>%
    # The expected row setup is as follows: 3 Clearwell Al rows, 1 MMF1 Al row,
    # 1 MMF12 Al row, 2 PreGAC Al rows
    mutate(station = case_when(row_number() == 1 | row_number() == 2 | row_number() == 3
                               ~ "Clearwell",
                               row_number() == 4
                               ~ "MMF1",
                               row_number() == 5
                               ~ "MMF12",
                               row_number() == 6 | row_number() == 7
                               ~ "PreGAC")) %>%
    select(parameter = Parameters, unit = Units,
           station, everything()) %>%
    pivot_longer(cols = -c(parameter, unit, station),
                 names_to = "date_ymd", values_to = "result") %>%
    mutate(date_ymd = excel_numeric_to_date(as.numeric(date_ymd))) %>%
    filter(!is.na(date_ymd)) %>%
    # Mixed Media Filter 1 and 12 are renamed to Mixed Media Filter A and L,
    # respectively, in the 2004 sheet and onwards
    mutate(station = ifelse(station == "MMF1" &
                              year(date_ymd) >= 2004,
                            "MMFA", station),
           station = ifelse(station == "MMF12" &
                              year(date_ymd) >= 2004,
                            "MMFL", station)) %>%
    add_column(datasheet = "Clearwell") %>%
    select(datasheet, station, parameter, unit, date_ymd, result)

  return(clearwell_Al)

}

#' Scrape ion data
#'
#' Read in all ion data. Identify parameters to read in based on
#' parameters.xlsx
#'
#' @inheritParams scrape_rawwater
#'
#' @return data frame of the ion data
scrape_ion_values <- function(weekly_data, labdat_parameters) {

  weekly_data <- weekly_data %>%
    # Earlier weekly data weekly_datas have empty rows in between ion rows
    filter(!is.na(Parameters))

  ions_parms_list <- labdat_parameters %>%
    as.data.table() %>%
    filter_ions(.) %>%
    as.data.frame()

  # Find where the ion values with silica included begin (desired values)
  silica_included_start_rw     <- which(grepl("RAW.*SILICA ADDED",
                                              weekly_data$Parameters,
                                              ignore.case = TRUE))
  silica_included_start_cw     <- which(grepl("TREATED.*SILICA ADDED",
                                              weekly_data$Parameters,
                                              ignore.case = TRUE))

  if (length(silica_included_start_rw) != 1 |
      length(silica_included_start_cw) != 1) {
    stop(paste("Issue with Weekly Data ion values.",
               "Could not identify the start of either the raw or Clearwell",
               "ion values by looking for Parameters that contain the words",
               "'RAW' and `SILICA ADDED`, and 'TREATED' and 'SILICA ADDED' in",
               "the Parameters column.\nEither these phrases are missing in",
               "the weekly spreadsheet or are matched more than once. Please",
               "alter weekly spreadsheet accordingly.",
               "\nCheck file requirements and weekly data"),
         call. = FALSE)
  }

  ions <- weekly_data %>%
    mutate(datasheet = case_when(row_number() > silica_included_start_rw &
                                   row_number() <= silica_included_start_rw + 3
                                 ~ "Raw",
                                 row_number() > silica_included_start_cw &
                                   row_number() <= silica_included_start_cw + 3
                                 ~ "Clearwell"),
           station   = case_when(row_number() > silica_included_start_rw &
                                   row_number() <= silica_included_start_rw + 3
                                 ~ "Raw",
                                 row_number() > silica_included_start_cw &
                                   row_number() <= silica_included_start_cw + 3
                                 ~ "Clearwell")) %>%
    filter(!is.na(station),
           # Don't want to read in Difference values, just % Difference
           !grepl("^difference$", Parameters, ignore.case = TRUE))

  if (any(!ions$Parameters %in% ions_parms_list$parameter)) {
    stop(paste("There is an issue with the ions section of the weekly data.",
               "The tool expects that the three rows after the Raw Silica Added",
               "and Treated Silica Added titles contain ion values. One or more",
               "of those six rows cannot be identified as an ion value as it is",
               "not present in parameters.xlsx. Check which 6 rows are located",
               "under the ion section titles and ensure they are in parameters.xlsx.",
               "\nCheck file requirements and weekly data."),
         call. = FALSE)
  }

  ions <- ions %>%
    select(parameter = Parameters, unit = Units, datasheet, station,
          everything()) %>%
    select(!starts_with("...")) %>%
    pivot_longer(cols = -c(parameter, unit, datasheet, station),
                names_to = "date_ymd", values_to = "result") %>%
    mutate(date_ymd = excel_numeric_to_date(as.numeric(date_ymd))) %>%
    filter(!is.na(date_ymd)) %>%
    select(datasheet, station, parameter, unit, date_ymd, result)

  return(ions)

}

#' Scrape DOC profiles
#'
#' Read in DOC profile data
#'
#' @inheritParams scrape_labdatxls
#' @param doc_data dataframe. Output from read_doc()
#'
#' @return data frame of the DOC profile data
scrape_docprofiles <- function(doc_data, labdat_parameters){

  # Summary stats are stored at the bottom of the DOC sheet and are not desired
  doc_profile_end <- min(which(is.na(doc_data[[1]])))

  doc_parms_list <- filter(labdat_parameters, datasheet == "DOCProfile")

  # The na.locf process can create excess columns, especially in later sheets
  # where a copy-paste typo exists and ~250 columns are read in, where the last
  # 230 columns are all given the same name. We want to remove the duplicates of
  # the last column
  incomparables <- setdiff(colnames(doc_data), tail(colnames(doc_data), 1))

  doc_data <- doc_data[, !duplicated(colnames(doc_data), incomparables)]

  # Want to pull in only desired columns AND the date column
  doc_data <- doc_data[, c(names(doc_data)[(names(doc_data) %in%
                                        doc_parms_list$parameter)],
                       "Sample Date")]

  doc_data <- doc_data %>%
    filter(row_number() < doc_profile_end) %>%
    rename(date_ymd = "Sample Date") %>%
    pivot_longer(cols = -(date_ymd),
                 names_to = "parameter",
                 values_to = "result") %>%
    left_join(doc_parms_list, by = "parameter") %>%
    mutate(date_ymd = excel_numeric_to_date(as.numeric(date_ymd))) %>%
    select(datasheet, station, parameter, unit, date_ymd, result)

  return(doc_data)

}

