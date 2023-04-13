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
#' @param spreadsheet dataframe. Raw weekly data outputted from read_weekly()
#' @param labdat_parameters dataframe. Output from check_parameters_content
#'
#' @return data frame of all the data contained in the Weekly lab data file
scrape_labdatxls <- function(spreadsheet, labdat_parameters) {

  if (colnames(spreadsheet)[1] != "Parameters") {
    stop(paste0("Issue with weekly data column names. ",
                "Do column names start on row 8? ",
                "Is the parameters column correctly named as \"Parameters\"? ",
                "Check file requirements and weekly data."),
         call. = FALSE)
  }

  clearwell_start <- which(spreadsheet$Parameters == "CLEAR WELL")

  if (is_empty(clearwell_start)) {
    stop(paste0("Issue with the start of the Clear Well data. ",
                "Is the start identified with the string \"CLEAR WELL\"? ",
                "Check file requirements and weekly data."),
         call. = FALSE)
  }

  rawwater        <- scrape_rawwater(      spreadsheet,
                                           clearwell_start,
                                           labdat_parameters)
  clearwell       <- scrape_clearwell(     spreadsheet,
                                           clearwell_start,
                                           labdat_parameters)
  clearwell_THMs  <- scrape_clearwell_thms(spreadsheet,
                                           clearwell_start,
                                           labdat_parameters)
  clearwell_al    <- scrape_clearwell_al(  spreadsheet,
                                           clearwell_start,
                                           labdat_parameters)
  ion_values      <- scrape_ion_values(    spreadsheet,
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
#' @param spreadsheet dataframe. All data in the Weekly sheet of the lab data file
#' @param clearwell_start numeric value. The start of the Clearwell data as
#'  indicated by the phrase "CLEAR WELL" in the Parameters column of the Weekly
#'  data
#' @inheritParams scrape_labdatxls
#'
#' @return data frame of the raw water data
scrape_rawwater <- function(spreadsheet, clearwell_start, labdat_parameters) {

  rw_parms_list <- labdat_parameters %>%
    filter(datasheet == "RawWater") %>%
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

  rawwater <- spreadsheet %>%
    filter(row_number() < clearwell_start - 1) %>%
    filter(Parameters %in% rw_parms_list$parameter) %>%
    select(!starts_with("...")) %>%
    pivot_longer(cols = -c(Parameters, Units),
                 names_to = "date_ymd", values_to = "result") %>%
    rename(parameter = Parameters, unit = Units) %>%
    mutate(date_ymd = excel_numeric_to_date(as.numeric(date_ymd)),
           # As directed by Blair. Raw water THMs are only measured at the
           # PreFM stage
           station = as.factor(ifelse(parameter %in% rw_thms$parameter |
                                        grepl("PreFM", parameter),
                                      "PreFM","Raw"))) %>%
    filter(!is.na(date_ymd)) %>%
    add_column(datasheet = "RawWater") %>%
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
scrape_clearwell <- function(spreadsheet, clearwell_start, labdat_parameters) {

  # Aluminum, THMs, and ion values are read in separately
  cw_parms_list <- labdat_parameters %>%
    filter(datasheet == "ClearWell") %>%
    as.data.table() %>%
    # Ion values are not read in at this point. They are read in together with
    # CW ion values using scrape_ion_values() because they are found together at
    # the bottom of the lab data sheet
    fsetdiff(filter_ions(.)) %>%
    fsetdiff(filter_thms(.)) %>%
    fsetdiff(filter_al(.)) %>%
    as.data.frame()

  # based on premise that the aluminum and THMs are NOT read in here
  clearwell <- spreadsheet %>%
    filter(row_number() > clearwell_start) %>%
    filter(Parameters %in% cw_parms_list$parameter) %>%
    select(!starts_with("...")) %>%
    pivot_longer(cols = -c(Parameters, Units),
                 names_to = "date_ymd", values_to = "result") %>%
    rename(parameter = Parameters, unit = Units) %>%
    mutate(date_ymd = excel_numeric_to_date(as.numeric(date_ymd)),
           station = case_when(grepl("PreGAC", parameter)
                               ~ "PreGAC",
                               grepl("Channel 1", parameter)
                               ~ "Channel 1",
                               grepl("Channel 2", parameter)
                               ~ "Channel 2",
                               grepl("PreFM", parameter)
                               ~ "PreFM",
                               TRUE ~ "Clearwell"),
           station = as.factor(station)) %>%
    filter(!is.na(date_ymd)) %>%
    add_column(datasheet = "ClearWell") %>%
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
scrape_clearwell_thms <- function(spreadsheet, clearwell_start, labdat_parameters) {

  # Expecting consistently 4 THM rows for each of Clearwell, Channel, PreGAC
  cw_thms_parms_list <- labdat_parameters %>%
    filter(datasheet == "ClearWell") %>%
    as.data.table() %>%
    filter_thms(.) %>%
    as.data.frame()

  clearwell_THMs <- spreadsheet %>%
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
    stop(paste0("Issue with Clear Well THMs. ",
                "Too many rows were identified. There should be 15. ",
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
    add_column(datasheet = "ClearWell") %>%
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
scrape_clearwell_al <- function(spreadsheet, clearwell_start, labdat_parameters) {

  # Expecting parameters that look like:
  # "Aluminum (dissolved 0.45\u00b5)",
  # "Aluminum (total)",
  # "Aluminum (Total)",
  # "Aluminum (dissolved)",
  # "Aluminum (particulate)"
  # Using labdat_parameters instead of hard-coding to account for small typos
  cw_al_parms_list <- labdat_parameters %>%
    filter(datasheet == "ClearWell") %>%
    as.data.table() %>%
    filter_al(.) %>%
    as.data.frame()

  clearwell_Al <- spreadsheet %>%
    filter(row_number() > clearwell_start) %>%
    select(!starts_with("...")) %>%
    filter(Parameters %in% cw_al_parms_list$parameter)

  if (nrow(clearwell_Al) != 7) {
    stop(paste0("Issue with Clear Well Aluminum values. ",
                "Too many rows were identified. There should be 7. ",
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
    add_column(datasheet = "ClearWell") %>%
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
scrape_ion_values <- function(spreadsheet, labdat_parameters) {

  ions_parms_list <- labdat_parameters %>%
    as.data.table() %>%
    filter_ions(.) %>%
    as.data.frame()

  # Find where the ion values with silica included begin (desired values)
  silica_excluded_start        <- first(which(grepl("SILICA NOT ADDED",
                                                    spreadsheet$Parameters)))
  silica_included_start_rw     <- first(which(grepl("SILICA ADDED",
                                                    spreadsheet$Parameters)))
  silica_included_start_cw     <- nth(which(grepl("SILICA ADDED",
                                                  spreadsheet$Parameters)),
                                      2)


  if (is.na(silica_included_start_rw) | is.na(silica_included_start_cw)) {
    stop(paste0("Issue with Weekly Data ion values. ",
                "Could not identify the start of either the raw or Clearwell ",
                "ion values through the phrase `SILICA ADDED` in the ",
                "Parameters column. ",
                "Check file requirements and weekly data."),
         call. = FALSE)
  } else if (!is_empty(silica_excluded_start)) {
    if (silica_excluded_start > silica_included_start_rw) {
      stop(paste0("Issue with Weekly Data ion values. ",
                  "Ion values with silica added are located before ion values ",
                  "without silica. ",
                  "Check file requirements and weekly data."),
           call. = FALSE)
    }
  }

  if (length(silica_included_start_rw) == 0) {
    # There are no ion values with silica included. We will not store any ions
    # values in the DB
    ions <- as.data.frame(NULL)
  } else {
    # There exist values with silica included (desired)
    ions <- spreadsheet %>%
      filter(row_number() >= silica_included_start_rw,
             Parameters %in% ions_parms_list$parameter) %>%
      mutate(datasheet = ifelse(row_number() >= 1 & row_number() <= nrow(.)/2,
                              "RawWater",
                              ifelse(row_number() >= nrow(.)/2 + 1 & row_number() <= nrow(.),
                                    "ClearWell",
                                    NA)),
            station = ifelse(row_number() >= 1 & row_number() <= nrow(.)/2,
                              "Raw",
                              ifelse(row_number() >= nrow(.)/2 + 1 & row_number() <= nrow(.),
                                    "Clearwell",
                                    NA))) %>%
      select(parameter = Parameters, unit = Units, datasheet, station,
            everything()) %>%
      select(!starts_with("...")) %>%
      pivot_longer(cols = -c(parameter, unit, datasheet, station),
                  names_to = "date_ymd", values_to = "result") %>%
      mutate(date_ymd = excel_numeric_to_date(as.numeric(date_ymd))) %>%
      filter(!is.na(date_ymd)) %>%
      select(datasheet, station, parameter, unit, date_ymd, result)
  }

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
  # where a copy-paste typo exists and ~250 columns are read in
  doc_data <- doc_data[, !duplicated(colnames(doc_data))]

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
    select(datasheet, station, parameter = parameter_updated, unit = unit_updated,
           date_ymd, result)

  return(doc_data)

}

