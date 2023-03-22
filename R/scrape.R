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
#' @inheritParams prepare_labdat
#'
#' @return data frame of all the data contained in the Weekly lab data file
scrape_labdatxls <- function(path_to_labdat_file, path_to_parameters) {

  # Note that if the number of rows to skip changes, the skip value here must
  # be changed as well as the skip value and addition used to calculate the
  # row number within check_missing_params in utilities.R
  spreadsheet <- suppressMessages(read_excel(path_to_labdat_file,
                                             sheet = 1,
                                             col_names = TRUE, col_types = NULL,
                                             skip = 7))


  if (colnames(spreadsheet)[1] != "Parameters") {
    stop(paste0("Issue with weekly data column names. ",
                "Do column names start on row 8? ",
                "Is the parameters column correctly named as \"Parameters\"?",
                "Check file requirements and weekly data."))
  }

  clearwell_start <- which(spreadsheet$Parameters == "CLEAR WELL")

  if (is_empty(clearwell_start)) {
    stop(paste0("Issue with the start of the Clear Well data. ",
                "Is the start identified with the string \"CLEAR WELL\"? ",
                "Check file requirements and weekly data."))
  }

  labdat_parameters <- read_xlsx(path_to_parameters) %>%
    mutate(tbl_parameter = as.character(tbl_parameter),
           tbl_unit = as.character(tbl_unit))

  # To simplify column names while the data frame is being used. Blair wanted
  # columns to start with _tbl
  colnames(labdat_parameters) <- str_remove(colnames(labdat_parameters), "tbl_")

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
#' @param labdat_parameters dataframe. Slightly processed parameters.xlsx data
#'
#' @return data frame of the raw water data
scrape_rawwater <- function(spreadsheet, clearwell_start, labdat_parameters) {

  rw_parms_list <- labdat_parameters %>%
    filter(datasheet == "RawWater",
           # Ion values are read in separately
           !grepl("ion sum|% Difference", parameter, ignore.case = TRUE))

  thms <- rw_parms_list %>%
    filter(parm_tag == "THM")

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
           station = as.factor(ifelse(parameter %in% thms$parameter |
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
    filter(datasheet == "ClearWell",
           !grepl("^Aluminum \\(", parameter, ignore.case = TRUE),
           parm_tag  != "THM",
           !grepl("ion sum|% Difference", parameter, ignore.case = TRUE))

  # based on premise that the aluminum and THMs are NOT read in here
  clearwell <- spreadsheet %>%
    filter(row_number() > clearwell_start) %>%
    filter(Parameters %in% cw_parms_list$parameter) %>%
    select(!starts_with("...")) %>%
    pivot_longer(cols = -c(Parameters, Units),
                 names_to = "date_ymd", values_to = "result") %>%
    rename(parameter = Parameters, unit = Units) %>%
    mutate(date_ymd = excel_numeric_to_date(as.numeric(date_ymd)),
           station = ifelse(grepl("PreGAC",parameter), "PreGAC",
                            ifelse(grepl("Channel 1", parameter) & grepl("Coagulation", parameter),
                                   "Channel 1",
                                   ifelse(grepl("Channel 2", parameter) & grepl("Coagulation", parameter),
                                          "Channel 2",
                                          ifelse(grepl("Channel", parameter),
                                                 "Channel",
                                                 ifelse(grepl("PreFM", parameter),
                                                        "PreFM", "Clearwell"))))),
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
    filter(datasheet == "ClearWell",
           parm_tag  == "THM")

  clearwell_THMs <- spreadsheet %>%
    filter(row_number() > clearwell_start) %>%
    select(!starts_with("...")) %>%
    filter(Parameters %in% cw_thms_parms_list$parameter) %>%
    mutate(station = ifelse(row_number() >= 1 & row_number() <= 5,
                            "Clearwell", NA),
           station = ifelse(row_number() >= 6 & row_number() <= 10,
                            "Channel", station),
           station = ifelse(row_number() >= 11 & row_number() <= 15,
                            "PreGAC", station))

  if (nrow(clearwell_THMs) != 15) {
    stop(paste0("Issue with Clear Well THMs. ",
                "Too many rows were identified. There should be 15. ",
                "Check file requirements and weekly data."))
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
    filter(datasheet == "ClearWell",
           grepl("^Aluminum \\(", parameter, ignore.case = TRUE))

  clearwell_Al <- spreadsheet %>%
    filter(row_number() > clearwell_start) %>%
    select(!starts_with("...")) %>%
    filter(Parameters %in% cw_al_parms_list$parameter)

  if (nrow(clearwell_Al) != 7) {
    stop(paste0("Issue with Clear Well Aluminum values. ",
                "Too many rows were identified. There should be 7. ",
                "Check file requirements and weekly data."))
  }

  clearwell_Al <- clearwell_Al %>%
    # The expected row setup is as follows: 3 Clearwell Al rows, 1 MMF1 Al row,
    # 1 MMF12 Al row, 2 PreGAC Al rows
    mutate(station = ifelse(row_number() == 1 | row_number() == 2 | row_number() == 3,
                            "Clearwell", NA),
           station = ifelse(row_number() == 4, "MMF1", station),
           station = ifelse(row_number() == 5, "MMF12", station),
           station = ifelse(row_number() == 6 | row_number() == 7,
                            "PreGAC", station)) %>%
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
    filter(datasheet == "ClearWell",
           grepl("ion sum|% Difference", parameter, ignore.case = TRUE))

  # Find where the ion values with silica included begin (desired values)
  silica_excluded_start     <- first(which(grepl("SILICA NOT ADDED",
                                                 spreadsheet$Parameters)))
  silica_included_start     <- first(which(grepl("SILICA ADDED",
                                                 spreadsheet$Parameters)))

  if (is_empty(silica_included_start)) {
    stop(paste0("Issue with Weekly Data ion values. ",
                "Could not identify the start of ion values through the phrase ",
                "`SILICA ADDED` in the Parameters column. ",
                "Check file requirements and weekly data."))
  } else if (!is_empty(silica_excluded_start)) {
    if (silica_excluded_start > silica_included_start) {
      stop(paste0("Issue with Weekly Data ion values. ",
                  "Ion values with silica added are located before ion values ",
                  "without silica. ",
                  "Check file requirements and weekly data."))
    }
  }

  if (length(silica_included_start) == 0) {
    # There are no ion values with silica included. We will not store any ions
    # values in the DB
    ions <- as.data.frame(NULL)
  } else {
    # There exist values with silica included (desired)
    ions <- spreadsheet %>%
      filter(row_number() >= silica_included_start,
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
#'
#' @return data frame of the DOC profile data
scrape_docprofiles <- function(path_to_labdat_file){

  # Unnecessary messages printed to console. Can be suppressed
  labdat <- suppressMessages(read_excel(path_to_labdat_file,
                                        sheet = "WTP DOC Profile",
                                        range = cell_limits(ul = c(3, 2)),
                                        col_names = TRUE))

  if (!grepl("date", colnames(labdat)[1], ignore.case = TRUE)) {
    stop(paste0("Issue with DOC Profile sheet. ",
                "An issue with the column setup was detected. ",
                "Check file requirements and weekly data."))
  }

  # Need separate line as suppressMessages() is not for use in pipes and
  # options(warn = -1) does not suppress all of the printed messages
  labdat <- select(labdat, date_ymd = 1, contains(c("PreFM", "FM", "Channel")))

  # There is a typo in WTP DOC profile, where a date is entered as "29Jan-18"
  if (as_datetime("2018-01-22") %in% labdat$date_ymd) {
    error_spot <- which(as_datetime("2018-01-22") == labdat$date_ymd) + 1
    labdat$date_ymd[error_spot] <- as.Date("2018-01-29")
  }

  doc_profile_end <- min(which(is.na(labdat[[1]])))

  # Reading in the doc_profile sometimes includes an empty few rows at the start
  # which need to be removed
  while (doc_profile_end == 1){
    labdat <- labdat[-1,]
    doc_profile_end <- min(which(is.na(labdat[[1]])))
  }

  labdat <- labdat %>%
    filter(row_number() < doc_profile_end)

  # Columns at this point are not properly named, and should be identified as
  # either being DOC values or DOC % Removal values
  for (i in 1:((ncol(labdat)-1)/2)) {
    # At this point, colnames look like:
    # "date_ymd" "PreFM...4" "PreFM...9" "FM...5" "FM...10" "Channel...6" "Channel...11"
    # TBD 2022 MISSING ONE OF THESE SO CHANGED TO NCOL-1/2
    # , where there are two columns per station (col#1 = DOC, col#2 = DOC removal).
    # This line allows us to pull out the station from each pair of columns
    station <- unlist(strsplit(colnames(labdat)[i*2], "[.]"))[1]

    labdat <- labdat %>%
      as.data.frame() %>%
      # Every second column starting from col 2 contain DOC values
      setnames(i*2, paste("DOC", station, sep = "_")) %>%
      # Every second column starting from column 3 contain DOC removal values
      setnames(i*2+1, paste("DOC Removal", station, sep = " - "))
  }

  labdat_doc <- labdat %T>%
    # options() line to suppress messages printed to console when pivoting,
    # as "station" value is not extracted from DOC Removal column names
    {options(warn = -1)} %>%
    # DOC removal values and older files' date values are read in as character
    mutate_if(is.character, as.numeric) %>%
    # This correctly sets the station for the DOC values but sets the station as
    # NA for DOC removal values (to later fill with "Combined Stations")
    pivot_longer(cols = -(date_ymd),
                 names_to = c("parameter", "station"),
                 values_to = "result",
                 names_sep = "_") %T>%
    {options(warn = 0)} %>%
    mutate(datasheet = "doc_profile",
           station = replace_na(station, "Combined Stations"),
           date_ymd = as.Date(date_ymd, origin = "1899-12-30"),
           unit = ifelse(grepl("Removal", parameter), "%", "mg/L C"),
           result = round(result, digits = 2)) %>%
    arrange(parameter, station, date_ymd) %>%
    select(datasheet, station, parameter, unit, date_ymd, result)

  return(labdat_doc)

}
