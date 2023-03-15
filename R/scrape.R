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
#
# 3. All calculated parameters have been excluded.


#' Scrape data from BPWTP routine lab data
#'
#' Read in all data contained in the Weekly (1st) sheet of the lab data file.
#'
#' @param path_to_labdat_file character string. Path to the lab data file
#'
#' @return data frame of all the data contained in the Weekly lab data file
scrape_labdatxls <- function(path_to_labdat_file) {

  spreadsheet <- suppressMessages(read_excel(path_to_labdat_file,
                                             sheet = 1,
                                             col_names = TRUE, col_types = NULL,
                                             skip = 7))
  clearwell_start <- which(spreadsheet$Parameters == "CLEAR WELL")

  rawwater        <- scrape_rawwater(spreadsheet, clearwell_start)
  clearwell       <- scrape_clearwell(spreadsheet, clearwell_start)
  clearwell_THMs  <- scrape_clearwell_thms(spreadsheet, clearwell_start)
  clearwell_al    <- scrape_clearwell_al(spreadsheet, clearwell_start)
  ion_values      <- scrape_ion_values(spreadsheet)

  new_data_weekly <- bind_rows(rawwater, clearwell, clearwell_THMs,
                               clearwell_al, ion_values)

  return(new_data_weekly)

}


#' Scrape raw water data
#'
#' Read in raw water data from the Weekly (1st) sheet of the lab data file.
#' Identify parameters to read in based on bp_parms_list.xlsx
#'
#' @param spreadsheet dataframe. All data in the Weekly sheet of the lab data file
#' @param clearwell_start numeric value. The start of the Clearwell data as
#'  indicated by the phrase "CLEAR WELL" in the Parameters column of the Weekly
#'  data
#'
#' @return data frame of the raw water data
scrape_rawwater <- function(spreadsheet, clearwell_start){

  rw_parms_list <- read_excel(path = "./data/bp_parms_list.xlsx",
                              sheet = "bp_rw_parms_list",
                              col_names = TRUE)

  thms <- c("TTHM's (total)", "Chloroform", "Bromodichloromethane",
            "Chlorodibromomethane", "Bromoform")

  rawwater <- spreadsheet %>%
    filter(row_number() < clearwell_start - 1) %>%
    filter(Parameters %in% rw_parms_list$Parameters) %>%
    select(!starts_with("...")) %>%
    pivot_longer(cols = -c(Parameters, Units),
                 names_to = "date_ymd", values_to = "result") %>%
    rename(parameter = Parameters, unit = Units) %>%
    mutate(date_ymd = excel_numeric_to_date(as.numeric(date_ymd)),
           station = as.factor(ifelse(parameter %in% thms |
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
#' read in based on bp_parms_list.xlsx
#'
#' @inheritParams scrape_rawwater
#'
#' @return data frame of the Clearwell data (excluding THMs and Al)
scrape_clearwell <- function(spreadsheet, clearwell_start){

  cw_parms_list <- read_excel(path = "./data/bp_parms_list.xlsx",
                             sheet = "bp_cw_parms_list",
                             col_names = TRUE)

  clearwell <- spreadsheet %>%
    filter(row_number() > clearwell_start) %>%
    filter(Parameters %in% cw_parms_list$Parameters) %>%
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
#' bp_parms_list.xlsx
#'
#' @inheritParams scrape_rawwater
#'
#' @return data frame of the Clearwell THMs data
scrape_clearwell_thms <- function(spreadsheet, clearwell_start){

  cw_THMs <- c("TTHM's (total)", "Chloroform", "Chlorodibromomethane",
               "Bromodichloromethane", "Bromoform")

  clearwell_THMs <- spreadsheet %>%
    filter(row_number() > clearwell_start) %>%
    select(!starts_with("...")) %>%
    filter(Parameters %in% cw_THMs) %>%
    mutate(station = ifelse(row_number() >= 1 & row_number() <= 5,
                            "Clearwell", NA),
           station = ifelse(row_number() >= 6 & row_number() <= 10,
                            "Channel", station),
           station = ifelse(row_number() >= 11 & row_number() <= 15,
                            "PreGAC", station)) %>%
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
#' bp_parms_list.xlsx
#'
#' @inheritParams scrape_rawwater
#'
#' @return data frame of the Clearwell aluminum data
scrape_clearwell_al <- function(spreadsheet, clearwell_start){

  cw_Al <- c("Aluminum (dissolved 0.45\u00b5)", "Aluminum (total)",
             "Aluminum (dissolved)", "Aluminum (particulate)")

  clearwell_Al <- spreadsheet %>%
    filter(row_number() > clearwell_start) %>%
    select(!starts_with("...")) %>%
    filter(Parameters %in% cw_Al) %>%
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
#' bp_parms_list.xlsx
#'
#' @inheritParams scrape_rawwater
#'
#' @return data frame of the ion data
scrape_ion_values <- function(spreadsheet){

  ions_parms_list <- read_excel(path = "./data/bp_parms_list.xlsx",
                              sheet = "ions_parms_list",
                              col_names = TRUE)

  # Find where the ion values with silica included begin (desired values)
  silica_included_start     <- first(which(grepl("SILICA ADDED",
                                                 spreadsheet$Parameters)))
  if (length(silica_included_start) == 0) {
    # There are no ion values with silica included. We will not store any ions
    # values in the DB
    ions <- as.data.frame(NULL)
  } else {
    # There exist values with silica included (desired)
    ions <- spreadsheet %>%
      filter(row_number() >= silica_included_start,
             Parameters %in% ions_parms_list$Parameters) %>%
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
