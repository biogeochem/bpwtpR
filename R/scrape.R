# Notes:
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

## 1981, 1983, 1985 had the wrong year and needed to be corrected
## build_database should be run once. After data is scraped, it needs to be prepared.
# use a prepare function to update information scraped from a datasheet?

## create function to update database
# package janitor is in here somewhere??


#' Build database from scraped BPWTP routine lab data
#'
#' @param datadir data directory
#' @param outdir output directory
#' @param outfile out file name
#' @param save_output toggled option to save data file
#'
#' @return single data file with all historical data
#' @export
#'
#' @examples
#' # build_database(datadir = "data/labdat_files", outdir = "data", outfile = "testfile.csv")
build_database <- function(datadir = "data/labdat_datafiles",
                           outdir = "output",
                           outfile = "test_output.csv",
                           save_output = FALSE){
  fpath <- file.path(datadir)

  files <- list.files(path = fpath, pattern = ".xlsx",
                      ignore.case = TRUE, full.names = TRUE)
  files <- files[!grepl("active_", files)]

  labdat.historical <- do.call(rbind,lapply(files, scrape_labdatxls))

  outpath <- file.path(outdir, outfile)
  write.csv(labdat.historical, file = outpath, row.names = F)

}


#' Scrape data from BPWTP routine lab data (`.xls`)
#'
#' @param labdat_filename file name for routine lab data `.xls` file
#' @param save_output Toggled option to save data output
#'
#' @return data frame of scraped data from an individual sheet
#' @export
#'
#' @importFrom DescTools SplitPath
#' @importFrom dplyr bind_rows
#'
#' @examples
#' # scrape_labdatxls(labdat_filename = "routinelabdat_2019.xls",
#' save_output = FALSE)
scrape_labdatxls <- function(labdat_filename, save_output = FALSE) {

  if(grepl("202", labdat_filename)){
    rawwater_range = "A8:BE136"
    clearwell_range = "A8:BE276"
  } else {
    rawwater_range = "A8:BE130"
    clearwell_range = "A8:BE266"
  }

  print(rawwater_range); print(clearwell_range)

  rawwater <- scrape_rawwater(labdat_filename, rawwater_range = rawwater_range)
  clearwell <- scrape_clearwell(labdat_filename, clearwell_range = clearwell_range)
  clearwell_THMs <- scrape_clearwell_thms(labdat_filename, clearwell_range = clearwell_range)
  clearwell_al <- scrape_clearwell_al(labdat_filename, clearwell_range = clearwell_range)

  outname <- SplitPath(labdat_filename)$filename
  print(outname)
  bp_longterm <- bind_rows(rawwater, clearwell,
                           clearwell_THMs, clearwell_al) %>%
    mutate(sheet_year = substr(outname, start = 1, stop = 4))
  if(isTRUE(save_output)){
    dir.create("./output/scraped_data", showWarnings = FALSE)
    write.csv(bp_longterm, file = paste0("./output/scraped_data/", outname,".csv"))
  }

  return(as.data.frame(bp_longterm))

}


#' Scrape raw water data from `.xls` file
#'
#' @param labdat_filename file name for routine lab data `.xls` file
#'
#' @return scraped raw water data
#' @export
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter mutate select rename
#' @importFrom magrittr %>%
#' @importFrom tibble add_column
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' # scrape_rawwater(labdat_filename = "routinelabdata_2019.xls")
scrape_rawwater <- function(labdat_filename, rawwater_range = "A8:BE130"){
  rw_parms_list <- read_excel(path = "./data/bp_parms_list.xlsx",
                              sheet = "bp_rw_parms_list",
                              col_names = TRUE)

  thms <- c("Chloroform", "Bromodichloromethane",
            "Chlorodibromomethane", "Bromoform")

  rawwater <- read_excel(labdat_filename, sheet = 1, range = rawwater_range,
                         col_names = TRUE, col_types = NULL) %>%
    filter(Parameters %in% rw_parms_list$Parameters) %>%
    filter(!Parameters %in% c("Langelier Index (RTW)",
                              "Langelier Saturation Index (LSI #2, new as of 2018)",
                              "Aluminum (particulate)", "SUVA",
                              "UV@254/Raw DOC", "TTHM's (total)",
                              "Total Green & B-G", "Alum-DOC Stoich",
                              "Total Chlorine Dose", "Alum\\Raw DOC")) %>%
    add_column(., parm_eval = NA, .after = 2) %>%
    mutate(parm_eval = ifelse(Parameters == "Bench Diss. Oxygen" &
                                Units == "%", "calc", NA),
           parm_eval = ifelse(Parameters == "TDS" &
                                Units == "mg/L(calc)", "calc", parm_eval)) %>%
    filter(!parm_eval %in% "calc") %>%
    select(!parm_eval & !starts_with("...")) %>%
    pivot_longer(
      cols = -c(Parameters, Units),
      names_to = "datetime_ymd.hms",
      values_to = "result") %>%
    rename(parameter = Parameters, unit = Units) %>%
    mutate(datetime_ymd.hms = as.numeric(datetime_ymd.hms),
           datetime_ymd.hms = excel_numeric_to_date(datetime_ymd.hms),
           station = as.factor(ifelse(parameter %in% thms |
                                        grepl("PreFM", parameter),
                                      "PreFM","Raw"))) %>%
    filter(!is.na(datetime_ymd.hms)) %>%
    add_column(datasheet = "RawWater") %>%
    select(datasheet, station, parameter, unit, datetime_ymd.hms, result)

  return(rawwater)
}


#' Scrape clearwell data from `.xls` file
#'
#' @param labdat_filename file name for routine lab data `.xls` file
#' @param clearwell_range excel cell range in file
#'
#' @return scraped clearwell data
#' @export
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter mutate select rename
#' @importFrom magrittr %>%
#' @importFrom tibble add_column
#' @importFrom tidyr pivot_longer
#' @importFrom janitor excel_numeric_to_date
#'
#' @examples
#' # scrape_clearwell(labdat_filename = "routinelabdata_2019.xls")
scrape_clearwell <- function(labdat_filename, clearwell_range){
  cw_parms_list <- read_excel(path = "./data/bp_parms_list.xlsx",
                             sheet = "bp_cw_parms_list",
                             col_names = TRUE)

  clearwell <- read_excel(labdat_filename, sheet = 1, range = clearwell_range,
                          col_names = TRUE, col_types = NULL) %>%
    mutate(rownum = as.numeric(rownames(.))) %>%
    select(rownum, everything()) %>%
    filter(rownum > 122) %>%
    select(!rownum & !starts_with("...")) %>%
    filter(Parameters %in% cw_parms_list$Parameters) %>%
    pivot_longer(cols = -c(Parameters, Units), names_to = "datetime_ymd.hms",
                 values_to = "result") %>%
    rename(parameter = Parameters, unit = Units) %>%
    mutate(datetime_ymd.hms = as.numeric(datetime_ymd.hms),
           datetime_ymd.hms = excel_numeric_to_date(datetime_ymd.hms),
           station = ifelse(grepl("PreGAC",parameter), "PreGAC",
                            ifelse(grepl("Channel 1", parameter) &
                                     grepl("Coagulation", parameter), "Channel 1",
                                   ifelse(grepl("Channel 2", parameter) &
                                            grepl("Coagulation", parameter), "Channel 2",
                                          ifelse(grepl("Channel", parameter), "Channel",
                                                 ifelse(grepl("PreFM", parameter), "PreFM", "Clearwell"))))),
           station = as.factor(station)) %>%
    filter(!is.na(datetime_ymd.hms)) %>%
    add_column(datasheet = "ClearWell") %>%
    select(datasheet, station, parameter, unit, datetime_ymd.hms, result)

#filter(!Units == "mg/L(calc)")) %>%

return(clearwell)

}


#' Scrape clearwell total halomethanes (thms) data from `.xls` file
#'
#' @param labdat_filename file name for routine lab data `.xls` file
#' @param clearwell_range excel range for clearwell data
#'
#' @return scraped clearwell thms data
#' @export
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter mutate select rename
#' @importFrom magrittr %>%
#' @importFrom tibble add_column
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' # scrape_clearwell_thms(labdat_filename = "routinelabdata_2019.xls")
scrape_clearwell_thms <- function(labdat_filename, clearwell_range){
  # clearwell THMs
  cw_THMs <- c("Chloroform", "Chlorodibromomethane",
               "Bromodichloromethane", "Bromoform")

  clearwell_THMs <- read_excel(labdat_filename, sheet = 1, range = clearwell_range,
                               col_names = TRUE, col_types = NULL) %>%
    mutate(rownum = as.numeric(rownames(.))) %>%
    select(rownum, everything()) %>%
    filter(rownum > 122) %>%
    select(!rownum & !starts_with("...")) %>%
    filter(Parameters %in% cw_THMs) %>%
    mutate(rownum = as.numeric(rownames(.))) %>%
    select(rownum, everything()) %>%
    mutate(station = ifelse(rownum == 1 | rownum == 2 |
                              rownum == 3 | rownum == 4, "Clearwell", NA)) %>%
    mutate(station = ifelse(rownum == 5 | rownum == 6 |
                              rownum == 7 | rownum == 8,
                            "Channel", station)) %>%
    mutate(station = ifelse(rownum == 9 | rownum == 10 |
                              rownum == 11 | rownum == 12,
                            "PreGAC", station)) %>%
    select(rownum, Parameters, Units, station, everything()) %>%
    # select(-c(5, 6)) %>%
    select(-c(rownum)) %>%
    pivot_longer(
      cols = -c(Parameters, Units, station),
      names_to = "datetime_ymd.hms",
      values_to = "result") %>%
    rename(parameter = Parameters, unit = Units, station = station) %>%
    mutate(datetime_ymd.hms = as.numeric(datetime_ymd.hms),
           datetime_ymd.hms = excel_numeric_to_date(datetime_ymd.hms)) %>%
    filter(!is.na(datetime_ymd.hms)) %>%
    add_column(datasheet = "ClearWell") %>%
    select(datasheet, station, parameter, unit, datetime_ymd.hms, result)

  return(clearwell_THMs)

}


#' Scrape clearwell aluminum data from `.xls` file
#'
#' @param labdat_filename file name for routine lab data `.xls` file
#' @param clearwell_range excel range for clearwell data
#'
#' @return scraped clearwell aluminum data
#' @export
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter mutate select rename
#' @importFrom magrittr %>%
#' @importFrom tibble add_column
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' # scrape_clearwell_al(labdat_filename = "routinelabdata_2019.xls")
scrape_clearwell_al <- function(labdat_filename, clearwell_range){
  # clearwell TAl and DAl
  cw_Al <- c("Aluminum (dissolved 0.45µ)", "Aluminum (total)",
             "Aluminum (dissolved)")

  clearwell_Al <- read_excel(labdat_filename, sheet = 1, range = clearwell_range,
                             col_names = TRUE, col_types = NULL) %>%
    mutate(rownum = as.numeric(rownames(.))) %>%
    select(rownum, everything()) %>%
    filter(rownum > 122) %>%
    select(!rownum & !starts_with("...")) %>%
    #filter(grepl("Aluminum", Parameters))
    filter(Parameters %in% cw_Al) %>%
    mutate(rownum = as.numeric(rownames(.))) %>%
    select(rownum, everything()) %>%
    mutate(station = ifelse(rownum == 1 | rownum == 2, "Clearwell", NA)) %>%
    mutate(station = ifelse(rownum == 3, "MMF1", station)) %>%
    mutate(station = ifelse(rownum == 4, "MMF12", station)) %>%
    mutate(station = ifelse(rownum == 5 | rownum == 6, "PreGAC", station)) %>%
    select(rownum, Parameters, Units, station, everything()) %>%
    # select(-c(5, 6)) %>%
    select(-c(rownum)) %>%
    pivot_longer(cols = -c(Parameters, Units, station),
                 names_to = "datetime_ymd.hms",
                 values_to = "result") %>%
    rename(parameter = Parameters,
           unit = Units,
           station = station) %>%
    mutate(datetime_ymd.hms = as.numeric(datetime_ymd.hms)) %>%
    mutate(datetime_ymd.hms = excel_numeric_to_date(datetime_ymd.hms)) %>%
    filter(!is.na(datetime_ymd.hms)) %>%
    mutate(station = ifelse(station == "MMF1" &
                              datetime_ymd.hms >= "2005-01-01",
                            "MMFA", station)) %>%
    mutate(station = ifelse(station == "MMF12" &
                              datetime_ymd.hms >= "2005-01-01",
                            "MMFL", station)) %>%
    add_column(datasheet = "ClearWell") %>%
    select(datasheet, station, parameter, unit, datetime_ymd.hms, result)

  return(clearwell_Al)

}


#' Scrape DOC profiles
#'
#' @param labdat_filename provided file name for the active data file
#'
#' @importFrom lubridate year month week
#' @importFrom readxl cell_limits
#'
#' @return
#' @export
#'
#' @examples #scrape_docprofiles(labdat_filename)
scrape_docprofiles <- function(labdat_filename){
  labdat <- read_excel(labdat_filename,
             sheet = "WTP DOC Profile", range = cell_limits(ul = c(3, 2)),
             col_names = TRUE)
  labdat <- labdat %>%
    select(c(`Sample Date`:CW...10)) %>%
    select(-`Jar Test`) %>%
    filter(grepl(c("^1|^2"), `Sample Date`)) %>%
    rename(datetime_ymd.hms = `Sample Date`,
           `Alum Dose` = `Alum Dose (mg/L)`,
           `CPAC Dose` = `CPAC Dose (mg/L)`,
           `Raw` = `Raw`,
           `PreFM` = PreFM...5,
           `FM` = FM...6,
           `Channel` = Channel...7,
           `PreGAC` = `PreGAC/CW...9`,
           `Clearwell` = CW...10)

  labdat_doc <- select(labdat, c(datetime_ymd.hms, Raw:Clearwell)) %>%
    pivot_longer(cols = Raw:Clearwell, names_to = "station",
                 values_to = "result") %>%
    mutate(datasheet = "doc_profile",
           sheet_year = "",
           parameter = "DOC",
           unit = "mg/L",
           parm_unit = "DOC_mg.L", result = as.character(result))

  labdat_dose <- select(labdat, datetime_ymd.hms: `CPAC Dose`) %>%
    pivot_longer(cols = `Alum Dose`:`CPAC Dose`,
                 names_to = "parameter", values_to = "result") %>%
    mutate(datasheet = "doc_profile",
           sheet_year = "",
           station = "Raw",
           unit = "mg/L",
           parm_unit = ifelse(parameter == "Alum Dose",
                              "alumdose_mg.L", "cpacdose_mg.L"))


  labdat_merge <- bind_rows(labdat_doc, labdat_dose) %>%
    mutate(parm_eval = "measured",
           parm_tag = "",
           datetime_ymd.hms = as.Date(datetime_ymd.hms),
           year = year(datetime_ymd.hms),
           month = month(datetime_ymd.hms, label = T, abbr = T),
           week = week(datetime_ymd.hms),
           result_org = result,
           result = as.numeric(result))

}
