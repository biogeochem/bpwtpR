#' Update database with new data
#'
#' @param labdat_file base data file
#' @param datadir data directory
#' @param xlsdir labdat xls directory
#' @param outdir output directory
#' @param outfilename name of output file
#'
#' @return
#' @export
#'
#' @examples
#' #update_database()
update_database <- function(labdat_file =
                              "BPWTP_routinelabdata_historicalbase_clean.csv",
                            datadir,
                            xlsdir = "labdat_datafiles",
                            outdir = "./data",
                            outfilename = "BPWTP_labdat_updated.csv"){

  labdatfile_fpath <- file.path(datadir, xlsdir)
  files <- list.files(path = labdatfile_fpath, pattern = ".xlsx",
                      ignore.case = TRUE, full.names = TRUE)
  labdat_newfilename <- files[grepl("active", files)]

  ## load data
  old_data <- read_labdat(datadir = datadir, labdat_file = labdat_file) %>%
    mutate(sheet_year = as.factor(sheet_year))

  tmp <- scrape_labdatxls(labdat_newfilename) %>%
    mutate(datetime_ymd.hms = as.POSIXct(datetime_ymd.hms),
           sheet_year = as.factor(year(datetime_ymd.hms)))

  tmp_doc <- scrape_docprofiles(labdat_newfilename) %>%
    select(datasheet, station, parameter, unit,
           datetime_ymd.hms, result, sheet_year) %>%
    mutate(datetime_ymd.hms = as.POSIXct(datetime_ymd.hms),
           result = as.character(result), sheet_year = as.factor(sheet_year))

  ## select new data
  database_dates <- unique(old_data$datetime_ymd.hms)
  current_dates <- c(unique(tmp$datetime_ymd.hms), unique(tmp_doc$datetime_ymd.hms))
  current_dates <- current_dates[which(current_dates <= as.POSIXct(Sys.Date()))]


  if(!all(database_dates %in% current_dates)){ # are all values false? TRUE = Yes
    # select the dates that are not in the database
    new_dates <- current_dates[which(!(current_dates %in% database_dates))]
    new_data <- tmp %>%
      bind_rows(tmp_doc) %>%
      filter(datetime_ymd.hms %in% new_dates)
  } else {
    stop("No data to add", call. = T)
  }

  ## update parameter names
  new_data <- update_parameters(new_data)

  new_data <- new_data %>%
    filter(parm_eval != "calculated_insheet") %>%
    mutate(result_org = result, ## create a new column for the orginal result
           result_flag = "") %>%
    replace_dl()%>%
    mutate(result = as.numeric(result))

  new_data <- convert_biocounts(new_data)
  new_data_calcs <- apply_calculations(new_data %>%
                                         filter(datasheet != "doc_profile"))

  new_data <- append_calc_values(new_data, new_data_calcs)

  new_data <- new_data %>%
    mutate(year = year(datetime_ymd.hms),
           month = month(datetime_ymd.hms, label = T, abbr = T),
           week = week(datetime_ymd.hms)) %>%
    select(datasheet:datetime_ymd.hms, year:week, parameter:result) %>%
    mutate_if(is.character, as.factor)

  labdat_updated <- append_newdata(old_data, new_data)
  print(c(dim(old_data), dim(new_data), dim(labdat_updated)))

  write_datafile(file = labdat_updated, outdir = outdir,
                 outfilename = outfilename) ## This will need to be changed to the updated file or saved with the date

  return(labdat_updated)
}
