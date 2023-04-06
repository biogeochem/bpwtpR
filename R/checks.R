#' Check parameters.xlsx
#'
#' Check parameters.xlsx column names and ensure that duplicate rows are removed
#'
#' @inheritParams prepare_labdat
#'
#' @return dataframe containing the corrected parameter information
check_parameters_doc <- function(path_to_parameters) {

  labdat_parameters <- read_xlsx(path_to_parameters) %>%
    mutate(tbl_parameter = as.character(tbl_parameter),
           tbl_unit = as.character(tbl_unit))

  labdat_parameters_cols <- c("tbl_datasheet",	"tbl_station",	"tbl_parameter",
                              "tbl_parameter_updated",	"tbl_unit",
                              "tbl_unit_updated",	"tbl_parm_unit",
                              "tbl_parm_eval",	"tbl_parm_tag")

  # Checking col names ---------------------------------------------------------
  if (!identical(colnames(labdat_parameters), labdat_parameters_cols)) {
    stop(paste0("Issue with parameters.xlsx input file. ",
                "Column names have been edited. ",
                "Check file requirements and parameter data."),
         call. = FALSE)
  }

  # Checking datasheet, station, parm_eval, parm_tag ---------------------------
  # Want to make sure that these columns are named consistently. Just handling
  # upper and lowercase issues because strings can be very similar - fuzzy
  # matching can cause other issues. User will be required to fix any mistakes
  # that a simple gsub cannot fix
  correct_inputs <- list(datasheet = c("ClearWell", "RawWater", "DOCProfile"),
                         station   = c("Clearwell", "Raw",
                                       "PreGAC", "PreFM", "FM",
                                       "MMF1", "MMF12", "MMFA", "MMFL",
                                       "Channel", "Channel 1", "Channel 2",
                                       "Combined Stations"),
                         parm_eval = c("measured", "calculated_insheet"),
                         parm_tag  = c("majorConstituents", "operations",
                                       "traceConstituents", "physical", "THM",
                                       "biological", "bacteriological"))

  for (i in 1:length(datasheet_options)) {
    gsub(correct_inputs$datasheet[i], correct_inputs$datasheet[i],
         labdat_parameters$datasheet,
         ignore.case = TRUE)
    gsub(correct_inputs$station[i],   correct_inputs$station[i],
         labdat_parameters$station,
         ignore.case = TRUE)
    gsub(correct_inputs$parm_eval[i], correct_inputs$parm_eval[i],
         labdat_parameters$parm_eval,
         ignore.case = TRUE)
    gsub(correct_inputs$parm_tag[i],  correct_inputs$parm_tag[i],
         labdat_parameters$parm_tag,
         ignore.case = TRUE)
  }

  input_mistakes <- list(which(!labdat_parameters$datasheet %in% correct_inputs$datasheet),
                         which(!labdat_parameters$station   %in% correct_inputs$station),
                         which(!labdat_parameters$parm_eval %in% correct_inputs$parm_eval),
                         which(!labdat_parameters$parm_tag  %in% correct_inputs$parm_tag))

  exists_mistake <- FALSE

  for (i in 1:4) {
    if (!is_empty(input_mistakes[[i]])) {
      exists_mistake <- TRUE
      print(sprintf("There exists a mistake with the %s column at rows:",
                    names(correct_inputs)[i]))
      print(input_mistakes[[i]])
    }
  }

  if (exists_mistake) {
    stop("Check file requirements and parameter data.")
  }

  # Checking for changes in Raw Water THM measurement --------------------------
  rw_thms <- labdat_parameters %>%
    filter(datasheet == "RawWater") %>%
    as.data.table() %>%
    filter_thms(.) %>%
    as.data.frame() %>%
    filter(station != "PreFM")

  if (nrow(rw_thms) != 0) {
    print.data.frame(rw_thms)
    stop(paste0("Issue with parameters.xlsx input file at row(s) listed above. ",
                "Tool functions on the premise that all Raw Water THM values ",
                "are associated with the PreFM station. Parameters file ",
                "includes values that are do not adhere to this requirement. ",
                "If Raw Water THM measurement locations have changed, contact ",
                "the tool creator. Otherwise, check that parameters.xlsx is  ",
                "filled correctly. ",
                "Check file requirements and parameter data."))
  }

  # Checking for NAs -----------------------------------------------------------
  # tbl_unit and tbl_unit_updated can reasonably have NAs. All other columns
  # should be filled in. Will ensure that later functions can complete as
  # they should
  location_nas <- as.data.frame(which(is.na(labdat_parameters), arr.ind=TRUE))

  if (!is_empty(which(location_nas$col != 5 & location_nas$col != 6))) {
    print.data.frame(location_nas[(which(location_nas$col != 5 &
                                           location_nas$col != 6)),])
    stop(paste0("Issue with parameters.xlsx input file. ",
                "Cells are missing values. Check cells at positions listed above. ",
                "Check file requirements and parameter data."),
         call. = FALSE)
  }

  # Checking for duplicate rows ------------------------------------------------
  if (anyDuplicated(labdat_parameters) != 0) {
    # Want to eliminate any duplicates as they will create duplicate data in final
    # dataframe
    labdat_parameters <- unique(labdat_parameters)

    write.xlsx(labdat_parameters, path_to_parameters)
  }

}

#' Check weekly data
#'
#' Check data read in with scrape_labdatxls. Ensure that station names, which
#' are assigned within the script and are not assigned using the parameters
#' document, are done correctly such that there is only one set of data values
#' for each combination of datasheet, station, parameter, unit
#'
#' @param new_data_weekly dataframe. Output from scrape_labdatxls
#' @param labdat_parameters dataframe. Slightly processed parameters.xlsx data
#'
#' @return
check_weekly_data <- function(new_data_weekly, labdat_parameters) {

  frequencies <- new_data_weekly %>%
    group_by(datasheet, station, parameter, unit) %>%
    summarise(n = n())

  counts <- summary(as.factor(frequencies$n))

  if (length(counts) != 1) {

    issue_new_data <- frequencies %>%
      ungroup() %>%
      select(-station) %>%
      filter(n != as.integer(names(counts))[1]) %>%
      select(-n)

    issue_parameters <- issue_new_data %>%
      left_join(labdat_parameters,
                by = c("datasheet", "parameter", "unit"),
                multiple = "all") %>%
      select(colnames(labdat_parameters))

    print.data.frame(issue_parameters)
    stop(paste("Issue with the data read-in process. This is most likely to",
               "occur if a new parameter was added to parameters.xlsx that has",
               "a station other than Raw or Clearwell AND does not have the",
               "new station name in the parameter name.\nIf this is the case,",
               "change the parameter name in the lab data file to contain",
               "the station name and update the parameters.xlsx document. See",
               "the issue rows from the parameters.xlsx doc that are printed above.",
               "\nIf this is not the case, contact the tool creator."),
         call. = FALSE)
  }

}
