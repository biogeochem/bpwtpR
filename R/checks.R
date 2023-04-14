#' Check parameters.xlsx
#'
#' Check parameters.xlsx column names and ensure that duplicate rows are removed
#'
#' @inheritParams scrape_labdatxls
#'
#' @return dataframe containing the corrected parameter information
#'
#' @export
check_parameters_setup <- function(labdat_parameters) {

  labdat_parameters_cols <- c("datasheet",	"station",	"parameter",
                              "parameter_updated",	"unit",
                              "unit_updated",	"parm_unit",
                              "parm_eval",	"parm_tag")

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
                         station   = c("Clearwell", "Raw", "PreGAC/Clearwell",
                                       "PreGAC", "PreFM", "FM",
                                       "Train A", "Train B",
                                       "MMF1", "MMF12", "MMFA", "MMFL",
                                       "Channel", "Channel 1", "Channel 2",
                                       "Combined Stations"),
                         parm_eval = c("measured", "calculated_insheet"),
                         parm_tag  = c("majorConstituents", "operations",
                                       "traceConstituents", "physical", "THM",
                                       "biological", "bacteriological"))

  for (i in 1:length(unlist(correct_inputs))) {
    gsub(unlist(correct_inputs)[i], unlist(correct_inputs)[i],
         labdat_parameters$datasheet,
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

  # Checking DOC profile parameters --------------------------------------------
  labdat_parameters_doc <- filter(labdat_parameters, datasheet == "DOCProfile")

  # Script expects a sheet setup such that units are listed within the header
  # columns and not on their own. Script later updates units by matching
  # parameter names. There should be no values in the units col
  if (any(!is.na(labdat_parameters_doc$unit))) {
    stop(paste("Issue with parameters.xlsx input file. Expected WTP DOC sheet",
               "setup includes no row of column solely for storing parameter",
               "units. All units should be set to NA. Enter the desired unit",
               "into the unit_updated column.",
               "Check file requirements and parameter data."))
  }

  # Check removal parameters ---------------------------------------------------
  labdat_parameters_removal <- filter(labdat_parameters, grepl("Removal",
                                                               labdat_parameters$parameter,
                                                               ignore.case = TRUE))

  if (any(labdat_parameters_removal$station != "Combined Stations")) {
    warning(paste("All parameters with 'Removal' in the parameter name are",
                  "assumed to require multiple station data to be calculated",
                  "and should therefore be given the station 'Combined Stations'.",
                  "\nAltering parameters.xlsx accordingly and rewriting the",
                  "file."))

    labdat_parameters <- labdat_parameters %>%
      mutate(station = case_when(grepl("Removal", parameter, ignore.case = TRUE) &
                                   station != "Combined Stations"
                                 ~ "Combined Stations",
                                 TRUE ~ station))
  }

  # Checking for duplicate rows ------------------------------------------------
  if (anyDuplicated(labdat_parameters) != 0) {
    # Want to eliminate any duplicates as they will create duplicate data in final
    # dataframe
    warning("Duplicate rows were identified in parameters.xlsx. Deleting duplicates.")

    labdat_parameters <- unique(labdat_parameters)
  }

  write_xlsx(labdat_parameters, path_to_parameters)

  return(labdat_parameters)

}

#' Identify which Raw/Clearwell parameters are missing in parameters.xlsx
#'
#' Inform the user that certain rows from the lab data weekly data sheet
#' are not read in by the script.
#'
#' @inheritParams prepare_labdat
#'
#' @return Print message showing user the next action steps
#'
#' @export
check_parameters_rwcw <- function(path_to_labdat_file, path_to_parameters) {

  labdat_parameters <- path_to_parameters %>%
    read_parameters() %>%
    check_parameters_setup()

  skip_num <- 7

  spreadsheet <- read_weekly(path_to_labdat_file, skip_num)

  clearwell_start <- which(spreadsheet$Parameters == "CLEAR WELL")
  clearwell_end   <- first(which(grepl(x = spreadsheet$Parameters,
                                       pattern = "ion balance",
                                       ignore.case = TRUE)))

  new_data_weekly <- scrape_labdatxls(spreadsheet, labdat_parameters)

  # To keep track of which position missing rows exist so that user can better
  # see where they are in the lab data file
  spreadsheet <- spreadsheet %>%
    mutate(`Row Number` = row_number() + skip_num + 1) %>%
    select(`Row Number`, Parameters)

  # Usually, section headers are identified by all characters being in CAPS.
  # These can be ignored when determining which Parameters were not read in
  missing_params <- data.frame(
    Parameters = str_subset(setdiff(spreadsheet$Parameters,
                                    new_data_weekly$parameter),
                            "^(([[:upper:]]*\\d*[[:punct:]]*)\\s?)*$",
                            negate = TRUE)) %>%
    left_join(spreadsheet, multiple = "all", by = "Parameters") %>%
    select(`Row Number`, Parameters) %>%
    arrange(`Row Number`)

  if (!is_empty(missing_params)) {
    print(paste("The following rows listed in the 'Parameters'",
                "column of the lab data are not read in."))
    print("These come from the RAW WATER section of the sheet:")
    print.data.frame(filter(missing_params, `Row Number` < clearwell_start))
    print("These come from the CLEAR WELL section of the sheet:")
    print.data.frame(filter(missing_params, `Row Number` >= clearwell_start &
                              `Row Number` < clearwell_end))
    cat(paste("Do any of these rows contain data that you need?",
              "If yes, add that parameter into parameters.xlsx and try again.",
              "\nNOTE: If new parameters have been added within the",
              "\n\tClearwell Al,\n\tClearwell THM,\n\tIon,\n\tEnd",
              "\nsections of the sheet, you must speak with the creator of",
              "this tool to ensure that that data is added into the",
              "database file.\nYou can ONLY add new parameters if they",
              "are within the RAW WATER or CLEAR WELL sections of the",
              "weekly data."))
  }

}

#' Identify which DOC Profile parameters are missing in parameters.xlsx
#'
#' Inform the user that certain rows from the lab data DOC Profile sheet
#' are not read in by the script.
#'
#' @inheritParams prepare_labdat
#'
#' @return Print message showing user the next action steps
#'
#' @export
check_parameters_doc <- function(path_to_labdat_file, path_to_parameters) {
  labdat_parameters <- path_to_parameters %>%
    read_parameters() %>%
    check_parameters_setup() %>%
    filter(datasheet == "DOCProfile")

  spreadsheet <- read_doc(path_to_labdat_file)

  missing_params <- setdiff(colnames(spreadsheet), labdat_parameters$parameter)

  missing_params <- missing_params[grep("date", missing_params,
                                        ignore.case = TRUE, invert = TRUE)]

  if (!is_empty(missing_params)) {
    print(sprintf("The following columns from the DOC profile are not read in: %s",
            paste(sprintf("%s", missing_params), collapse=", ")))
    cat(paste("Note that columns are named by combining all of the header row ",
              "text together (including the contents of merged cells).",
              "\nDo any of these columns contain data that you need?",
              "If yes, add that parameter into parameters.xlsx and try again."))
  }

}

#' Check scraped data
#'
#' Check data read in with scrape_labdatxls by and scrape_docprofiles. Ensure
#' that scrapes were done correctly such that there is only one set of data
#' values for each combination of datasheet, station, parameter, unit
#'
#' @param new_data dataframe. Output from scrape_labdatxls
#' @param labdat_parameters dataframe. Slightly processed parameters.xlsx data
#'
#' @return Print message showing user the next action steps
check_scraped_data <- function(new_data, labdat_parameters) {

  frequencies <- new_data %>%
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

    if (all(new_data$datasheet == "DOCProfile")) {
      stop(paste("Issue with the DOC Profile data read-in process. This is most",
                 "likely to occur if the DOC Profile sheet setup was altered",
                 "such that this tool does not know how to read in the data.",
                 "\nIf this is the case, change the DOC profile setup to match",
                 "the file requirements. See the issue rows from the",
                 "parameters.xlsx doc that are printed above.",
                 "\nIf this is not the case, contact the tool creator."),
           call. = FALSE)
    } else {
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

  return(new_data)

}

