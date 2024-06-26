#' Convert biological data
#'
#' Identifies biological values stored in per mL by identifying `10^-3` in the
#'  parameters name and converts the values to per L by multiplying by 1000.
#'
#' @param labdat dataframe containing the parsed lab data
#'
#' @return dataframe containing the parsed lab data with the required biological
#'  data values and units converted to per L as required
convert_biocounts <- function(labdat){
  df <- labdat %>%
    mutate(result      = ifelse(parm_tag == "biological" & unit == "per mL",
                                result * 1000, result),
           result_flag = ifelse(parm_tag == "biological" & unit == "per mL",
                                "converted", result_flag),
           parameter   = ifelse(parm_tag == "biological" & unit == "per mL",
                                gsub(" \\(x10\\^-?3\\)", "", parameter),
                                as.character(parameter)),
           unit        = ifelse(parm_tag == "biological" & unit == "per mL",
                                "per litre", unit))
}

#' Convert UV 254 values.
#'
#' Convert UV 254 values from Abs 10cm to Abs 1cm by dividing values by 10
#'
#' @inheritParams convert_biocounts
#'
#' @return dataframe containing the parsed lab data with the required UV 254
#'  data values and units converted to Abs 1cm as required
convert_UV254 <- function(labdat) {

  df <- labdat %>%
    mutate(result      = ifelse(parameter == "UV 254" & unit == "Abs 10cm",
                                result / 10, result),
           result_flag = ifelse(parameter == "UV 254" & unit == "Abs 10cm",
                                "converted", result_flag),
           unit        = ifelse(parameter == "UV 254" & unit == "Abs 10cm",
                                "Abs 1cm", unit))
}

#' Round values stored in result_org and result
#'
#' Round values stored in result_org and result as indicated in the process
#'  document. Required as there exists an issue with number storage such that
#'  when values are read in, some have excess decimal places. Especially
#'  pertinent for values calculated within the lab data sheet
#'
#' @inheritParams convert_biocounts
#'
#' @return dataframe containing the parsed lab data where result_org and result
#'  are recorded with the correct number of decimal points
round_values <- function(labdat) {

  df <- labdat %T>%
    {options(warn = -1)} %>%
          # This mutation handles the issues with number storage
    mutate(result_org = ifelse(grepl("\\d{1,}\\.\\d{5}", result_org),
                               as.character(as.numeric(result_org)),
                               result_org),
           # At this point, there exist more numbers with many decimal places
           # because of the sheet's calculated values. These are each rounded
           # to have the same number of digits are are displayed in the labdats.
           # grepl statement is used so as to only replace digits and not
           # strings such as "uncalculable"
           result_org = ifelse(grepl("\\d{1,}\\.\\d{1}", result_org),
                               case_when(
                                 parameter == "Percent DO" |
                                   parameter == "Total Green & B-G" |
                                   parameter == "Total Chlorine Dose" |
                                   parameter == "Turbidity Log Removal"
                                 ~ as.character(round(as.numeric(result_org), 1)),
                                 parameter == "Langelier Saturation Index 2" |
                                   parameter == "Alum to DOC ratio" |
                                   parameter == "Anion Sum" |
                                   parameter == "Cation Sum" |
                                   parameter == "Ion Percent Difference" |
                                   parameter == "Langelier Index (RTW)"
                                 ~ as.character(round(as.numeric(result_org), 2)),
                                 # Removal values are stored as digits but
                                 # displayed as percent (0.75 vs 75%)
                                 grepl("Removal", parameter, ignore.case = TRUE)
                                 ~ as.character(round(as.numeric(result_org), 1)),
                                 grepl("SUVA", parameter, ignore.case = TRUE)
                                 ~ as.character(round(as.numeric(result_org), 4)),
                                 parameter == "Total THMs" |
                                   parameter == "Aluminum (particulate)" |
                                   parameter == "Dissolved Solids"
                                 ~ as.character(round(as.numeric(result_org), 0)),
                                 TRUE ~ result_org),
                               result_org),
           # Same issue as above (calculated values have many decimal places)
           # exists in the results column, but there are no strings to save
           result     = case_when(parameter == "Percent DO" |
                                   parameter == "Total Green & B-G" |
                                   parameter == "Total Chlorine Dose" |
                                   parameter == "Turbidity Log Removal"
                                 ~ round(result, 1),
                                 parameter == "Langelier Saturation Index 2" |
                                   parameter == "Alum to DOC ratio" |
                                   parameter == "Anion Sum" |
                                   parameter == "Cation Sum" |
                                   parameter == "Ion Percent Difference" |
                                   parameter == "Langelier Index (RTW)"
                                 ~ round(result, 2),
                                 grepl("Removal -", parameter, ignore.case = TRUE)
                                 ~ round(result, 1),
                                 grepl("SUVA", parameter, ignore.case = TRUE)
                                 ~ round(result, 4),
                                 parameter == "Total THMs" |
                                   parameter == "Aluminum (particulate)" |
                                   parameter == "Dissolved Solids"
                                 ~ round(result, 0),
                                 TRUE ~ result)) %T>%
    {options(warn = 0)}

  return(df)
}

#' Handle values above and below detection limit, asterisked data
#'
#' Handle values as defined below:
#' * If value contains `<`, value is below detection limit. Flag and replace with 0
#' * If value contains `>`, value is above detection limit. Flag and replace with NA
#' * If value contains an asterisk, value is flagged with `mod` and replaced with NA
#'
#' @inheritParams convert_biocounts
#'
#' @return dataframe containing the parsed lab data where the result column and
#'  flag column are altered as specified above
replace_dl <- function(labdat){
  dl <- c("[<]", "[>]", "TNTC", "[*]")

  labdat <- labdat %>%
    mutate(result_flag = ifelse(grepl("[<]",  result_org), "bdl", result_flag),
           result_flag = ifelse(grepl("[>]",  result_org), "adl", result_flag),
           result_flag = ifelse(grepl("TNTC", result_org), "adl", result_flag),
           result_flag = ifelse(grepl("[*]",  result_org), "mod", result_flag)) %>%
    mutate(result      = ifelse(grepl("[<]",  result_org), 0,     result),
           result      = ifelse(grepl("[>]",  result_org), NA,    result),
           result      = ifelse(grepl("TNTC", result_org), NA,    result),
           result      = ifelse(grepl("[*]",  result_org), NA,    result))

  return(labdat)
}

#' Update parameter names and units
#'
#' Update parameter names and units as indicated in `parameters.xlsx`. There
#'  exist inconsistencies between years in parameter names and units, including
#'  human error. Update to ensure consistency
#'
#' @inheritParams convert_biocounts
#' @param file_sheet_year numeric value indicating the sheet year
#' @inheritParams scrape_labdatxls
#'
#' @return data frame with updated parameter name, unit
update_parameters <- function(labdat, file_sheet_year,
                              labdat_parameters){

  labdat_mod <- labdat %>%
    left_join(labdat_parameters, by = c("datasheet", "station", "parameter",
                                        "unit"))

  parm_check <- labdat_mod %>%
    filter(is.na(parameter_updated)) %>%
    distinct(datasheet, station, parameter, unit) %>%
    arrange(datasheet)

  if (nrow(parm_check) != 0) {

    stop(print("The following parameters that were read in cannot be identified:\n"),
         print.data.frame(parm_check),
         paste("\nThis issue usually arises because of station assignment.\na)",
               "If the parameter is the Raw Water datasheet,",
               "and is associated with the `PreFM`, 'Train A', or 'Train B'",
               "stations, rename the Parameter in the lab data file such that",
               "the correct station name is within the parameter name. Update",
               "parameters.xlsx accordingly.\nb)",
               "If the parameter is the Clear Well datasheet,",
               "and is associated with the `PreGAC`, 'Channel 1', 'Channel 2',",
               "'PreFM' stations,  rename the Parameter in the lab data file such that",
               "the correct station name is within the parameter name. Update",
               "parameters.xlsx accordingly. If the parameter is a Removal",
               "parameter (ie has 'Removal' in the parameter name), ensure that",
               "the station is listed as 'Combined Stations' within parameters.xlsx.",
               "\nc) If the parameter is in the WTP DOC sheet, ensure that the",
               "station name recorded within the header rows is spelled correctly."),
         call. = FALSE)

  }

  if (file_sheet_year < 2003) {
    labdat_mod <- labdat_mod %>%
      # DOC/TOC values from 1980 to 2002 (inclusive) were measured using the UV
      # absorbance method (Note that this does NOT include GF diss values).
      # Adding "(UV)" to the units to indicate the use of this method.
      # This is not done with the left_join above as DOC/TOC units in the
      # labdat sheets are not accurate
      mutate(unit_updated = ifelse(!grepl("GF", parameter) &
                                     (parameter_updated == "DOC" |
                                        parameter_updated == "TOC"),
                                   paste(unit_updated, "(UV)"),
                                   unit_updated))
  }

  if (file_sheet_year < 2018) {
    # E. coli values before 2018 were measured using an experimental method.
    # Plant was accredited for E. coli measurements in 2018
    labdat_mod <- labdat_mod %>%
      filter(!grepl("E. coli", parameter))
  }

  # Free Chlorine and Combined Chlorine are in the Raw section of yearly
  # spreadsheets but should be stored as Clearwell values
  labdat_mod <- labdat_mod %>%
    select(datasheet, sheet_year, station, date_ymd,
           parameter = parameter_updated, unit = unit_updated,
           parm_eval, parm_tag, result) %>%
    mutate(datasheet = ifelse(parameter == "Free Chlorine" |
                                parameter == "Combined Chlorine",
                              "Clearwell", datasheet),
           station   = ifelse(parameter == "Free Chlorine" |
                                parameter == "Combined Chlorine",
                              "Clearwell", station))

  return(labdat_mod)
}

