#' Calculate all values
#'
#' Calculate desired values
#'
#' @inheritParams update_parameters
#'
#' @return dataframe containing all calculated values. Note that the results of
#'  `determine_NA_NC()` are employed, and that parameters for which a value
#'  cannot be calculated are assigned NA or NaN, as defined in the function
#'  description
apply_calculations <- function(labdat, file_sheet_year){

  column_names <- c("datasheet", "sheet_year", "station", "date_ymd",
                    "parameter", "unit", "parm_tag", "result")

  calculated_values <- bind_rows(al_particulate(column_names, labdat),
                                 suva(column_names, labdat),
                                 PACl_values <- PACl(column_names, labdat),
                                 PACl_DAE_values <- PACl_DAE(column_names, PACl_values),
                                 overall_coag <- overall_coag_dose(column_names, labdat, PACl_DAE_values),
                                 tot_chlorine_dose(column_names, labdat),
                                 ion_balance(column_names, labdat),
                                 alum_DOC_ratio(column_names, labdat, overall_coag),
                                 total_THMs(column_names, labdat),
                                 DO_percent(column_names, labdat),
                                 # From 1980-2017, DS values are calculated
                                 # From 2006 onwards, DS values are measured
                                 # gravimetrically (are therefore TDS values).
                                 # Note: from 2006-2017, both calc and meas
                                 # values are kept
                                 (if(file_sheet_year <= 2017)
                                   dissolved_solids(column_names, labdat)
                                 ),
                                 # From 1980-2017, LSI RTW values are used
                                 # From 2018 onwards, LSI1 and LSI2 are
                                 # calculated and langelier_SatIndex() is called
                                 (if(!"Langelier Index (RTW)" %in% labdat$parameter)
                                   langelier_SatIndex(column_names, labdat)
                                 ),
                                 turbidity_logRemoval(column_names, labdat),
                                 tot_B_BG_algae(column_names, labdat),
                                 removal_coag_filt(column_names, labdat, "TOC", "mg/L C"),
                                 removal_GACfilt(  column_names, labdat, "TOC", "mg/L C"),
                                 removal_overall(  column_names, labdat, "TOC", "mg/L C"),
                                 removal_coag_filt(column_names, labdat, "DOC", "mg/L C"),
                                 removal_GACfilt(  column_names, labdat, "DOC", "mg/L C"),
                                 removal_overall(  column_names, labdat, "DOC", "mg/L C"),
                                 removal_coag_filt(column_names, labdat, "Odour", "T.O.N."),
                                 removal_overall(  column_names, labdat, "Odour", "T.O.N.")) %>%
    mutate(parm_eval = "calculated_inscript")

  return(calculated_values)

}

# Trace Constituents -------
#' Calculate aluminum particulate
#'
#' Calculate aluminum particulate, equal to total aluminum - dissolved aluminum
#'
#' @inheritParams update_parameters
#' @param column_names string vector defined in apply_calculations. The desired final
#'  columns for each calculated dataframe
#'
#' @return numeric vector. The calculated values
al_particulate <- function(column_names, labdat) {

  parms <- c("Aluminum (total)", "Aluminum (dissolved)")

  cols <- det_cols(parms)

  df <- labdat %>%
    filter(parameter %in% parms,
           unit == "\u00b5g/L",
           station %in% c("Raw", "Clearwell")) %>%
    pivot_wider(names_from = parameter, values_from = result,
                id_cols = datasheet:date_ymd) %>%
    # In case if any of the required parameters are missing
    handle_missing_cols(cols) %>%
    determine_NA_NC(5, 6) %>%
    mutate(result = case_when(!is.na(result)
                                ~ `Aluminum (total)` - `Aluminum (dissolved)`,
                              TRUE
                                ~ result),
           result = round(result, 2),
           parameter = "Aluminum (particulate)", unit = "\u00b5g/L",
           parm_tag  = "traceConstituents") %>%
    select(all_of(column_names))

}

#' Calculate SUVA
#'
#' Calculate SUVA, equal to (UV254)*100/DOC
#'
#' @inheritParams al_particulate
#'
#' @return numeric vector. The calculated values
suva <- function(column_names, labdat) {

  parms <- c("DOC", "UV 254")

  cols <- det_cols(parms)

  df <- labdat %>%
    filter(((parameter %in% parms[1] & unit == "mg/L C") |
            (parameter %in% parms[2] & unit == "Abs 1cm")),
            station %in% c("Raw", "Clearwell")) %>%
    pivot_wider(names_from = parameter, values_from = result,
                id_cols = datasheet:date_ymd) %>%
    # In case if any of the required parameters are missing
    handle_missing_cols(cols) %>%
    determine_NA_NC(5, 6) %>%
    mutate(result = case_when(!is.na(result)
                              ~ `UV 254` * 100 / DOC,
                              TRUE
                              ~ result),
           result = round(result, 4),
           parameter = "SUVA", unit = "L/(mg-m)",
           parm_tag  = "traceConstituents") %>%
    select(all_of(column_names))

}

# Operations ---------------
#' Calculate PACl
#'
#' Calculate PACl, equal to (PACl Train A + PACl Train B)/2
#'
#' @inheritParams al_particulate
#'
#' @return numeric vector. The calculated values
PACl <- function(column_names, labdat) {

  # Columns required for proper calculation of df. Is different than what is
  # determined using det_cols because of the pivot using parameter AND station
  cols <- c(sheet_year = NA_real_, date_ymd = NA_real_,
            `PACl_Train A` = NA_real_, `PACl_Train B` = NA_real_)

  parms <- c("PACl")

  station_list <- c("Train A", "Train B")

  df <- labdat %>%
    filter(parameter %in% parms, station %in% station_list,
           datasheet == "Raw",
           unit == "mg/L") %>%
    pivot_wider(names_from = c(parameter, station), values_from = result,
                id_cols = c("sheet_year","date_ymd")) %>%
    # In case if any of the required parameters are missing
    handle_missing_cols(cols) %>%
    group_by(sheet_year, date_ymd) %>%
    summarise(result = mean(c(`PACl_Train A`, `PACl_Train B`), na.rm = TRUE)) %>%
    mutate(result = ifelse(is.nan(result), NA, result),
           result = round(result, 2),
           datasheet = "Raw",
           station = "Raw",
           parameter = "PACl", unit = "mg/L",
           parm_tag = "operations") %>%
    select(all_of(column_names))

}

#' Calculate PACl DAE
#'
#' Calculate PACl as dry alum equivalent, equal to PACl\*2.07
#'
#' @inheritParams al_particulate
#' @param PACl_values numeric vector. The values calculated by `PACl()`
#'
#' @return dataframe containing all required columns, including the calculated
#'  result. Note that the results of `determine_NA_NC()` are employed, and that
#'  parameters for which a value cannot be calculated are assigned NA or NaN, as
#'  defined in the function description
PACl_DAE <- function(column_names, PACl_values) {

  df <- PACl_values %>%
    mutate(result = .data$result*2.07,
           result = round(.data$result, 2),
           unit ="mg/L DAE") %>%
    select(all_of(column_names))

}

#' Calculate the overall coagulant dose
#'
#' Calculate the overall coagulant dose, equal to:
#' * PACl DAE, if both train values exist
#' * (PACl Train X\*2.07)+Alum)/2, if only one train value exists
#' * Alum, if neither train value exists
#'
#' @inheritParams al_particulate
#' @param PACl_DAE_values numeric vector. The values calculated by
#'  `PACl_DAE()`
#'
#' @return dataframe containing all required columns, including the calculated
#'  result. Note that the results of `determine_NA_NC()` are employed, and that
#'  parameters for which a value cannot be calculated are assigned NA or NaN, as
#'  defined in the function description
overall_coag_dose <- function(column_names, labdat, PACl_DAE_values) {

  parms <- c("Alum")
  # Columns required for proper calculation of df
  cols <- c(sheet_year = NA_real_, date_ymd = NA_real_,
            Alum  = NA_real_, PACl_DAE  = NA_real_)

  df <- labdat %>%
    filter(parameter %in% parms, station == "Raw", unit == "mg/L") %>%
    pivot_wider(names_from = parameter, values_from = result,
                id_cols = datasheet:date_ymd) %>%
    left_join(PACl_DAE_values, by = c("datasheet", "sheet_year", "station",
                                      "date_ymd")) %>%
    rename(PACl_DAE = result) %>%
    # In case if any of the required parameters are missing
    handle_missing_cols(cols) %>%
    determine_NA_NC(3, 4) %>%
    group_by(sheet_year, date_ymd) %>%
    # If Alum DNE, result = DAE;
    # If one train value is missing, result = mean(DAE, Alum);
    # If both train values missing, result = Alum.
    # Above cases are equivalent to the mean equation used below (na.rm
    # necessary). Value should be calculated when determine_NA_NC returns
    # NaN or 999 (both are numeric)
    summarise(result = case_when(is.nan(result) | result == 999
                                   ~ mean(c(PACl_DAE, Alum), na.rm = TRUE),
                                 TRUE
                                   ~ result)) %>%
    mutate(result = round(result, 2),
           datasheet = "Raw", station = "Raw",
           parameter = "Overall Coagulant Dose", unit = "mg/L DAE",
           parm_tag = "operations") %>%
    select(all_of(column_names))

}

#' Calculate total chlorine dose
#
#' Calculate the total chlorine dose, equal to the sum of chlorine pre,
#'  intermed, and post
#'
#' @inheritParams al_particulate
#'
#' @return dataframe containing all required columns, including the calculated
#'  result. Note that the results of `determine_NA_NC()` are employed, and that
#'  parameters for which a value cannot be calculated are assigned NA or NaN, as
#'  defined in the function description
tot_chlorine_dose <- function(column_names, labdat) {

  parms <- c("Chlorine-pre", "Chlorine-intermed", "Chlorine-post")
  # Columns required for proper calculation of df
  cols <- det_cols(parms)

  df <- labdat %>%
    filter(parameter %in% parms, unit == "mg/L") %>%
    pivot_wider(names_from = parameter, values_from = result,
                id_cols = datasheet:date_ymd) %>%
    # In case if any of the required parameters are missing
    handle_missing_cols(cols) %>%
    mutate(result = rowSums(.[,c(5:7)], na.rm = TRUE),
           result = replace(result, result == 0, NA),
           result = round(result, 1),
           parameter = "Total Chlorine Dose", unit = "mg/L",
           parm_tag = "operations") %>%
    select(all_of(column_names))

}

#' Calculate the ion balance
#
#' Calculate the ion balance, equal to:
#' * anion sum = Sulphate\*0.0208 + Chloride\*0.0282 + Bicarbonate\*0.0164 +
#'  Carbonate\*0.0333
#' * cation sum = Calcium\*0.0499 + Magnesium\*0.0822 + Sodium\*0.0435 +
#'  Potassium\*0.0256 + Silica\*0.02629
#' * ion percent difference = (cation sum - anion sum)/(cation sum + anion sum)\*100
#'
#' @inheritParams al_particulate
#'
#' @return dataframe containing all required columns, including the calculated
#'  result. Note that the results of `determine_NA_NC()` are employed, and that
#'  parameters for which a value cannot be calculated are assigned NA or NaN, as
#'  defined in the function description
ion_balance <- function(column_names, labdat){

  parms <- c("Sulphate","Chloride","Bicarbonate", "Carbonate",
             "Calcium", "Magnesium", "Sodium", "Potassium","Silica (SiO3)")
  # Columns required for proper calculation of df
  cols <- det_cols(parms)

  df <- labdat %>%
    filter(parameter %in% parms, unit == "mg/L") %>%
    # Only for Carbonate are values == 0 acceptable
    mutate(result = ifelse(parameter != "Carbonate" & result == 0, NA, result),
           result = ifelse(parameter == "Carbonate" & is.na(result), 0, result)) %>%
    pivot_wider(names_from = parameter, values_from = result,
                id_cols = datasheet:date_ymd) %>%
    # In case if any of the required parameters are missing
    handle_missing_cols(cols) %>%
    determine_NA_NC(5, 7, include_between = TRUE, col_name = "anion") %>%
    determine_NA_NC(9, 13, include_between = TRUE, col_name = "cation") %>%
    mutate(anion_sum  = case_when(!is.na(anion) ~ (Sulphate * 0.0208) + (Chloride * 0.0282) +
                                    (Bicarbonate * 0.0164) + (Carbonate * 0.0333),
                                  TRUE ~ anion),
           cation_sum = case_when(!is.na(cation) ~ (Calcium * 0.0499) + (Magnesium * 0.0822) +
                                    (Sodium * 0.0435) + (Potassium * 0.0256) +
                                    (`Silica (SiO3)` * 0.02629),
                                  TRUE ~ cation)) %>%
    determine_NA_NC(14, 15, col_name = "ion_percdiff") %>%
    mutate(ion_percdiff = case_when(!is.na(ion_percdiff) ~ ((cation_sum - anion_sum)/(cation_sum + anion_sum)) * 100,
                                    TRUE ~ ion_percdiff)) %>%
    pivot_longer(cols = c("anion_sum", "cation_sum", "ion_percdiff"),
                 names_to = "parameter", values_to = "result") %>%
    mutate(parameter = case_when(parameter == "anion_sum" ~ "Anion Sum",
                                 parameter == "cation_sum" ~ "Cation Sum",
                                 parameter == "ion_percdiff" ~ "Ion Percent Difference")) %>%
    arrange(parameter) %>%
    mutate(unit = ifelse(parameter == "Anion Sum" | parameter == "Cation Sum",
                         "meq/L", "%"),
           parm_tag = "operations",
           result = round(result, 2)) %>%
    select(all_of(column_names))

}

#' Calculate alum to DOC ratio
#'
#' Calculate alum to DOC ratio, equal to Alum/DOC
#'
#' @inheritParams al_particulate
#' @param overall_coag dataframe. Output from overall_coag_dose()
#'
#' @return numeric vector. The calculated values
alum_DOC_ratio <- function(column_names, labdat, overall_coag) {

  # If overall_coag dose exists, use that instead of Alum
  if (nrow(overall_coag) != 0) {
    parms <- c("DOC")

    # Columns required for proper calculation of df
    cols <- c(sheet_year = NA_real_, date_ymd = NA_real_,
              DOC  = NA_real_, overall_coag  = NA_real_)

    df <- labdat %>%
      filter(((parameter %in% parms[1] & unit == "mg/L C")),
             station == "Raw") %>%
      pivot_wider(names_from = parameter, values_from = result,
                  id_cols = datasheet:date_ymd) %>%
      left_join(overall_coag, by = c("datasheet", "sheet_year", "station", "date_ymd")) %>%
      mutate(overall_coag = result) %>%
      # In case if any of the required parameters are missing
      handle_missing_cols(cols) %>%
      determine_NA_NC(3, 4) %>%
      mutate(result = case_when(!is.na(result) ~ overall_coag/DOC,
                                TRUE ~ result),
             result = round(result, 2),
             datasheet = "Raw", station = "Raw",
             parameter = "Alum to DOC ratio", unit = "ratio",
             parm_tag  = "operations") %>%
      select(all_of(column_names))

  } else {
    parms <- c("Alum", "DOC")

    cols <- det_cols(parms)

    df <- labdat %>%
      filter(((parameter %in% parms[1] & unit == "mg/L") |
              (parameter %in% parms[2] & unit == "mg/L C")),
             station == "Raw") %>%
      pivot_wider(names_from = parameter, values_from = result,
                  id_cols = datasheet:date_ymd) %>%
      # In case if any of the required parameters are missing
      handle_missing_cols(cols) %>%
      determine_NA_NC(5, 6) %>%
      mutate(result = case_when(!is.na(result) ~ Alum/DOC,
                                TRUE ~ result),
             result = round(result, 2),
             parameter = "Alum to DOC ratio", unit = "ratio",
             parm_tag  = "operations") %>%
      select(all_of(column_names))
  }

  return(df)
}

# THM ---------------------

#' Calculate the total THMs
#
#' Calculate the total THMs, equal to the sum of bromodichloromethane,
#' bromoform, chlorodibromomethane, chloroform
#'
#' @inheritParams al_particulate
#'
#' @return dataframe containing all required columns, including the calculated
#'  result. Note that the results of `determine_NA_NC()` are employed, and that
#'  parameters for which a value cannot be calculated are assigned NA or NaN, as
#'  defined in the function description
total_THMs <- function(column_names, labdat) {

  parms <- c("Bromodichloromethane", "Bromoform",
             "Chlorodibromomethane", "Chloroform")

  df <- labdat %>%
    filter(parameter %in% parms, unit == "\u00b5g/L") %>%
    group_by(datasheet, sheet_year, station, date_ymd) %>%
    summarise(result = ifelse(sum(result, na.rm = TRUE) > 0,
                              sum(result, na.rm = TRUE), NA)) %>%
    mutate(parameter = "Total THMs", unit = "\u00b5g/L",
           parm_tag = "THM") %>%
    select(all_of(column_names))

}

# Physical ---------------
#' Calculate percent DO
#
#' Calculate the precent dissolved oxygen, equal to DO/oxygen solubility*100
#'
#' @inheritParams al_particulate
#'
#' @return dataframe containing all required columns, including the calculated
#'  result. Note that the results of `determine_NA_NC()` are employed, and that
#'  parameters for which a value cannot be calculated are assigned NA or NaN, as
#'  defined in the function description
DO_percent <- function(column_names, labdat) {

  O2Table <- bpwtpR:::O2Table

  # O2Table was sent by Blair and only accounts for 0.1 degree C jumps between
  # temperature values. (O2Table is used to calculate DO percent)
  temp_vals <- labdat %>%
    filter(parameter == "Temperature",
           round(result, digits = 1) != result)

  if (nrow(filter(temp_vals, round(result, digits = 1) != result)) != 0) {
    message(paste("The tool has detected temperature values with greater than 1",
                  "decimal place. Note that tool uses a lookup table and",
                  "requires that temperature values have 1 decimal place to",
                  "accurately recalculate DO percent.\nThe final tool output",
                  "will still store the DO percent values from the lab data",
                  "file but will not contain any script-calculated DO percent",
                  "values."))
  }

  parms <- c("Temperature", "Diss. Oxygen", "Bench Diss. Oxygen")
  # Columns required for proper calculation of df
  cols <- det_cols(parms)

  df <- labdat %>%
    filter(parameter == "Temperature" | parameter == "Diss. Oxygen" |
             parameter == "Bench Diss. Oxygen", unit == "\u00b0C" | unit == "mg/L") %>%
    pivot_wider(id_cols = "datasheet":"date_ymd",
                names_from = "parameter", values_from = "result")  %>%
    handle_missing_cols(cols) %>%
    left_join(O2Table, by = c("Temperature" = "temperature_C")) %>%
    # These two parameters are equivalent but are given different names
    mutate(DO = case_when(datasheet == "Raw" ~ `Bench Diss. Oxygen`,
                          datasheet == "Clearwell" ~ `Diss. Oxygen`)) %>%
    determine_NA_NC(8, 9) %>%
    mutate(result = case_when(!is.na(.data$result) ~ DO / oxy.sol_mg.L * 100,
                              TRUE ~ .data$result),
           result = round(.data$result, 1),
           parameter = "Percent DO", unit = "%",
           parm_tag = "physical") %>%
    select(all_of(column_names))

}

#' Calculate the dissolved solids
#
#' Calculate the dissolved solids, equal to the sum of bicarbonate, carbonate,
#'  calcium, magnesium, sodium, potassium, sulphate, chloride, silica
#'
#' @inheritParams al_particulate
#'
#' @return dataframe containing all required columns, including the calculated
#'  result. Note that the results of `determine_NA_NC()` are employed, and that
#'  parameters for which a value cannot be calculated are assigned NA or NaN, as
#'  defined in the function description
dissolved_solids <- function(column_names, labdat) {

  parms <- c("Carbonate", "Sulphate", "Chloride", "Bicarbonate",
             "Calcium", "Magnesium", "Sodium", "Potassium",  "Silica (SiO3)")
  # Columns required for proper calculation of df
  cols <- det_cols(parms)

  df <- labdat %>%
    filter(parameter %in% parms, unit == "mg/L", station %in% c("Raw", "Clearwell")) %>%
    mutate(result = gsub(x = result, pattern = "<", replacement = NA),
           result = as.numeric(result),
           # Only for Carbonate are values == 0 acceptable
           result = ifelse(parameter != "Carbonate" & result <= 0, NA, result),
           result = ifelse(parameter == "Carbonate" & is.na(result), 0, result)) %>%
    pivot_wider(names_from = parameter, values_from = result,
                id_cols = datasheet:date_ymd) %>%
    # In case if any of the required parameters are missing
    handle_missing_cols(cols) %>%
    determine_NA_NC(6, 13, include_between = TRUE) %>%
    mutate(result = case_when(!is.na(result) ~ Bicarbonate + Carbonate + Calcium +
                                Magnesium + Sodium + Potassium + Sulphate +
                                Chloride + `Silica (SiO3)`,
                              TRUE ~ result),
           result = round(result, 0),
           parameter = "Dissolved Solids", unit = "mg/L",
           parm_tag = "physical") %>%
    select(all_of(column_names))

}

#' Calculate the LSI
#
#' Calculate the Langelier Saturation Indices 1 and 2
#'
#' @inheritParams al_particulate
#'
#' @return dataframe containing all required columns, including the calculated
#'  result. Note that the results of `determine_NA_NC()` are employed, and that
#'  parameters for which a value cannot be calculated are assigned NA or NaN, as
#'  defined in the function description
langelier_SatIndex <- function(column_names, labdat){

  parms <- c("TDS", "Temperature", "pH", "Alkalinity (total)", "Calcium")
  # Columns required for proper calculation of df
  cols <- det_cols(parms)
  station_list <- c("Clearwell", "Raw")

  df <- labdat %>%
    filter(((parameter == "TDS" & unit == "mg/L") |
             (parameter == "Temperature" & unit == "\u00b0C") |
             (parameter == "pH" & unit == "pH units") |
             (parameter == "Alkalinity (total)" & unit == "mg/L CaCO3") |
             (parameter == "Calcium" & unit == "mg/L")),
           station %in% station_list) %>%
    pivot_wider(names_from = parameter, values_from = result,
                id_cols = datasheet:date_ymd) %>%
    # In case if any of the required parameters are missing
    handle_missing_cols(cols) %>%
    determine_NA_NC(5, 9, include_between = TRUE, col_name = "NA_NC") %>%
    mutate(LSI1_A = 2.24961 - (0.017853 * Temperature) +
             (0.00008238 * Temperature^2) - (0.00000041 * Temperature^3),
           LSI1_m = 0.000025 * TDS,
           LSI1_B = ifelse(TDS < 500, 9.7 + ((2.5 * LSI1_m^0.5)/
                                               (1.0 + 5.3 * LSI1_m^0.5 + 5.5 * LSI1_m)),
                           10),
           LSI1_ph_calc = LSI1_A + LSI1_B - log10(Calcium/0.4) -
                            log10(`Alkalinity (total)`),
           LSI1 = case_when(!is.na(NA_NC) ~ pH - LSI1_ph_calc,
                            TRUE ~ NA_NC)) %>%
    mutate(LSI2_A = (log10(TDS) - 1)/10,
           LSI2_B = -13.12 * log10(Temperature + 273)+ 34.55,
           LSI2_C = log10(Calcium/0.4) - 0.4,
           LSI2_D = log10(`Alkalinity (total)`),
           LSI2_ph_calc = (9.3 + LSI2_A + LSI2_B) - (LSI2_C + LSI2_D),
           LSI2 = case_when(!is.na(NA_NC) ~ pH - LSI2_ph_calc,
                            TRUE ~ NA_NC)) %>%
    pivot_longer(cols = c("LSI1", "LSI2"),
                 names_to = "parameter", values_to = "result") %>%
    mutate(parameter = ifelse(parameter == "LSI1", "Langelier Saturation Index 1",
                              ifelse(parameter == "LSI2", "Langelier Saturation Index 2", NA)),
           unit = "pH units",
           parm_tag = "physical",
           result = round(result, 2)) %>%
    arrange(datasheet, parameter, date_ymd) %>%
    select(all_of(column_names))

}

#' Calculate the turbidity log removal
#
#' Calculate the turbidity log removal, equal to
#'  log10(raw turbidity/Clearwell turbidity)
#'
#' @inheritParams al_particulate
#'
#' @return dataframe containing all required columns, including the calculated
#'  result. Note that the results of `determine_NA_NC()` are employed, and that
#'  parameters for which a value cannot be calculated are assigned NA or NaN, as
#'  defined in the function description
turbidity_logRemoval <- function(column_names, labdat) {

  # Columns required for proper calculation of df
  cols <- c(date_ymd = NA_real_, sheet_year = NA_real_,
            RW_Turbidity = NA_real_, CW_Turbidity = NA_real_)

  parms <- c("Turbidity")

  df <- labdat %>%
    filter(parameter %in% parms, unit == "NTU", station %in% c("Raw", "Clearwell")) %>%
    mutate(parameter = ifelse(datasheet == "Raw",
                              "RW_Turbidity",
                              "CW_Turbidity")) %>%
    pivot_wider(names_from = parameter, values_from = result,
                id_cols = c(date_ymd, sheet_year)) %>%
    # In case if any of the required parameters are missing
    handle_missing_cols(cols) %>%
    determine_NA_NC(3, 4) %>%
    mutate(result = case_when(!is.na(result) ~ log10(RW_Turbidity / CW_Turbidity),
                              TRUE ~ result),
           result = ifelse(is.infinite(result), NA, result),
           result = round(result, 2),
           datasheet = "Clearwell", station = "Combined Stations",
           parameter = "Turbidity Log Removal", unit = "ratio",
           parm_tag = "physical") %>%
    select(all_of(column_names))

}

# Biological ----------

#' Calculate total B+BG algae
#'
#' Calculate the total blue plus blue green algae, equal blue green algae +
#'  green algae
#'
#' @inheritParams al_particulate
#'
#' @return numeric vector. The calculated values
tot_B_BG_algae <- function(column_names, labdat) {

  parms <- c("Blue Green Algae", "Green Algae")
  cols <- det_cols(parms)

  df <- labdat %>%
    filter(parameter %in% parms,
           unit == "per litre",
           station %in% c("Raw", "Clearwell")) %>%
    pivot_wider(names_from = parameter, values_from = result,
                id_cols = datasheet:date_ymd) %>%
    # In case if any of the required parameters are missing
    handle_missing_cols(cols) %>%
    determine_NA_NC(5, 6) %>%
    mutate(result = case_when(!is.na(result)
                                ~ `Blue Green Algae` + `Green Algae`,
                              TRUE
                                ~ result),
           result = round(result, 1),
           parameter = "Total Green & B-G Algae", unit = "per litre",
           parm_tag  = "biological") %>%
    select(all_of(column_names))
}

# Removal functions ------------------------------------------------------------

#' Calculate removal by coagulation, filtration
#'
#' Calculate removal by coagulation, filtration. Equal to:
#'  * percent yield between the raw and Clearwell values if Clearwell values
#'    exist
#'  * percent yield between the raw and PreGAC values if Clearwell values do not
#'    exist
#'
#' @inheritParams al_particulate
#' @param parameter string. Parameter for which to calculate removal by
#'  coagulation, filtration. Likely one of: DOC, TOC, Odour
#' @param units string. The possible unit(s) for the parameter of interest
#'
#' @return dataframe containing all required columns, including the calculated
#'  result. Note that the results of `determine_NA_NC()` are employed, and that
#'  parameters for which a value cannot be calculated are assigned NA or NaN, as
#'  defined in the function description
removal_coag_filt <- function(column_names, labdat, parameter, units){

  # Columns required for proper calculation of df
  cols <- c(sheet_year = NA_real_, date_ymd = NA_real_,
            Raw = NA_real_, Clearwell = NA_real_, PreGAC = NA_real_)

  parms <- c(parameter)
  station_list = c("Raw", "Clearwell", "PreGAC")

  df <- labdat %>%
    filter(parameter %in% parms, station %in% station_list, unit == units) %>%
    select(station, sheet_year, date_ymd, result) %>%
    pivot_wider(names_from = station, values_from = result) %>%
    # In case if any of the required parameters are missing
    handle_missing_cols(cols) %>%
    determine_NA_NC(3, 5, col_name = "raw_vs_pregac") %>%
    determine_NA_NC(3, 4, col_name = "raw_vs_clearwell") %>%
    mutate(result = ifelse(is.na(PreGAC) & !is.na(Clearwell),
                      ifelse(!is.na(raw_vs_clearwell),
                             percent_yield(pre = Raw, post = Clearwell),
                             raw_vs_clearwell),
                      ifelse(!is.na(raw_vs_pregac),
                             percent_yield(pre = Raw, post = PreGAC),
                             raw_vs_pregac)),
           result = round(result, 2),
           datasheet = "Clearwell", station = "Combined Stations",
           parm_tag  = paste(ifelse(parameter == "Odour", "physical", "traceConstituents")),
           parameter = paste(parameter, "Removal - Coagulation & Filtration", sep = " "),
           unit      = "%") %>%
    select(all_of(column_names))

}

#' Calculate overall removal
#'
#' Calculate overall filtration. Equal to percent yield between the raw and
#'  Clearwell values
#'
#' @inheritParams al_particulate
#' @inheritParams removal_coag_filt
#'
#' @return dataframe containing all required columns, including the calculated
#'  result. Note that the results of `determine_NA_NC()` are employed, and that
#'  parameters for which a value cannot be calculated are assigned NA or NaN, as
#'  defined in the function description
removal_overall <- function(column_names, labdat, parameter, units){

  # Columns required for proper calculation of df
  cols <- c(sheet_year = NA_real_, date_ymd = NA_real_,
            Raw = NA_real_, Clearwell = NA_real_)

  parms <- c(parameter)
  station_list = c("Raw", "Clearwell")

  df <- labdat %>%
    filter(parameter %in% parms, station %in% station_list, unit == units) %>%
    # DOC Overall is "uncalculable" in labdat files IF the DOC values are 0 or
    # NA. Odour Overall is "uncalculable" in labdat files IF the Odour values
    # are NA. Therefore, DOC values equal to 0 should be converted to NA
    mutate(result = ifelse(result == 0 & parameter == "DOC", NA, result)) %>%
    select(station, sheet_year, date_ymd, result) %>%
    pivot_wider(names_from = station, values_from = result) %>%
    # In case if any of the required parameters are missing
    handle_missing_cols(cols) %>%
    determine_NA_NC(3, 4) %>%
    mutate(result = case_when(!is.na(result) ~ percent_yield(pre = Raw, post = Clearwell),
                              TRUE ~ result),
           result = round(result, 2),
           datasheet = "Clearwell", station = "Combined Stations",
           parm_tag  = paste(ifelse(parameter == "Odour", "physical", "traceConstituents")),
           parameter = paste(parameter, "Removal - Overall", sep = " "),
           unit      = "%") %>%
    select(all_of(column_names))

}

#' Calculate removal by GAC filtration
#'
#' Calculate removal by GAC filtration. Equal to percent yield between the
#'  PreGAC and Clearwell values
#'
#' @inheritParams al_particulate
#' @inheritParams removal_coag_filt
#'
#' @return dataframe containing all required columns, including the calculated
#'  result. Note that the results of `determine_NA_NC()` are employed, and that
#'  parameters for which a value cannot be calculated are assigned NA or NaN, as
#'  defined in the function description
removal_GACfilt <- function(column_names, labdat, parameter, units){

  # Columns required for proper calculation of df
  cols <- c(sheet_year = NA_real_, date_ymd = NA_real_,
            Clearwell = NA_real_, PreGAC = NA_real_)

  parms <- c(parameter)
  station_list = c("Clearwell", "PreGAC")

  df <- labdat %>%
    filter(parameter %in% parms, station %in% station_list, unit == units) %>%
    # In labdat files, values are "uncalculable" based on eqn
    # IF(OR(PreGAC DOC = 0, Clearwell DOC = 0))
    # Empty cells and cells with value 0 return TRUE for this if statement.
    # Therefore, cells with value 0 are converted to NA
    mutate(result = ifelse(result == 0, NA, result)) %>%
    select(station, sheet_year, date_ymd, result) %>%
    pivot_wider(names_from = station, values_from = result) %>%
    # In case if any of the required parameters are missing
    handle_missing_cols(cols) %>%
    determine_NA_NC(3, 4) %>%
    mutate(result = case_when(!is.na(result) ~ percent_yield(pre = PreGAC, post = Clearwell),
                              TRUE ~ result),
           result = round(result, 2),
           datasheet = "Clearwell", station = "Combined Stations",
           unit      = "%",
           parm_tag  = "traceConstituents",
           parameter = paste(parameter, "Removal - GAC Filtration", sep = " ")) %>%
    select(all_of(column_names))

}
