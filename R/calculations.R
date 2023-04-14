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
                    "parameter", "unit",	"parm_eval", "parm_tag",
                    "result")

  calculated_values <- bind_rows(calculate_value(column_names, labdat,
                                      parms = c("Aluminum (total)",
                                                "Aluminum (dissolved)"),
                                      station_list = c("Clearwell", "Raw"),
                                      fn = al_particulate,
                                      tbl_parameter = "Aluminum (particulate)",
                                      tbl_unit = "\u00b5g/L",
                                      tbl_parm_tag = "traceConstituents",
                                      round_to = 2),
                                 calculate_value(column_names, labdat,
                                      parms = c("DOC", "UV 254"),
                                      station_list = c("Raw"),
                                      fn = suva,
                                      tbl_parameter = "SUVA",
                                      tbl_unit = "L/(mg-m)",
                                      tbl_parm_tag = "traceConstituents",
                                      round_to = 4),
                                 PACl_values <- calculate_value(column_names,
                                                                labdat,
                                      parms = c("PACl Train A",
                                                "PACl Train B"),
                                      fn = PACl,
                                      tbl_parameter = "PACl",
                                      tbl_unit = "mg/L",
                                      tbl_parm_tag = "operations",
                                      round_to = 1),
                                 PACl_DAE_values <- PACl_DAE(column_names,
                                                             PACl_values),
                                 overall_coag_dose(column_names, labdat,
                                                   PACl_DAE_values),
                                 tot_chlorine_dose(column_names, labdat),
                                 ion_balance(column_names, labdat),
                                 calculate_value(column_names, labdat,
                                      parms = c("Alum", "DOC"),
                                      station_list = c("Raw"),
                                      fn = alum_DOC_ratio,
                                      tbl_parameter = "Alum to DOC ratio",
                                      tbl_unit = "ratio",
                                      tbl_parm_tag = "operations",
                                      round_to = 2),
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
                                 calculate_value(column_names, labdat,
                                      parms = c("Blue Green Algae",
                                                "Green Algae"),
                                      station_list = c("Raw"),
                                      fn = tot_B_BG_algae,
                                      tbl_parameter = "Total Green & B-G Algae",
                                      tbl_unit = "per litre",
                                      tbl_parm_tag = "biological",
                                      round_to = 1),
                                 removal_coag_filt(column_names, labdat, "TOC"),
                                 removal_GACfilt(  column_names, labdat, "TOC"),
                                 removal_overall(  column_names, labdat, "TOC"),
                                 removal_coag_filt(column_names, labdat, "DOC"),
                                 removal_GACfilt(  column_names, labdat, "DOC"),
                                 removal_overall(  column_names, labdat, "DOC"),
                                 removal_coag_filt(column_names, labdat, "Odour"),
                                 removal_overall(  column_names, labdat, "Odour"))

  return(calculated_values)

}

# General function ------

#' Calculate vector of desired columns
#'
#' Create a numeric vector to store the names of the columns required for the
#'  calculation of interest. Used to later reorganize data and to add columns
#'  that are missing from a dataset but that are required for a calculation.
#'
#' @param parms string vector. The names of the columns specific to the
#'  calculation of interest
#'
#' @return string vector. The names of all the columns of interest for a
#'  calculation of interest (including datasheet, sheet year, station, date, and
#'  whatever other columns are specific to the calculation of interest)
det_cols <- function(parms) {

  cols_ds_to_dt <- c("datasheet", "sheet_year", "station", "date_ymd")

  cols <- rep.int(NA_real_, length(parms) + length(cols_ds_to_dt))
  names(cols) <- c(cols_ds_to_dt, parms)

  return(cols)

}

#' Calculate value
#'
#' Calculate a specific value given the specified function
#'
#' @param column_names constant string vector. The desired column names in order
#' @inheritParams update_parameters
#' @param parms string vector. The names of the columns required for the
#'  calculation of interest
#' @param station_list string vector. The stations whose parameters are required
#'  for the calculation of interest. Relevant particularly for calculations that
#'  use parameters that exist for multiple stations but are only required for
#'  certain stations. Default: NULL
#' @param fn function to apply to `parms`
#' @param tbl_parameter string. New calculated parameter name
#' @param tbl_unit string.  New calculated parameter unit
#' @param tbl_parm_tag string. New calculated parameter tag
#' @param round_to numeric. Number of desired decimal places
#'
#' @return dataframe containing all required columns, including the calculated
#'  result.
#'  Note that the results of `determine_NA_NC()` are employed, and that
#'  parameters for which a value cannot be calculated are assigned NA or NaN, as
#'  defined in the function description
calculate_value <- function(column_names, labdat, parms, station_list = NULL,
                            fn,
                            tbl_parameter, tbl_unit, tbl_parm_tag,
                            round_to) {

  cols <- det_cols(parms)

  df <- labdat %>%
    filter(parameter %in% parms) %>%
    filter(if(!is.null(station_list)) station %in% station_list else TRUE) %>%
    pivot_wider(names_from = parameter, values_from = result,
                id_cols = datasheet:date_ymd) %>%
    # In case if any of the required parameters are missing
    handle_missing_cols(cols) %>%
    determine_NA_NC(5, 6) %>%
    mutate(result = case_when(!is.na(result) ~ fn(.),
                              TRUE ~ result),
           result = round(result, round_to),
           parameter = tbl_parameter, unit = tbl_unit,
           parm_eval = "calculated",
           parm_tag  = tbl_parm_tag ) %>%
    select(all_of(column_names))

  return(df)

}

# Trace Constituents -------
#' Calculate aluminum particulate
#'
#' Calculate aluminum particulate, equal to total aluminum - dissolved aluminum
#'
#' @inheritParams update_parameters
#'
#' @return numeric vector. The calculated values
al_particulate <- function(labdat) {
  al_particulate_calculated <- labdat$`Aluminum (total)` - labdat$`Aluminum (dissolved)`
  return(al_particulate_calculated)
}

#' Calculate SUVA
#'
#' Calculate SUVA, equal to (UV254)*100/DOC
#'
#' @inheritParams update_parameters
#'
#' @return numeric vector. The calculated values
suva <- function(labdat) {
  suva_calculated <- labdat$`UV 254` * 100 / labdat$DOC
  return(suva_calculated)
}

#' Calculate SUVA - Coagulation, Filtration
#'
#' @description
#' Calculate SUVA - Coagulation, Filtration, equal to:
#' * (PreGAC UV254)\*100/PreGAC DOC if the PreGAC UV and DOC values exist
#' * (Clearwell UV254)\*100/Clearwell DOC if the PreGAC UV and DOC values do not
#'  exist
#'
#' Blair decided that this value is no longer required. Code kept in case if
#'  it is required in future
#'
#' @inheritParams calculate_value
#'
#' @return dataframe containing all required columns, including the calculated
#'  result. Note that the results of `determine_NA_NC()` are employed, and that
#'  parameters for which a value cannot be calculated are assigned NA or NaN, as
#'  defined in the function description
suva_coag_filt <- function(column_names, labdat){

  # Columns required for proper calculation of df
  cols <- c(sheet_year = NA_real_, date_ymd = NA_real_,
            DOC_Clearwell  = NA_real_, DOC_PreGAC  = NA_real_,
            `UV 254_Clearwell` = NA_real_, `UV 254_PreGAC` = NA_real_)

  parms <- c("DOC", "UV 254")
  station_list <- c("Clearwell", "PreGAC")

  df <- labdat %>%
    filter(parameter %in% parms, station %in% station_list) %>%
    pivot_wider(names_from = c(parameter, station), values_from = result,
                id_cols = c("sheet_year","date_ymd")) %>%
    # In case if any of the required parameters are missing
    handle_missing_cols(cols) %>%
    determine_NA_NC(4, 6, col_name = "PreGAC") %>%
    determine_NA_NC(3, 5, col_name = "Clearwell") %>%
    # If there exist PreGAC UV and DOC values, then the PreGAC calculation is desired
    mutate(result = case_when(!is.na(PreGAC) ~ `UV 254_PreGAC`*10/DOC_PreGAC,
                              # If no PreGAC values exist but there exist Clearwell UV and DOC values,
                              # then the Clearwell calculation is desired
                              !is.na(Clearwell) ~ `UV 254_Clearwell`*10/DOC_Clearwell,
                              TRUE ~ Clearwell),
           result = round(result, 4),
           datasheet = "ClearWell",
           station = case_when(!is.na(PreGAC) ~ "PreGAC",
                               TRUE ~ "Clearwell"),
           parameter = "SUVA - Coagulation, Filtration", unit ="L/(mg-m)",
           parm_eval = "calculated",
           parm_tag = "traceConstituents") %>%
    select(all_of(column_names))

  return(df)

}

# Operations ---------------
#' Calculate PACl
#'
#' Calculate PACl, equal to (PACl Train A + PACl Train B)/2
#'
#' @inheritParams update_parameters
#'
#' @return numeric vector. The calculated values
PACl <- function(labdat) {
  PACl_calculated <- (labdat$`PACl Train A` + labdat$`PACl Train B`)/2
  return(PACl_calculated)
}

#' Calculate PACl DAE
#'
#' Calculate PACl as dry alum equivalent, equal to PACl\*2.07
#'
#' @inheritParams calculate_value
#' @param PACl_values numeric vector. The values calculated by `PACl()`
#'
#' @return dataframe containing all required columns, including the calculated
#'  result. Note that the results of `determine_NA_NC()` are employed, and that
#'  parameters for which a value cannot be calculated are assigned NA or NaN, as
#'  defined in the function description
PACl_DAE <- function(column_names, PACl_values) {

  df <- PACl_values %>%
    mutate(result = .data$result*2.07,
           result = round(.data$result, 1),
           parameter = "PACl",
           unit ="mg/L DAE",
           parm_eval = "calculated", parm_tag = "operations") %>%
    select(all_of(column_names))

  return(df)

}

#' Calculate the overall coagulant dose
#'
#' Calculate the overall coagulant dose, equal to:
#' * PACl DAE, if both train values exist
#' * (PACl Train X\*2.07)+Alum)/2, if only one train value exists
#' * Alum, if neither train value exists
#'
#' @inheritParams calculate_value
#' @param dry_alum_equiv_values numeric vector. The values calculated by
#'  `PACl_DAE()`
#'
#' @return dataframe containing all required columns, including the calculated
#'  result. Note that the results of `determine_NA_NC()` are employed, and that
#'  parameters for which a value cannot be calculated are assigned NA or NaN, as
#'  defined in the function description
overall_coag_dose <- function(column_names, labdat, dry_alum_equiv_values) {

  parms <- c("Alum", "PACl Train A", "PACl Train B")
  # Columns required for proper calculation of df
  cols <- det_cols(parms)

  df <- labdat %>%
    filter(parameter %in% parms) %>%
    pivot_wider(names_from = parameter, values_from = result,
                id_cols = datasheet:date_ymd) %>%
    # In case if any of the required parameters are missing
    handle_missing_cols(cols) %>%
    left_join(dry_alum_equiv_values) %>%
    select(rownames(as.data.frame(cols)), DAE = result) %>%
    # If Alum is.na, overall coag dose comes from DAE
    mutate(result = ifelse(is.na(Alum), DAE,
                           # By this point, DAE can either be NA (!is.nan() = T, neither train val exists)
                           # or NaN (!is.nan() = F, one train value exists)
                           ifelse(!is.nan(DAE), Alum,
                                  case_when(is.na(`PACl Train A`) ~ ((`PACl Train B`*2.07)+Alum)/2,
                                            TRUE ~ ((`PACl Train A`*2.07)+Alum)/2))),
           result = round(result, 1),
           parameter = "Overall Coagulant Dose",
           unit ="mg/L DAE",
           parm_eval = "calculated", parm_tag = "operations") %>%
    select(all_of(column_names))

  return(df)

}

#' Calculate total chlorine dose
#
#' Calculate the total chlorine dose, equal to the sum of chlorine pre,
#'  intermed, and post
#'
#' @inheritParams calculate_value
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
    filter(parameter %in% parms) %>%
    pivot_wider(names_from = parameter, values_from = result,
                id_cols = datasheet:date_ymd) %>%
    # In case if any of the required parameters are missing
    handle_missing_cols(cols) %>%
    mutate(result = rowSums(.[,c(5:7)], na.rm = TRUE),
           result = replace(result, result == 0, NA),
           result = round(result, 1),
           parameter = "Total Chlorine Dose", unit = "mg/L",
           parm_eval = "calculated",
           parm_tag = "operations") %>%
    select(all_of(column_names))

  return(as.data.frame(df))
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
#' @inheritParams calculate_value
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
    filter(parameter %in% parms) %>%
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
           parm_eval = "calculated",
           parm_tag = "operations",
           result = round(result, 2)) %>%
    select(all_of(column_names))

  return(df)

}

#' Calculate alum to DOC ratio
#'
#' Calculate alum to DOC ratio, equal to Alum/DOC
#'
#' @inheritParams update_parameters
#'
#' @return numeric vector. The calculated values
alum_DOC_ratio <- function(labdat) {
  alum_DOC_ratio_calculated <- labdat$Alum/labdat$DOC
  return(alum_DOC_ratio_calculated)
}

# THM ---------------------

#' Calculate the total THMs
#
#' Calculate the total THMs, equal to the sum of bromodichloromethane,
#' bromoform, chlorodibromomethane, chloroform
#'
#' @inheritParams calculate_value
#'
#' @return dataframe containing all required columns, including the calculated
#'  result. Note that the results of `determine_NA_NC()` are employed, and that
#'  parameters for which a value cannot be calculated are assigned NA or NaN, as
#'  defined in the function description
total_THMs <- function(column_names, labdat) {

  parms <- c("Bromodichloromethane", "Bromoform",
             "Chlorodibromomethane", "Chloroform")

  df <- labdat %>%
    filter(parameter %in% parms) %>%
    group_by(datasheet, sheet_year, station, date_ymd) %>%
    summarise(result = ifelse(sum(result, na.rm = TRUE) > 0,
                              sum(result, na.rm = TRUE), NA)) %>%
    mutate(parameter = "Total THMs", unit = "\u00b5g/L",
           parm_eval = "calculated",
           parm_tag = "THM") %>%
    select(all_of(column_names))

  return(df)

}

# Physical ---------------
#' Calculate percent DO
#
#' Calculate the precent dissolved oxygen, equal to DO/oxygen solubility*100
#'
#' @inheritParams calculate_value
#'
#' @return dataframe containing all required columns, including the calculated
#'  result. Note that the results of `determine_NA_NC()` are employed, and that
#'  parameters for which a value cannot be calculated are assigned NA or NaN, as
#'  defined in the function description
DO_percent <- function(column_names, labdat) {

  O2Table <- bpwtpR:::O2Table


  parms <- c("Temperature", "Diss. Oxygen", "Bench Diss. Oxygen")
  # Columns required for proper calculation of df
  cols <- det_cols(parms)

  df <- labdat %>%
    filter(parameter == "Temperature" | parameter == "Diss. Oxygen" |
             parameter == "Bench Diss. Oxygen") %>%
    pivot_wider(id_cols = "datasheet":"date_ymd",
                names_from = "parameter", values_from = "result")  %>%
    handle_missing_cols(cols) %>%
    left_join(O2Table, by = c("Temperature" = "temperature_C")) %>%
    # These two parameters are equivalent but are given different names
    mutate(DO = case_when(datasheet == "RawWater" ~ `Bench Diss. Oxygen`,
                          datasheet == "ClearWell" ~ `Diss. Oxygen`)) %>%
    determine_NA_NC(8, 9) %>%
    mutate(result = case_when(!is.na(.data$result) ~ DO / oxy.sol_mg.L * 100,
                              TRUE ~ .data$result),
           result = round(.data$result, 1),
           parameter = "DO Percent", unit = "%",
           parm_eval = "calculated",
           parm_tag = "physical") %>%
    select(all_of(column_names))

  return(df)

}

#' Calculate the dissolved solids
#
#' Calculate the dissolved solids, equal to the sum of bicarbonate, carbonate,
#'  calcium, magnesium, sodium, potassium, sulphate, chloride, silica
#'
#' @inheritParams calculate_value
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
    filter(parameter %in% parms) %>%
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
           parm_eval = "calculated",
           parm_tag = "physical") %>%
    select(all_of(column_names))

  return(df)

}

#' Calculate the LSI
#
#' Calculate the Langelier Saturation Indices 1 and 2
#'
#' @inheritParams calculate_value
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
    filter(parameter %in% parms, station %in% station_list) %>%
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
           parm_eval = "calculated",
           parm_tag = "physical",
           result = round(result, 2)) %>%
    arrange(datasheet, parameter, date_ymd) %>%
    select(all_of(column_names))

  return(df)

}

#' Calculate the turbidity log removal
#
#' Calculate the turbidity log removal, equal to
#'  log10(raw turbidity/Clearwell turbidity)
#'
#' @inheritParams calculate_value
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
    filter(parameter %in% parms) %>%
    mutate(parameter = ifelse(datasheet == "RawWater",
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
           result = round(result, 1),
           datasheet = "Combined Sheets", station = "Combined Stations",
           parameter = "Turbidity Log Removal", unit = "ratio",
           parm_eval = "calculated",
           parm_tag = "physical") %>%
    select(all_of(column_names))

  return(df)

}

# Biological ----------

#' Calculate total B+BG algae
#'
#' Calculate the total blue plus blue green algae, equal blue green algae +
#'  green algae
#'
#' @inheritParams update_parameters
#'
#' @return numeric vector. The calculated values
tot_B_BG_algae <- function(labdat) {
  tot_B_BG_algae_calculated <- labdat$`Blue Green Algae` + labdat$`Green Algae`
  return(tot_B_BG_algae_calculated)
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
#' @inheritParams calculate_value
#' @param parameter string. Parameter for which to calculate removal by
#'  coagulation, filtration. Likely one of: DOC, TOC, Odour
#'
#' @return dataframe containing all required columns, including the calculated
#'  result. Note that the results of `determine_NA_NC()` are employed, and that
#'  parameters for which a value cannot be calculated are assigned NA or NaN, as
#'  defined in the function description
removal_coag_filt <- function(column_names, labdat, parameter){

  # Columns required for proper calculation of df
  cols <- c(sheet_year = NA_real_, date_ymd = NA_real_,
            Raw = NA_real_, Clearwell = NA_real_, PreGAC = NA_real_)

  parms <- c(parameter)
  station_list = c("Raw", "Clearwell", "PreGAC")

  df <- labdat %>%
    filter(parameter %in% parms, station %in% station_list) %>%
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
           result = round(result, 3),
           datasheet = "Combined Sheets", station = "Combined Stations",
           parm_tag  = paste(ifelse(parameter == "Odour", "physical", "traceConstituents")),
           parameter = paste(parameter, "Removal - Coagulation & Filtration", sep = " "),
           unit      = "%",
           parm_eval = "calculated") %>%
    select(all_of(column_names))

  return(df)

}

#' Calculate overall removal
#'
#' Calculate overall filtration. Equal to percent yield between the raw and
#'  Clearwell values
#'
#' @inheritParams calculate_value
#' @param parameter string. Parameter for which to calculate removal by
#'  coagulation, filtration. Likely one of: DOC, TOC, Odour
#'
#' @return dataframe containing all required columns, including the calculated
#'  result. Note that the results of `determine_NA_NC()` are employed, and that
#'  parameters for which a value cannot be calculated are assigned NA or NaN, as
#'  defined in the function description
removal_overall <- function(column_names, labdat, parameter){

  # Columns required for proper calculation of df
  cols <- c(sheet_year = NA_real_, date_ymd = NA_real_,
            Raw = NA_real_, Clearwell = NA_real_)

  parms <- c(parameter)
  station_list = c("Raw", "Clearwell")

  df <- labdat %>%
    filter(parameter %in% parms, station %in% station_list) %>%
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
           result = round(result, 3),
           datasheet = "Combined Sheets", station = "Combined Stations",
           parm_tag  = paste(ifelse(parameter == "Odour", "physical", "traceConstituents")),
           parameter = paste(parameter, "Removal - Overall", sep = " "),
           unit      = "%",
           parm_eval = "calculated") %>%
    select(all_of(column_names))

  return(df)

}

#' Calculate removal by GAC filtration
#'
#' Calculate removal by GAC filtration. Equal to percent yield between the
#'  PreGAC and Clearwell values
#'
#' @inheritParams calculate_value
#' @param parameter string. Parameter for which to calculate removal by
#'  coagulation, filtration. Likely one of: DOC, TOC, Odour
#'
#' @return dataframe containing all required columns, including the calculated
#'  result. Note that the results of `determine_NA_NC()` are employed, and that
#'  parameters for which a value cannot be calculated are assigned NA or NaN, as
#'  defined in the function description
removal_GACfilt <- function(column_names, labdat, parameter){

  # Columns required for proper calculation of df
  cols <- c(sheet_year = NA_real_, date_ymd = NA_real_,
            Clearwell = NA_real_, PreGAC = NA_real_)

  parms <- c(parameter)
  stationlist = c("Clearwell", "PreGAC")

  df <- labdat %>%
    filter(parameter %in% parms, station %in% stationlist) %>%
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
           result = round(result, 3),
           datasheet = "Combined Sheets", station = "Combined Stations",
           unit      = "%",
           parm_eval = "calculated",
           parm_tag  = "traceConstituents",
           parameter = paste(parameter, "Removal - GAC Filtration", sep = " ")) %>%
    select(all_of(column_names))

  return(df)

}
