#' TSummarize trihalomethanes for figure
#'
#' @param labdat
#'
#' @return
#' @export
summarize_THM <- function(labdat) {
  thm.parms <- c("chloroform_ug.L",
                 "bromodichloromethane_ug.L",
                 "chlorodibromomethane_ug.L",
                 "bromoform_ug.L")

  thms <- labdat %>%
    filter(parm_unit %in% thm.parms) %>%
    #filter(parm_tag == THM) %>%
    group_by(station, datetime_ymd.hms) %>%
    mutate(TTHMs = sum(result)) %>%
    group_by(parm_unit) %>%
    mutate(hist.max = max(result), hist.min = min(result))


  return(as.data.frame(thms))
}


## -- ##

#' Apply weekly calculations
#'
#' @param labdat routine lab dat file
#'
#' @importFrom dplyr bind_rows mutate_if
#'
#' @return calculated values
#' @export
apply_calculations <- function(labdat){

  suva_values <- suva(labdat = labdat)
  LSI_values <- langelier_SatIndex(labdat = labdat)
  DO_percent_values <- DO_percent(labdat = labdat)
  calc_TDS_values <- calc_TDS(labdat = labdat)
  tot_B_BG_algae_values <- tot_B_BG_algae(labdat = labdat)
  tot_chlorineDose_values <- tot_chlorineDose(labdat = labdat)
  #calc_partAl_values <- calc_partAl(labdat = labdat)
  #CW_PreGAC_TAl_values <- CW_PreGAC_TAl(labdat = labdat)
  ion_balance <- ion_balance(labdat = labdat)
  alumDOC_ratio <- alumDOC_ratio(labdat = labdat)
  alumDOC_stoich <- alumDOC_stoich(labdat = labdat)
  turbidity_logRemoval <- turbidity_logRemoval(labdat = labdat)
  TOCremoval_coag <- TOCremoval_coag(labdat = labdat)
  #TOCremoval_filt <- TOCremoval_filt(labdat = labdat)
  DOCremoval_coag <- DOCremoval_coag(labdat = labdat)
  DOCremoval_filt <- DOCremoval_filt(labdat = labdat)
  DOCremoval_GACfilt <- DOCremoval_GACfilt(labdat = labdat)
  odourremoval_filt <- odourremoval_filt(labdat = labdat)
  odourremoval_coag <- odourremoval_coag(labdat = labdat)

  df <- bind_rows(suva_values, LSI_values, DO_percent_values,
                  calc_TDS_values, tot_B_BG_algae_values,
                  tot_chlorineDose_values,
                  ion_balance, alumDOC_ratio, alumDOC_stoich,
                  turbidity_logRemoval,
                  TOCremoval_coag, #TOCremoval_filt,
                  DOCremoval_filt, DOCremoval_coag,
                  DOCremoval_GACfilt, odourremoval_filt,
                  odourremoval_coag) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(result_org = NA, result_flag = NA,
           year = year(datetime_ymd.hms)) %>%
    select(datasheet, sheet_year, station:datetime_ymd.hms, year,
           parameter:result, result_org, result_flag)

  return(df)
}


#' Calculate SUVA
#'
#' @param labdat data
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate
#' @importFrom tidyr pivot_wider gather
#'
#' @return calculated suva values
#' @export
suva <- function(labdat){
  parms <- c("UV254_abs.10cm", "DOC.GFdiss_mg.L.C")

  suva <- labdat %>%
    filter(parm_unit %in% parms) %>%
    select(-c(parameter, unit, parm_tag, parm_eval)) %>%
    pivot_wider(names_from = parm_unit, values_from = result,
                id_cols = c("datasheet","station","datetime_ymd.hms")) %>%
    #spread(parm_unit, result) %>%
    mutate(suva = UV254_abs.10cm * 10 / DOC.GFdiss_mg.L.C) %>%
    select(datasheet:datetime_ymd.hms, suva) %>%
    gather(parm_unit, result, suva) %>%
    mutate(parameter = "SUVA", parm_eval = "calculated",
           parm_tag = "traceConstituents", unit = "L/(mg-m)") %>%
    select(datasheet: datetime_ymd.hms, parameter, unit, parm_unit, parm_eval, parm_tag, result) %>%
    filter(result != "Inf")
  return(suva)
}


#' Calculate langelier saturation indeces
#'
#' @param labdat routine lab data
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate
#' @importFrom tidyr spread gather
#'
#' @return calculated lsi values
#' @export
langelier_SatIndex <- function(labdat){

  parms <- c("TDS", "Temperature", "pH", "Alkalinity (total)", "Calcium")

  df <- labdat %>%
    filter(parameter %in% parms) %>%
    select(-c(parm_unit, unit,  parm_tag, parm_eval, result_org, result_flag)) %>%
    pivot_wider(id_cols = datasheet:datetime_ymd.hms,
                names_from = parameter,
                values_from = result) %>%
    mutate(LSI1_A = 2.24961 - (0.017853 * Temperature) +
             (0.00008238 * Temperature^2) - (0.00000041 * Temperature^3),
           LSI1_m = 0.000025 * TDS,
           LSI1_B = ifelse(TDS < 500, 9.7 + ((2.5 * LSI1_m^0.5)/
                                               (1.0 + 5.3 * LSI1_m^0.5 + 5.5 *
                                                  LSI1_m)), 10),
           LSI1_ph_calc = LSI1_A + LSI1_B -
             log10(Calcium/0.4) - log10(`Alkalinity (total)`),
           LSI1 = pH - LSI1_ph_calc) %>%
    mutate(LSI2_A = (log10(TDS) - 1)/10,
           LSI2_B = -13.12 * log10(Temperature + 273)+ 34.55,
           LSI2_C = log10(Calcium/0.4) - 0.4,
           LSI2_D = log10(`Alkalinity (total)`),
           LSI2_ph_calc = (9.3 + LSI2_A + LSI2_B)-(LSI2_C + LSI2_D),
           LSI2 = pH - LSI2_ph_calc) %>%
    mutate(RSI = 2 * LSI2_ph_calc - pH) %>%
    select(datasheet:datetime_ymd.hms, LSI1, LSI2, RSI) %>%
    gather(parm_unit, result, LSI1:RSI) %>%
    mutate(parameter = parm_unit,
           parameter = ifelse(parameter == "LSI1", "Langelier Saturation Index 1",
                              ifelse(parameter == "LSI2", "Langelier Saturation Index 2", parameter)),
           parm_eval = "calculated", parm_tag = "physical",
           unit = "mg/L") %>%
    select(datasheet: datetime_ymd.hms, parameter, unit, parm_unit, parm_eval, parm_tag, result)

  return(df)

}


#' Bench Dissolved Oxygen (%) (Percent Saturation DO) Calculation (Raw Water & Clearwell)
#'
#' Calculates dissolved oxygen concentration (%).
#' Measured dissolved oxygen concentration (mg/L) is divided by the
#' saturation concentration of oxygen at the water temperature the sample
#' was collected at. This value is multiplied by 100 to express as a
#' percentage. The saturation of DO is determined from an oxygen solubility
#' table.
#'
#' Dissolved Oxygen (%) = Bench Diss. Oxygen (mg/L) ÷ Oxygen Solubility (mg/L) * 100.
#'
#' @param labdat Weekly routine sampling data.
#' @param O2table_file Table of oxygen solubilities in water.
#'
#' @importFrom utils read.csv
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate
#' @importFrom tidyr spread
#'
#' @return Dataframe containing datasheet (RawWater, ClearWell), station (Raw,
#'         Clearwell), datetimes, parameter (DO_percent), and result (DO
#'         concentration expressed as a percentage).
#'
#' @export

DO_percent <- function(labdat, datadir = "data/data_tables",
                       O2table_file = "O2Table.csv") {

  fpath <- file.path(datadir, O2table_file)

  O2Table <- read.csv(fpath) %>%
    select(1:2) %>%
    rename(temperature_C = temp_C)

  parms <- c("temperature_C", "DO_mg.L")

  labdat <- labdat %>%
    filter(parm_unit %in% parms) %>%
    select(-c(parameter, unit, parm_tag, parm_eval)) %>%
    pivot_wider(id_cols = datasheet:datetime_ymd.hms,
                names_from = parm_unit, values_from = result)

  labdat_O2Table <- left_join(labdat, O2Table, by = "temperature_C")

  df <- labdat_O2Table %>%
    mutate(DO_percent = DO_mg.L / oxy.sol_mg.L * 100) %>%
    select(datasheet:datetime_ymd.hms, DO_percent) %>%
    gather(parm_unit, result, DO_percent) %>%
    mutate(parameter = "Percent DO", parm_eval = "calculated",
           parm_tag = "physical",
           unit = "%") %>%
    select(datasheet: datetime_ymd.hms, parameter, unit,
           parm_unit, parm_eval, parm_tag, result)

  return(as.data.frame(df))
}


#' Total Dissolved Solids Calculation (Raw Water & Clearwell)
#'
#' From 2014—2017, and in addition to a direct TDS measurement, TDS was
#' calculated as the sum of bicarbonate, carbonate, calcium, magnesium, sodium,
#' potassium, sulphate, chloride, and silica ion concentrations (mg/L).
#'
#' TDS (mg/L) = Bicarbonate + Carbonate + Calcium + Magnesium + Sodium +
#'              Potassium + + Sulphate + Chloride + Silica.
#'
#' @param labdat Weekly routine sampling data.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate group_by
#' @importFrom tidyr gather
#'
#' @return Dataframe containing datasheet (RawWater, ClearWell), station (Raw,
#'         Clearwell), datetimes, parameter (calc.TDS_mg.L), and result (raw
#'         water and clearwell TDS concentration in mg/L).
#'
#' @examples calc_TDS(labdat = labdat)
#'
#' @export
#'
calc_TDS <- function(labdat) {

  parms <- c("Bicarbonate", "Carbonate", "Calcium", "Magnesium", "Sodium",
             "Potassium", "Sulphate", "Chloride", "Silica (SiO3)")

  df <-labdat %>%
    filter(parameter %in% parms) %>%
    select(-c(parm_unit, unit,  parm_tag, result_org, result_flag)) %>%
    mutate(result = gsub(x = result, pattern = "<", replacement = "-1"),
           result = as.numeric(result)) %>%
    filter(result >= 0) %>%
    spread(parameter, result) %>%
    group_by(datetime_ymd.hms) %>%
    mutate(Carbonate = replace(Carbonate, is.na(Carbonate), 0)) %>%
    mutate(calc.TDS_mg.L = Bicarbonate + Carbonate + Calcium + Magnesium +
             Sodium + Potassium + Sulphate + Chloride + `Silica (SiO3)`) %>%
    select(datasheet:datetime_ymd.hms, calc.TDS_mg.L) %>%
    gather(parm_unit, result, calc.TDS_mg.L) %>%
    filter(!is.na(result)) %>%
    mutate(parameter = "TDS", parm_eval = "calculated", parm_tag = "physical",
           unit = "mg/L") %>%
    select(datasheet: datetime_ymd.hms, parameter, unit,
           parm_unit, parm_eval, parm_tag, result)

  return(as.data.frame(df))

}


#' Sum total blue green algae and green algae
#'
#' @param labdat routine labdat
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select group_by mutate
#' @importFrom tidyr spread gather
#'
#' @return summed blue greens and greens
#' @export
tot_B_BG_algae <- function(labdat) {

  parms <- c("Blue Green Algae", "Green Algae")

  df <- labdat %>%
    filter(parameter %in% parms) %>%
    select(-c(parm_unit, unit,  parm_tag, result_org, result_flag)) %>%
    spread(parameter, result) %>%
    group_by(datetime_ymd.hms) %>%
    mutate(tot_B_BG_algae = `Blue Green Algae` + `Green Algae`) %>%
    select(datasheet:datetime_ymd.hms, tot_B_BG_algae) %>%
    gather(parameter, result, tot_B_BG_algae) %>%
    mutate(parameter = "Total BG + G", parm_eval = "calculated",
           parm_tag = "biological",
           unit = "mg/L", parm_unit = "cells.L") %>%
    select(datasheet: datetime_ymd.hms, parameter, unit,
           parm_unit, parm_eval, parm_tag, result)

  return(as.data.frame(df))
}


#' Total Chlorine Dose Calculation (Raw Water)
#'
#' Calculates the sum of Chlorine-pre, Chlorine-intermed, and Chlorine-post.
#'
#' Total Chlorine dose = Chlorine-pre + Chlorine-intermed + Chlorine-post.
#'
#' @param labdat Weekly routine sampling data.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate group_by
#' @importFrom tidyr spread
#'
#' @return Dataframe containing datasheet (RawWater), station (Raw), datetimes,
#'         parameter (tot.chlorineDose_mg.L), and result (Total Chlorine dose).
#'
#' @examples #tot_chlorineDose(labdat = labdat)

tot_chlorineDose <- function(labdat) {

  parms <- c("Chlorine-pre", "Chlorine-intermed", "Chlorine-post")

  df <- labdat %>%
    filter(parameter %in% parms) %>%
    select(-c(parm_unit, unit,  parm_tag, result_org, result_flag)) %>%
    mutate(result = as.numeric(result)) %>%
    spread(parameter, result) %>%
    mutate(
      `Chlorine-pre` = replace(`Chlorine-pre`, is.na(`Chlorine-pre`), 0),
      `Chlorine-intermed` = replace(`Chlorine-intermed`,
                                    is.na(`Chlorine-intermed`), 0),
      `Chlorine-post` = replace(`Chlorine-post`, is.na(`Chlorine-post`), 0)
    ) %>%
    group_by(datetime_ymd.hms) %>%
    mutate(tot.chlorineDose_mg.L = `Chlorine-pre` + `Chlorine-intermed` +
             `Chlorine-post`) %>%
    select(datasheet:datetime_ymd.hms, tot.chlorineDose_mg.L) %>%
    gather(parm_unit, result, tot.chlorineDose_mg.L) %>%
    mutate(parameter = "Total Chlorine Dose", parm_eval = "calculated",
           parm_tag = "operations",
           unit = "mg/L") %>%
    select(datasheet: datetime_ymd.hms, parameter, unit, parm_unit, parm_eval, parm_tag, result)

  return(as.data.frame(df))
}


#' Calculate ion balance
#'
#' @param labdat routine labdat
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate
#' @importFrom tidyr spread gather
#'
#' @return calculated ion balance
#' @export
ion_balance <- function(labdat){


  parms <- c("Calcium","Magnesium", "Sodium", "Potassium","Sulphate",
             "Chloride", "Bicarbonate", "Carbonate", "Silica (SiO3)")

  df <- labdat %>%
    filter(parameter %in% parms) %>%
    select(-c(parm_unit, unit, parm_tag, result_org, result_flag)) %>%
    spread(parameter, result) %>%
    mutate(anion_sum = (Sulphate * 0.0208) + (Chloride * 0.0282) +
             (Bicarbonate * 0.0164) + (Carbonate * 0.0333),
           cation_sum = (Calcium * 0.0499) + (Magnesium * 0.0822) +
             (Sodium * 0.0435) + (Potassium * 0.0256) +
             (`Silica (SiO3)` * 0.02629),
           ion_percdiff = ((cation_sum - anion_sum)/
                             (cation_sum + anion_sum)) * 100) %>%
    select(datasheet:datetime_ymd.hms, anion_sum, cation_sum, ion_percdiff) %>%
    gather(parm_unit, result, anion_sum:ion_percdiff) %>%
    mutate(datasheet = "NA",
           unit = ifelse(parm_unit == "anion_sum" |
                           parm_unit == "cation_sum", "meq/L", "%"),
           parameter = ifelse(parm_unit == "anion_sum", "Anion Sum",
                              ifelse(parm_unit == "cation_sum", "Cation Sum",
                                     ifelse(parm_unit == "ion_percdiff",
                                            "Ion Perc Difference", NA))),
           parm_eval = "calculated", parm_tag = "operations") %>%
    select(datasheet, station, datetime_ymd.hms, parameter, unit:parm_tag, result)

  return(as.data.frame(df))


}

#' Calculate Alum to DOC ratio
#'
#' @param labdat routine lab dat file
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate group_by
#' @importFrom tidyr spread
#'
#' @return calculated alum:DOC ratio
#' @export
alumDOC_ratio <- function(labdat) {

  parms <- c("Alum", "DOC")

  df <- labdat %>%
    filter(station == "Raw",
           parameter %in% parms) %>%
    select(-c(parm_unit, unit,  parm_tag, result_org, result_flag)) %>%
    mutate(result = as.numeric(result)) %>%
    spread(parameter, result) %>%
    group_by(datetime_ymd.hms) %>%
    mutate(result = Alum/DOC,
           datasheet = "NA", station = "NA",
           parameter = "alum to DOC ratio",
           unit = "NA", parm_unit = "NA",
           parm_eval = "calculated", parm_tag = "operations") %>%
    select(datasheet, station, datetime_ymd.hms, parameter:parm_unit,
           parm_eval, parm_tag, result)

  return(as.data.frame(df))
}


#' Calculate Alum to DOC stoichiometry
#'
#' @param labdat routine lab dat file
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate group_by
#' @importFrom tidyr spread
#'
#' @return calculated alum:DOC stoichiometry
#' @export
alumDOC_stoich <- function(labdat) {

  parms <- c("Alum", "DOC")

  df <- labdat %>%
    filter(station == "Raw",
           parameter %in% parms) %>%
    select(-c(parm_unit, unit,  parm_tag, result_org, result_flag)) %>%
    mutate(result = as.numeric(result)) %>%
    spread(parameter, result) %>%
    group_by(datetime_ymd.hms) %>%
    mutate(result = (Alum/DOC) / 12.33,
           datasheet = "NA", station = "NA",
           parameter = "alum to DOC ratio",
           unit = "NA", parm_unit = "NA",
           parm_eval = "calculated", parm_tag = "operations") %>%
    select(datasheet, station, datetime_ymd.hms, parameter:parm_unit,
           parm_eval, parm_tag, result)

  return(as.data.frame(df))
}


#' Calculate turbidity log removal
#'
#' @param labdat routine labdat file
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select group_by
#' @importFrom tidyr spread
#'
#' @return calculated turbidity log removal
#' @export
turbidity_logRemoval <- function(labdat) {

  df <- labdat %>%
    filter(parameter == "Turbidity") %>%
    mutate(result = as.numeric(result)) %>%
    mutate(parameter = ifelse(datasheet == "RawWater", "RW_Turbidity",
                              "CW_Turbidity")) %>%
    select(-c(datasheet, station, parm_unit, unit,  parm_tag)) %>%
    spread(parameter, result) %>%
    group_by(datetime_ymd.hms) %>%
    mutate(result = log10(RW_Turbidity / CW_Turbidity),
           datasheet = "NA", station = "NA",
           parameter = "Turbidity Log Removal",
           unit = "NA", parm_unit = "NA",
           parm_eval = "calculated", parm_tag = "operations") %>%
    select(datasheet, station, datetime_ymd.hms, parameter:parm_unit,
           parm_eval, parm_tag, result)

  return(as.data.frame(df))
}


#' Calculate TOC removal by coagulation
#'
#' @param labdat routine labdat dataset
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr pivot_wider
#'
#' @return calculated odour removal by coagulation
#' @export
#'
#' @examples
#'  #DOCremoval_GACfilt(labdat)
TOCremoval_coag <- function(labdat){

  stationlist = c("Clearwell", "Raw")

  toc_removal <- labdat %>%
    filter(parameter == "TOC",
           station %in% stationlist) %>%
    select(station, datetime_ymd.hms, result) %>%
    pivot_wider(id_cols = datetime_ymd.hms,
                names_from = station,
                values_from = result) %>%
    filter(!is.na(Clearwell)) %>%
    mutate(result = percent_yield(pre = Raw, post = Clearwell),
           datasheet = "NA", station = "NA",
           parameter = "TOC Removal - Coagulation",
           unit ="percent", parm_unit = "NA",
           parm_eval = "calculated", parm_tag = "operations") %>%
    select(datasheet, station, datetime_ymd.hms, parameter:parm_tag, result)

  return(toc_removal)

}


#' Calculate DOC removal by filtration
#'
#' @param labdat routine labdat dataset
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr pivot_wider
#'
#' @return calculated odour removal by coagulation
#' @export
#'
#' @examples
#'  #DOCremoval_GACfilt(labdat)
DOCremoval_filt <- function(labdat){

  stationlist = c("Clearwell", "PreGAC")

  doc_removal <- labdat %>%
    filter(parameter == "DOC",
           station %in% stationlist) %>%
    select(station, datetime_ymd.hms, result) %>%
    pivot_wider(id_cols = datetime_ymd.hms,
                names_from = station,
                values_from = result) %>%
    filter(!is.na(Clearwell)) %>%
    mutate(result = percent_yield(pre = PreGAC, post = Clearwell),
           datasheet = "NA", station = "NA",
           parameter = "DOC Removal - Filtration",
           unit ="percent", parm_unit = "NA",
           parm_eval = "calculated", parm_tag = "operations") %>%
    select(datasheet, station, datetime_ymd.hms, parameter:parm_tag, result)

  return(doc_removal)

}


#' Calculate DOC removal by coagulation
#'
#' @param labdat routine labdat dataset
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr pivot_wider
#'
#' @return calculated odour removal by coagulation
#' @export
#'
#' @examples
#'  #DOCremoval_GACfilt(labdat)
DOCremoval_coag <- function(labdat){

  stationlist = c("Clearwell", "Raw")

  doc_removal <- labdat %>%
    filter(parameter == "DOC",
           station %in% stationlist) %>%
    select(station, datetime_ymd.hms, result) %>%
    pivot_wider(id_cols = datetime_ymd.hms,
                names_from = station,
                values_from = result) %>%
    filter(!is.na(Clearwell)) %>%
    mutate(result = percent_yield(pre = Raw, post = Clearwell),
           datasheet = "NA", station = "NA",
           parameter = "DOC Removal - Coagulation",
           unit ="percent", parm_unit = "NA",
           parm_eval = "calculated", parm_tag = "operations") %>%
    select(datasheet, station, datetime_ymd.hms, parameter:parm_tag, result)

  return(doc_removal)

}


#' Calculate DOC removal my GAC
#'
#' @param labdat routine labdat dataset
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr pivot_wider
#'
#' @return calculated odour removal by coagulation
#' @export
#'
#' @examples
#'  #DOCremoval_GACfilt(labdat)
DOCremoval_GACfilt <- function(labdat){

  stationlist = c("Clearwell", "PreGAC")

  doc_removal <- labdat %>%
    filter(parameter == "DOC",
           station %in% stationlist) %>%
    select(station, datetime_ymd.hms, result) %>%
    pivot_wider(id_cols = datetime_ymd.hms,
                names_from = station,
                values_from = result) %>%
    filter(!is.na(Clearwell)) %>%
    mutate(result = percent_yield(pre = PreGAC, post = Clearwell),
           datasheet = "NA", station = "NA",
           parameter = "DOC Removal - GAC filtration",
           unit ="percent", parm_unit = "NA",
           parm_eval = "calculated", parm_tag = "operations") %>%
    select(datasheet, station, datetime_ymd.hms, parameter:parm_tag, result)

  return(doc_removal)

}


#' Calculate odour removal during filtration
#'
#' @param labdat routine labdat dataset
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr pivot_wider
#'
#' @return calculated odour removal by filtration
#' @export
#'
#' @examples
#'  #odourremoval_filt(labdat)
odourremoval_filt <- function(labdat){

  stationlist = c("Clearwell", "PreGAC")

  odour_removal <- labdat %>%
    filter(parameter == "Odour",
           station %in% stationlist) %>%
    select(station, datetime_ymd.hms, result) %>%
    pivot_wider(id_cols = datetime_ymd.hms,
                names_from = station,
                values_from = result) %>%
    filter(!is.na(Clearwell)) %>%
    mutate(result = percent_yield(pre = PreGAC, post = Clearwell),
           datasheet = "NA", station = "NA",
           parameter = "Odour Removal - Filtration",
           unit ="percent", parm_unit = "NA",
           parm_eval = "calculated", parm_tag = "operations") %>%
    select(datasheet, station, datetime_ymd.hms, parameter:parm_tag, result)

  return(odour_removal)

}


#' Calculate odour removal during coagulation
#'
#' @param labdat routine labdat dataset
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select
#' @importFrom tidyr pivot_wider
#'
#' @return calculated odour removal by coagulation
#' @export
#'
#' @examples
#'  #odourremoval_coag(labdat)
odourremoval_coag <- function(labdat){

  stationlist = c("Clearwell", "Raw")

  odour_removal <- labdat %>%
    filter(parameter == "Odour",
           station %in% stationlist) %>%
    select(station, datetime_ymd.hms, result) %>%
    pivot_wider(id_cols = datetime_ymd.hms,
                names_from = station,
                values_from = result) %>%
    filter(!is.na(Clearwell)) %>%
    mutate(result = percent_yield(pre = Raw, post = Clearwell),
           datasheet = "NA", station = "NA",
           parameter = "Odour Removal - Coagulation",
           unit ="percent", parm_unit = "NA",
           parm_eval = "calculated", parm_tag = "operations") %>%
    select(datasheet, station, datetime_ymd.hms, parameter:parm_tag, result)

  return(odour_removal)

}

