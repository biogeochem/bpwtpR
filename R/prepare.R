
#' Update parameters to consistent naming convention
#'
#' @param labdat routine lab data file
#' @param parameters_file name of parameters file
#' @param data_validation option for data validation. Produces data table with extended column names for verification.
#'
#' @return data frame with updated parameter names
#' @export
#'
#' @examples 
#' # update_parameters(labdat = labdat, parameters_file = "parameters.csv")
update_parameters <- function(labdat, parameters_file = "parameters.csv",
                              data_validation = FALSE){
  dir <- "./data"
  fpath <- file.path(dir, parameters_file)
  
  labdat_parameters <- read.csv(fpath) %>% 
    mutate(parameter = as.character(parameter),
           unit = as.character(unit))
  
  labdat.mod <- labdat  %>%
    left_join(labdat_parameters, by = c("datasheet","station","parameter","unit"))
  
  parm_check <- labdat.mod %>% 
    filter(is.na(parm_unit)) %>% 
    distinct(datasheet, station, parameter, unit) %>% 
    arrange(datasheet)
  print(parm_check)
  
  if(isTRUE(data_validation)){
    labdat.updated <- labdat.mod %>%
      select(datasheet, sheet_year, station, datetime_ymd.hms, year, parameter,
             parameter_updated, unit, unit_updated, parm_unit, 
             parm_eval, parm_tag, result) %>%
      #filter(parm_eval == "measured") %>%
      mutate(station = as.factor(station))
    
  } else {
    
  labdat.updated <- labdat.mod %>%
    select(datasheet, station, datetime_ymd.hms, 
           parameter_updated, unit_updated, parm_unit, 
           parm_eval, parm_tag, result) %>%
    #filter(parm_eval == "measured") %>%
    rename("parameter" = parameter_updated,
          "unit" = unit_updated) %>%
    mutate(station = as.factor(station))}
  return(labdat.updated)
}

