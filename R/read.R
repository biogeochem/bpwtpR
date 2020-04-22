#' Read routine lab data file
#'
#' @param datadir data directory
#' @param labdat_file lab data filename
#' @param bdl_replacement replacment option for below detection limit values (bdl)
#' @param data_validation toggled option for data validation. Provides extra column to compare replaced result values.
#'
#' @return data table with replaced bdl values
#' @export
#'
#' @importFrom dplyr %>% mutate
#'
#' @examples 
#' # read_labdat(datadir = "data", labdat_file = "BPWTP_labdat.csv")
read_labdat <- function(datadir = "data", 
                        labdat_file = "", 
                        bdl_replacement = TRUE,
                        datavalidation = FALSE){
  
  fpath <- file.path(datadir, labdat_file)
  labdat <- read.csv(fpath) 
  
  if(isTRUE(bdl_replacement)){
    
    if(isTRUE(datavalidation)){
      labdat <- labdat %>% 
        mutate(result_rpl = gsub(x = result, pattern = "<.", replacement = "0"), 
               result_rpl = as.numeric(result_rpl)) %>%
        #filter(result >= 0) %>%
        mutate(datetime_ymd.hms = as.POSIXct(datetime_ymd.hms))
      
    } else{
      
      labdat <- labdat %>% 
        mutate(result = gsub(x = result, pattern = "<.", replacement = "0"), 
               result = as.numeric(result)) %>%
        #filter(result >= 0) %>%
        mutate(datetime_ymd.hms = as.POSIXct(datetime_ymd.hms))
    }
    
  }else{
    
    labdat <- labdat %>% 
      mutate(datetime_ymd.hms = as.POSIXct(datetime_ymd.hms))}
  
  return(labdat)
  
}

#' Read routine lab data file
#'
#' @param datadir data directory
#' @param labdat_file lab data filename
#' @param bdl_replacement replacment option for below detection limit values (bdl)
#' @param data_validation toggled option for data validation. Provides extra column to compare replaced result values.
#'
#' @return data table with replaced bdl values
#' @export
#'
#' @importFrom dplyr %>% mutate
#'
#' @examples 
#' # read_labdat(datadir = "data", labdat_file = "BPWTP_labdat.csv")
read_labdat <- function(datadir = "data", 
                        labdat_file = "", 
                        bdl_replacement = TRUE,
                        datavalidation = FALSE){
  
  fpath <- file.path(datadir, labdat_file)
  labdat <- read.csv(fpath) 
  
  labdat <- labdat %>% 
    mutate(datetime_ymd.hms = as.POSIXct(datetime_ymd.hms))

return(labdat)

}

#' Read routine lab data file
#'
#' @param datadir data directory
#' @param labdat_file lab data filename
#' @param bdl_replacement replacment option for below detection limit values (bdl)
#' @param data_validation toggled option for data validation. Provides extra column to compare replaced result values.
#'
#' @return data table with replaced bdl values
#' @export
#'
#' @importFrom dplyr %>% mutate
#'
#' @examples 
#' # read_labdat(datadir = "data", labdat_file = "BPWTP_labdat.csv")
read_labdat <- function(datadir = "data", 
                        labdat_file = "", 
                        bdl_replacement = TRUE,
                        datavalidation = FALSE){
  
  fpath <- file.path(datadir, labdat_file)
  labdat <- read.csv(fpath) 
  
  if(isTRUE(bdl_replacement)){
    
    if(isTRUE(datavalidation)){
      labdat <- labdat %>% 
        mutate(result_rpl = gsub(x = result, pattern = "<.", replacement = "0"), 
               result_rpl = as.numeric(result_rpl)) %>%
        #filter(result >= 0) %>%
        mutate(datetime_ymd.hms = as.POSIXct(datetime_ymd.hms))
      
    } else{
      
      labdat <- labdat %>% 
        mutate(result = gsub(x = result, pattern = "<.", replacement = "0"), 
               result = as.numeric(result)) %>%
        #filter(result >= 0) %>%
        mutate(datetime_ymd.hms = as.POSIXct(datetime_ymd.hms))
    }
    
  }else{
    
    labdat <- labdat %>% 
      mutate(datetime_ymd.hms = as.POSIXct(datetime_ymd.hms))}
  
  return(labdat)
  
}
## options for bdl values:

#   1. "remove"   = remove from dataset using a negative number indicator
#
#   2. "mdl"      = replace bdl values with method detection limit values (mdl) for
#       the parameter 
#
#   3. "simulate" = write a data simulation function to generate a
#       random number based on the probability distribution