#' Select ion rows
#'
#' @inheritParams scrape_labdatxls
#'
#' @return dataframe. All ion-related parameters
filter_ions <- function(labdat_parameters) {

  labdat_parameters_ion <- labdat_parameters %>%
    filter(agrepl("Anion Sum", parameter_updated, max.distance = 0.05,
                  ignore.case = TRUE) |
           agrepl("Cation Sum", parameter_updated, max.distance = 0.05,
                  ignore.case = TRUE) |
           agrepl("Ion Percent Difference", parameter_updated, max.distance = 0.05,
                  ignore.case = TRUE)) %>%
    as.data.table()

}

#' Select Al rows
#'
#' @inheritParams scrape_labdatxls
#'
#' @return dataframe. All Al-related parameters
filter_al <- function(labdat_parameters) {

  labdat_parameters_al <- labdat_parameters %>%
    filter(agrepl("Aluminum total",       parameter_updated,
                  ignore.case = TRUE) |
           agrepl("Aluminum dissolved",   parameter_updated,
                  ignore.case = TRUE) |
           agrepl("Aluminum particulate", parameter_updated,
                  ignore.case = TRUE)) %>%
    as.data.table()

}

#' Select THM rows
#'
#' @inheritParams scrape_labdatxls
#'
#' @return dataframe. All THM-related parameters
filter_thms <- function(labdat_parameters) {

  labdat_parameters_thms <- labdat_parameters %>%
    # Want to handle cases where the parm_tag is correctly input as "THM"
    # AS WELL AS cases when it is not (the parameter should be relied on
    # instead)
    filter(parm_tag  == "THM" |
           agrepl("TTHM's total", parameter_updated,
                  ignore.case = TRUE) |
           agrepl("Chloroform",  parameter_updated,
                  ignore.case = TRUE) |
           agrepl("Bromodichloromethane", parameter_updated,
                  ignore.case = TRUE) |
           agrepl("Chlorodibromomethane", parameter_updated,
                  ignore.case = TRUE) |
           agrepl("Bromoform", parameter_updated,
                  ignore.case = TRUE)) %>%
    as.data.table()

}
