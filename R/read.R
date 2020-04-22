#' Read routine lab data file
#'
#' @param datadir data directory
#' @param labdat_file lab data filename
#'
#' @return data table with replaced bdl values
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom utils read.csv
#'
#' @examples
#' # read_labdat(datadir = "data", labdat_file = "BPWTP_labdat.csv")
read_labdat <- function(datadir = "data",
                        labdat_file = ""){

  fpath <- file.path(datadir, labdat_file)
  labdat <- read.csv(fpath)

  labdat <- labdat %>%
    mutate(datetime_ymd.hms = as.POSIXct(datetime_ymd.hms))

return(labdat)

}
