#' Calculate percent yield
#'
#' @param pre first measurement
#' @param post last measurement
#'
#' @return
#' @export
#'
#' @examples
#' #percent_yield()
percent_yield <- function(pre, post){
  yield = ((pre - post)/ pre) * 100
}

#' Save a labdat data file
#'
#' @param file filename
#' @param outdir output directory
#' @param outfilename output file name
#'
#' @return
#' @export
#'
write_datafile <- function(file, outdir, outfilename){
  outpath <- file.path(outdir, outfilename)
  sprintf("Writing updated file to: %s", outpath)
  write.csv(x = file, file = outpath, row.names = FALSE, fileEncoding = "ISO-8859-1")
}

#' Add new data to dataset
#'
#' @param labdat labdat datafile
#' @param new_data new data file
#'
#' @return
#' @export
#'
append_newdata <- function(labdat, new_data){
  df <- bind_rows(labdat, new_data)
  return(df)
}

#' Add calculated values to dataset
#'
#' @param labdat labdat datafile
#' @param labdat_calcs calculated values
#'
#' @return
#' @export
#'
append_calc_values <- function(labdat, labdat_calcs){
  df <- bind_rows(labdat, labdat_calcs)
  return(df)
}
