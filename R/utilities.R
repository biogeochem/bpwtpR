#' Calculate percent yield
#'
#' @param pre
#' @param post
#'
#' @return
#' @export
#'
#' @examples
percent_yield <- function(pre, post){
  yield = ((pre - post)/ pre) * 100
}

#' Save a labdat data file
#'
#' @param file
#' @param outdir
#' @param outfilename
#'
#' @return
#' @export
#'
#' @examples
write_datafile <- function(file, outdir, outfilename){
  outpath <- file.path(outdir, outfilename)
  print(outpath)
  write.csv(x = file, file = outpath, row.names = FALSE)
}

#' TAdd new data to dataset
#'
#' @param labdat
#' @param new_data
#'
#' @return
#' @export
#'
#' @examples
append_newdata <- function(labdat, new_data){
  df <- bind_rows(labdat, new_data)
  return(df)
}

#' Add calculated values to dataset
#'
#' @param labdat
#' @param labdat_calcs
#'
#' @return
#' @export
#'
#' @examples
append_calc_values <- function(labdat, labdat_calcs){
  df <- bind_rows(labdat, labdat_calcs)
  return(df)
}
