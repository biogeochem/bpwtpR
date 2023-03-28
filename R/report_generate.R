#' @title Generate bpwtp report
#'
#' @param path_to_db_file string. Path to the database csv file
#' @param outdir string. Path to the folder in which to deposit the outputs
#' @param selected_year numeric. Year of interest to compare to historical data
#' @param selected_month string. Abbreviated 3 letter month label. Ex "Jan"
#'
#' @return .Rmd
#'
#' @importFrom rmarkdown render
#'
#' @export
report_generate <- function(path_to_db_file,
                            outdir,
                            selected_year,
                            selected_month) {

  render("inst/rmd/report_generation.Rmd",
         params = list(path_to_db_file,
                       outdir,
                       selected_year,
                       selected_month),
         output_dir  = outdir,
         output_file = paste("bpwtp_report",
                             selected_month,
                             selected_year,
                             "summary",
                             sep = "_"))

}
