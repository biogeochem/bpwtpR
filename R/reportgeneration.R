bpwtp_report <- function(labdat, 
                          report_type = "",                          
                          reportdir = "report", 
                          report_Rmd = "report.Rmd",
                          #report_template = "report_template.docx",
                          report_author = NULL){

  outfile <- file.path(reportdir, report_Rmd)
  report_title <- "BPWTP"
  render("report/report.Rmd", params = list(title = report_title,
                                            author = report_author))
}
