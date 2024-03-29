% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{read_weekly}
\alias{read_weekly}
\title{Read Weekly Data sheet of lab data workbook}
\usage{
read_weekly(path_to_labdat_file, skip_num = 7)
}
\arguments{
\item{path_to_labdat_file}{string. Full path (from root directory) to the lab
data file to be inserted into the database file}

\item{skip_num}{integer. Number of rows to skip over when reading in data.
Default = 7}
}
\value{
dataframe containing the raw weekly data
}
\description{
Read Weekly Data sheet of lab data workbook
}
