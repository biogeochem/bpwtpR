% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrape.R
\name{scrape_clearwell_thms}
\alias{scrape_clearwell_thms}
\title{Scrape Clearwell THM data}
\usage{
scrape_clearwell_thms(weekly_data, clearwell_start, labdat_parameters)
}
\arguments{
\item{weekly_data}{dataframe. All data in the Weekly sheet of the lab data file}

\item{clearwell_start}{numeric value. The start of the Clearwell data as
indicated by the phrase "CLEAR WELL" in the Parameters column of the Weekly
data}

\item{labdat_parameters}{dataframe. Output from check_parameters_content}
}
\value{
data frame of the Clearwell THMs data
}
\description{
Read in Clearwell THM data. Identify parameters to read in based on
parameters.xlsx
}
