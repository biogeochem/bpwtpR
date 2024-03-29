% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrape.R
\name{scrape_ion_values}
\alias{scrape_ion_values}
\title{Scrape ion data}
\usage{
scrape_ion_values(weekly_data, labdat_parameters)
}
\arguments{
\item{weekly_data}{dataframe. All data in the Weekly sheet of the lab data file}

\item{labdat_parameters}{dataframe. Output from check_parameters_content}
}
\value{
data frame of the ion data
}
\description{
Read in all ion data. Identify parameters to read in based on
parameters.xlsx
}
