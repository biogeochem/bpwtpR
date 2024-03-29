% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corrections.R
\name{update_parameters}
\alias{update_parameters}
\title{Update parameter names and units}
\usage{
update_parameters(labdat, file_sheet_year, labdat_parameters)
}
\arguments{
\item{labdat}{dataframe containing the parsed lab data}

\item{file_sheet_year}{numeric value indicating the sheet year}

\item{labdat_parameters}{dataframe. Output from check_parameters_content}
}
\value{
data frame with updated parameter name, unit
}
\description{
Update parameter names and units as indicated in \code{parameters.xlsx}. There
exist inconsistencies between years in parameter names and units, including
human error. Update to ensure consistency
}
