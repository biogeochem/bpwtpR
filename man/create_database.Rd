% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/handle_database.R
\name{create_database}
\alias{create_database}
\title{Create database}
\usage{
create_database(
  path_to_labdat_dir,
  path_to_db_file,
  path_to_db_dates_file,
  path_to_parameters,
  file_sheet_year = NULL
)
}
\arguments{
\item{path_to_labdat_dir}{string. Path to the directory that contains all
the lab data files to be entered into the database file}

\item{path_to_db_file}{string. Full path (from root directory) to the .csv
database file if it exists, and desired path to a .csv database file if it
does not exist}

\item{path_to_db_dates_file}{string. Full path (from root directory) to the .csv
database dates file if it exists, and desired path to a .csv database dates file if it
does not exist}

\item{path_to_parameters}{string. Full path (from root directory) to the
.xlsx file that contains the desired parameters and associated info
(station, units, corrected units...)}

\item{file_sheet_year}{numeric. The year of the labdata file of interest}
}
\value{
dataframe
}
\description{
Access all of the lab data files contained in the specified directory.
Process each of them and place them all into the specified database file
}
