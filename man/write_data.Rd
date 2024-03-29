% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write.R
\name{write_data}
\alias{write_data}
\title{Add processed data to existing db file}
\usage{
write_data(old_data, new_data, path_to_file)
}
\arguments{
\item{old_data}{dataframe. Output from read_db() (both db and db_dates)}

\item{new_data}{dataframe. Data to append to db}

\item{path_to_file}{string. Full path (from root directory) to the .csv
database file if it exists, and desired path to a .csv database file if it
does not exist}
}
\value{
result of write.table()
}
\description{
Add processed data to existing db file
}
