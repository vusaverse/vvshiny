% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/server_misc.R
\name{display_name}
\alias{display_name}
\title{Get a user-friendly display name}
\usage{
display_name(col_name, mapping_table)
}
\arguments{
\item{col_name}{A string specifying the name of the column.}

\item{mapping_table}{A named list with as name the original colum name and as value the display
name}
}
\value{
A string containing the user-friendly name for the column.
}
\description{
This function provides a user-friendly name for a column based on a mapping table, if available.
}
\examples{
  mapping <- list(
col1 = "Column 1",
col2 = "Column 2"
)
display_name("col1", mapping)
}
