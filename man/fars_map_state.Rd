% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Map FARS Data for a State}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{An integer representing the state number.}

\item{year}{An integer representing the year of interest.}
}
\value{
A map displaying the locations of accidents, or a message if no data is available.
Errors will occur if the state number is invalid.
}
\description{
This function plots the locations of FARS accidents for a given state and year.
}
\examples{
\dontrun{
fars_map_state(1, 2013)
}
}
