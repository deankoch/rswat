% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rswat_db_helpers.R
\name{rswat_sim_dates}
\alias{rswat_sim_dates}
\title{Get the start/end dates for a simulation, or for the printed output files}
\usage{
rswat_sim_dates(lazy = TRUE, prt = FALSE, render = TRUE, .db = .rswat_db)
}
\arguments{
\item{lazy}{logical, indicates to load time.sim and/or print.prt as needed}

\item{prt}{logical, if TRUE the function fetches info from time.sim (else from print.prt)}

\item{render}{logical, indicates to return a string instead of data frame}

\item{.db}{rswat_db object reference, for internal use}
}
\value{
data frame of dates or string reporting the dates
}
\description{
work in progress
}
