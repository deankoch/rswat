% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rswat_run.R
\name{rswat_exec}
\alias{rswat_exec}
\title{Run the SWAT+ executable}
\usage{
rswat_exec(quiet = FALSE, .db = .rswat_db)
}
\arguments{
\item{quiet}{logical, suppresses console output}

\item{.db}{rswat reference object, for internal use}
}
\value{
a tibble of output files created
}
\description{
Makes a system call to run the SWAT+ executable in the currently assigned SWAT+
project directory (see \code{?rswat}). This runs the simulation specified parameterized
by the various config files in \code{swat_dir} (list them with \code{rswat_files}).
}
\details{
Simulation outputs are printed to one or more plain text output files as
specified by table 4 in 'print.prt'. The function returns a list of all such files
modified/created by the simulator, as well as any log files written.
}
