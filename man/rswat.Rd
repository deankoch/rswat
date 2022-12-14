% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rswat.R
\name{rswat}
\alias{rswat}
\title{Open a SWAT+ project directory}
\usage{
rswat(
  swat_dir = NULL,
  exe_path = NA_character_,
  include = "default",
  exclude = .rswat_gv_exclude(),
  quiet = FALSE,
  reset = FALSE,
  refresh = FALSE,
  .db = .rswat_db
)
}
\arguments{
\item{swat_dir}{character, path to the SWAT+ project directory (should contain "file.cio")}

\item{exe_path}{character, path to the SWAT+ simulation executable (should have extension ".exe")}

\item{exclude}{character vector, regular expressions for groups/names to not load}

\item{reset}{logical, indicates to re-initialize rswat (erases any loaded data)}

\item{.db}{rswat_db object, for internal use}

\item{load_all}{logical, indicating to load all configuration files except as specified in \code{exclude}}
}
\value{
A data frame of information on the SWAT+ files in \code{swat_dir}
}
\description{
Scans a SWAT+ project directory for configuration files and optionally loads them. This
initializes an \code{rswat_db} reference class object in your session, allowing SWAT+ configuration
files in \code{swat_dir} files to be listed, and their contents displayed and modified.
}
\details{
When \code{swat_dir} is set for the first time in an R session, or when it is changed, the
function will scan the directory for SWAT+ files, building a data frame of information
about them which can be accessed with functions like \code{rswat_files} or \code{rswat_find}.

The function invisibly returns a data frame of information on the files in \code{swat_dir},
printing its first few rows (by default, when \code{quiet=TRUE}).

Files of type 'output', 'decision_table', 'gwflow' are not loaded by default because they
can be large and slow to parse. Set \code{exclude=""} to load everything.
}
