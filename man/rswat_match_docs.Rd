% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rswat_docs.R
\name{rswat_match_docs}
\alias{rswat_match_docs}
\title{Match SWAT+ variable names found in config files to definitions in documentation}
\usage{
rswat_match_docs(
  file_df,
  desc_weight = 1,
  desc_len = 100L,
  div_penalty = 1,
  k_select = NULL,
  trim = 3L,
  quiet = FALSE,
  .db = .rswat_db
)
}
\arguments{
\item{file_df}{input config file data}

\item{desc_weight}{switch this to logical}

\item{desc_len}{how many characters of description to print}

\item{div_penalty}{internal}

\item{k_select}{k-diagonals}

\item{trim}{print level}

\item{quiet}{logical, supresses console output}

\item{.db}{rswat_db object, for internal use}
}
\value{
a
}
\description{
Match SWAT+ variable names found in config files to definitions in documentation
}
