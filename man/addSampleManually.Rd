% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addSampleManually.R
\name{addSampleManually}
\alias{addSampleManually}
\title{Add sample manually}
\usage{
addSampleManually(
  source_df = sourceDataFrame(),
  new_sample_input,
  update,
  in_shiny = FALSE
)
}
\arguments{
\item{source_df}{The source data.frame. Defaults to the current \code{source_df}
of the package.}

\item{new_sample_input}{A named list. Each slot corresponds to a variable
of the \code{source_df} and should be named accordingly. Apart from the variable
\emph{sample_name} missing variables are allowed (not recommended though) and
will result in NA for the respective slot. See details for more information.}

\item{update}{Logical. Use with caution! If TRUE, the updated output data.frame
will replace the current \code{source_df} of the package!}
}
\value{
The updated data.frame is returned invisibly.
}
\description{
Adds a new sample to the source data.frame.
}
\details{
Slots must be named as unnamed slots are discarded!
Content of slot \emph{sample_name} must not be among the samples names.
Slot \emph{tags} can be a character vector of length > 1. Every other slot
should be a single value. Apart from slot \emph{stage}, which should be an integer,
these are always character values.
}
\keyword{internal}
