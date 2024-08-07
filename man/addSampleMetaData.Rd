% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addSampleMetaData.R
\name{addSampleMetaData}
\alias{addSampleMetaData}
\title{Add sample meta data}
\usage{
addSampleMetaData(object, meta_data)
}
\arguments{
\item{object}{An object of class \code{SPATA2}.}

\item{meta_data}{A named list of meta data.}
}
\description{
Function to populate slot @meta_sample of the \code{SPATA2} object.
}
\examples{
# example code

library(SPATA2)
data("example_data")

object <- example_data$object_UKF275T_diet

meta_list <- list(donor_id = "UKF275", organ = "Cerebrum", hist_class = "Glioblastoma")

# prior
getSampleMetaData(object)

# add meta data
object <- addSampleMetaData(object, meta_data = meta_list)

# afterwards
getSampleMetaData(object)

getSampleMetaData(object, as_list = FALSE) # return as data.frame

getSampleMetaData(object, na_rm = FALSE) # return all source_df slots

}
\seealso{
\code{\link{source_df}} for meta slots known to the SPATAData package.
}
