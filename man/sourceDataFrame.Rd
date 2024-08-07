% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miscellaneous.R
\name{sourceDataFrame}
\alias{sourceDataFrame}
\title{SPATAData source data.frame}
\usage{
sourceDataFrame(..., .rm_na_cols = NULL)
}
\arguments{
\item{...}{<\code{\link[rlang:args_data_masking]{data-masking}}> Expressions that
return a logical value, and are defined in terms of the variables in
\code{.data}. If multiple expressions are included, they are combined with the
\code{&} operator. Only rows for which all conditions evaluate to \code{TRUE} are
kept.}

\item{.rm_na_cols}{Logical value. Decides whether columns of the output data.frame
that contain only missing values are removed. Defaults to \code{TRUE} if \code{...}
contains any subset instructions. Else, defaults to \code{FALSE}. Specifying the input
forces the respective behaviour.}
}
\value{
Data.frame in which each row corresponds to a spatial data set stored
in a \code{SPATA2} object. The following meta variables provide additional information.

\itemize{
\item{\strong{sample_name}}{: Character. Name of the sample and its unique identifier.}
\item{\strong{comment}}{: Character. Additional comments about the sample.}
\item{\strong{donor_id}}{: Character. Unique identifier for the donor.}
\item{\strong{donor_species}}{: Character. Species of the donor.}
\item{\strong{grade}}{: Character. Grade of the sample in case of histologically classified malignancies.}
\item{\strong{grade_sub}}{: Character. Sub-grade of the sample.}
\item{\strong{histo_class}}{: Character. Histological classification.}
\item{\strong{histo_class_sub}}{: Character. Sub-classification of the histological class.}
\item{\strong{institution}}{: Character. Institution where the sample was collected.}
\item{\strong{organ}}{: Character. Organ from which the sample was taken.}
\item{\strong{organ_part}}{: Character. Specific part of the organ from which the sample was taken.}
\item{\strong{pathology}}{: Character. Pathological state of the sample.}
\item{\strong{platform}}{: Character. \code{\link[SPATA2:spatial_methods]{Platform}} used for the experiment.}
\item{\strong{pub_citation}}{: Character. Citation for the publication related to the sample.}
\item{\strong{pub_doi}}{: Character. DOI of the publication related to the sample.}
\item{\strong{pub_journal}}{: Character. Journal where the related publication was published.}
\item{\strong{pub_year}}{: Numeric. Year of publication.}
\item{\strong{sex}}{: Character. Sex of the donor. Either \emph{female} or \emph{male}.}
\item{\strong{side}}{: Character. Side of the organ from which the sample was taken.}
\item{\strong{tags}}{: Character. Tags related to the sample.}
\item{\strong{tissue_age}}{: Numeric. Age of the tissue in years.}
\item{\strong{workgroup}}{: Character. Workgroup or team responsible for the sample.}
}

Furthermore, there are quality control and file-specific meta variables:

\itemize{
\item{\strong{lm_source}}{: Date-time. Last instance when the corresponding \code{SPATA2} object was modified.}
\item{\strong{mean_counts}}{: Numeric. Mean counts of the measurements.}
\item{\strong{median_counts}}{: Numeric. Median counts of the measurements.}
\item{\strong{modality_gene}}{: Logical. Indicates if the modality includes genes.}
\item{\strong{modality_metabolite}}{: Logical. Indicates if the modality includes metabolites.}
\item{\strong{modality_protein}}{: Logical. Indicates if the modality includes proteins.}
\item{\strong{n_obs}}{: Numeric. Number of observations.}
\item{\strong{n_tissue_sections}}{: Numeric. Number of tissue sections as identified by \code{\link[SPATA2:identifyTissueOutline]{identifyTissueOutline()}} with default parameters.}
\item{\strong{obj_size}}{: Storage size of the object.}
\item{\strong{obs_unit}}{: Character. Unit of observation.}
\item{\strong{web_link}}{: Character. Weblinkg with which to download the \code{SPATA2} object.}
}
}
\description{
Access the source data.frame which contains information about
various spatial transcriptomic experiments. Use \code{...} to subset according
to \code{\link[dplyr:filter]{dplyr::filter()}}.
}
\examples{

library(SPATA2)
library(SPATAData)
library(dplyr)
library(stringr)

sdf <- sourceDataFrame()

#----- example dplyr logic to filter the source data.frame

# 1. obtain glioblastoma samples from the temporal lobe

temporal_gbms <- sourceDataFrame(histo_class == "Glioblastoma" & organ_part == "temporal")

# show results
temporal_gbms

# get sample names
temporal_gbms$sample_names

# downlaod as collection
downloadSpataObjects(sample_names = gbm_samples, folder = "spata_objects/gbm") 

# 2. obtain data from specific publications

sdf_kuppe <- 
 sourceDataFrame(str_detect(pub_citation, pattern = "^Kuppe"))
 
kuppe_samples <- sdf_kuppe$sample_name

print(kuppe_samples)  

}
\seealso{
\code{\link[=downloadSpataObject]{downloadSpataObject()}}, \code{\link[=downloadSpataObjects]{downloadSpataObjects()}} for easy download of
the filtering results.
}
