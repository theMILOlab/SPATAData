


#' @title Meta data and links for MILO Lab database
#'
#' @description A data.frame of multiple variables containing information 
#' around the data sets that are available in \code{SPATAData}.
#'
"source_df"

#' @title Meta data of features and cell type deconvolution 
#'
#' @description A list of data.frames named by samples of the samples available. 
#' The cell type annotations were performed by spaceXR 
#' Cable, D.M., Murray, E., Shanmugam, V. et al. 
#' Cell type-specific inference of differential expression in 
#' spatial transcriptomics. Nat Methods (2022). 
#' https://doi.org/10.1038/s41592-022-01575-3 
#' using the reference scRNA-seq dataset from Cristian Ruiz-Moreno 
#' "https://www.biorxiv.org/content/10.1101/2022.08.27.505439v1" (not published)
#' @examples
#'
#' # Get the list()
#' features <- data(features_df)[["275_T"]]
#' object <- SPATA2::addFeatures(object, features, overwrite = T)
#'  
#'
"features_df"

