

#' @title Add sample meta data
#' 
#' @description
#' Function to populate slot @@meta_sample of the `SPATA2` object. 
#' 
#' @param object An object of class `SPATA2`.
#' @param meta_data A named list of meta data. 
#' 
#' @seealso [`source_df`] for meta slots known to the SPATAData package.
#'
#' @export
#' 
#' @examples
#' # example code
#' 
#' library(SPATA2)
#' data("example_data")
#' 
#' object <- example_data$object_UKF275T_diet
#' 
#' meta_list <- list(donor_id = "UKF275", organ = "Cerebrum", hist_class = "Glioblastoma")
#' 
#' # prior
#' getSampleMetaData(object)
#' 
#' # add meta data
#' object <- addSampleMetaData(object, meta_data = meta_list)
#' 
#' # afterwards
#' getSampleMetaData(object)
#' 
#' getSampleMetaData(object, as_list = FALSE) # return as data.frame
#' 
#' getSampleMetaData(object, na_rm = FALSE) # return all source_df slots
#' 
addSampleMetaData <- function(object, meta_data){
  
  input <- confuns::keep_named(input = meta_data)
  
  for(i in seq_along(input)){
    
    name <- base::names(input)[i]
    info <- input[[i]]
    
    object@meta_sample[[name]] <- info
    
  }
  
  return(object)
  
}



#' @title Get sample meta data
#'
#' @description This function retrieves the metadata for samples in an object.
#'  It allows for various options to format the output, remove missing values, 
#'  and provide warnings for class mismatches.
#'
#' @param object An object of class `SPATA2`.
#' @param as_list Logical, if TRUE, the output will be a list. Default is TRUE.
#' @param source_only Logical, if TRUE, only meta slots that fit to a column
#' of the \link[=source_df]{source data.frame} will be included.
#' @param na_rm Logical, if TRUE, removes columns with NA values - missing values. Default is TRUE.
#' @param warn Logical, if TRUE, warnings will be issued for class mismatches
#' between the expected class as defined in the  \link[=source_df]{source data.frame} 
#' and the object's metadata. Default is TRUE.
#' 
#' @note Only meta slots of one dimension (vectors) are returned. If your `SPATA2`
#' object contains meta data that has multiple dimensions (e.g. data.frames, matrices)
#' extract via `object@meta_sample`.
#' 
#' @return A data frame or list containing the sample metadata.
#' 
#' \dontrun{
#'   # Assuming 'sample_object' is your SPATA2 object 
#'   metadata <- getSampleMetaData(sample_object)
#'   metadata_list <- getSampleMetaData(sample_object, as_list = TRUE)
#'   metadata_df <- getSampleMetaData(sample_object, as_list = FALSE)
#'   metadata_source_only <- getSampleMetaData(sample_object, source_only = TRUE)
#'   metadata_no_na <- getSampleMetaData(sample_object, na_rm = TRUE)
#' }
#' 
#' @export
#' 
#' @inherit addSampleMetaData examples
#' 
getSampleMetaData <- function(object,
                              as_list = TRUE,
                              source_only = FALSE,
                              na_rm = TRUE, 
                              warn = TRUE){
  
  
  meta_sample <- 
    purrr::keep(
      .x = object@meta_sample, 
      .p = ~ is.vector(.x) & !is.list(.x)
    )
  
  if(identical(meta_sample, list())){
    
    warning(glue::glue("Slot @meta_sample of sample {object@sample} is an empty list."))
    meta_data <- list()
    
  } else {
    
    meta_data <- source_df_v3_blueprint
    snames <- base::colnames(meta_data)
    meta_data$sample_name <- getSampleName(object)
    
    for(meta_slot in base::names(meta_sample)){
      
      if(meta_slot %in% snames){
        
        c_should <- class(meta_data[[meta_slot]])
        c_is <- class(meta_sample[[meta_slot]])
        
        # correct class? 
        cc <- identical(x = c_should, y = c_is)
        
        if(!cc && base::isTRUE(warn)){
          
          warning(glue::glue("Meta slot '{meta_slot}' is of class '{c_is}' but should be of class '{c_should}'."))
          
        }
        
      }
      
      # specific rules
      if(meta_slot == "tags"){
        
        meta_data[[meta_slot]] <- stringr::str_c(meta_sample[[meta_slot]], collapse = ";")
        
      } else {
        
        meta_data[[meta_slot]] <- meta_sample[[meta_slot]]
        
      }
      
    }
    
    if(base::isTRUE(source_only)){
      
      meta_data <- meta_data[snames]
      
    }
    
    
    if(base::isTRUE(na_rm)){
      
      meta_data <- 
        purrr::discard(.x = meta_data, .p = base::is.na) %>% 
        purrr::map_dfc(.f = ~ .x)
      
    }
    
    if(base::isTRUE(as_list)){
      
      meta_data <- as.list(meta_data)
      
    }
    
  }
  
  
  
  return(meta_data)
  
}
