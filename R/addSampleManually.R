


#' @title Add sample manually
#' 
#' @description Adds a new sample to the source data.frame. 
#'
#' @param source_df The source data.frame. Defaults to the current \code{source_df}
#' of the package.
#' @param new_sample_input A named list. Each slot corresponds to a variable 
#' of the \code{source_df} and should be named accordingly. Apart from the variable 
#' \emph{sample_name} missing variables are allowed (not recommended though) and
#' will result in NA for the respective slot. See details for more information.
#' @param update Logical. Use with caution! If TRUE, the updated output data.frame 
#' will replace the current \code{source_df} of the package!
#' 
#' @details Slots must be named as unnamed slots are discarded!
#' Content of slot \emph{sample_name} must not be among the samples names. 
#' Slot \emph{tags} can be a character vector of length > 1. Every other slot
#' should be a single value. Apart from slot \emph{stage}, which should be an integer,
#' these are always character values. 
#'
#' @return The updated data.frame is returned invisibly.

addSampleManually <- function(source_df = sourceDataFrame(),
                              new_sample_input,
                              update,
                              in_shiny = FALSE){
  
  # check sample name
  sample_name <- new_sample_input$sample
  
  if(sample_name %in% source_df$sample){
    
    msg <- glue::glue("Sample name '{sample_name}' is already in use.")
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop", in.shiny = in_shiny, with.time = FALSE)
    
  }
  
  # check and adjust tags
  new_sample_input$tags <- stringr::str_c(new_sample_input$tags, collapse = "|")
  
  # create status 
  new_sample_input$status <- 
    base::ifelse(
      test = new_sample_input$pathology == ""| base::is.na(new_sample_input$pathology),
      yes = "h",
      no = "p"
      )
  
  new_sample_input$date_added <- base::Sys.Date()
  
  source_df <- dplyr::add_row(.data = source_df, !!!new_sample_input) 
  
  if(base::isTRUE(update)){
    
    usethis::use_data(source_df, overwrite = TRUE)
    
    confuns::give_feedback(msg = "Updated data/source_df.rda", with.time = FALSE, in.shiny = in_shiny)
    
  } else if(!base::isTRUE(in_shiny)) {
    
    base::message("Did not update data/source_df.rda")
    
  }
  
  base::invisible(source_df)
  
}



#' @title Remove sample
#' 
#' @description Removes a sample from the \code{source_df} of the package.
#'
#' @param sample_name The sample name(s) to be removed.
#' @inherit addSampleManually params return
#' 
#' @details As a precaution \code{udpate} must be manually set to TRUE. 
#'

removeSampleManually <- function(sample_name, update){
  
  base::stopifnot(base::isTRUE(update))
  
  confuns::check_one_of(
    input = sample_name, 
    against = validSampleNames()
  )
  
  source_df <- 
    sourceDataFrame() %>% 
    dplyr::filter(!sample %in% {{sample_name}})
  
  usethis::use_data(source_df, overwrite = TRUE)
  
  base::invisible(source_df)
  
}





