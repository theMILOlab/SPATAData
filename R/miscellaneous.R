




# a -----------------------------------------------------------------------
#' @title Add sample tags manually
#' 
#' @description Adds tags to a sample. 
#'
#' @param tags Character vector of tags.
#' @inherit addSampleManually params return
#' 
#' @keywords internal
#' 
addSampleTagsManually <- function(source_df = sourceDataFrame(), 
                                  sample_name, 
                                  tags,
                                  update){
  
  confuns::check_one_of(
    input = sample_name, 
    against = source_df$sample
  )
  
  if(base::any(stringr::str_detect(tags, pattern = "|"))){
    
    stop("Tags must not contain '|'.")
    
  }
  
  confuns::check_none_of(
    input = tags, 
    against = sampleTags(source_df = source_df, sample_name = sample_name), 
    ref.against = glue::glue("tags of sample '{sample_name}'")
  )
  
  tags_input <- 
    stringr::str_c(tags, collapse = "|") %>% 
    stringr::str_c("|", ., "|")
  
  tags_sample <- 
    dplyr::filter(source_df, sample == {{sample_name}}) %>% 
    dplyr::pull(tags)
  
  tags_both <- stringr::str_c(tags_sample, tags_input, sep = "|")
  
  source_df[source_df$sample == sample_name, "tags"] <- tags_both
  
  if(base::isTRUE(update)){
    
    usethis::use_data(source_df, overwrite = TRUE)
    
  } else {
    
    base::message("Did not update data/source_df.rda")
    
  }
  
  base::invisible(source_df)
  
}


#' @title Adjust sample info manually
#' 
#' @description Instead of adding a new sample the samples info can be 
#' manually adjusted using this function.
#' @param sample_name Character value. The sample that is supposed to 
#' be adjusted.
#' @param adjust_sample_input A named list. Each slot 
#' corresponds to the variable of \code{source_df} that is supposed
#' to be adjusted. 
#' @inherit addSampleManually params return
#' 
#' @keywords internal
#' 
#' @examples 
#' 
#'  # adjust image link
#'  
#'  adjustSampleManually(
#'     source_df = sourceDataFrame(), # default 
#'     sample_name = "275_T", 
#'     adjust_sample_input = list(link_image = "https://some-link-to-image.jpeg?dl=1")
#'     update = TRUE # immediately saves the result in /data/source_df.rda
#'     )
#'
adjustSampleManually <- function(source_df = sourceDataFrame(), 
                                 sample_name,
                                 adjust_sample_input,
                                 update){
  
  input <- confuns::keep_named(adjust_sample_input)
  
  confuns::is_value(sample_name, "character")
  
  confuns::check_one_of(
    input = sample_name, 
    against = base::unique(source_df$sample)
  )
  
  confuns::check_one_of(
    input = base::names(input), 
    against = all_variables %>% confuns::vselect(-sample), 
    ref.input = "sample input"
  )
  
  for(var in base::names(input)){
    
    base::message(glue::glue("Adjusting: {var}."))
    
    source_df[source_df$sample == sample_name, var] <- input[[var]]
    
  }
  
  if(base::isTRUE(update)){
    
    usethis::use_data(source_df, overwrite = TRUE)
    
  }
  
  return(source_df)
  
}

#' @keywords internal
adjustSourceDataFrame <- function(source_df = sourceDataFrame()){
  
  dplyr::mutate(
    .data = source_df, 
    dplyr::across(
      .cols = dplyr::starts_with("link"), 
      .fns = base::as.character
    )
  )
  
}


# c -----------------------------------------------------------------------

#' @keywords internal
checkSourceDataFrame <- function(source_df = sourceDataFrame(), ...){
  
  chr_vars <- c(selectize_variables, text_variables, "tags")
  num_vars <- c("stage")
  
  confuns::check_data_frame(
    df = source_df, 
    var.class = 
      list(
        purrr::map(
          .x = chr_vars,
          .f = function(v){ "character"}
        ) %>%
          purrr::set_names(nm = chr_vars),
        purrr::map(
          .x = num_vars, 
          .f = function(v){ "numeric"}
        ) %>% 
          purrr::set_names(nm = num_vars)
      ) %>% 
      purrr::flatten(), 
    ...
  )
  
}



# g -----------------------------------------------------------------------

#' @title Obtain citation info
#'
#' @description If you have used a downloadable spata object
#' please use this function to obtain the proper
#' form of citation to give credits to the researchers who
#' made this data available
#'
#' @param object An object of class `SPATA2`.
#' @param write_lines Logical. If `TRUE`, the string is printed in the
#' console via `writeLines()`.
#' @param sample_names Character vector of sample names for which
#' you want to obtain the correct citation.
#'
#' @return Citation and - if one of SPATAData objects - the download source.
#'
#' @export
#'

getCitation <- function(object, write_lines = TRUE){
  
  sample_name <- getSampleName(object)
  citation <- object@meta_sample$pub_citation
  
  if(!base::is.character(citation)){
    
    warning("No valid citation found. Returning NULL.")
    
    citation <- NULL
    
    return(citation)
    
  } else {
    
    citation <- stringr::str_c("Citation: ", citation)
    
  }
  
  if(sample_name %in% source_df$sample_name){
    
    source <- 
      dplyr::filter(source_df, sample_name == {{sample_name}}) %>% 
      dplyr::pull(source) %>% 
      stringr::str_replace_all(pattern = "_", replacement = " ") %>% 
      stringr::str_c("\nSource: ", .)
    
    citation <- stringr::str_c(citation, source)
    
  }
  
  if(base::isTRUE(write_lines)){
    
    writeLines(citation)
    
  } else {
    
    return(citation)
    
  }
  
}

#' @rdname getCitation
#' @export
getCitationBySample <- function(sample_names){
  
  confuns::check_one_of(
    input = sample_names,
    against = validSampleNames()
  )
  
  purrr::walk(
    .x = sample_names,
    .f = function(sample_name){
      
      citation <- 
        dplyr::filter(source_df, sample_name == {{sample_name}}) %>% 
        dplyr::pull(pub_citation)
      
      citation <- stringr::str_c("\nCitation: ", citation)
      
      citation <- stringr::str_c("Sample: ", sample_name, citation)
      
      source <- 
        dplyr::filter(source_df, sample_name == {{sample_name}}) %>% 
        dplyr::pull("source") %>% 
        stringr::str_replace_all(pattern = "_", replacement = " ") %>% 
        stringr::str_c("\nSource: ", .)
      
      citation <- stringr::str_c(citation, source)
      
      if(length(sample_names) > 1){
        
        citation <- stringr::str_c(citation, "\n-------------------------")
        
      }
      
      writeLines(citation)
      
  }) 
  
}



# i -----------------------------------------------------------------------

#' @keywords internal
is_creatable <- function(file){
  
  base::tryCatch({
    
    base::saveRDS(object = list(), file = file)
    
    base::file.remove(file)
    
    TRUE
    
  }, error = function(error){
    
    FALSE
    
  })
  
}




# p -----------------------------------------------------------------------

#' @keywords internal
plotSampleImage <- function(sample_name, which = "lowres"){
  
  if(FALSE){
    
    link_image <- 
      sourceDataFrame(sample = sample_name) %>% 
      dplyr::pull(var = "link_image")
    
    # download image from path
    if(base::length(link_image) == 1 && !base::is.na(link_image)){
      
      img <- 
        base::tryCatch({
          
          magick::image_read(path = link_image)
          
        }, error = function(error){
          
          NULL
          
        })
      
      if(!base::is.null(img)){
        
        grDevices::as.raster(img) %>% 
          base::plot()
        
      } else {
        
        magick::image_read(path = "https://grrrls.at/wp-content/uploads/2020/01/no-image.jpg") %>%
          grDevices::as.raster() %>% 
          base::plot()
        
      }
      
    } else {
      
      magick::image_read(path = "https://grrrls.at/wp-content/uploads/2020/01/no-image.jpg") %>%
        grDevices::as.raster() %>% 
        base::plot()
      
    }
    
  } else { # get image from image list
    
    img <- image_list[[sample_name]]
    
    if(base::is.null(img)){
      
      magick::image_read(path = "https://grrrls.at/wp-content/uploads/2020/01/no-image.jpg") %>%
        grDevices::as.raster() %>% 
        base::plot()
      
    } else {
      
      grDevices::as.raster(img) %>% 
        base::plot()
      
    }
    
  }
  
  
  
}








#' @keywords internal
load_data_file <- function(directory){
  
  if(stringr::str_detect(string = directory, pattern = ".csv$")){
    
    df <- 
      base::suppressMessages({
        
        base::suppressWarnings({
          
          readr::read_csv(file = directory)
          
        })
        
      })
    
  }
  
  if(stringr::str_detect(string = directory, pattern = ".xlsx$")){
    
    df <- readxl::read_xlsx(path = directory, sheet = 1)
    
  }
  
  if(stringr::str_detect(string = directory, pattern = ".xls")){
    
    df <- readxl::read_xls(path = directory, sheet = 1)
    
  }
  
  if(stringr::str_detect(string = directory, pattern = ".txt")){
    
    df <- utils::read.delim(file = directory, header = TRUE)
    
  }
  
  if(stringr::str_detect(string = directory, pattern = ".RDS$")){
    
    df <- base::readRDS(file = directory)
    
  }
  
  if(tibble::has_rownames(df)){
    
    df <- tibble::rownames_to_column(df, var = "rownames")
    
  }
  
  return(df)
  
}




#' @title Obtain the SPATAData source data.frame
#' 
#' @description Returns the \link[=source_df]{source data.frame}.
#' 
#' @return Data.frame in which each row corresponds to a spatial data set stored
#' in a `SPATA2` object.
#' 
#' @examples
#' 
#' library(SPATA2)
#' library(SPATAData)
#' library(dplyr)
#' library(stringr)
#' 
#' sdf <- sourceDataFrame()
#' 
#' #----- example dplyr logic to filter the source data.frame
#' 
#' # 1. obtain glioblastoma samples from the temporal lobe
#' 
#' sdf_gbm <- filter(sdf, histo_class == "Glioblastoma" & organ_part == "temporal")
#' 
#' gbm_samples <- sdf_gbm$sample_name
#' 
#' print(gbm_samples)
#' 
#' # downlaod as collection
#' downloadSpataObjects(sample_names = gbm_samples, folder = "spata_objects/gbm") 
#' 
#' # 2. obtain data from specific publications
#' 
#' sdf_kuppe <- 
#'  filter(sdf, str_detect(pub_citation, pattern = "^Kuppe") & pub_journal == "Nature")
#'  
#' kuppe_samples <- sdf_kuppe$sample_name
#' 
#' print(kuppe_samples)
#'
#' @export
#'
sourceDataFrame <- function(){
  
  source_df
  
}



#' @title Obtain all sample tags
#' 
#' @description Returns sample tags of the \code{source_df}.
#' 
#' @param sample_name Character value. The sample of interest.
#'
#' @return Character vector.
#'
sourceDataFrameTags <- function(source_df = sourceDataFrame(), sample_name = NULL){
  
  if(base::is.character(sample_name)){
    
    confuns::check_one_of(
      input = sample_name, 
      against = source_df$sample
    )
    
    source_df <- dplyr::filter(source_df, sample == {{sample_name}})
    
  }
  
  stringr::str_split(source_df$tags, pattern = "\\;") %>% 
    purrr::flatten_chr() %>% 
    base::unique()
  
}


#' @rdname sourceDataFrameTags
sourceDataFrameOrgans <- function(source_df = sourceDataFrame()){
  
  source_df$organ %>% base::unique()
  
}


#' @rdname sourceDataFrameTags
sampleTags <- function(source_df = sourceDataFrame(), sample_name){
  
  confuns::check_one_of(
    input = sample_name, 
    against = source_df$sample
  )
  
  tags <- 
    dplyr::filter(source_df, sample == {{sample_name}}) %>% 
    dplyr::pull(tags)
  
  stringr::str_split(tags, pattern = "\\|") %>% 
    purrr::flatten_chr()
  
}


#' @keywords internal
filterSamples <- function(source_df = sourceDataFrame(), input, organ, status, all){
  
  test <- base::ifelse(test = base::isTRUE(all), yes = "all", no = "any")
  
  sdf <- dplyr::filter(source_df, organ == {{organ}} & status == {{status}})
  
  input_filter <- confuns::lselect(lst = input, dplyr::starts_with("filter_sample"))
  
  if(status == "h"){
    
    variables <- confuns::vselect(filter_sample_variables, -pathology)
    
  } else if(status == "p"){
    
    variables <- filter_sample_variables
    
  }
  
  if(test == "all"){
    
    for(variable in variables){
      
      var_id <- stringr::str_c("filter_sample", organ, status, variable, sep = "_")
      
      content <- input_filter[[var_id]]
      
      if(base::length(content) >= 1){
        
        if(variable == "tags"){
          
          sdf <- 
            dplyr::filter(sdf, stringr::str_detect(string = tags, pattern = stringr::str_c(content, collapse = "|"))) %>% 
            dplyr::pull(sample)
          
        } else {
          
          sdf <- dplyr::filter(sdf, !!rlang::sym(variable) %in% {{content}})  
          
        }
        
      }
      
    }
    
    out <- sdf$sample
    
  } else if(test == "any"){
    
    snames <- list()
    
    for(variable in variables){
      
      content <- 
        confuns::lselect(
          lst = input_filter,
          dplyr::ends_with(stringr::str_c(status, variable, sep = "_"))
          )
      
      content <- content[[1]]
      
      if(base::length(content) >= 1){
        
        if(variable == "tags"){
          
          snames[[variable]] <- 
            dplyr::filter(sdf, stringr::str_detect(string = tags, pattern = stringr::str_c(content, collapse = "|"))) %>% 
            dplyr::pull(sample)
          
        } else {
          
          snames[[variable]] <- 
            dplyr::filter(sdf, !!rlang::sym(variable) %in% {{content}}) %>% 
            dplyr::pull(sample)
          
        }
        
      }
      
    }
    
    out <- 
      purrr::flatten_chr(snames) %>% 
      base::unique()
    
  }
  
  return(out)
  
}





# r -----------------------------------------------------------------------

#' @export
rgx_lookahead <- function(pattern, negate = FALSE, match = ".*"){
  
  if(base::isFALSE(negate)){
    
    out <- stringr::str_c(match, "(?=", pattern, ")", sep = "")
    
  } else if(base::isTRUE(negate)){
    
    out <- stringr::str_c(match, "(?!=", pattern = ")", sep = "")
    
  }
  
  return(out)
  
}

#' @export
rgx_lookbehind <- function(pattern, negate = FALSE, match = ".*"){
  
  if(base::isFALSE(negate)){
    
    out <- stringr::str_c("(?<=", pattern, ")", match, sep = "")
    
  } else if(base::isTRUE(negate)){
    
    out <- stringr::str_c("(?!<=", pattern = ")", match, sep = "")
    
  }
  
  return(out)
  
}



# s -----------------------------------------------------------------------

#' @title Set citation info
#'
#' @description Sets information about how to cite this
#' `SPATA2` object.
#'
#' @inherit getCitation params
#' @param citation Character value containing the complete citation.
#' 
#' @details
#' Input for `citation` is set in @@meta_sample$pub_citation.
#' 
#'
#' @return The updated input object, containing the added, removed or computed results.
#' @export
#'
setCitation <- function(object, citation){
  
  confuns::is_value(x = citation, mode = "character")
  
  object@information$citation <- citation
  
  return(object)
  
}


#' @keywords internal
#' @export
str_extract_before <- function(string,
                               pattern,
                               match = "^.*",
                               negate = FALSE,
                               cut.right = TRUE,
                               cut.left = TRUE){
  
  out <- 
    stringr::str_extract(
      string = string, 
      pattern = rgx_lookahead(pattern = pattern, negate = negate, match = match)
    )
  
  if(base::isTRUE(cut.right)){
    
    out <- stringr::str_remove(out, pattern = " *$")
    
  }
  
  if(base::isTRUE(cut.left)){
    
    out <- stringr::str_remove(out, pattern = "^ *")
    
  }
  
  return(out)
  
}

#' @keywords internal
#' @export
str_extract_after <- function(string,
                              pattern,
                              match = ".*$",
                              negate = FALSE,
                              cut.right = TRUE,
                              cut.left = TRUE){
  
  out <- 
    stringr::str_extract(
      string = string,
      pattern = rgx_lookbehind(pattern = pattern, negate = negate, match = match)
    )
  
  if(base::isTRUE(cut.right)){
    
    out <- stringr::str_remove(out, pattern = " *$")
    
  }
  
  if(base::isTRUE(cut.left)){
    
    out <- stringr::str_remove(out, pattern = "^ *")
    
  }
  
  return(out)
  
}




# v -----------------------------------------------------------------------

#' @title Valid sample names
#'
#' @description Returns the sample names of the samples that you can download
#' via \code{downloadSpataObject()}, \code{downloadSpataObject(()}.
#'
#' @return Character vector.
#' @export
#'
validSampleNames <- function(){
  
  sourceDataFrame()$sample_name
  
}



# w -----------------------------------------------------------------------



#' @title Create vectors of WHO-Grades
#'
#' @export
#'
WHOGrades <- function(x, suffixes = NULL, combine = FALSE){
  
  if(base::is.list(suffixes)){

    if(!confuns::is_named(suffixes) && base::length(suffixes) >= 1){
      
      base::names(suffixes) <- 
        base::seq_along(suffixes) %>% 
        utils::as.roman() %>% 
        base::as.character()
      
    }
    
    out <- 
      purrr::map(
        .x = x, 
        .f = function(xi){
          
          val <-
            utils::as.roman(xi) %>% 
            base::as.character()
          
          suffixesx<- suffixes[[val]]
          
          vals <- stringr::str_c(val, suffixesx, sep = "")
          
          if(!base::is.null(suffixesx)){
            
            if(base::is.list(combine)){
              
              combinex <- combine[[val]]
              
            } else {
              
              combinex <- combine
              
            }
            
            if(base::isTRUE(combinex)){
              
              vals <- c(val, vals)
              
            }
            
          }
          
          return(vals)
          
        }
      ) %>% 
        purrr::flatten_chr()
    
  } else {
    
    out <- 
      purrr::map(
        .x = x, 
        .f = function(xi){
          
          val <-
            utils::as.roman(xi) %>% 
            base::as.character()
          
          vals <-  
            stringr::str_c(val, suffixes, sep = "") %>% 
            base::unique()
          
          if(!base::is.null(suffixes)){
            
            if(base::is.list(combine)){
              
              combinex <- combine[[val]]
              
            } else {
              
              combinex <- combine
              
            }
            
            if(base::isTRUE(combinex)){
              
              vals <- c(val, vals)
              
            }
            
          }
          
          return(vals)
          
        }
      ) %>% 
        purrr::flatten_chr()
    
  }
  
  return(out)
  
}





