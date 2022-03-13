



#' @title Add sample tags manually
#' 
#' @description Adds tags to a sample. 
#'
#' @param tags Character vector of tags.
#' @inherit addSampleManually params return
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



#' @title Plot the sample image
plotSampleImage <- function(sample_name, which = "lowres"){
  
  image_dir <- stringr::str_c("images/", sample_name, "/tissue_", which, "_image.png")
  
  if(base::file.exists(image_dir)){
    
    img <- 
      magick::image_read(path = image_dir) %>% 
      grDevices::as.raster()
    
    plot(img)
    
  }
  
}


#' @title Obtain the SPATAData \code{source_df}
#' 
#' @description Returns the \code{source_df} and allows some filtering.
#'
#' @param organ Character value. Filter for organs.
#' @param status Character value. Filter for tissue status. \emph{'p'} equals
#' pathological. \emph{'h'} equals healthy. 
#'
#' @return Data.frame.
#' @export
#'
sourceDataFrame <- function(organ = NULL, status = NULL, sample = NULL){
  
  df <- SPATAData::source_df
  
  if(base::is.character(organ)){
    
    df <- dplyr::filter(df, organ %in% {{organ}})
    
  }
  
  if(base::is.character(status)){
    
    df <- dplyr::filter(df, status %in% {{status}})
    
  }
  
  if(base::is.character(sample)){
    
    df <- dplyr::filter(df, sample %in% {{sample}})
    
  }
  
  return(df)
  
}



#' @title Obtain all sample tags
#' 
#' @description Returns sample tags of the \code{source_df}.
#' 
#' @param sample_name Character value. The sample of interest.
#'
#' @return Character vector.
#' @export
#'
sourceDataFrameTags <- function(source_df = sourceDataFrame(), sample_name = NULL){
  
  if(base::is.character(sample_name)){
    
    confuns::check_one_of(
      input = sample_name, 
      against = source_df$sample
    )
    
    source_df <- dplyr::filter(source_df, sample == {{sample_name}})
    
  }
  
  stringr::str_split(source_df$tags, pattern = "\\|") %>% 
    purrr::flatten_chr() %>% 
    base::unique()
  
}


#' @rdname sourceDataFrameTags
#' @export
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


# string handling ---------------------------------------------------------

rgx_lookahead <- function(pattern, negate = FALSE, match = ".*"){
  
  if(base::isFALSE(negate)){
    
    out <- stringr::str_c(match, "(?=", pattern, ")", sep = "")
    
  } else if(base::isTRUE(negate)){
    
    out <- stringr::str_c(match, "(?!=", pattern = ")", sep = "")
    
  }
  
  return(out)
  
}

rgx_lookbehind <- function(pattern, negate = FALSE, match = ".*"){
  
  if(base::isFALSE(negate)){
    
    out <- stringr::str_c("(?<=", pattern, ")", match, sep = "")
    
  } else if(base::isTRUE(negate)){
    
    out <- stringr::str_c("(?!<=", pattern = ")", match, sep = "")
    
  }
  
  return(out)
  
}

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










