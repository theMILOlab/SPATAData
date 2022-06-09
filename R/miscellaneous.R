




# a -----------------------------------------------------------------------
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
#' @inherit SPATA2::argument_dummy params
#' @param sample_names Character vector of sample names for which
#' you want to obtain the correct citation.
#'
#' @return Citation in form of character vectors or a list of such.
#'
#' @export
#'

getCitation <- function(object){
  
  citation <- object@information$citation
  
  if(!base::is.character(citation)){
    
    warning("No citation found. Returning NULL.")
    
    citation <- NULL
    
  }
  
  return(citation)
  
}

#' @rdname getCitation
#' @export
getCitationBySample <- function(sample_names = validSampleNames()){
  
  confuns::check_one_of(
    input = sample_names,
    against = validSampleNames()
  )
  
  purrr::map(
    .x = sample_names,
    .f = function(sample){
      
      dplyr::filter(sourceDataFrame(), sample == {{sample}}) %>%
        dplyr::pull(citation) %>%
        base::unique()
      
    }
  ) %>%
    purrr::set_names(nm = sample_names)
  
}



# i -----------------------------------------------------------------------

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




#' @title Obtain the SPATAData \code{source_df}
#' 
#' @description Returns the \code{source_df} and allows some filtering.
#'
#' @param organ Character value. Filter for organs.
#' @param status Character value. Filter for tissue status. \emph{'p'} equals
#' pathological. \emph{'h'} equals healthy. 
#'
#' @return Data.frame.
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
#' spata object.
#'
#' @inherit SPATA2::argument_dummy params
#' @param citation Character value.
#'
#' @return An updated spata object.
#' @export
#'
setCitation <- function(object, citation){
  
  confuns::is_value(x = citation, mode = "character")
  
  object@information$citation <- citation
  
  return(object)
  
}



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
#' via \code{downloadSpataObject()}, \code{downloadSpataObject()}, 
#' \code{downloadRawData()}.
#' 
#' @param type Character value. If \emph{'SPATA'} returns valid input
#' options when it comes to download spata objects. If \emph{'RAW'}
#' returns valid input options when it comes to download 
#' whole raw 10X Visium data sets.
#'
#' @return Character vector.
#' @export
#'
validSampleNames <- function(){
  
  dplyr::pull(SPATAData::source_df, sample) %>% 
    base::unique()
  
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





