
# packages and functions required by all scripts --------------------------

library(devtools)
library(glue)
library(laborga)
library(SPATA2) #version >=3.0.0
library(tidyverse)
library(pryr)
load_all()



create_subfolder <- function(subfolder){
  
  dir.create(file.path("spata2v3_objects", subfolder))
  
}


extract_sample_name <- function(dir){
  
  stringr::str_remove(dir, pattern = "\\.RDS$") %>% 
    stringr::str_remove(pattern = "spata2v3_objects\\/.*\\/")
  
}


getObsUnit <- function(object){
  
  getSpatialMethod(object)@observational_unit
  
}

list_raw_visium <- function(){
  
  folders <- 
    stringr::str_c("/Users/heilandr/lab/data/spatial_seq/raw/10XVisium") %>%
    base::list.files(full.names = T)
  
  map(folders, .f = ~ list.files(.x, full.names = T)) %>% 
    flatten_chr()
  
}


list_spata2v3_objects <- function(pattern = NULL, full_names = TRUE){
  
  out <- 
    list.files(path = "spata2v3_objects", full.names = full_names, recursive = T) %>% 
    stringr::str_subset(pattern = ".RDS$")
  
  if(is.character(pattern)){
    
    out <- stringr::str_subset(out, pattern= pattern)
    
  }
  
  return(out)
  
}

nTissueSections <- function(object){
  
  containsTissueOutline(object, error = TRUE)
  
  getMetaDf(object) %>% 
    dplyr::filter(tissue_section != "tissue_section_0") %>% 
    dplyr::pull(tissue_section) %>% 
    dplyr::n_distinct()
  
}

open_overview_pdf <- function(subfolder){
  
  pdf(file.path("spata2v3_objects", subfolder, "sample_overview.pdf"))
  
}

plot_overview <- function(object){
  
  p_overview <- 
    (plotSurface(object, pt_alpha = 0) + labs(subtitle = object@sample)) +
    (plotSurface(object, color_by = "tissue_section") + legendBottom())
  
  plot(p_overview)
  
}

read_matrix_mtx <- function(dir){
  
  all_files <- list.files(dir, full.names = T)
  
  dir_mtr <- str_subset(all_files, ".mtx.gz$")
  dir_bcs <- str_subset(all_files, "barcodes.tsv.gz")
  dir_features <- str_subset(all_files, "features.tsv.gz")
  
  mtr <- Matrix::readMM(dir_mtr)
  bcs <- readr::read_tsv(dir_bcs, col_names = F)
  feats <- readr::read_tsv(dir_features, col_names =F)
  
  colnames(mtr) <- as.character(bcs[[1]])
  rownames(mtr) <- as.character(feats[[2]])
  
  return(mtr)
  
}

read_spata2v3_object <- function(pattern){
  
  dir <- 
    list_spata2v3_objects() %>% 
    stringr::str_subset(pattern = pattern)
  
  if(length(dir) > 1){
    
    print(dir)
    
    stop("More than one directory matches `pattern`.")
    
  } else if(length(dir) == 0){
    
    stop("No directory matches `pattern`.")
    
  }
  
  message(glue::glue("Reading '{dir}'."))
  
  readRDS(dir)
  
}
