
######---------- script to populate source data.frame ----------#####

# 1. source packages and functions to deal with local files 

source("scripts/init_session.R")

library(googledrive)

dribble_all <- googledrive::drive_find(type = "folder")

dribble_main <- get_folder_content("SPATAData", recursive = TRUE, dribble = dribble_all)

overview_df <- get_overview_df()

# 2. list all local object directories

all_dirs <- list_spata2v3_objects() 

sdf <- 
  purrr::map_df(
    .x = all_dirs, 
    .f = function(dir){
      
      sample_name <- extract_sample_name(dir)
      
      source_df_sub <- 
        filter(source_df, sample_name == {{sample_name}}) 
      
      odf_sub <- 
        dplyr::filter(overview_df, sample_name == {{sample_name}})
      
      # needs to be updated? 
      if(sample_name %in% source_df$sample_name){
        
        lm_source <- 
          dplyr::pull(source_df_sub, "lm_source")
        
        lm_local <- uniform_timezone(file.info(dir)$mtime)
        
        # has the latest modification of the locally stored update happened 
        # after the last modifiation of the its corresponding rwo in the source data.frame?
        if(lm_local > lm_source){
          
          continue <- TRUE
          
        } else {
          
          continue <- FALSE
          
        }
        
      } else { # has not been added yet
        
        continue <- TRUE
        
      }
      
      if(continue){
        
        confuns::give_feedback(msg = glue::glue("Reading {dir}."))
        
        object <- readRDS(dir)
        
        df <- getSampleMetaData(object, as_list = F, source_only = T, na_rm = F)
        
        # populate slots 
        count_mtr <- getCountMatrix(object) %>% as.matrix()
        
        df$mean_counts <- mean(colSums(count_mtr), na.rm = T)
        df$median_counts <- median(colSums(count_mtr), na.rm = T)
        
        rm(count_mtr)
        
        df$modality_gene <- containsModality(object, modality = "gene")
        df$modality_metabolite <- containsModality(object, modality = "metabolite")
        df$modality_protein <- containsModality(object, modality = "protein")
        
        df$n_obs <- nObs(object)
        df$n_tissue_sections <- nTissueSections(object)
        df$obs_unit <- getObsUnit(object)
        
        
      } else {
        
        confuns::give_feedback(msg = glue::glue("Skipping {dir}."))
        
        df <- source_df_sub
        
      }
      
      df$web_link <-  odf_sub$dir_web
      df$source <- odf_sub$subfolder
      
      return(df)
      
    }
  )

# 3. check and test
View(sdf)


# 4. use as data
source_df <- sdf

use_data(source_df, overwrite = T)



