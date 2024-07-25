
######---------- script to populate google drive ----------#####

# 1. source packages and functions to deal with local files and google drive

source("scripts/init_session.R")
source("scripts/google_drive.R")

# 2. prepare dribble_main from SPATAData folder on google drive
library(googledrive)

dribble_all <- googledrive::drive_find(type = "folder")

dribble_main <- get_folder_content("SPATAData", recursive = TRUE, dribble = dribble_all)

# 3. obtain overview about file status locally and on web

overview_df <- get_overview_df()

# 4. print informative message about what needs to be done

ns <- nrow(overview_df)
n_is_upl <- sum(overview_df$is_uploaded)
n_not_upl <- sum(!overview_df$is_uploaded)
n_need_upd <- sum(overview_df$needs_update)

{
  message(glue("{ns} samples locally."))
  message(glue("{n_not_upl} samples will be uploaded."))
  message(glue("{n_is_upl} samples are uploaded of which {n_need_upd} samples need to be updated."))
}

# 5. iterate over samples that need to be updated

if(n_need_upd != 0){
  
  update_df <- dplyr::filter(overview_df, needs_update)
  
  message(glue("Updating {n_need_upd} samples."))
  for(i in 1:n_need_upd){
    
    instr <- update_df[i,]
    
    confuns::give_feedback(msg = glue("Updating sample {instr$sample_name}."))
    
    googledrive::drive_update(
      file = instr$id, 
      media = instr$dir_local
    )
    
    confuns::give_feedback(msg = "Update finished.")
    
  }
  
}


# 6. iterate over all samples that need to be uploaded

if(n_not_upl != 0){
  
  upload_df <- dplyr::filter(overview_df, !is_uploaded)
  
  # make sure that all subfolders exist
  subfolders <- unique(upload_df$subfolder)
  
  for(sf in subfolders){
    
    id <- get_subfolder_id(sf)
    
    if(length(id) == 0){
      
      message(glue::glue("Creating subfolder '{sf}' on google drive and updating dribble..."))
      
      googledrive::drive_mkdir(name = sf, path = spata2v3_objects_id())
      
      Sys.sleep(60)
      
      dribble_all <- googledrive::drive_find(type = "folder")
      dribble_main <- get_folder_content("SPATAData", recursive = TRUE, dribble = dribble_all)
      
    }
    
  }
  
  message(glue("Uploading {n_not_upl} samples."))
  
  for(i in 1:nrow(upload_df)){
    
    instr <- upload_df[i,]
    
    confuns::give_feedback(msg = glue("Uploading sample {instr$sample_name}."))
    
    googledrive::drive_upload(
      media = instr$dir_local, 
      path = get_subfolder_id(name = instr$subfolder), 
      name = stringr::str_c(instr$sample_name, ".RDS")
    )
    
    confuns::give_feedback(msg = "Upload finished.")
    
  }
  
}




