


get_dribble <- function(dribble = NULL){
  
  if(base::is.null(dribble)){
    
    dribble <- base::get(x = "dribble_main", envir = .GlobalEnv)
    
  } 
  
  stopifnot(googledrive::is_dribble(dribble))
  
  return(dribble)
  
}

get_folder_content <- function(folder_name = NULL, 
                               folder_id = NULL, 
                               subset = FALSE, 
                               recursive = FALSE,
                               dribble = NULL){
  
  dribble <- get_dribble(dribble)
  
  if(is.character(folder_name)){
    
    dribble <- dplyr::filter(dribble, name == {{folder_name}})
    
    folder_id <- dribble$id
    
  } else if(!is.character(folder_id)) {
    
    stop("Specify `folder_name` or `folder_id`.")
    
  }
  
  
  gd_out <- googledrive::drive_ls(path = folder_id, recursive = recursive)
  
  if(nrow(gd_out) >= 1){
    
    if(subset == "files"){
      
      test_out <- stringr::str_detect(gd_out$name, pattern = "\\.([A-Za-z0-9]|_)*$")
      
      gd_out <- gd_out[test_out, ]
      
    } else if(subset == "folders"){
      
      test_out <- stringr::str_detect(gd_out$name, pattern = "\\.([A-Za-z0-9]|_)*$")
      
      gd_out <- gd_out[!test_out, ]
      
    }
    
  }
  
  return(gd_out)
  
}

get_file_ids <- function(folder_name, named = TRUE, recursive = FALSE, dribble = NULL){
  
  dribble <- get_dribble(dribble)
  
  fc_out <- 
    get_folder_content(
      folder_name = folder_name,
      subset = "files", 
      recursive = recursive, 
      dribble = dribble
    )
  
  out <- fc_out$id
  
  if(base::isTRUE(named)){
    
    out <- purrr::set_names(x = out, nm = fc_out$name)
    
  }
  
  return(out)
  
}

get_id <- function(name, dribble = NULL){
  
  dribble <- get_dribble(dribble)
  
  dplyr::filter(dribble, name == {{name}})$id
  
}

uniform_timezone <- function(dt){
  
  suppressMessages({
    
    lubridate::as_datetime(x = dt, tz = "Europe/Berlin")
    
  })
  
  
}

# SPATAData specific ------------------------------------------------------

# these functions expect an object called dribble_main in the global environment!

spata2v3_objects_id <- function(){
  
  get_id(name = "spata2v3_objects", dribble = NULL)
  
}

create_new_subfolder <- function(name){
  
  googledrive::drive_mkdir(name = name, path = spata2v3_objects_id(), overwrite = FALSE)
  
}

list_subfolders <- function(){
  
  get_folder_content(folder_name = "spata2v3_objects", recursive = FALSE)
  
}

get_subfolder_id <- function(name = NULL, id = NULL){
  
  dribble <- list_subfolders()
  
  if(is.character(name)){
    
    dribble <- dplyr::filter(dribble, name == {{name}})
    
  }
  
  if(is.character(id)){
    
    dribble <- dplyr::filter(dribble, id == {{id}})
    
  }
  
  return(dribble$id)
  
}

download_spata2_object <- function(sample_name){
  
  sub_sdf <- dplyr::filter(source_df, sample_name == {{sample_name}})
  
  loc <- sub_sdf$object_loc
  
  link <- sub_sdf$object_link
  
  ## continue here
  
}

upload_spata2_object <- function(dir, subfolder, overwrite = FALSE){
  
  id <- get_subfolder_id(name = subfolder)
  
  googledrive::drive_upload(media = dir, path = id, overwrite = overwrite)
  
}


get_spata2_object_dribble <- function(sample_name){
  
  name <- stringr::str_c(sample_name, ".RDS")
  
  dribble <- get_dribble(NULL)
  
  dplyr::filter(dribble, name == {{name}})
  
}

get_spata2_object_subfolder <- function(sample_name){
  
  if(!stringr::str_detect(sample_name, pattern = ".RDS$")){
    
    sample_name <- stringr::str_c(sample_name, ".RDS")
    
  }
  
  file_name <- list_spata2v3_objects(sample_name)
  
  stringr::str_remove(string = file_name, pattern = "spata2v3_objects/") %>% 
    stringr::str_remove(string = ., pattern = stringr::str_c("\\/", sample_name))
  
}

get_spata2_object_weblink <- function(sample_name){
  
  get_spata2_object_dribble(sample_name)[["drive_resource"]][[1]][["webContentLink"]]
  
}

last_time_modified_gd <- function(sample_name){
  
  dribble_sub <- get_spata2_object_dribble(sample_name = sample_name)
  
  mtime <- dribble_sub$drive_resource[[1]]$modifiedTime 
  
  uniform_timezone(mtime)
  
}

last_time_modified_local <- function(sample_name){
  
  fi <- 
    list_spata2v3_objects(sample_name) %>% 
    base::file.info()
  
  uniform_timezone(fi$mtime)
  
}


# 

get_overview_df <- function(){
  
  local_dirs <- list_spata2v3_objects(full_names = T)
  
  sample_names <- 
    purrr::map(
      .x = list.files("spata2v3_objects", full.names = TRUE, recursive = FALSE),
      .f = ~ 
        list.files(.x, full.names = FALSE, recursive = FALSE) %>% 
        stringr::str_subset(pattern = ".RDS$") %>% 
        stringr::str_remove(pattern = ".RDS$")
    ) %>% 
    purrr::flatten_chr()
  
  now <- uniform_timezone(Sys.time())
  
  odf <- 
    tibble::tibble(
      sample_name = {{sample_names}}, 
      dir_local = {{local_dirs}}, 
      subfolder = "",
      lm_local = {{now}} # create date_time
    )  
  
  for(i in 1:nrow(odf)){
    
    odf$lm_local[i] <- uniform_timezone(file.info(local_dirs[i])$mtime)
    odf$subfolder[i] <- get_spata2_object_subfolder(odf$sample_name[i])
    
  }
  
  dribble <- 
    get_dribble(dribble = NULL) %>% # expects dribble_main in .GlobalEnv
    dplyr::filter(stringr::str_detect(name, pattern = "\\.RDS$")) %>% 
    dplyr::mutate(
      sample_name = stringr::str_remove(name, pattern = "\\.RDS$"), 
      dir_web = purrr::map_chr(.x = drive_resource, .f = ~ .x$webContentLink), 
      lm_web = purrr::map(.x = drive_resource, .f = ~ uniform_timezone(.x$modifiedTime)) 
      ) %>% 
    tidyr::unnest(cols = lm_web)
  
  odf_merged <- 
    dplyr::left_join(x = odf, y = dribble, by = "sample_name") %>% 
    dplyr::mutate(
      is_uploaded = !base::is.na(name), 
      needs_update = dplyr::if_else(is_uploaded, lm_web < lm_local, FALSE) # lm web happened early that lm_local -> needs update on web
    ) %>% 
    dplyr::select(sample_name, id, name, lm_local, lm_web, subfolder, dir_local, dir_web, is_uploaded, needs_update, drive_resource)
  
  return(odf_merged)
  
}










