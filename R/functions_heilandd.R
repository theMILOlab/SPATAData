#' @title  getData
#' @author Dieter Henrik Heiland
#' @description getData
#' @param type Can be either 'SPATA' to download the Spata objects or 'RAW' for access the raw data 
#' @return 
#' 

getData <- function(sample_name, folder=NULL, type="SPATA"){
  
  warning("This function is deprecated. Please use downloadSpataObject(), downloadSpataObjects() and downloadRawData()")
  
  org.wd <- getwd()
  
  if(is.null(folder)){
    folder=paste0(getwd(), "/Data.download")
    if(!file.exists(folder)){ 
      dir.create("Data.download"); 
      }else{
        "Download Folder '.../Data.download...' exists, data will be saved in the default location"
    }
  }else{
    if(file.exists(folder)){ 
      setwd(folder)}else{"Folder does not exist"}}
  
  setwd(folder)
  message(paste0(Sys.time()," ---- Downloads will be in the: ", folder, " folder ---- "))
  
  
  

# Check if sample name is part of the listed data ------------------------

  if (type=="SPATA"){
  
  data <- SPATAData::list.data() %>% dplyr::filter(Sample %in% sample_name) %>% dplyr::filter(type=="SPATA")
  
  if(nrow(data)!=0){
    
    out <- purrr::map_df(.x=1:nrow(data), .f=function(i){
      
      #Set parameters
      link <- data[i, "link"]
      name <- data[i, "Sample"]
      localisation <- paste0(folder,"/",name)
      
      utils::download.file(link, paste0(name, ".RDS") )
      
      out <- data.frame(sample_name=name, localisation=localisation)
      return(out)
      
    })
    
  }else{" No match between sample_name and the listed data were found. Please use a valid sample_name/s"}
  return(list(folder, out)) 
  
  }else{
    if(type=="RAW"){
      
      data <- SPATAData::list.data() %>% dplyr::filter(Sample %in% sample_name) %>% dplyr::filter(type=="RAW")
      
      if(nrow(data)!=0){
        
        out <- purrr::map_df(.x=1:nrow(data), .f=function(i){
          
          #Set parameters
          link <- data[i, "link"]
          name <- data[i, "Sample"]
          localisation <- paste0(folder,"/",name)
          
          utils::download.file(link, paste0(name, ".zip") )
          
          out <- data.frame(sample_name=name, localisation=localisation)
          return(out)
          
        })
        
      }else{" No match between sample_name and the listed data were found. Please use a valid sample_name/s ...  or raw data of samples not in source file"}
      
      return(list(folder, out)) 
      
    }else{message("Type undefined")}
  }
  
  setwd(org.wd)
  
}



#' @title  list.data
#' @author Dieter Henrik Heiland
#' @description list.data
#' @return 
#' 
loadData <- function(sample_name, folder){
  
  file <- paste0(folder, "/", sample_name, ".RDS")
  
  if(file.exists(file)){readRDS(file)}else{"Sample is not in the download folder"}
  
}



#' @title  list.data
#' @author Dieter Henrik Heiland
#' @description list.data
#' @return 
#' 
#' 
list.data <- function(){
  
  out <- 
    base::tryCatch({
      
      utils::read.csv(
        file = base::system.file("data", "source.csv", package = "SPATAData"),
        sep = ";"
        )
      
    }, error = function(error){
      
      NULL
      
    })
  
  if(base::is.data.frame(out)){
    
    out_df <- out 
    
  } else {
    
    out_df <- source_df %>% tibble::as_tibble()
    
  }
  
  return(out_df)
  
  }


#' @title  CleanObject
#' @author Dieter Henrik Heiland
#' @description CleanObject
#' @return 
#' 
#'

cleanObject <- function(object){
  
  message(paste0(Sys.time()," ---- Remove all slots with processed data ---- "))
  
  sample <- SPATA2::getSampleNames(object)
  object@cnv[[sample]] <- NULL
  object@spatial[[sample]] <- NULL
  object@fdata[[sample]] <- object@fdata[[sample]] %>% dplyr::select(barcodes,sample,segmentation,nCount_Spatial,nFeature_Spatial)
  
  return(object)
}


#' @title  AddSourceFile
#' @author Dieter Henrik Heiland
#' @description AddSourceFile
#' @return 
#' 
#'

addSourceFile <- function(source.csv){
  
  message(base::paste0(Sys.time()," ---- Source file will be updated ---- "))

  path <- base::system.file("data", "source.csv", package = "SPATAData")
  
  source <- utils::read.csv(source.csv, sep = ";")
  
  utils::write.table(source, file = path, sep = ";")

  path <- paste0(base::system.file("data", package = "SPATAData"), "/source.csv")
  source_df <- utils::read.csv(source.csv,sep=";")
  utils::write.table(source_df, file=path, sep=";")
  save(source_df, file = paste0(base::system.file("data", package = "SPATAData"), "/source_df.rda"))

  return(source_df[1:2,1:4])
  
}












