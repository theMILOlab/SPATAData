#' @title  getData
#' @author Dieter Henrik Heiland
#' @description getData
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 

getData <- function(sample.name, folder=NULL){
  
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

  
  data <- SPATAData::list.data() %>% dplyr::filter(Sample %in% sample.name)
  
  if(nrow(data)!=0){
    
    out <- purrr::map_df(.x=1:nrow(data), .f=function(i){
      
      #Set parameters
      link <- data[i, "link"]
      name <- data[i, "Sample"]
      localisation <- paste0(folder,"/",name)
      
      utils::download.file(link, paste0(name, ".RDS") )
      
      out <- data.frame(sample.name=name, localisation=localisation)
      return(out)
      
    })
    
  }else{" No match between sample.name and the listed data were found. Please use a valid sample.name/s"}
 
  setwd(org.wd)
  return(list(folder, out)) 
}



#' @title  list.data
#' @author Dieter Henrik Heiland
#' @description list.data
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
loadData <- function(sample.name, folder){
  
  file <- paste0(folder, "/", sample.name, ".RDS")
  
  if(file.exists(file)){readRDS(file)}else{"Sample is not in the download folder"}
  
}



#' @title  list.data
#' @author Dieter Henrik Heiland
#' @description list.data
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#' 
list.data <- function(){read.csv(system.file("data", "source.csv", package = "SPATAData"), sep=";")}


#' @title  CleanObject
#' @author Dieter Henrik Heiland
#' @description CleanObject
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#'

CleanObject <- function(object){
  
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
#' @inherit 
#' @return 
#' @examples 
#' 
#' @export
#'

AddSourceFile <- function(source.csv){
  
  message(paste0(Sys.time()," ---- Source file will be updated ---- "))
  path <- system.file("data", "source.csv", package = "SPATAData")
  source <- read.csv(source.csv,sep=";")
  write.table(source, file=path, sep=";")
  
}


#' Convert \code{data.frame} to \code{list}.
#' 
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param x A \code{data.frame} object.
#' @examples
#' my_result <- foo(iris)
#'
foo <- function(x) {
  x %>%
    as.list()
}











