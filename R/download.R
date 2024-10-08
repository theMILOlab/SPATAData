
adjust_gdrive_link <- function(initial_url){
  
  initial_response <- httr::GET(initial_url)
  
  if(httr::status_code(initial_response) == 200){
    
    html_content <- httr::content(initial_response, as = "text")
    
    html_page <- rvest::read_html(html_content)
    
    form_action <- 
      rvest::html_node(html_page, "form#download-form") %>%
      rvest::html_attr("action")
    
    params <-
      rvest::html_nodes(html_page, "form#download-form input[type='hidden']") %>%
      rvest::html_attrs()
    
    query_params <- list()
    
    for(param in params) {
      
      query_params[[param["name"]]] <- param["value"]
      
    }
    
    final_download_url <- httr::modify_url(form_action, query = query_params)
    
    return(final_download_url)
    
  } else {
    
    stop(glue::glue("Unable to download SPATA2 object directy. Pleae enter this weblink directly in the browser and download manually: {initial_url}"))
    
  }
  
}



#' @title Download a SPATA2 object
#'
#' @description Downloads a single `SPATA2` object and returns it. 
#'
#' @param sample_name Character value. The name of the sample you want to
#' download. Use \code{validSampleNames()} to obtain all valid input options.
#' @param file If you want to save the object on disc:
#' The filename of the `SPATA2` object. Must end with \emph{'.RDS'} if provided
#' as a character. If `NULL`, the function saves the object 
#' under the sample name with an *'.RDS'* suffix. If `FALSE`, the saving 
#' is skipped and the object is simply returned.
#' @param folder Character value. If character, specifies the output
#' folder in which the `SPATA2` object is saved. Defaults to the working directory.
#' @param overwrite Logical. Must be set to `TRUE` if file directories
#' under which downloaded files are to be saved already exist.
#' @param adjust_link Logical value. Defaults to `TRUE`. Allows the function to adjust the link
#' if the download fails due to Google Drive warnings. See section below
#' for more information. 
#' @param timeout Numeric value. Indicates the number of seconds the function has time
#' to download the object before the process is aborted. Defaults to 600s (10min).
#' @inherit argument_dummy params
#'
#' @details If `file` is not `FALSE`. The downloaded `SPATA2` object is immediately saved after the download before
#' it is returned by the function. Note that the file directory is assembled by combining
#' `folder` and `file`!
#'
#' @return The downloaded `SPATA2` object.
#' 
#' @seealso For convenient downloads of multiple `SPATA2` objects 
#' check out [`downloadSpataObjects()`].
#' 
#' @export
#' 
#' @section Google Drive Warning:
#' `SPATA2` objects are stored in a Google Drive repository and downloaded via their weblink as 
#' stored in the \link[=sourceDataFrame]{source data.frame}. Often, `SPATA2` objects are too 
#' large for the automatic Google Drive virus scan. As a result, the weblink initially leads 
#' to a webpage that asks if you are okay with skipping this virus scan.
#' 
#' In cases where the Google Drive link leads to this warning page, the function
#' will automatically adjust the download link to bypass the warning and attempt
#' the download again. If the adjusted download still fails, an error message is 
#' displayed, prompting the user to manually download the file using the provided link.
#' 
#' Since the virus scan cannot be performed by Google Drive regardless of whether 
#' you download it from within R or manually, the function defaults to bypass this
#' warning automatically. If you prefer not to bypass the warning, you can set `adjust_link = FALSE`.
#' In this case, the function will give a warning and ask you to download the object manually.
#' 
#' The downloaded objects do not contain viruses. The way they have been created,
#' uploaded as well as how the web links are added to the \link[=sourceDataFrame]{source data.frame} 
#' can be reconstructed with the *populate_<location>* scripts provided on 
#' the [SPATAData repository](https://github.com/theMILOlab/SPATAData) on github under */scripts/*.
#'
#' @examples
#'
#' # download & assign (no saving on the disk)
#' object <- downloadSpataObject(sample_name = "UKF275T")
#'
#' # download, assign and save on disk
#' # -> stores the file under ~/UKF275T.RDS (where '~' is your working directory)
#' object <- downloadSpataObject(sample_name = "UKF275T", file = TRUE)
#' 
#' # download, assign and save on disk in a specified directory
#' object <- downloadSpataObject(sample_name = "UKF275T", file = "my/path/to/spata_object.RDS")
#'
downloadSpataObject <- function(sample_name,
                                overwrite = FALSE,
                                file = FALSE,
                                adjust_link = TRUE,
                                timeout = 600,
                                verbose = TRUE,
                                ...){

  require(SPATA2)
  
  confuns::is_value(x = overwrite, mode = "logical")

  in_shiny <- base::isTRUE(list(...)[["in_shiny"]])
  
  source_df <- list(...)[["source_df"]]

  if(base::is.null(source_df)){

    source_df <- sourceDataFrame()

  }

  confuns::check_one_of(
    input = sample_name,
    against = base::unique(source_df$sample_name), 
    fdb.opt = 2, 
    ref.opt.2 = "SPATAData sample names"
  )

  download_dir <-
    dplyr::filter(source_df, sample_name == {{sample_name}}) %>%
    dplyr::pull(web_link)
  
  if(!shiny::isTruthy(download_dir)){
    
    confuns::give_feedback(
      msg = glue::glue("Could not find valid link to `SPATA2` object for sample {sample_name}."), 
      fdb.fn = "stop", 
      in.shiny = in_shiny
    )
    
  }

  if(base::is.character(file)){

    if(!stringr::str_detect(file, pattern = ".RDS$|.rds$")){

      stop("Input for argument `file` must end with either '.RDS' or '.rds'")

    }

    directory_spata <- file

    if(base::file.exists(directory_spata)){

      if(!base::isTRUE(overwrite)){

        stop(glue::glue("File '{directory_spata}' already exists. Set argument `overwrite` to TRUE in order to overwrite it."))

      }

    } else {

      # test if dir is creatable

      x <- list()

      valid_dir <-
        base::tryCatch({

          base::saveRDS(object = x, file = directory_spata)

          base::file.remove(directory_spata)

          TRUE

        }, error = function(error){

          FALSE

        })

      if(!valid_dir){

        stop(glue::glue("File '{directory_spata}' can not be created."))

      }

    }

  }

  confuns::give_feedback(
    msg = glue::glue("Downloading `SPATA2` object '{sample_name}' from '{download_dir}'."),
    verbose = verbose, 
    in.shiny = in_shiny
    )
  
  if(base::is.numeric(timeout)){ options(timeout = timeout)}

  # download the object
  downloaded_object <- 
    base::tryCatch({
      
      base::url(download_dir) %>%
        base::readRDS()
      
    }, error = function(error){
      
      FALSE
      
    })
  
  # adjust directory to circumvent warning page of googledrive which mentions
  # that virusscan is not possible
  if(base::isFALSE(downloaded_object) & base::isTRUE(adjust_link)){
    
    confuns::give_feedback(
      msg = "Adjusting weblink for download.", 
      verbose = verbose
    )
    
    download_dir_adj <- adjust_gdrive_link(download_dir)
    
    confuns::give_feedback(
      msg = glue::glue("Trying: {download_dir_adj}."), 
      verbose = verbose
    )
    
    downloaded_object <- 
      base::tryCatch({
        
        base::url(download_dir_adj) %>%
          base::readRDS()
        
      }, error = function(error){
        
        FALSE
        
      })
    
  }
  
  if(base::isFALSE(downloaded_object)){
    
    stop(glue::glue("Unable to download SPATA2 object directy. Pleaes enter this weblink directly in the browser and download manually: {download_dir}"))
    
  }
  
  confuns::give_feedback(
    msg = "Download successful.",
    verbose = verbose
  )

  if(!base::isFALSE(list(...)[["update"]])){
    
    downloaded_object <- SPATA2::updateSpataObject(downloaded_object)
    
  }

  if(base::is.character(file)){

    downloaded_object <- SPATA2::setSpataDir(downloaded_object, dir = file)

    confuns::give_feedback(
      msg = glue::glue("Saving `SPATA2` object under '{directory_spata}'."),
      verbose = verbose
    )

    base::saveRDS(object = downloaded_object, file = directory_spata)

  }

  base::invisible(downloaded_object)

}


#' @title Download and save several SPATA2 objects
#'
#' @description Main function that downloads several `SPATA2` objects
#' at the same time and saves each as an .RDS file.
#'
#' @param sample_names Character vector. The sample names of the `SPATA2` objects
#' to be downloaded. Use \code{validSampleNames()} to obtain all valid input options.
#' @param files Character vector or NULL. Specifies the file names under which the
#' `SPATA2` objects are saved. If character, the input must be of the same length
#' as the input for argument \code{sample_names}. If NULL, the files are named
#' according to the sample name.
#' @inherit downloadSpataObject params 
#'
#' @return An invisible `TRUE`.
#'
#' @section Google Drive Warning:
#' `SPATA2` objects are stored in a Google Drive repository and downloaded via their weblink as 
#' stored in the \link[=sourceDataFrame]{source data.frame}. Often, `SPATA2` objects are too 
#' large for the automatic Google Drive virus scan. As a result, the weblink initially leads 
#' to a webpage that asks if you are okay with skipping this virus scan.
#' 
#' In cases where the Google Drive link leads to this warning page, the function
#' will automatically adjust the download link to bypass the warning and attempt
#' the download again. If the adjusted download still fails, an error message is 
#' displayed, prompting the user to manually download the file using the provided link.
#' 
#' Since the virus scan cannot be performed by Google Drive regardless of whether 
#' you download it from within R or manually, the function defaults to bypass this
#' warning automatically. If you prefer not to bypass the warning, you can set `adjust_link = FALSE`
#' In this case, the function will give a warning and ask you to download the object manually.
#' 
#' The downloaded objects do not contain viruses. The way they have been created,
#' uploaded as well as how the web links are added to the \link[=sourceDataFrame]{source data.frame} 
#' can be reconstructed with the *populate_<location>* scripts provided on 
#' the [SPATAData repository](https://github.com/theMILOlab/SPATAData) on github under */scripts/*.
#'
#' @export
#'
#' @examples
#'
#' # downloads three objects and
#' # saves them as "spata_objects/UKF275T.RDS", "spata_objects/UKF313t.RDS", ... etc.
#' 
#'   downloadSpataObjects(
#'     sample_names = c("UKF275T", "UKF313T", "UKF334T"),
#'     folder = "spata_objects" # the folder in which to save the files
#'    )
#'
downloadSpataObjects <- function(sample_names,
                                 files = NULL,
                                 folder = base::getwd(),
                                 overwrite = FALSE,
                                 adjust_link = TRUE,
                                 timeout = 600,
                                 verbose = TRUE,
                                 ...){

  require(SPATA2)
  
  in_shiny <- base::isTRUE(list(...)[["in_shiny"]])
  
  source_df <- list(...)[["source_df"]]

  if(base::is.null(source_df)){

    source_df <- sourceDataFrame()

  }

  update <- list(...)[["update"]]

  if(base::is.null(update)){

    update <- TRUE

  }

  sample_names <- base::unique(sample_names)

  confuns::check_one_of(
    input = sample_names,
    against = base::unique(source_df$sample_name)
  )
  
  if(!base::dir.exists(folder)){
    
    base::dir.create(path = folder, recursive = TRUE)
    
  }

  if(base::is.null(files)){

    files <-
      purrr::map_chr(
        .x = sample_names,
        .f = ~ stringr::str_c(folder, "/", .x, ".RDS")
      )

  } else {

    n_samples <- base::length(sample_names)
    n_files <- base::length(files)

    if(!base::identical(n_samples, n_files)){

      stop("Please provide as many filenames '{n_files}' as you provide sample names '{n_samples}'.")

    }

    files <- stringr::str_c(folder, files, sep = "/")

  }

  # discard files that already exist if overwrite
  existing_files <- purrr::keep(.x = files, .p = ~ base::file.exists(.x))

  if(base::length(existing_files) >= 1 && !base::isTRUE(overwrite)){

    ref <- confuns::scollapse(existing_files)

    ref2 <- confuns::adapt_reference(existing_files, sg = "exist", pl = "")

    stop(glue::glue("{ref} already {ref2}. Set overwrite to TRUE to continue."))

  }

  not_creatable <- purrr::discard(.x = files,.p = is_creatable)

  if(base::length(not_creatable) >= 1){

    ref1 <- confuns::adapt_reference(not_creatable, sg = "directory", pl = "directories")

    ref2 <- confuns::scollapse(not_creatable)

    stop(glue::glue("Invalid storage directories. Can not create {ref1} '{ref2}'"))

  }
  
  out <-
    purrr::map2(
      .x = sample_names,
      .y = files,
      .f = purrr::safely(
        .f = function(sample_name, file){

          object <- 
            downloadSpataObject(
              sample_name = sample_name, 
              adjust_link = adjust_link, 
              timeout = timeout
            )
          
          object <- 
            saveSpataObject(object = object, dir = file, verbose = verbose)

          return(TRUE)

        },
        otherwise = FALSE
      )
    ) %>%
    purrr::set_names(nm = sample_names)

  failed_downloads <- purrr::discard(.x = out, .p = ~ base::is.null(.x$error))

  for(i in base::seq_along(failed_downloads)){

    fail <- failed_downloads[[i]]

    sample <- base::names(fail)
    error <- fail$error

    msg <- glue::glue("Download of sample '{sample}' failed with error message: {error}")

    confuns::give_feedback(msg = msg, verbose = verbose, in.shiny = in_shiny)

  }

  successful_downloads <-
    purrr::keep(.x = out, .p = ~ base::is.null(.x$error)) %>%
    base::names() %>%
    confuns::scollapse()

  msg <- glue::glue("Successfully downloaded '{successful_downloads}'.", in.shiny = in_shiny)

  confuns::give_feedback(msg = msg, verbose = TRUE)

  base::invisible(TRUE)

}




#' @title Download raw data
#'
#' @description Currently deprecated. 
#' 
#' @export
#'
downloadRawData <- function(...){
  
  stop("This function is currently deprecated.")
  
}

download_raw_data <- function(sample_names,
                              files = NULL,
                              folder = base::getwd(),
                              overwrite = FALSE,
                              in_shiny = FALSE,
                              verbose = TRUE, 
                              ...){
  
  confuns::is_value(x = folder, mode = "character", skip.allow = TRUE, skip.val = NULL)
  
  source_df <- list(...)[["source_df"]]
  
  if(base::is.null(source_df)){
    
    source_df <- sourceDataFrame()
    
  }
  
  source_df <- dplyr::filter(source_df, shiny::isTruthy(link_raw))
  
  if(base::nrow(source_df) == 0){
    
    stop("No links to raw data found.")
    
  }
  
  confuns::check_one_of(
    input = sample_names,
    against = source_df$sample,
    fdb.opt = 2,
    ref.opt.2 = "samples for which raw data is available",
    in.shiny = in_shiny
  )
  
  if(base::is.character(files)){
    
    if(base::length(files) != base::length(sample_names)){
      
      stop("Number of filenames must be equal to the number of samples.")
      
    }
    
  } else if(!base::dir.exists(folder)){
    
    base::dir.create(folder, recursive = TRUE)
    
  }
  
  if(!base::is.character(files)){
    
    files <- stringr::str_c(folder, "/", sample_names, ".zip")
    
  }
  
  existing_files <- purrr::keep(.x = files, .p = ~ base::file.exists(.x))
  
  if(base::length(existing_files) >= 1 && !base::isTRUE(overwrite)){
    
    ref <- confuns::scollapse(existing_files)
    
    ref2 <- confuns::adapt_reference(existing_files, sg = "exists", pl = "exist")
    
    stop(glue::glue("{ref} already {ref2}. Set argument `overwrite` to TRUE to continue."))
    
  }
  
  out <-
    purrr::map2(
      .x = sample_names,
      .y = files,
      .f = purrr::safely(
        .f = function(sample, file){
          
          download_dir <-
            dplyr::filter(.data = source_df, sample == {{sample}}) %>%
            dplyr::pull(link_raw)
          
          confuns::give_feedback(
            msg = glue::glue("Downloading RAW data of sample '{sample}' from '{download_dir}' and saving under '{file}'."),
            verbose = verbose, 
            in.shiny = in_shiny
          )
          
          downloader::download(url = download_dir, dest = file, mode = "wb")
          
          confuns::give_feedback(
            msg = "Download successful.",
            verbose = TRUE
          )
          
          return(TRUE)
          
        },
        otherwise = FALSE
      )
    ) %>%
    purrr::set_names(sample_names)
  
  failed_downloads <- purrr::discard(.x = out, .p = ~ base::is.null(.x$error))
  
  for(i in base::seq_along(failed_downloads)){
    
    fail <- failed_downloads[i]
    
    sample <- base::names(fail)
    error <- fail[[1]]$error
    
    msg <- glue::glue("Download of sample '{sample}' failed with error message: {error}")
    
    confuns::give_feedback(msg = msg, verbose = verbose, in.shiny = in_shiny)
    
  }
  
  successful_downloads <-
    purrr::keep(.x = out, .p = ~ base::is.null(.x$error)) %>%
    base::names() %>%
    confuns::scollapse()
  
  msg <- glue::glue("Successfully downloaded '{successful_downloads}'.")
  
  confuns::give_feedback(msg = msg, verbose = verbose, in.shiny = in_shiny)
  
  base::invisible(TRUE)
  
}



