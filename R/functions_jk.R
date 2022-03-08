



# helper ------------------------------------------------------------------

is_creatable <- function(file){

  base::tryCatch({

    base::saveRDS(object = list(), file = file)

    base::file.remove(file)

    TRUE

  }, error = function(error){

    FALSE

  })

}


#' @title Valid spata object names
#'
#' @description Returns the sample names
#' of the spata objects that you can download
#' via \code{downloadSpataObject()} and \code{downloadSpataObject()}.
#'
#' @return Character vector.
#' @export
#'
validSampleNames <- function(){

  list.data()$Sample %>%
    base::unique()

}



#' @title Set citation info
#'
#' @description Sets information about how to cite this
#' spata object.
#'
#' @inherit argument_dummy params
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

#' @title Obtain citation info
#'
#' @description If you have used a downloadable spata object
#' please use this function to obtain the proper
#' form of citation to give credits to the researchers who
#' made this data available
#'
#' @inherit argument_dummy params
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

      dplyr::filter(list.data(), Sample == {{sample}}) %>%
        dplyr::pull(Citation) %>%
        base::unique()

    }
  ) %>%
    purrr::set_names(nm = sample_names)

}




#' @title Download spata objects
#'
#' @description Downloads spata objects from the internet and saves them
#' on your device. See details for more information about the differences
#' between \code{downloadSpataObject()} and \code{downloadSpataObjects()}.
#'
#' @param sample_name Character value. The name of the sample you want to
#' download. Use \code{validSampleNames()} to obtain all valid input options.
#' @param overwrite Logical. Must be set to TRUE if file directories
#' under which downloaded files are about to be saved already exist.
#' @param folder Character value. If character, specifies the output
#' folder in which the spata object is saved. Defaults to the working directory.
#' @param file The filename of the spata object. Must end with \emph{'.RDS'}. By
#' default the file is named like the sample name. Set to NULL if you don't
#' want the object to be saved.
#'
#' @inherit argument_dummy params
#'
#' @details The downloaded spata object is immediately saved after the download before
#' it is returned by the function.
#'
#' @return The downloaded spata object.
#' @export
#'
#' @examples
#'
#' # only download
#' downloadSpataObject(sample_name = "275_T")
#'
#' # download AND assign
#' object <- downloadSpataObject(sample_name = "275_T")
#'
#'
downloadSpataObject <- function(sample_name,
                                overwrite = FALSE,
                                folder = base::getwd(),
                                file = stringr::str_c(sample_name, ".RDS"),
                                verbose = TRUE,
                                ...){

  confuns::are_values(c("folder", "file"), mode = "character", skip.allow = TRUE, skip.val = NULL)

  confuns::is_value(x = overwrite, mode = "logical")

  data_df <- list(...)[["data_df"]]

  if(base::is.null(data_df)){

    data_df <- list.data()

  }

  confuns::check_one_of(
    input = sample_name,
    against = base::unique(data_df$Sample)
  )

  download_dir <-
    dplyr::filter(data_df, Sample == {{sample_name}}) %>%
    dplyr::filter(type == "SPATA") %>%
    dplyr::pull(link)

  if(base::is.character(file)){

    if(!stringr::str_detect(file, pattern = ".RDS$|.rds$")){

      stop("Input for argument `file` must end with either '.RDS' or '.rds'")

    }

    directory_spata <- stringr::str_c(folder, "/", file)

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
    msg = glue::glue("Downloading spata object '{sample_name}' from '{download_dir}'."),
    verbose = verbose
    )

  downloaded_object <-
    base::url(download_dir) %>%
    base::readRDS()

  confuns::give_feedback(
    msg = "Download successfull.",
    verbose = verbose
  )

  downloaded_object <- updateSpataObject(downloaded_object)

  citation <-
    dplyr::filter(data_df, Sample == {{sample_name}}) %>%
    dplyr::pull(Citation) %>%
    base::unique()

  downloaded_object <- setCitation(downloaded_object, citation = citation)

  if(base::is.character(file)){

    downloaded_object <-
      adjustDirectoryInstructions(
        object = downloaded_object,
        to = "spata_object",
        directory_new = directory_spata
      )

    confuns::give_feedback(
      msg = glue::glue("Saving spata object under '{directory_spata}'."),
      verbose = verbose
    )

    base::saveRDS(object = downloaded_object, file = directory_spata)

  }

  base::invisible(downloaded_object)

}


#' @title Download several spata objects
#'
#' @description Convenience function that downloads several spata objects
#' at the same time.
#'
#' @param sample_names Character vector. The sample names of the spata objects
#' to be downloaded. Use \code{validSampleNames()} to obtain all valid input options.
#' @param files Character vector or NULL. Specifies the file names under which the
#' spata objects are saved. If character, the input must be of the same length
#' as the input for argument \code{sample_names}. If NULL, the files are named
#' according to the sample name.
#' @inherit downloadSpataObject params
#'
#' @return An invisible TRUE. This function only saves spata objects. In contrast
#' to \code{downloadSpataObject()}
#'
#' @export
#'
#' @examples
#'
#' # downloads three spata objects and stores them as "spata_data/275_T.RDS", "spata_data/313_T.RDS" etc.
#' downloadSpataObjects(sample_names = ("275_T", "313_T", "334_T"), folder = "spata_data")
#'
downloadSpataObjects <- function(sample_names,
                                 files = NULL,
                                 folder = base::getwd(),
                                 overwrite = FALSE,
                                 verbose = TRUE,
                                 ...){

  confuns::is_value(x = folder, mode = "character", skip.allow = TRUE, skip.val = NULL)

  data_df <- list(...)[["data_df"]]

  if(base::is.null(data_df)){

    data_df <- list.data()

  }

  update <- list(...)[["update"]]

  if(base::is.null(update)){

    update <- TRUE

  }

  sample_names <- base::unique(sample_names)

  confuns::check_one_of(
    input = sample_names,
    against = base::unique(data_df$Sample)
  )

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
        .f = function(sample, file){

          download_dir <-
            dplyr::filter(data_df, Sample == {{sample}} & type == "SPATA") %>%
            dplyr::pull(link)

          citation <-
            dplyr::filter(data_df, Sample == {{sample}} & type == "SPATA") %>%
            dplyr::pull(Citation) %>%
            base::unique()

          confuns::give_feedback(
            msg = glue::glue("Downloading sample {sample} from '{download_dir}'."),
            verbose = verbose
            )

          # download and save immediately
          base::url(download_dir) %>%
            base::readRDS() %>%
            {if(base::isTRUE(update)){

              updateSpataObject(., verbose = verbose)

            } else {

              .

            }} %>%
            setCitation(object = ., citation = citation) %>%
            saveSpataObject(directory_spata = file, verbose = verbose)

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

    give_feedback(msg = msg, verbose = verbose)

  }

  successful_downloads <-
    purrr::keep(.x = out, .p = ~ base::is.null(.x$error)) %>%
    base::names() %>%
    confuns::scollapse()

  msg <- glue::glue("Successfully downloaded '{successful_downloads}'.")

  confuns::give_feedback(msg = msg, verbose = TRUE)

  base::invisible(TRUE)

}




#' @title Download Raw data
#'
#' @description Downloads raw Visium10X output of samples as a .zip folder.
#'
#' @param sample_names Character vector. The sample names of the data
#' to be downloaded. Use \code{validSampleNames()} to obtain all valid input options.
#' @inherit downloadSpataObjects params
#'
#' @details The directory of the .zip folder is eventually created
#' with \code{stringr::str_c(folder, "/", sample, ".zip")}. To access
#' the data you have to unzip the folder manually.
#'
#' @return An invisible TRUE.
#' @export
#'
downloadRawData <- function(sample_names,
                            folder = base::getwd(),
                            overwrite = FALSE,
                            verbose = TRUE){

  confuns::is_value(x = folder, mode = "character", skip.allow = TRUE, skip.val = NULL)

  confuns::check_one_of(
    input = sample_names,
    against = dplyr::filter(list.data(), type == "RAW") %>% dplyr::pull(Sample),
    fdb.opt = 2,
    ref.opt.2 = "samples for which raw data is available"
  )

  if(!base::dir.exists(folder)){

    base::dir.create(folder, recursive = TRUE)

  }

  files <- stringr::str_c(folder, "/", sample_names, ".zip")

  existing_files <- purrr::keep(.x = files, .p = ~ base::file.exists(.x))

  if(base::length(existing_files) >= 1 && !base::isTRUE(overwrite)){

    ref <- confuns::scollapse(existing_files)

    ref2 <- confuns::adapt_reference(existing_files, sg = "exists", pl = "exist")

    stop(glue::glue("{ref} already {ref2}. Set argument `overwrite` to TRUE to continue."))

  }

  out <-
    purrr::map(
      .x = sample_names,
      .f = purrr::safely(
        .f = function(sample){

          download_dir <-
            dplyr::filter(.data = list.data(), Sample == {{sample}} & type == "RAW") %>%
            dplyr::pull(link)

          confuns::give_feedback(
            msg = glue::glue("Downloading RAW data of sample '{sample}' from '{download_dir}' and storing in '{folder}'."),
            verbose = verbose
          )

          file <- stringr::str_c(folder, "/", sample, ".zip")

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

    give_feedback(msg = msg, verbose = verbose)

  }

  successful_downloads <-
    purrr::keep(.x = out, .p = ~ base::is.null(.x$error)) %>%
    base::names() %>%
    confuns::scollapse()

  msg <- glue::glue("Successfully downloaded '{successful_downloads}'.")

  base::invisible(TRUE)

}







