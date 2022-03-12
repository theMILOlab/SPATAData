

htmlBreak <- function(n){
  
  shiny::HTML(stringr::str_c(base::rep("<br>", n), collapse = ""))
  
}

htmlCol <- function(width, ...,  align = "center", offset = 0){
  
  shiny::column(width = width, ..., align = align, offset = offset)
  
}


htmlHelper <- function(shiny_tag, content, title = "What do I have to do here?", type = "inline", size = "s", ...){
  
  shinyhelper::helper(
    shiny_tag = shiny_tag,
    content = content,
    title = title,
    size = size,
    type = type,
    ...)
  
}



htmlOrganizeInColumns <- function(tag_list, ncol = 3){
  
  n_inputs <- length(tag_list)
  
  seq_inputs <- base::seq(1, n_inputs, ncol)
  
  purrr::map(
    .x = seq_inputs, 
    .f = function(nth){
      
      # 1:4, 5:8, etc.
      selected_inputs <-
        purrr::discard(tag_list[nth:(nth+ncol-1)], .p = base::is.null) %>% 
        shiny::tagList()
      
      # return tag list of up to four pickers in row
      shiny::fluidRow(selected_inputs)
      
    }
  ) %>% 
    shiny::tagList()
  
}


htmlSampleId <- function(sample_name, mode, pref = NULL, suff = NULL){
  
  df <- 
    sourceDataFrame(mode = mode, distinct = TRUE) %>% 
    dplyr::filter(Sample == {{sample_name}})
  
  mode <- htmlTissueMode(mode = mode)
  
  stringr::str_c(pref, df$Organ, mode, "tissue", sample_name, suff, sep = "_")
  
}

htmlSelectizeInput <- function(source_df, variable, ncol){
  
  choices <- c("", base::unique(source_df[[variable]]))
  
  create <- TRUE
  
  if(variable == "organ"){
    
    choices <- c("", all_organs)
    create <- FALSE
    
  } 
  
  shiny::column(
    width = 12/ncol, 
    shiny::tags$h5(shiny::strong(variable)) %>% htmlHelper(content = variable_info[[variable]]),
    shiny::selectizeInput(
      inputId = variable,
      label = NULL,
      choices = choices,
      multiple = FALSE,
      selected = "",
      options = list(create = create)
    )
  )
  
}


htmlTextInput <- function(variable, ncol){
  
  shiny::column(
    width = 12/ncol, 
    shiny::tags$h5(shiny::strong(variable)) %>% htmlHelper(content = variable_info[[variable]]), 
    shiny::textInput(
      inputId = variable,
      label = NULL, 
      width = "100%"
    )
  )
  
}


htmlTissueMenuItem <- function(organ){
  
  organ <- confuns::make_capital_letters(organ)
  
  shinydashboard::menuItem(
    text = organ, 
    tabName = stringr::str_c("tissue", organ, sep = "_"), 
    shinydashboard::menuSubItem(
      text = "Healthy", 
      tabName = htmlTissueTabName(organ = organ, mode = "h"),
    ), 
    shinydashboard::menuSubItem(
      text = "Pathological", 
      tabName = htmlTissueTabName(organ = organ, mode = "p")
    )
  )
  
}


htmlTissueBox <- function(sample_name, organ, mode, width = 4){
  
  id_download_spata <- htmlSampleId(sample_name, mode = mode, pref = "download_spata")
  
  id_download_raw <- htmlSampleId(sample_name, mode = mode, pref = "download_raw")
  
  id_info <- htmlSampleId(sample_name, mode = mode, pref = "info")
  
  id_image <- htmlSampleId(sample_name, mode = mode, pref = "image")
  
  id_plot <- htmlSampleId(sample_name, mode = mode, pref = "plot")
  
  id_zoom <- htmlSampleId(sample_name, mode = mode, pref = "zoom")

  shinydashboard::box(
    title = NULL, 
    width = width, 
    shiny::column(
      width = 12, 
      align = "center", 
      shiny::tags$h4(shiny::strong(confuns::make_pretty_name(sample_name))),
      htmlTissueBoxSubtitle(sample_name = sample_name),
      shiny::plotOutput(outputId = id_image, height = "500px"), 
      shiny::fluidRow(
        htmlCol(5, 
                shiny::splitLayout(
                  shiny::actionButton(inputId = id_plot, label = "Plot", width = "100%"), 
                  shiny::actionButton(inputId = id_zoom, label = "Zoom", width = "100%"), 
                  cellWidths = c("50%")
                )
        ),
        htmlCol(5,
                shiny::splitLayout(
                  shiny::downloadButton(outputId = id_download_spata, label = "SPATA", width = "100%"), 
                  shiny::downloadButton(outputId = id_download_raw, label = "RAW", width = "100%"),
                  cellWidths = c("50%")
                )
        ),
        htmlCol(2, shiny::actionButton(inputId = id_info, label = "", icon = shiny::icon("question-circle"), width = "100%"))
      ), 
      htmlBreak(1)
    )
  )
  
}

htmlTissueBoxSubtitle <- function(sample_name){
  
  df <- dplyr::filter(sourceDataFrame(), Sample == {{sample_name}} & Data_Type == "SPATA")
  
  det <- df$Histology_Detailed
  
  short <- df$Histology_Short
  
  if(base::identical(det, short)){
    
    subtitle <- det
    
  } else {
    
    subtitle <- stringr::str_c(det, " (", short, ")")
    
  }
  
  shiny::tags$h4(subtitle)
  
}


htmlTissueTabItem <- function(organ = NULL, mode = NULL, ncol = 3){
  
  df <- sourceDataFrame(organ = organ, mode = mode, distinct = TRUE)
  
  samples <- base::unique(df$Sample)
  
  if(base::length(samples) >= 1){
    
    df <- dplyr::arrange(df, Sample, Histology_Short, Histology_Detailed)
    
    purrr::map(
      .x = samples, 
      .f = function(sample_name){
        
          htmlTissueBox(
            sample_name = sample_name, 
            organ = organ, 
            mode = mode, 
            width = 12/ncol
          )
          
      }
    ) %>% 
      htmlOrganizeInColumns(ncol = ncol) %>% 
      shiny::tagList() %>% 
      shinydashboard::tabItem(tabName = htmlTissueTabName(organ = organ, mode = mode))
    
  } else {
    
    shinydashboard::tabItem(tabName = htmlTissueTabName(organ = organ, mode = mode))
    
  }
  
 
  
}






htmlTissueMode <- function(mode){
  
  if(mode == "p"){
    
    mode <- "pathological"
    
  } else if(mode == "h"){
    
    mode <- "healthy"
    
  }
  
  return(mode)
  
}

htmlTissueTabName <- function(organ, mode){
  
  organ <- confuns::make_capital_letters(organ)
  
  mode <- htmlTissueMode(mode)
  
  stringr::str_c("tissue", organ, mode, sep = "_")
  
}



