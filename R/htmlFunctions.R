

htmlBreak <- function(n){
  
  shiny::HTML(stringr::str_c(base::rep("<br>", n), collapse = ""))
  
}

htmlCol <- function(width, ...,  align = "center", offset = 0){
  
  shiny::column(width = width, ..., align = align, offset = offset)
  
}


htmlEmptyTissueBox <- function(width = 4, ...){
  
  if(base::exists(x = "box_type", where = .GlobalEnv) &&
     base::get(x = "box_type", envir = .GlobalEnv) == "CSS"){
    
    shiny::tagList(
      shiny::column(
        width = width,
        shiny::div(
          class = "tissue-box", 
          ...
        )
      )
    )
    
  } else {
    
    shinydashboard::box(width = width, ...)
    
  }
  
}

htmlFilterSamplesActionButtons <- function(prel_id){
  
  shiny::fluidRow(
    shiny::column(
      width = 6,
      align = "right",
      shinyWidgets::actionBttn(
        inputId = stringr::str_c(prel_id, "apply_filter", sep = "_"), 
        label = "Apply Filter", 
        style = "material-flat", 
        color = "primary", 
        size = "sm"
      )
    ),
    shiny::column(
      width = 6,
      align = "left",
      shinyWidgets::actionBttn(
        inputId = stringr::str_c(prel_id, "reset_filter", sep = "_"), 
        label = "Reset Filter", 
        style = "material-flat", 
        color = "primary", 
        size = "sm"
      )
    )
  )
  
}


htmlFilterSamplesBox <- function(...){
  
  shinydashboard::box(
    title = "Filter Samples", 
    width = 12, 
    collapsed = FALSE, 
    collapsible = FALSE, 
    ...
  )
  
}

htmlFilterSamplesGroupCheckbox <- function(source_df = sourceDataFrame(), variable, prel_id){
  
  if(variable == "tags"){
    
    choices <- sourceDataFrameTags(source_df = source_df)
    
    inline <- TRUE
    
  } else {
    
    choice_values <- 
      base::unique(source_df[[variable]]) %>% 
      purrr::keep(.p = ~ shiny::isTruthy(.x)) %>% 
      base::sort()
    
    choices <- 
      purrr::set_names(
        x = choice_values, 
        nm = confuns::make_pretty_names(choice_values)
      )
    
    inline <- FALSE
    
  }
  
  shiny::checkboxGroupInput(
    inputId = stringr::str_c(prel_id, variable, sep = "_"), 
    label = confuns::make_pretty_name(variable), 
    choices = choices, 
    selected = NULL, 
    inline = inline
  )
  
}

htmlFilterSamplesGroupCheckboxes <- function(source_df = sourceDataFrame(), organ, status, ncol = 4){
  
  if(status == "h"){
    
    variables <- confuns::vselect(filter_sample_variables, -pathology, -who_grade)
    
  } else if(status == "p") {
    
    variables <- filter_sample_variables
    
  }
  
  source_df <- dplyr::filter(source_df, status == {{status}})
  
  prel_id <- stringr::str_c("filter_sample", organ, status, sep = "_")
  
  purrr::map(
    .x = variables, 
    .f = function(variable){
      
      shiny::column(
        width = 12/ncol,
        shiny::verticalLayout(
          htmlFilterSamplesGroupCheckbox(
            source_df = source_df,
            variable = variable,
            prel_id = prel_id
          )
        )
      )
      
    }
  ) %>% 
    htmlOrganizeInColumns(ncol = ncol)
  
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

htmlOrganizeInColumns <- function(tag_list, ncol = 3, breaks = 1){
  
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
      
      shiny::tagList(
        shiny::fluidRow(htmlBreak(n = breaks)),
        shiny::fluidRow(selected_inputs),
        shiny::fluidRow(htmlBreak(n = breaks))
      )
      
      
    }
  ) %>% 
    shiny::tagList()
  
}

htmlSampleId <- function(sample_name, pref = NULL, suff = NULL, source_df = sourceDataFrame()){
  
  df <- dplyr::filter(source_df, sample == {{sample_name}}) 
  
  stringr::str_c(pref, df$organ, df$status, "tissue", sample_name, suff, sep = "_")
  
}

htmlSelectizeInput <- function(source_df = sourceDataFrame(), variable, ncol){
  
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



htmlTissueBox <- function(sample_name, organ, status, width = 4, source_df = sourceDataFrame()){
  
  id_download_spata <- htmlSampleId(sample_name, pref = "download_spata", source_df = source_df)
  
  id_download_raw <- htmlSampleId(sample_name, pref = "download_raw", source_df = source_df)
  
  id_info <- htmlSampleId(sample_name, pref = "info", source_df = source_df)
  
  id_image <- htmlSampleId(sample_name, pref = "image", source_df = source_df)
  
  id_plot <- htmlSampleId(sample_name, pref = "plot", source_df = source_df)
  
  id_zoom <- htmlSampleId(sample_name, pref = "zoom", source_df = source_df)
  
  df <- dplyr::filter(source_df, sample == {{sample_name}})
  
  if(base::is.na(df$anatomical_region)){
    
    region <- NULL
    
  } else {
    
    region <- stringr::str_c("(", df$anatomical_region, ")")
    
  }
  
  title <- stringr::str_c(df$hist_classification, region, sep = " ")
  
  
  htmlEmptyTissueBox(
    width = width, 
    shiny::column(
      width = 12, 
      align = "center", 
      shiny::tags$h4(shiny::strong(confuns::make_pretty_name(title))),
      shiny::tags$h4(stringr::str_c("Sample:", confuns::make_pretty_name(sample_name), sep = " ")),
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

htmlTissueBoxSubtitle <- function(source_df = sourceDataFrame(), sample_name){
  
  df <- dplyr::filter(source_df, sample == {{sample_name}})
  
  det <- df$hist_classification
  
  short <- df$hist_abbreviation
  
  if(base::identical(det, short)){
    
    subtitle <- det
    
  } else {
    
    subtitle <- stringr::str_c(det, " (", short, ")")
    
  }
  
  shiny::tags$h4(subtitle)
  
}


htmlTissueMenuItem <- function(organ){
  
  organ <- confuns::make_capital_letters(organ)
  
  shinydashboard::menuItem(
    text = organ, 
    tabName = stringr::str_c("tissue", organ, sep = "_"), 
    shinydashboard::menuSubItem(
      text = "Healthy", 
      tabName = htmlTissueTabName(organ = organ, status = "h"),
    ), 
    shinydashboard::menuSubItem(
      text = "Pathological", 
      tabName = htmlTissueTabName(organ = organ, status = "p")
    )
  )
  
}


htmlTabItemVisualize <- function(){
  
  shinydashboard::tabItem(
    tabName = "visualize",
    shiny::fluidRow(
      
      shiny::column(
        width = 8,
        shinydashboard::box(
          width = 12,
          shiny::fluidRow(
            shiny::column(
              width = 12,
              align = "center",
              shiny::plotOutput(outputId = "surface_plot", height = "800px")
            )
          )
        )
      ), 
      
      shiny::column(
        width = 4, 
        shinydashboard::box(
          width = 12,
          shiny::fluidRow(
            shiny::column(
              width = 10, 
              align = "center",
              offset = 1,
              shiny::fluidRow(
                htmlCol(6,
                        shinyWidgets::pickerInput(
                          inputId = "color_by_opt",
                          label = "Color by:",
                          choices = c("Features" = "features", "Genes" = "genes", "Gene-sets (Mean)" = "gene_sets"),
                          selected = "features",
                          multiple = FALSE
                        ), 
                        shinyWidgets::pickerInput(
                          inputId = "pt_clrsp",
                          label = "Colorspectrum",
                          choices = SPATA2::validColorSpectra(),
                          selected = "inferno",
                          multiple = FALSE
                        )
                ), 
                htmlCol(6,
                        shiny::uiOutput(outputId = "color_by_var")
                )
              ), 
              sliderInput(
                inputId = "pt_smooth",
                label = "Point-Smoothing:",
                min = 0, max = 0.5, value = 0, step = 0.01
              ),
              shiny::sliderInput(
                inputId = "pt_transp",
                label = "Point-Transparency:",
                min = 0,
                max = 1,
                value = 0.25,
                step = 0.01
              ),
              shiny::sliderInput(
                inputId = "pt_size",
                label = "Point-Size:",
                min = 0.25, max = 5, value = 1.6, step = 0.01
              )
            )
          )
          
        )
      )
    )
  )
  
}



htmlTissueTabItem <- function(organ = NULL, status = NULL, ncol = 3){
  
  df <- sourceDataFrame(organ = organ, status = status)
  
  samples <- base::unique(df$sample)
  
  if(base::length(samples) >= 1){
    
    df <- dplyr::arrange(df, sample, hist_abbreviation, hist_classification)
    
    tissue_boxes <- 
      purrr::map(
        .x = samples, 
        .f = function(sample_name){
          
            htmlTissueBox(
              sample_name = sample_name, 
              organ = organ, 
              status = status, 
              width = 12/ncol
            )
            
        }
      ) %>% 
        htmlOrganizeInColumns(ncol = ncol) %>% 
        shiny::tagList()
    
    out <- 
      shinydashboard::tabItem(
        tabName = htmlTissueTabName(organ = organ, status = status), 
        shiny::fluidRow(box(title = "Filter Samples", collapsible = TRUE, collapsed = TRUE, width = 12, tags$h3("Some text"))),
        shiny::fluidRow(shiny::column(width = 12, tissue_boxes))
        )
    
  } else {
    
    out <- shinydashboard::tabItem(tabName = htmlTissueTabName(organ = organ, status = status))
    
  }
  
  return(out)
  
}

htmlTissueBoxes <- function(source_df = sourceDataFrame(), sample_names = NULL, ncol = 3){
  
  if(base::is.character(sample_names)){
    
    source_df <- dplyr::filter(source_df, sample %in% {{sample_names}})
    
  } 
  
  purrr::map(
    .x = source_df$sample, 
    .f = function(sample_name){
      
      htmlTissueBox(
        sample_name = sample_name, 
        organ = organ, 
        status = status, 
        width = 12/ncol, 
        source_df = source_df
        
      )
      
    }
  ) %>% 
    purrr::set_names(nm = source_df$sample)
  
}


htmlTissueTabItems <- function(source_df = sourceDataFrame()){
  
  shiny::tagList(
    purrr::map(
      .x = base::unique(source_df$organ), 
      .f = function(organ){
        
        purrr::map(
          .x = c("h", "p"), 
          .f = function(status){
            
            tab_name <- htmlTissueTabName(organ = organ, status = status)
            
            id_select_box <- stringr::str_c(tab_name, "tab_ui_output_filter_box", sep = "_")
            id_tissue_boxes <- stringr::str_c(tab_name, "tab_ui_output_tissue_boxes", sep = "_")
            
            shinydashboard::tabItem(
              tabName = tab_name, 
              shiny::uiOutput(outputId = id_select_box),
              shiny::uiOutput(outputId = id_tissue_boxes)
            )
            
          }
        )
        
      }
    )
  )
  
  
}




htmlTissueTabName <- function(organ, status){
  
  organ <- confuns::make_capital_letters(organ)
  
  stringr::str_c("tissue", organ, status, sep = "_")
  
}



