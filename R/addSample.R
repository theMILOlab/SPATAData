#' @title Add sample interactively
#' 
#' @description Adds a new sample to the source data.frame. 
#'
#' @inherit addSampleManually params return
#' 
#' @keywords internal
#'
addSample <- function(source_df = sourceDataFrame(), update){
  
  confuns::is_value(x = update, mode = "logical")
  
  # start app
  out_df <- 
    shiny::runApp(
      shiny::shinyApp(
        ui = addSampleUI(),
        server = function(input, output, session){
          
          #html add ons
          shinyhelper::observe_helpers()
          
          updated_source_df <- shiny::reactiveVal(value = source_df)  
          
          # render uis 
          output$provide_information <- shiny::renderUI({
            
            # one value inputs 
            selectize_inputs <- 
              purrr::map(
                .x = selectize_variables, 
                .f = ~ htmlSelectizeInput(source_df = updated_source_df(), variable = .x, ncol = 4)
              ) %>% 
              htmlOrganizeInColumns(ncol = 4)
            
            stage_input <- 
              shiny::fluidRow(
                shiny::column(
                  width = 3, 
                  align = "center",
                  shiny::tags$h5(shiny::strong("stage")) %>% htmlHelper(content = variable_info[["stage"]]),
                  shiny::numericInput(
                    inputId = "stage",
                    label = NULL,
                    value = 0, 
                    min = 0, 
                    max = Inf,
                    step = 1)
                )
              )

            
            # tag input
            choices <- sourceDataFrameTags(source_df = updated_source_df())
            
            tag_input <- 
              shiny::fluidRow(
                shiny::column(
                  width = 12, 
                  shiny::tags$h5(shiny::strong("tags")) %>% htmlHelper(content = variable_info[["tags"]]), 
                  shiny::selectizeInput(
                    inputId = "tags", 
                    choices = choices, 
                    label = NULL, 
                    multiple = TRUE, 
                    options = list(create = TRUE)
                  )
                )
              )
            
            # text inputs
            text_inputs <- 
              purrr::map(
                .x = text_variables, 
                .f = ~ htmlTextInput(variable = .x, ncol = 1)
              ) %>% 
              htmlOrganizeInColumns(ncol = 1)
            
            # citation 
            citation <-
              htmlSelectizeInput(source_df = updated_source_df(), variable = "citation", ncol = 1)
            
            link_pub <- 
              htmlSelectizeInput(source_df = updated_source_df(), variable = "link_pub", ncol = 1)
            
            # finish 
            add_sample <- 
              shiny::fluidRow(
                shiny::column(
                  width = 12, 
                  shinyWidgets::actionBttn(
                    inputId = "add_new_sample",
                    label = "Add New Sample",
                    color = "success",
                    style = "gradient"
                  )
                )
              )
            
            # output
            shiny::tagList(
              selectize_inputs,
              stage_input,
              tag_input,
              text_inputs, 
              citation, 
              link_pub,
              add_sample
            )
            
          })
          
          # observe events
          
          oe <- shiny::observeEvent(input$add_new_sample, {
            
            print(update)
            
            sdf <- updated_source_df()
            
            input_list <- shiny::reactiveValuesToList(input)
            
            sdf_new <- 
              addSampleManually(
                source_df = sdf, 
                new_sample_input = confuns::lselect(input_list, dplyr::any_of(all_variables)), 
                update = update, 
                in_shiny = TRUE
              )
            
            updated_source_df(sdf_new)
            
          }, ignoreInit = TRUE)
          
          
          oe <- shiny::observeEvent(input$close_app, {
            
            shiny::stopApp(returnValue = updated_source_df())
            
          })
          
          
          # table output
          output$all_samples <- shiny::renderDataTable({
            
            updated_source_df()
            
          },options = list(scrollX = TRUE))
          
        }
      )
    )

  
  if(base::isFALSE(update)){
    
    return(out_df)
    
  } else {
    
    source_df <- out_df
    
    usethis::use_data(source_df, overwrite = TRUE)
    
    return(source_df)
    
  }
  
}