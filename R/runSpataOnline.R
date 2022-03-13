





#' @title SPATAs online presence
#' @export
#'
runSpataOnline <- function(object){
  
  shiny::runApp(
    shiny::shinyApp(
      ui = runSpataOnlineUI(), 
      server = function(input, output, session){
        
        # html add ons 
        shinyhelper::observe_helpers()
        
        # app start
        sdf <- sourceDataFrame()
        
        spata_object <- shiny::reactiveVal(val = NULL)
        

        # Tab: Menu items ---------------------------------------------------------
        
        output$tissue_organs_menu_items <- shiny::renderUI({
          
          if(FALSE){
            
            shinydashboard::menuItem(text = "Tissue - Organs", tabName = "tissue_organs",
                                     htmlTissueMenuItem(organ = "Brain"),
                                     htmlTissueMenuItem(organ = "Heart"),
                                     htmlTissueMenuItem(organ = "Liver")
            )
            
          }
          
          map(.x = c("Liver", "Brain", "Heart"), .f = ~ htmlTissueMenuItem(organ = .x)) %>% tagList()
          
          
          
          
        })
        
        
        output$tot <- shiny::renderUI({
          
          # TabItem - Tissue - Organ 
          shiny::tagList(
            purrr::map(
              .x = base::unique(source_df$organ),
              .f = function(organ){ 
                
                purrr::map(
                  .x = c("h", "p"), 
                  .f = ~ htmlTissueTabItem(organ = organ, status = .x, ncol = 3)
                )
                
              }) %>% 
              purrr::flatten()
          )
          
        })

        
        
        
        
        # Tab: Tissue-Organs ------------------------------------------------------
        
        sample_names <- base::unique(sdf$sample)
        
        for(i in base::seq_along(sample_names)){
          
          base::local({
            
            sample_name <- sample_names[i]
            
            sample_df <- dplyr::filter(sdf, sample == {{sample_name}})
            
            status <- sample_df$status
            
            # create zoom button 
            shiny::observeEvent(input[[htmlSampleId(sample_name, pref = "zoom")]], {
              
              shiny::showModal(
                ui = shiny::modalDialog(
                  shiny::plotOutput(outputId = htmlSampleId(sample_name, pref = "image_zoomed"), height = "600px"), 
                  footer = shiny::tagList(
                    shiny::fluidRow(
                      htmlCol(
                        width = 12, offset = 2,
                        shiny::actionButton(
                          inputId = htmlSampleId(sample_name, pref = "close", suff = "image_zoomed"), 
                          label = "Close"
                        )
                      )
                    )
                  ), 
                  size = "l"
                )
              )
              
            })
            
            # create close modal button
            shiny::observeEvent(input[[htmlSampleId(sample_name, pref = "close", suff = "image_zoomed")]], {
              
              shiny::removeModal()
              
            })
            
            # create download raw handler
            download_raw_id <- htmlSampleId(sample_name, pref = "download_raw")
            
            output[[download_raw_id]] <- shiny::downloadHandler(
              
              filename = function() { stringr::str_c("visium_raw", sample_name, ".zip") }, 
              
              content = function(file){
                
                confuns::give_feedback(
                  msg = glue::glue("Downloading Raw Visium Data '{sample_name}'."),
                  in.shiny = TRUE,
                  duration = 120
                )
                
                SPATAData::downloadRawData(
                  sample_names = sample_name, 
                  files = file, 
                  overwrite = TRUE,
                  verbose = FALSE,
                  source_df = sourceDataFrame()
                )
                
                confuns::give_feedback(msg = "Done.", in.shiny = TRUE, duration = 10)
                
              }
            )
            
            # create download spata handler
            download_spata_id <- htmlSampleId(sample_name, pref = "download_spata")
            
            output[[download_spata_id]] <- shiny::downloadHandler(
              
              filename = function() { stringr::str_c("spata_object_", sample_name, ".RDS") }, 
              
              content = function(file){
                
                confuns::give_feedback(
                  msg = glue::glue("Downloading SPATA object '{sample_name}'."),
                  in.shiny = TRUE,
                  duration = 120
                )
                
                object <- 
                  SPATAData::downloadSpataObject(
                    sample_name = sample_name,
                    file = NULL, 
                    verbose = FALSE, 
                    source_df = sourceDataFrame()
                  )
                
                saveRDS(object = object, file = file)
                
                confuns::give_feedback(msg = "Done.", in.shiny = TRUE, duration = 10)
                
              }
            )
            
            # create image output
            image_id <- htmlSampleId(sample_name, pref = "image")
            
            output[[image_id]] <- shiny::renderPlot({
              
              image_dir <- stringr::str_c("images/", sample_name, "/tissue_lowres_image.png")
              
              if(base::file.exists(image_dir)){
                
                img <- 
                  magick::image_read(path = image_dir) %>% 
                  grDevices::as.raster()
                
                plot(img)
                
              }
              
            })
            
            # create image zoom output 
            image_id <- htmlSampleId(sample_name, pref = "image_zoomed")
            
            output[[image_id]] <- shiny::renderPlot({
              
              image_dir <- stringr::str_c("images/", sample_name, "/tissue_lowres_image.png")
              
              if(base::file.exists(image_dir)){
                
                img <- 
                  magick::image_read(path = image_dir) %>% 
                  grDevices::as.raster()
                
                plot(img)
                
              }
              
            })
            
            
            # create info button 
            shiny::observeEvent(input[[htmlSampleId(sample_name, pref = "info")]], {
              
              shiny::updateTabsetPanel(session = session, inputId = "visualize", selected = "visualize")
              
            })
            
            # create plot button
            shiny::observeEvent(input[[htmlSampleId(sample_name, pref = "plot")]], {
              
              shinydashboard::updateTabItems(session = session, inputId = "sidebar", selected = "visualize")
              
              if(base::exists("object", envir = .GlobalEnv)){
                
                object <- get(x = "object", envir = .GlobalEnv)
                
              } else {
                
                object <- SPATA2::loadSpataObject(directory_spata = 'C:/Informatics/R-Folder/Packages/SPATAOnline/334_T.RDS')
                
              }
              
              spata_object(object)
              
            })
            
          })
          
        }
        
        
        # Tab: Visualization ------------------------------------------------------
        
        active_spata_object <- shiny::reactive({
          
          shiny::validate(
            shiny::need(
              expr = shiny::isTruthy(spata_object()), 
              message = "No SPATA objects chosen."
            )
          )
          
          spata_object()
          
        })
        
        
        # render uis
        
        output$color_by_var <- shiny::renderUI({
          
          if(input$color_by_opt == "genes"){
            
            choices <- SPATA2::getGenes(active_spata_object())
            
            selected <- NULL
            
          } else if(input$color_by_opt == "gene_sets"){
            
            choices <- SPATA2::getGeneSets(active_spata_object())
            
            selected <- NULL
            
          } else {
            
            choices <- SPATA2::getFeatureNames(active_spata_object()) %>% base::unname()
            
            selected <- "nCount_Spatial"
            
          }
          
          shinyWidgets::pickerInput(
            inputId = "color_by_var",
            label = "Variable:",
            choices = choices,
            selected = selected,
            multiple = FALSE,
            options = shinyWidgets::pickerOptions(liveSearch = TRUE)
          )
          
          
        })
        
        # reactive vals
        
        alpha_by <- shiny::reactive({
          
          if(SPATA2::isNumericVariable(active_spata_object(), color_by()) && base::isTRUE(input$scale_transp)){
            
            out <- color_by()
            
          } else {
            
            out <- NULL
            
          }
          
          return(out)
          
          
        })
        
        color_by <- shiny::reactive({ input$color_by_var })
        
        coords_df <- shiny::reactive({ SPATA2::getCoordsDf(active_spata_object()) })
        
        pt_alpha <- shiny::reactive({ 1 - input$pt_transp})
        
        pt_clrsp <- shiny::reactive({ input$pt_clrsp })
        
        pt_size <- shiny::reactive({ input$pt_size })
        
        smooth <- shiny::reactive({
          
          out <- list()
          
          if(input$pt_smooth == 0){
            
            out$smooth <- FALSE
            out$smooth_span <- 0
            
          } else {
            
            out$smooth <- TRUE
            out$smooth_span <- input$pt_smooth
            
          }
          
          return(out)
          
        })
        
        xrange <- shiny::reactive({ SPATA2::getImageRange(active_spata_object())$x })
        
        yrange <- shiny::reactive({ SPATA2::getImageRange(active_spata_object())$y })
        
        output$surface_plot <- shiny::renderPlot({
          
          shiny::req(color_by())
          
          SPATA2::plotSurface(
            object = active_spata_object(),
            color_by = color_by(),
            alpha_by = alpha_by(),
            pt_size = pt_size(),
            pt_alpha = pt_alpha(),
            smooth = smooth()$smooth,
            smooth_span = smooth()$smooth_span,
            pt_clrsp = pt_clrsp(),
            display_image = TRUE, 
            verbose = FALSE
          ) +
            SPATA2::ggpLayerFrameByImage(object = active_spata_object())
          
          
        })  
        
      }
    )
  )
  
}