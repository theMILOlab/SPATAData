


runSpataOnlineUI <- function(){
  
  shinydashboard::dashboardPage(
    
    shinydashboard::dashboardHeader(title = "Spatial Biology"), 
    
    shinydashboard::dashboardSidebar(
      collapsed = FALSE, 
      shinydashboard::sidebarMenu(
        id = "sidebar",
        shinydashboard::menuItem(text = "Welcome", tabName = "welcome"), 
        shinydashboard::menuItem(text = "Tissue - Overview", tabName = "tissue_overview"), 
        shinydashboard::menuItem(text = "Tissue - Organs", tabName = "tissue_organs", 
                                 htmlTissueMenuItem(organ = "brain"),
                                 htmlTissueMenuItem(organ = "heart"),
                                 htmlTissueMenuItem(organ = "liver")
        ),
        shinydashboard::menuItem(text = "Visualize", tabName = "visualize"), 
        shinydashboard::menuItem(text = "SPATA Framework", tabName = "spata_framework"),
        shinydashboard::menuItem(text = "Contributions", tabName = "contributions",
                                 shinydashboard::menuSubItem(
                                   text = "Bengsch Lab", 
                                   tabName = "contribution_bengsch_lab"
                                 ), 
                                 shinydashboard::menuSubItem(
                                   text = "Wolf Lab", 
                                   tabName = "contribution_wolf_lab"
                                 )
        ), 
        shinydashboard::menuItem(text = "FAQ", tabName = "faq")
        
      )
    ), 
    
    shinydashboard::dashboardBody(

      shinybusy::add_busy_spinner(spin = "cube-grid", color = "red"),
      
      shinydashboard::tabItems(
        
        htmlTissueTabItem(organ = "Brain", mode = "h", ncol = 3),
        htmlTissueTabItem(organ = "Brain", mode = "p", ncol = 3), 
        htmlTissueTabItem(organ = "Liver", mode = "h", ncol = 3),
        htmlTissueTabItem(organ = "Liver", mode = "p", ncol = 3), 
        
        
        shinydashboard::tabItem(
          tabName = "visualize",
          shiny::fluidRow(
            
            shiny::column(
              width = 8,
              shinydashboard::box(
                #title = "Surface Plots", 
                #status = "primary", 
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
                #title = "Adjustments", 
                #status = "primary", 
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
                    #1
                    sliderInput(
                      inputId = "pt_smooth",
                      label = "Point-Smoothing:",
                      min = 0, max = 0.5, value = 0, step = 0.01
                    ),
                    #2
                    shiny::sliderInput(
                      inputId = "pt_transp",
                      label = "Point-Transparency:",
                      min = 0,
                      max = 1,
                      value = 0.25,
                      step = 0.01
                    ),
                    #3
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
      )
    )
  )
  
}