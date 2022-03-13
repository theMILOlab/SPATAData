


runSpataOnlineUI <- function(source_df = sourceDataFrame()){
  
  shinydashboard::dashboardPage(
    
    shinydashboard::dashboardHeader(title = "Spatial Biology"), 
    
    shinydashboard::dashboardSidebar(
      collapsed = FALSE, 
      shinydashboard::sidebarMenu(
        id = "sidebar",
        shinydashboard::menuItem(text = "Welcome", tabName = "welcome"), 
        shinydashboard::menuItem(text = "Tissue - Overview", tabName = "tissue_overview"), 
        shinydashboard::menuItem(text = "Tissue - Organs", tabName = "tissue_organs",
                                 shiny::tagList(
                                   purrr::map(
                                     .x = base::unique(source_df$organ),
                                     .f = ~ htmlTissueMenuItem(organ = .x)
                                     )
                                 )
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
      
      shiny::tags$div(class = "tab-content",

        shiny::uiOutput(outputId = "all_tab_items"),

      )
    )
  )
  
}