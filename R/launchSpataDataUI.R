


launchSpataDataUI <- function(path_css = "C:\\Informatics\\R-Folder\\Packages\\SPATAData\\webApp\\www\\styles.css"){
  
  shinydashboard::dashboardPage(
    
    shinydashboard::dashboardHeader(title = "Spatial Biology"), 
    
    shinydashboard::dashboardSidebar(
      collapsed = FALSE, 
      shinydashboard::sidebarMenuOutput(outputId = "menu")
      ), 
    
    shinydashboard::dashboardBody(

      shinybusy::add_busy_spinner(
        spin = "cube-grid",
        color = "red",
        height = "100px", 
        width = "100px",
        margins = c(500, 950),
        position = "bottom-right",
        timeout = 100
        ),

      shiny::tags$div(class = "tab-content",

        shiny::uiOutput(outputId = "all_tab_items"),

      )
    )
  )
  
}