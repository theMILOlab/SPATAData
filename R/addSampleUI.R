
# The html for the user interface of 
addSampleUI <- function(){
  
  shinydashboard::dashboardPage(
    
    header = shinydashboard::dashboardHeader(title = "Spatial Biology"), 
    
    sidebar = shinydashboard::dashboardSidebar(
      collapsed = TRUE,
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem(text = "Add Sample", tabName = "add_sample", selected = TRUE)
      )
    ), 
    
    
    body = shinydashboard::dashboardBody(
      
      shinydashboard::tabItems(
        
        shinydashboard::tabItem(
          tabName = "add_sample",
          shiny::fluidRow(
            shiny::column(
              width = 12, 
              align = "center",
              shinydashboard::box(
                title = "Provide Information", 
                solidHeader = TRUE, 
                collapsible = TRUE,
                width = 12, 
                shiny::uiOutput(outputId = "provide_information")  
              ), 
              shinydashboard::box(
                title = "All Samples", 
                solidHeader = TRUE,
                collapsible = TRUE,
                width = 12,
                shiny::dataTableOutput(outputId = "all_samples")
              ),
              shinyWidgets::actionBttn(
                inputId = "close_app",
                label = "Close app",
                color = "success",
                style = "gradient", 
                size = "lg"
              )
            )
          )
        )
      )
    )
  )
  
  
  
}