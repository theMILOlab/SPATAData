ui <- dashboardPage(
  dashboardHeader(title = "My Page"),
  dashboardSidebar(sidebarMenuOutput("sideBar_menu_UI")),
  dashboardBody(
    uiOutput("body_UI"),
    uiOutput("test_UI")
  )
)

server <- shinyServer(function(input, output, session) { 
  output$sideBar_menu_UI <- renderMenu({
    sidebarMenu(id = "sideBar_Menu",
                menuItem("Menu 1", tabName="menu1_tab", icon = icon("calendar")),
                menuItem("Menu 2", tabName="menu2_tab", icon = icon("database"))
    )
  }) 
  output$test_UI <- renderUI ({
    tabItems(
      tabItem(tabName = "menu1_tab", uiOutput("menu1_UI")),
      tabItem(tabName = "menu2_tab", uiOutput("menu2_UI"))
    )
  })
  output$body_UI <- renderUI ({
    p("Default content in body outsite any sidebar menus.")
  })
  output$menu1_UI <- renderUI ({
    box("Menu 1 Content")
  })
  output$menu2_UI <- renderUI ({
    box("Menu 2 Content")
  })
  
})

runApp(list(ui= ui, server = server))

if (interactive()) {
  library(shiny)
  
  body <- dashboardBody(
    fluidRow(
      tabBox(
        title = "First tabBox",
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "250px",
        tabPanel("Tab1", "First tab content"),
        tabPanel("Tab2", "Tab content 2")
      ),
      tabBox(
        side = "right", height = "250px",
        selected = "Tab3",
        tabPanel("Tab1", "Tab content 1"),
        tabPanel("Tab2", "Tab content 2"),
        tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
      )
    ),
    fluidRow(
      tabBox(
        # Title can include an icon
        title = tagList(shiny::icon("gear"), "tabBox status"),
        tabPanel("Tab1",
                 "Currently selected tab from first box:",
                 verbatimTextOutput("tabset1Selected")
        ),
        tabPanel("Tab2", "Tab content 2")
      )
    )
  )
  
  shinyApp(
    ui = dashboardPage(dashboardHeader(title = "tabBoxes"), dashboardSidebar(), body),
    server = function(input, output) {
      # The currently selected tab from the first box
      output$tabset1Selected <- renderText({
        input$tabset1
      })
    }
  )
}


## Only run examples in interactive R sessions
if (interactive()) {
  
  ui <- fluidPage(
    p("The first checkbox group controls the second"),
    checkboxGroupInput("inCheckboxGroup", "Input checkbox",
                       c("Item A", "Item B", "Item C")),
    checkboxGroupInput("inCheckboxGroup2", "Input checkbox 2",
                       c("Item A", "Item B", "Item C"))
  )
  
  server <- function(input, output, session) {
    observe({
      x <- input$inCheckboxGroup
      
      # Can use character(0) to remove all choices
      if (is.null(x))
        x <- character(0)
      
      # Can also set the label and select items
      updateCheckboxGroupInput(session, "inCheckboxGroup2",
                               label = paste("Checkboxgroup label", length(x)),
                               choices = x,
                               selected = x
      )
    })
  }
  
  shinyApp(ui, server)
}
