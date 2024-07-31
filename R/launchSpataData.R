


#' @title Interactive SPATAData
#' @keywords internal
#'
launchSpataData <- function(){
  
  shiny::runApp(
    shiny::shinyApp(
      ui = launchSpataDataUI(), 
      server = launchSpataDataServer
    )
  )
  
}