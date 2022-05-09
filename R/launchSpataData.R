


#' @title Interactive SPATAData
#' @export
#'
launchSpataData <- function(){
  
  shiny::runApp(
    shiny::shinyApp(
      ui = launchSpataDataUI(), 
      server = launchSpataDataServer
    )
  )
  
}