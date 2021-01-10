#' report_9 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_report_9_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' report_9 Server Functions
#'
#' @noRd 
mod_report_9_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_report_9_ui("report_9_ui_1")
    
## To be copied in the server
# mod_report_9_server("report_9_ui_1")
