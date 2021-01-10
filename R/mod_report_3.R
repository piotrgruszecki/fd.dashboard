#' report_3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_report_3_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' report_3 Server Functions
#'
#' @noRd 
mod_report_3_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_report_3_ui("report_3_ui_1")
    
## To be copied in the server
# mod_report_3_server("report_3_ui_1")
