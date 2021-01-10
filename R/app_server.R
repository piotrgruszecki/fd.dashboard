#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic


    con <- get_aws_connection()

    output$con_prt <- shiny::renderText(con %>% typeof())

}
