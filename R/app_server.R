#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import glue glue
#' @importFrom data.table .N .BY
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic

    if (!exists("config")) config <<- config::get(file = app_sys("golem-config.yml"), use_parent = TRUE)
    output$config_host <- shiny::renderText(glue::glue("Host: {config$host}"))

    # establish connection and verify by printout
    con <- get_aws_connection()
    output$con_prt <- shiny::renderText(glue::glue("DB Connection type: {con %>% typeof()}"))

    # pull data, get number of rows
    leads_dt <- get_data_adapt_for_report_9()
    leads_n_rows <- leads_dt[, .N]
    output$leads_n_rows <- shiny::renderText(glue::glue("Leads DT, .N: {leads_n_rows}"))

    # aggregate by months
    leads_summary_dt <- leads_dt[, .N, by = .(year, month)]
    output$leads_summary_table <- shiny::renderDataTable(leads_summary_dt)


}
