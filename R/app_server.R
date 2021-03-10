#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import glue glue
#' @import R6
#' @importFrom data.table .N .BY dcast.data.table
#' @importFrom lubridate days
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic

    if (!exists("config")) config <<- config::get(file = app_sys("golem-config.yml"), use_parent = TRUE)
    output$config_host <- shiny::renderText(glue::glue("Host: {config$host}"))

    # establish connection and verify by printout
    #con <- get_aws_connection()
    #output$con_prt <- shiny::renderText(glue::glue("DB Connection type: {con %>% typeof()}"))

    # pull data, get number of rows
    leads_dt <- get_clean_leads()

    leads_n_rows <- leads_dt[, .N]
    output$leads_n_rows <- shiny::renderText(glue::glue("Leads DT, .N: {leads_n_rows}"))

    # aggregate by months
    leads_summary_dt <- leads_dt[, .N, by = .(year, month)][, dcast.data.table(.SD, year ~ month)]
    output$leads_summary_table <- shiny::renderDataTable(leads_summary_dt)

    # profiles
    profiles_dt <- fd.dashboard::get_clean_profiles()


    # Sun Jan 10 15:16:47 2021 ------------------------------
    # initiate R6 object
    AwsBuffer <- R6Class(classname = "Buffer", list(
      leads    = 0,
      profiles = 0,
      websites = 0,
      websites2= 0,
      leads_dates_range = 0,
      leads_sent = 0,
      leads_credited = 0,
      table_for_industries = 0,
      #leads_marked_credited = 0,
      pull_leads    = function(dates_range) {2 + 2},
      pull_profiles = function(dates_range) {3 + 3}
    ))

    aws_buffer <- AwsBuffer$new()
    aws_buffer$leads <- leads_dt

    aws_buffer$websites <- leads_dt[, unique(website_iso2c)]
    aws_buffer$websites2 <- leads_dt[, .N, .(website_iso2c, year, month)]

    aws_buffer$leads_dates_range <- aws_buffer$leads[, range(Date)]
    aws_buffer$leads_dates_range[2] <- aws_buffer$leads_dates_range[2] - lubridate::days(1)

    aws_buffer$profiles <- profiles_dt

    #-- trim leads to sent, and in addition mark credited with a flag
    aws_buffer$leads_sent <- mark_credited_leads(aws_buffer$leads)

    #-- separate table with credited leads
    #aws_buffer$leads_credited <- get_credited_leads(aws_buffer$leads_sent)

    #--
    aws_buffer$table_for_industries <- fd.dashboard::produce_join_table_for_industries()


    #-- modules below
    mod_report_3_server("report_3_ui_1", aws_buffer)
    mod_report_9_server("report_9_ui_1", aws_buffer)
    mod_report_14_server("report_14_ui_1", aws_buffer)
    mod_report_15_server("report_15_ui_1", aws_buffer)
    mod_report_21_server("report_21_ui_1", aws_buffer)



}
