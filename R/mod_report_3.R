#' report_3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import R6
mod_report_3_ui <- function(id){
  ns <- NS(id)

  tagList(

    h2("Report 3"),

    fluidRow(
         shiny::verbatimTextOutput(outputId = ns("r6_websites")),
         shiny::verbatimTextOutput(outputId = ns("r6_websites_number")),
         shiny::verbatimTextOutput(outputId = ns("dates_selected")),
         shiny::verbatimTextOutput(outputId = ns("dates_selected2")),
         shiny::verbatimTextOutput(outputId = ns("websites_sorted"))

    ),

    fluidRow(

      column(width = 4, shinydashboard::box(
        width = 12, title = "Data Subset", collapsible = TRUE, footer = "This box can be closed by minus sign above", status = "primary",
        shinyWidgets::airMonthpickerInput(inputId = ns("year_month_selected"), label = "Month to analyse:", value = lubridate::today(), minDate = "2020-10-01", maxDate = lubridate::today()),
        shiny::selectInput(inputId = ns("countries_selected"), label = "Websites (sorted by volume in last 30d):", choices = c("US", "UK"), selected = "US", multiple = TRUE),
        shiny::sliderInput("aggregation_level", "Countries (Geo's) Aggregation level", 1, 5, 3, step = 1, ticks = T)
        )
      ),

      column(width = 8, shinydashboard::box(
        width = 12, title = "Overview chart as a reference", status = "primary", collapsible = TRUE, plotOutput(outputId = ns("plot_overview"))
         )
      )

    )

  )
}

#' report_3 Server Functions
#'
#' @noRd
#' @import R6
#' @importFrom data.table fwrite
mod_report_3_server <- function(id, aws_buffer){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    websites_available <- aws_buffer$websites %>% stringr::str_c() %>% paste(collapse = ", ")
    aws_buffer$websites %>% stringr::str_c() %>% as.list() %>%
      data.table::fwrite("websites_available.csv")
    output$r6_websites <- shiny::renderText(glue::glue("R6 websites: {websites_available}"))

    websites_n <- aws_buffer$websites %>% length()
    output$r6_websites_number <- shiny::renderText(glue::glue("R6 websites, number of: {websites_n}"))

    output$dates_selected  <- shiny::renderText(glue::glue("Selected dates: {input$year_month_selected}"))
    output$dates_selected2 <- shiny::renderText(glue::glue("Selected year: {lubridate::year(input$year_month_selected)}, Selected month: {lubridate::month(input$year_month_selected)}"))


    observeEvent(eventExpr = input$year_month_selected,
                 handlerExpr = {
                   #all_cols <- map_lgl(.x = mtcars, ~ all(. %% 1 == 0))
                   #selected_cols <- names(all_cols[all_cols == input$varTypes])

                   month_sel <- lubridate::month(input$year_month_selected, label = T, abbr = T)
                   year_sel  <- lubridate::year(input$year_month_selected)
                   websites_choices <- aws_buffer$websites2[month %in% month_sel & year %in% year_sel, .(n = sum(N)), .(website_iso2c)][order(-n)][, website_iso2c]
                   #websites_choices <- c("FR", "DE", "CA")
                   updateSelectInput(session = session, inputId = "countries_selected", choices = websites_choices, selected = websites_choices[1:3])

                   websites_sorted <- websites_choices %>% stringr::str_c() %>% paste(collapse = ", ")
                   output$websites_sorted <- shiny::renderText(glue::glue("Websites, sorted: {websites_sorted}"))

                 })




    #websites_choices <- aws_buffer$websites %>% stringr::str_c()
    # Sun Jan 10 20:04:41 2021 ------------------------------
    #-- doesn't work, probably related to ns and also need to make it reactive
    #-- https://community.rstudio.com/t/accessing-inputs-in-a-updateselectinput-inside-a-shiny-module/12286
    #-- https://rtask.thinkr.fr/communication-between-modules-and-its-whims/
    #shiny::updateSelectInput(session = session, inputId = "countries_selected", choices = websites_choices, selected = websites_choices[1:3])

  })
}

## To be copied in the UI
# mod_report_3_ui("report_3_ui_1")

## To be copied in the server
# mod_report_3_server("report_3_ui_1")
