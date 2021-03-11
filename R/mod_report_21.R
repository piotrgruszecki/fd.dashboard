#' report_21 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectInput sliderInput verbatimTextOutput
#' @import R6
#' @importFrom shinycssloaders withSpinner
#' @importFrom sunburstR sund2bOutput
mod_report_21_ui <- function(id){
  ns <- NS(id)
  tagList(

    h2("Report 21: Industries, Categories"),

    fluidRow(

      column(width = 4, shinydashboard::box(
        width = 12, title = "Data Subset", collapsible = TRUE, status = "primary",
        shinyWidgets::airMonthpickerInput(inputId = ns("year_month_selected"), label = "Month to analyse:", value = lubridate::today(), minDate = "2020-10-01", maxDate = lubridate::today())
        #shiny::selectInput(inputId = ns("websites_selected"), label = "Websites (sorted by volume in given month):", choices = c("US", "GB"), selected = "US", multiple = TRUE)
      ))

      # column(width = 8, shinydashboard::box(
      #   width = 12, title = "Overview", status = "primary", collapsible = TRUE, sunburstR::sunburstOutput(outputId = ns("plot_01_overview")) %>% withSpinner(color = "#0dc5c1")
      #))
    ), # end fluidRow

    fluidRow(

      shinydashboard::tabBox(title = "First tabBox", id = "tabset1", height = "450px", width = 12,
        shiny::tabPanel(title = "Prime Industry",

          fluidRow(
            shinydashboard::box(width = 8, title = "Prime Industry. Leads, Revenue.",
              fluidRow(column(width = 8), #-- placeholder for the momentt
                       column(width = 4, shinyWidgets::radioGroupButtons(inputId = ns("overview_plot_switch"), label = "Leads or Revenue", choices = c("lead", "rev"), selected = "rev", justified = FALSE, checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
               ),
            sunburstR::sund2bOutput(outputId = ns("plot_01")) %>% withSpinner(color = "#0dc5c1")
            ),
            shinydashboard::box(width = 4, title = "Leads, Rev vs web", DT::DTOutput(outputId = ns("table_01"))
            )
          ), #-- fluidRow

          fluidRow(
            shinydashboard::box(width = 8, title = "Split by currencies. Leads, Revenue",
              fluidRow(column(width = 8, shinyWidgets::radioGroupButtons(inputId = ns("currency_selected"), label = "Currencies", choices = c("USD", "EUR", "ZAR", "GBP", "MXN"), selected = "USD", justified = FALSE, checkIcon = list(yes = icon("ok", lib = "glyphicon")))),
                       column(width = 4, shinyWidgets::radioGroupButtons(inputId = ns("currency_plot_switch"), label = "Leads or Revenue", choices = c("lead", "rev"), selected = "rev", justified = FALSE, checkIcon = list(yes = icon("ok", lib = "glyphicon"))))
              ),
              sunburstR::sund2bOutput(outputId = ns("plot_02")) %>% withSpinner(color = "#0dc5c1")
            ),
            shinydashboard::box(width = 4, title = "Currency vs web", DT::DTOutput(outputId = ns("table_02"))
            )
          ) #-- fluidRow

        ),

        shiny::tabPanel(title = "Industries",
          fluidRow(shinydashboard::box(width = 8,
            plotOutput(outputId = ns("plot_06"))))
        ),

        shiny::tabPanel(title = "Debug",
          fluidPage(
            fluidRow(
              shiny::verbatimTextOutput(outputId = ns("r6_table_for_industries_cols")),
              shiny::verbatimTextOutput(outputId = ns("subset_debug_dt_cols")),
              shiny::verbatimTextOutput(outputId = ns("subset_debug_dt_length")),
              shiny::verbatimTextOutput(outputId = ns("dates_selected")),
              shiny::verbatimTextOutput(outputId = ns("dates_selected2")),
              shiny::verbatimTextOutput(outputId = ns("dates_selected3")),
              shiny::verbatimTextOutput(outputId = ns("websites_sorted")),
              shiny::verbatimTextOutput(outputId = ns("currencies_sorted")),

              DT::DTOutput(outputId = ns("r6_table_for_industries")),
              DT::DTOutput(outputId = ns("subset_debug_dt")),
              DT::DTOutput(outputId = ns("subset_dt")),
              DT::DTOutput(outputId = ns("subset_web_dt")),
              DT::DTOutput(outputId = ns("subset_currency_dt"))
              #DT::DTOutput(outputId = ns("table_02"))


              #DT::DTOutput(outputId = ns("r6_table_for_industries_dcast"))
            )
          ) # fluidPage
        ) # tabPanel Debug
      ) # tabBox
    ) # fluidRow
  )
}

#' report_21 Server Functions
#' @import R6
#' @import data.table
#' @importFrom lubridate month year today %--% %within%
#' @importFrom sunburstR sund2b renderSund2b
#' @noRd
mod_report_21_server <- function(id, aws_buffer){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    #-- simulate menu choices
    dt <- aws_buffer$table_for_industries

    #-- begin debugging section
    subset_debug_dt <- dt[month == "Feb" & website_iso2c == "US" & ppl_price_currency == "USD"]

    table_for_industries_cols <- aws_buffer$table_for_industries %>% length()
    output$r6_table_for_industries_cols <- shiny::renderText(glue::glue("R6 table_for_industries, columns: {table_for_industries_cols}"))

    subset_debug_dt_cols <- subset_debug_dt %>% length()
    output$subset_debug_dt_cols <- shiny::renderText(glue::glue("subset_debug_dt, columns: {subset_debug_dt_cols}"))

    subset_debug_dt_length <- subset_debug_dt[, .N]
    output$subset_debug_dt_length <- shiny::renderText(glue::glue("subset_debug_dt, rows: {subset_debug_dt_length}"))

    output$r6_table_for_industries <- DT::renderDT(dt)
    output$subset_debug_dt <- DT::renderDT(subset_debug_dt)

    output$dates_selected  <- shiny::renderText(glue::glue("Selected dates: {input$year_month_selected}"))
    output$dates_selected2 <- shiny::renderText(glue::glue("Selected year: {lubridate::year(input$year_month_selected)}, Selected month: {lubridate::month(input$year_month_selected)}"))
    output$dates_selected3 <- shiny::renderText(glue::glue("SSelected month with label: {lubridate::month(input$year_month_selected, label = TRUE, abbr = TRUE)}"))

    subset_dcast_dt <- reactive({
      month_selected <- input$year_month_selected %>% lubridate::month(label = TRUE, abbr = TRUE)
      dt <- dt[month == month_selected, .(leads = sum(leads)), .(month, ppl_price_currency, web = website_iso2c)][, dcast.data.table(.SD, web ~  ppl_price_currency, fill = 0)]
      return(dt)
    })
    output$r6_table_for_industries_dcast <- DT::renderDT({subset_dcast_dt()})
    #-- end debugging section

    #-- adjust available websites, depending on selected dates
    observeEvent(eventExpr = input$year_month_selected,
                 handlerExpr = {

                   month_sel <- lubridate::month(input$year_month_selected, label = T, abbr = T)
                   year_sel  <- lubridate::year(input$year_month_selected)
                   tmp_dt <- aws_buffer$table_for_industries[month %in% month_sel, .(leads = sum(leads)), .(website_iso2c, ppl_price_currency)]

                   #-- websites
                   websites_choices <- tmp_dt[, .(leads = sum(leads)), .(website_iso2c)][order(-leads)][, website_iso2c]
                   updateSelectInput(session = session, inputId = "websites_selected", choices = websites_choices, selected = input$websites_selected)

                   websites_sorted <- websites_choices %>% stringr::str_c() %>% paste(collapse = ", ")
                   output$websites_sorted <- shiny::renderText(glue::glue("Websites, sorted: {websites_sorted}"))

                   #-- currencies
                   currency_choices <- tmp_dt[, .(leads = sum(leads)), .(ppl_price_currency)][order(-leads)][!is.na(ppl_price_currency), as.character(ppl_price_currency) ]
                   shinyWidgets::updateRadioGroupButtons(session = session, inputId = "currency_selected", choices = currency_choices, selected = input$currency_selected)

                   currencies_sorted <- currency_choices %>% stringr::str_c() %>% paste(collapse = ", ")
                   output$currencies_sorted <- shiny::renderText(glue::glue("Currencies, sorted: {currencies_sorted}"))

                 })

    #-- data subset, based on main filters - month, website
    #-- subset by dates, all websites - used for overview chart
    subset_dt <- reactive({
      month_selected <- input$year_month_selected %>% lubridate::month(label = TRUE, abbr = TRUE)
      dt <- dt[month == month_selected, .(leads = sum(leads), rev = sum(rev)), .(seq, website_iso2c, ppl_price_currency)][, .(seq, leads, rev, website_iso2c, ppl_price_currency)][order(-leads)]
      return(dt)
    })
    output$subset_dt <- DT::renderDT({subset_dt()})

    #-- further narrow down, including website selection
    subset_web_dt <- reactive({
      websites_selected <- input$websites_selected
      dt <- subset_dt()[website_iso2c %in% websites_selected, .(leads = sum(leads), rev = sum(rev)), .(seq)][, .(seq, leads, rev)][order(-leads)]
      return(dt)
    })
    output$subset_web_dt <- DT::renderDT({subset_web_dt()})

    #-- aggregate revenue
    subset_currency_dt <- reactive({
      currency_selected <- input$currency_selected
      dt <- subset_dt()[ppl_price_currency %in% currency_selected, .(leads = sum(leads), rev = sum(rev)), .(seq)][, .(seq, leads, rev)][order(-rev)]
      return(dt)
    })
    output$subset_currency_dt <- DT::renderDT({subset_currency_dt()})


    #-- charting
    #-- overview plot, all websites, for selected period
    render_overview_sunbursts_plots <- reactive({

      #-- whether we should chart leads or revenue; the table follows
      overview_plot_switch  <- input$overview_plot_switch

      plot <- switch(overview_plot_switch,
                     lead = subset_dt()[, .(leads = sum(leads)), .(seq)] %>% sunburstR::sund2b(),
                     rev  = subset_dt()[, .(rev   = sum(rev)),   .(seq)] %>% sunburstR::sund2b()
      )

      plot_overview_sunbursts_list <- list(plot = plot)
      return(plot_overview_sunbursts_list)
    })
    output$plot_01  <- sunburstR::renderSund2b({render_overview_sunbursts_plots()$plot})

    #-- table adjacent to plot_01
    render_overview_table <- reactive({

      dt <- subset_dt()[, .(leads = sum(leads), rev = sum(rev)), .(website_iso2c)]
      table_overview_sunbursts_list <- list(table = dt)
      return(table_overview_sunbursts_list)
    })
    output$table_01 <- DT::renderDT({render_overview_table()$table}, options = list(pageLength = 15))

    #-- detailed plot for selected month, currency
    render_currency_sunbursts_plots <- reactive({

       #-- whether we should chart leads or revenue; the table follows
       currency_plot_switch  <- input$currency_plot_switch

       aggregate_dt <- switch(currency_plot_switch,
                              lead = subset_currency_dt()[, .(leads = sum(leads)), .(seq)],
                              rev  = subset_currency_dt()[, .(rev   = sum(rev)),   .(seq)]
                              )

       plot <- switch(currency_plot_switch,
                      lead = aggregate_dt[, .(leads = sum(leads)), .(seq)] %>% sunburstR::sund2b(),
                      rev  = aggregate_dt[, .(rev   = sum(rev)),   .(seq)] %>% sunburstR::sund2b()
                      )

       table <- switch(currency_plot_switch,
                       lead = subset_dt()[, .(leads = sum(leads)), .(ppl_price_currency, web = website_iso2c)][, dcast.data.table(.SD, web ~ ppl_price_currency, fill = 0)],
                       rev  = subset_dt()[, .(rev   = sum(rev)),   .(ppl_price_currency, web = website_iso2c)][, dcast.data.table(.SD, web ~ ppl_price_currency, fill = 0)]
                       )

       plot_currency_sunbursts_list <- list(plot = plot, table = table)
       return(plot_currency_sunbursts_list)
    })
    output$plot_02  <- sunburstR::renderSund2b({render_currency_sunbursts_plots()$plot})
    output$table_02 <- DT::renderDT({render_currency_sunbursts_plots()$table}, options = list(pageLength = 15))


    #output$plot_03 <- sunburstR::renderSunburst(sunburstR::sunburst(data = subset_dt[, .(seq, leads)], count = T))
    #output$plot_04 <- sunburstR::renderSunburst(sunburstR::sunburst(data = subset_dt[, .(seq, rev)], count = T))


    #-- lorem ipsum below
    #output$plot_01 <- renderPlot({shinipsum::random_ggplot(type = "density")})
    #output$plot_02 <- renderPlot({shinipsum::random_ggplot(type = "bar")})
    output$plot_03 <- renderPlot({shinipsum::random_ggplot(type = "bar")})
    output$plot_04 <- renderPlot({shinipsum::random_ggplot(type = "bar")})
    output$plot_05 <- renderPlot({shinipsum::random_ggplot(type = "bar")})
    output$plot_06 <- renderPlot({shinipsum::random_ggplot(type = "bar")})

    # output$table_1       <- DT::renderDT({shinipsum::random_DT(nrow = 5, ncol = 3, type = "numeric")})
    # output$table_2       <- DT::renderDT({shinipsum::random_DT(nrow = 5, ncol = 3, type = "numeric")})


  })
}

## To be copied in the UI
# mod_report_21_ui("report_21_ui_1")

## To be copied in the server
# mod_report_21_server("report_21_ui_1")

