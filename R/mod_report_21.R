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
#'
mod_report_21_ui <- function(id){
  ns <- NS(id)
  tagList(

    h2("Report 21: Industries, Categories"),

    fluidRow(

      column(width = 4, shinydashboard::box(
        width = 12, title = "Data Subset", collapsible = TRUE, status = "primary",
        shinyWidgets::airMonthpickerInput(inputId = ns("year_month_selected"), label = "Month to analyse:", value = lubridate::today(), minDate = "2020-10-01", maxDate = lubridate::today()),
        shiny::selectInput(inputId = ns("websites_selected"), label = "Websites (sorted by volume in given month):", choices = c("US", "GB"), selected = "US", multiple = TRUE)

      )),

      column(width = 8, shinydashboard::box(
        width = 12, title = "Overview", status = "primary", collapsible = TRUE, sunburstR::sunburstOutput(outputId = ns("plot_01_overview")) %>% withSpinner(color = "#0dc5c1")
      ))
    ), # end fluidRow

    fluidRow(

      shinydashboard::tabBox(title = "First tabBox", id = "tabset1", height = "250px", width = 12,
        shiny::tabPanel(title = "Prime Industry",
          fluidRow(shinydashboard::box(width = 6, title = "Leads", sunburstR::sunburstOutput(outputId = ns("plot_01"))),
                   shinydashboard::box(width = 6, title = "Revenue", sunburstR::sunburstOutput(outputId = ns("plot_02")))),
          fluidRow(shinydashboard::box(width = 6, sunburstR::sund2bOutput(outputId = ns("plot_01b_overview"))),
                   shinydashboard::box(width = 6, sunburstR::sunburstOutput(outputId = ns("plot_04"))))
        ),
        shiny::tabPanel(title = "Industries",
          fluidRow(shinydashboard::box(width = 8, plotOutput(outputId = ns("plot_06"))))
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

              DT::DTOutput(outputId = ns("r6_table_for_industries")),
              DT::DTOutput(outputId = ns("subset_debug_dt")),
              DT::DTOutput(outputId = ns("subset_dt")),

              DT::DTOutput(outputId = ns("r6_table_for_industries_dcast"))
            )
          ))
      )
    )
  )
}

#' report_21 Server Functions
#' @import R6
#' @import data.table
#' @importFrom lubridate month year today %--% %within%
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
      dt <- dt[month == month_selected, .(leads = sum(leads)), .(month, ppl_price_currency, website_iso2c)][, dcast.data.table(.SD, website_iso2c ~  ppl_price_currency, fill = 0)]
      return(dt)
    })
    output$r6_table_for_industries_dcast <- DT::renderDT({subset_dcast_dt()})
    #-- end debugging section

    #-- adjust available websites, depending on selected dates
    observeEvent(eventExpr = input$year_month_selected,
                 handlerExpr = {

                   month_sel <- lubridate::month(input$year_month_selected, label = T, abbr = T)
                   year_sel  <- lubridate::year(input$year_month_selected)
                   websites_choices <- aws_buffer$table_for_industries[month %in% month_sel, .(leads = sum(leads)), .(website_iso2c)][order(-leads)][, website_iso2c]
                   updateSelectInput(session = session, inputId = "websites_selected", choices = websites_choices, selected = input$websites_selected)

                   websites_sorted <- websites_choices %>% stringr::str_c() %>% paste(collapse = ", ")
                   output$websites_sorted <- shiny::renderText(glue::glue("Websites, sorted: {websites_sorted}"))

                 })

    #-- data subset, based on main filters - month, website
    #-- subset by dates, all websites - used for overview chart
    subset_dt <- reactive({
      month_selected <- input$year_month_selected %>% lubridate::month(label = TRUE, abbr = TRUE)
      dt <- dt[month == month_selected, .(leads = sum(leads), rev = sum(rev)), .(seq, website_iso2c)][, .(seq, leads, rev, website_iso2c)][order(-leads)]
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

    #-- charting
    #-- overview plot, all websites, for selected period
    render_overview_sunburst_plot <- function(){
      dt <- subset_dt()
      plot_01  <- dt[, .(leads = sum(leads)), .(seq)] %>% sunburstR::sunburst(count = T)
      plot_01b <- dt[, .(leads = sum(leads)), .(seq)] %>% sunburstR::sund2b()

      plots_sunbursts_list <- list(plot_01 = plot_01, plot_01b = plot_01b)
      return(plots_sunbursts_list)
    }
    output$plot_01_overview  <- sunburstR::renderSunburst({render_overview_sunburst_plot()$plot_01})
    output$plot_01b_overview <- sunburstR::renderSund2b({render_overview_sunburst_plot()$plot_01b})

    #-- detailed plot for selected month, website
    render_sunbursts_plots <- function(){
       dt <- subset_web_dt()
       plot_01 <- dt[, .(seq, leads)] %>% sunburstR::sunburst(count = T)
       plot_02 <- dt[, .(seq, rev)]   %>% sunburstR::sunburst(count = T)
       plots_sunbursts_list <- list(plot_01 = plot_01, plot_02 = plot_02)
       return(plots_sunbursts_list)
    }
    output$plot_01 <- sunburstR::renderSunburst({render_sunbursts_plots()$plot_01})
    output$plot_02 <- sunburstR::renderSunburst({render_sunbursts_plots()$plot_02})

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
