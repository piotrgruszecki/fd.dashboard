#' report_3 UI Function
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
mod_report_3_ui <- function(id){
  ns <- NS(id)

  tagList(

    h2("Report 3: Lead Status"),

    # data subset & reference plot
    fluidRow(

      column(width = 4, shinydashboard::box(
        width = 12, title = "Data Subset", collapsible = TRUE, footer = "This box can be closed by minus sign above", status = "primary",
        shinyWidgets::airMonthpickerInput(inputId = ns("year_month_selected"), label = "Month to analyse:", value = lubridate::today(), minDate = "2020-10-01", maxDate = lubridate::today()),
        shiny::selectInput(inputId = ns("countries_selected"), label = "Websites (sorted by volume in given month):", choices = c("US", "GB"), selected = "US", multiple = TRUE),
        shiny::sliderInput(inputId = ns("aggregation_level"), label = "Countries (Geo's) Aggregation level", 1, 5, 3, step = 1, ticks = T)
        )
      ),

      column(width = 8, shinydashboard::box(
        width = 12, title = "Overview chart as a reference", status = "primary", collapsible = TRUE, plotOutput(outputId = ns("plot_01")) %>% withSpinner(color = "#0dc5c1")
         )
      )

    ),

    # actual plots
    fluidRow(
      shinydashboard::tabBox(title = "First tabBox", id = "tabset1", height = "250px", width = 12,
        shiny::tabPanel(title = "Geo analysis",
                        fluidRow(shinydashboard::box(width = 8, plotOutput(outputId = ns("plot_02"))), shinydashboard::box(width = 4, DT::DTOutput(outputId = ns("table_1")))),
          fluidRow(shinydashboard::box(width = 12,DT::DTOutput(outputId = ns("table_2"))))
        ),
        shiny::tabPanel(title = "Credited",
          fluidRow(shinydashboard::box(width = 6, plotOutput(outputId = ns("plot_03"))),
                   shinydashboard::box(width = 6, DT::DTOutput(outputId = ns("table_3"))))
        ),
        #-- tabPanel #1
        # shiny::tabPanel(title = "Countries table",
        #   fluidRow(DT::DTOutput(outputId = ns("dt_subset")))
        # ), #-- tabPanel #2
        # shiny::tabPanel(title = "Countries, aggregated",
        #   fluidPage(DT::DTOutput(outputId = ns("dt_subset_aggregated")))),
        # debug printouts
        shiny::tabPanel("Debug",
          fluidPage(
            fluidRow(
              shiny::verbatimTextOutput(outputId = ns("r6_websites")),
              shiny::verbatimTextOutput(outputId = ns("r6_websites_number")),
              shiny::verbatimTextOutput(outputId = ns("dates_selected")),
              shiny::verbatimTextOutput(outputId = ns("dates_selected2")),
              shiny::verbatimTextOutput(outputId = ns("websites_sorted")),
              shiny::verbatimTextOutput(outputId = ns("host")),
              shiny::verbatimTextOutput(outputId = ns("plot.color")),
              DT::DTOutput(outputId = ns("dt_credited_leads"))
            )
          )
        )
      ) #-- tabBox
    ) #-- fluidRow
  )
}

#' report_3 Server Functions
#'
#' @noRd
#' @import R6
# @importFrom data.table fwrite
#' @import data.table
#' @importFrom lubridate month year today %--% %within%
mod_report_3_server <- function(id, aws_buffer){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    websites_available <- aws_buffer$websites %>% stringr::str_c() %>% paste(collapse = ", ")
    aws_buffer$websites %>% stringr::str_c() %>% as.list() %>%
      data.table::fwrite("websites_available.csv")
    output$r6_websites <- shiny::renderText(glue::glue("R6 websites: {websites_available}"))
    output$host <- shiny::renderText(glue::glue("Host: {config$host}"))
    output$plot.color <- shiny::renderText(glue::glue("Plot Color: {config$plot.color}"))

    websites_n <- aws_buffer$websites %>% length()
    output$r6_websites_number <- shiny::renderText(glue::glue("R6 websites, number of: {websites_n}"))

    plot.color       <- config$plot_color
    plot.color.light <- config$plot_light_color
    line_thickness   <- config$line_thickness

    output$dates_selected  <- shiny::renderText(glue::glue("Selected dates: {input$year_month_selected}"))
    output$dates_selected2 <- shiny::renderText(glue::glue("Selected year: {lubridate::year(input$year_month_selected)}, Selected month: {lubridate::month(input$year_month_selected)}"))

    # Tue Jan 19 14:21:50 2021 ------------------------------
    # update available dates - based on a dataset

    observeEvent(eventExpr = input$year_month_selected,
                 handlerExpr = {

                   month_sel <- lubridate::month(input$year_month_selected, label = T, abbr = T)
                   year_sel  <- lubridate::year(input$year_month_selected)
                   websites_choices <- aws_buffer$websites2[month %in% month_sel & year %in% year_sel, .(n = sum(N)), .(website_iso2c)][order(-n)][, website_iso2c]
                   updateSelectInput(session = session, inputId = "countries_selected", choices = websites_choices, selected = input$countries_selected)

                   websites_sorted <- websites_choices %>% stringr::str_c() %>% paste(collapse = ", ")
                   output$websites_sorted <- shiny::renderText(glue::glue("Websites, sorted: {websites_sorted}"))

                 })


    #-- 2.1 get data subset, according to input filters
    #-- 2.1.1 data subset
    dt <- aws_buffer$leads_sent
    charting_period <- aws_buffer$leads_dates_range[1] %--% aws_buffer$leads_dates_range[2]

    dt_subset <- reactive({
      countries_selected <- input$countries_selected
      month_selected <- input$year_month_selected %>% lubridate::month(label = TRUE, abbr = TRUE)
      year_selected  <- input$year_month_selected %>% lubridate::year()

      dt <- dt[website_iso2c %in% countries_selected & Date %within% charting_period & month == month_selected & year == year_selected, .(leads = .N), .(Date, year, Country, Country_original, website_iso2c)]
      return(dt)
    })
    output$dt_subset <- DT::renderDT({dt_subset()})

    #-- 2.1.1.1 aggregate countries
    dt_subset_aggregated_countries <- reactive({
      aggregation_level <- input$aggregation_level
      dt = copy(dt_subset()[, `:=` (Country = fct_lump(Country_original, n = aggregation_level, w = leads, other_level = "Other Countries") %>% fct_reorder(.x = leads, .fun = sum) %>% fct_rev())
      ][, .(leads = sum(leads)), .(Date, Country)])
      return(dt)
    })
    output$dt_subset_aggregated <- DT::renderDT({dt_subset_aggregated_countries()})

    #-- 2.1.1.2 aggregate countries and websites
    dt_subset_aggregated_countries_websites <- reactive({
      aggregation_level <- input$aggregation_level
      #aggregation_level <- 3
      dt = copy(dt_subset()[, `:=` (Country = fct_lump(Country_original, n = aggregation_level, w = leads, other_level = "Other Countries") %>% fct_reorder(.x = leads, .fun = sum) %>% fct_rev())
      ][, .(leads = sum(leads)), .(Date, Country, website_iso2c)])
      return(dt)
    })

    #-- 2.1.2 include rollmean
    dt_subset_rollmean <- reactive({
      rolling_window <- input$rolling_window
      dt <- dt_subset()[, .(Date, leads_rm = frollmean(leads, n = rolling_window, align = "right")), .(year)]
      return(dt)
    })

    #-- 2.1.3 credited leads, trim to selected period
    dt_credited_leads <- reactive({
      month_selected <- input$year_month_selected %>% lubridate::month(label = TRUE, abbr = TRUE)
      year_selected  <- input$year_month_selected %>% lubridate::year()

      dt <- aws_buffer$leads_credited[lubridate::month(Date, label = T, abbr = T) == month_selected & lubridate::year(Date) == year_selected, ]
      dt <- dt[, `:=` (website_iso2c = fct_reorder(website_iso2c, .x = n, .fun = sum, na.rm = T) %>% fct_rev())]

      return(dt)
    })
    output$dt_credited_leads <- DT::renderDT({dt_credited_leads()})

    #-- 2.2 set of charts charts
    #-- 2.2.1 overview plot and possibly others, where we only need data subset
    plots_basic_list <- function(){
      month_selected <- input$year_month_selected %>% lubridate::month(label = TRUE, abbr = TRUE)

      countries_selected <- input$countries_selected
      selected_countries_string <- str_flatten(countries_selected, collapse = ", ")

      #countries_top_5 <- dt_subset()[, .(leads = sum(leads)), .(Country)][order(-leads)][1:5, Country]
      plot_01 <- dt_subset_aggregated_countries() %>%
        ggplot(aes(Date, leads, color = Country)) +
        geom_line(size = line_thickness) +
        theme_bw() +
        scale_color_brewer(palette = "Set1", direction = 1) +
        # scale_fill_brewer(palette = "Set1", direction = 1) +
        theme(legend.position = "bottom") +
        #facet_wrap("Country", scales = "free_y") +
        labs(title = glue("Number of Leads in {month_selected}"), subtitle = glue("Daily numbers. Websites: {selected_countries_string}"), y = "", x = "")

      #-- 2.2.1.1 number of leads, basic plot
      plot_02 <- dt_subset_aggregated_countries() %>%
        ggplot(aes(Date, leads, color = Country)) +
        geom_line(size = line_thickness) +
        theme_bw() +
        scale_color_brewer(palette = "Set1", direction = 1) +
        theme(legend.position = "bottom") +
        facet_wrap("Country", scales = "free_y") +
        labs(title = glue("Number of Leads in {month_selected}. Facet by Countries (Geo's)."), subtitle = glue("Daily numbers. Websites: {selected_countries_string}"), y = "", x = "")

      plots_basic_list <- list(plot_01 = plot_01, plot_02 = plot_02)
      return(plots_basic_list)
    }
    output$plot_01 <- renderPlot({plots_basic_list()["plot_01"]})
    output$plot_02 <- renderPlot({plots_basic_list()["plot_02"]})

    #-- plot credited leads
    plot_credited <- function(){

      month_selected <- input$year_month_selected %>% lubridate::month(label = TRUE, abbr = TRUE)

      plot_03 <- dt_credited_leads()[, .N, .(Date, web = website_iso2c)] %>%
        ggplot(aes(Date, N, color = web, fill = web)) +
        geom_col() +
        theme_bw() +
        scale_color_viridis_d(option = "A", alpha = 1) +
        scale_fill_viridis_d(option = "A", alpha = .6) +
        labs(title = glue::glue("Number of Credited Leads in {month_selected}."), y = "", x = "")

      plots_credited_list <- list(plot_03 = plot_03)
      return(plots_credited_list)
    }
    output$plot_03 <- renderPlot({plot_credited()["plot_03"]})


    #-- 2.3 tables
    #-- 2.3.1 basic
    tables_numeric <- function(){

      tab1 <- dt_subset_aggregated_countries()[, .(leads = sum(leads)), .(Country)][order(-leads)]
      tab2 <- dt_subset_aggregated_countries_websites()[, .(leads = sum(leads)), .(Country, website_iso2c)][, dcast.data.table(.SD, Country ~ website_iso2c, value.var = "leads")]

      tables_numeric <- list(tab1 = tab1, tab2 = tab2)
      return(tables_numeric)
    }
    output$table_1 <- DT::renderDT({tables_numeric()$tab1})
    output$table_2 <- DT::renderDT({tables_numeric()$tab2})

    #-- 2.3.2 credited leads
    tables_credited <- function(){

      tab3 <- dt_credited_leads()[, .(credited_leads = sum(n)), .(web = website_iso2c)][order(-credited_leads)]

      tables_credited <- list(tab3 = tab3)
      return(tables_credited)
    }
    output$table_3 <- DT::renderDT({tables_credited()$tab3})


    #-- lorem ipsum below
    # output$plot_overview <- renderPlot({shinipsum::random_ggplot(type = "density")})
    # output$plot_02       <- renderPlot({shinipsum::random_ggplot(type = "bar")})

    # output$table_1       <- DT::renderDT({shinipsum::random_DT(nrow = 5, ncol = 3, type = "numeric")})
    # output$table_2       <- DT::renderDT({shinipsum::random_DT(nrow = 5, ncol = 3, type = "numeric")})



  })
}

## To be copied in the UI
# mod_report_3_ui("report_3_ui_1")

## To be copied in the server
# mod_report_3_server("report_3_ui_1")
