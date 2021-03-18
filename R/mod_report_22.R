#' report_22 UI Function
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
mod_report_22_ui <- function(id){
  ns <- NS(id)
  tagList(

    h2("Report 22: Daily Profiles"),

    # data subset & reference plot
    fluidRow(

      column(width = 4, shinydashboard::box(
        width = 12, title = "Data Subset", collapsible = TRUE, footer = "This box can be closed by minus sign above", status = "primary",
        shinyWidgets::airMonthpickerInput(inputId = ns("year_month_selected"), label = "Month to analyse:", value = "2021-02-01", minDate = "2021-02-01", maxDate = "2021-02-28"),
        shiny::selectInput(inputId = ns("websites_selected"), label = "Websites (sorted by volume in given month):", choices = c("US"), selected = "US", multiple = TRUE),

        shinyWidgets::pickerInput(inputId = ns("countries_selected"), label = "Countries (Geo's):",
                                  choices  = c("United States", "Canada", "India", "Nigeria"),
                                  selected = c("United States"),
                                  options = list(`actions-box` = TRUE), multiple = TRUE),

        shiny::sliderInput(inputId = ns("aggregation_level"), label = "Countries (Geo's) Aggregation level", 1, 5, 3, step = 1, ticks = T),

        shinyWidgets::checkboxGroupButtons(inputId = ns("selected_devices"), label = "Devices",
                                        choices = c("desktop", "mobile", "tablet"), selected = c("desktop", "mobile"),
                                        justified = FALSE, checkIcon = list(yes = icon("ok", lib = "glyphicon")))

        )
      ),
      column(width = 8, shinydashboard::box(
        width = 12, title = "Overview chart as a reference (placeholder)", status = "primary", collapsible = TRUE
        )
      )
    ), #-- fluidRow

    # actual plots
    fluidRow(
      shinydashboard::tabBox(title = "First tabBox", id = "tabset1", height = "750px", width = 12,
        shiny::tabPanel(title = "Daily Profiles",
          fluidRow(
            shinydashboard::box(width = 6, height = "500px", plotOutput(outputId = ns("sessions_plot"))),
            shinydashboard::box(width = 6, height = "500px", plotOutput(outputId = ns("transactions_plot"))))
           ),

        shiny::tabPanel(title = "User vs Server",
          fluidRow(
            shinydashboard::box(width = 6, height = "500px", plotOutput(outputId = ns("sessions_user_plot"))),
            shinydashboard::box(width = 6, height = "500px", plotOutput(outputId = ns("sessions_serv_plot"))))
          ),

        shiny::tabPanel("Debug",
          fluidPage(fluidRow(
               shiny::verbatimTextOutput(outputId = ns("plot.color")),
               shiny::verbatimTextOutput(outputId = ns("countries_top5")),
               DT::DTOutput(outputId = ns("dt_subset_countries_aggr")),
               DT::DTOutput(outputId = ns("test1_table")),
               DT::DTOutput(outputId = ns("test2_table"))
             )
           )
        )
      ) #-- tabBox
    ) #-- fluidRow

  )
}

#' report_22 Server Functions
#' @import R6
#' @import data.table
#' @importFrom lubridate month year today %--% %within%
#' @noRd
mod_report_22_server <- function(id, aws_buffer){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #-- 1. read in data
    #-- 1.1 for not it is Rds data file; will be replaced by aws table
    dt <- readRDS("data/z3_dt.Rds")
    dt[, `:=` (country_orig = country)] # be careful, to do this operation just once
    aws_buffer$daily_profiles <- dt

    #-- 2. data subset
    #-- 2.1 dates, websites
    dt_subset <- reactive({
      websites_selected <- input$websites_selected
      month_selected <- input$year_month_selected %>% lubridate::month(label = TRUE, abbr = TRUE)
      year_selected  <- input$year_month_selected %>% lubridate::year()

      #-- add week number to z3_dt, which is an agregate already <- !!
      dt <- aws_buffer$daily_profiles
      return(dt)
    })
    output$dt_subset <- DT::renderDT({dt_subset()})

    #-- 2.1.1 based on selected dates, websites, get the countries from this dataset
    dt_countries_subset <- reactive({
      countries_selected <- input$countries_selected
      dt <- dt_subset()[country_orig %in% countries_selected, ]

      return(dt)
    })

    #-- 2.1.2 update list of countries available for selection
    observeEvent(eventExpr = input$websites_selected,
                 handlerExpr = {
                   countries_sorted <- dt_subset()[, .(transactions = sum(transactions)), country_orig][transactions > 0][order(-transactions)][, country_orig]
                   countries_top5 <- countries_sorted[1:5]
                   countries_top5_flat <- countries_top5 %>% stringr::str_c() %>% paste(collapse = ", ")
                   output$countries_top5 <- shiny::renderText(glue::glue("Countries, top5: {countries_top5_flat}"))

                   countries_count <- countries_sorted %>% length()
                   countries_6_N <- countries_sorted[6:countries_count]
                   countries_6_N <- countries_6_N[stringr::str_order(countries_6_N)]
                   shinyWidgets::updatePickerInput(session = session, inputId = "countries_selected",
                                     choices = list(top5_by_transactions     = countries_top5,
                                                    the_rest_alphabetically  = countries_6_N),
                                     selected = input$countries_selected)

                 }
    )

    #-- 2.2 country aggregation level
    dt_subset_countries_aggr <- reactive({

      #-- 2.2.1 refactor and aggregate countries, depending on user input
      aggregation_level <- input$aggregation_level
      dt <- dt_countries_subset() %>% aggregate_countries(aggregation_level)
      return(dt)
    })
    output$dt_subset_countries_aggr <- DT::renderDT({dt_subset_countries_aggr()})


    #-- 3. charting
    plot_profiles <- function(){

      selected_devices <- input$selected_devices
      dt <- dt_subset_countries_aggr()[deviceCategory %in% selected_devices, ]

      metric_options    <- forcats::as_factor(c("sessions", "transactions"))
      time_options      <- forcats::as_factor(c("user", "serv"))

      sessions_plot     <- dt %>% produce_daily_profile_plot(metric_selection = metric_options[1], time_selection = time_options[1])
      transactions_plot <- dt %>% produce_daily_profile_plot(metric_selection = metric_options[2], time_selection = time_options[1])
      sessions_serv_plot<- dt %>% produce_daily_profile_plot(metric_selection = metric_options[1], time_selection = time_options[2])

      profiles_plots_list <- list(
        sessions_plot = sessions_plot,
        transactions_plot = transactions_plot,
        sessions_serv_plot = sessions_serv_plot)
      return(profiles_plots_list)
    }
    #-- 3.1 two main plots: sessions, transactions
    output$sessions_plot      <- renderPlot({plot_profiles()["sessions_plot"]})
    output$transactions_plot  <- renderPlot({plot_profiles()["transactions_plot"]})

    #-- 3.2 sessions, compring user and server time
    output$sessions_user_plot <- renderPlot({plot_profiles()["sessions_plot"]})
    output$sessions_serv_plot <- renderPlot({plot_profiles()["sessions_serv_plot"]})



    #-- lorem ipsum below
    output$plot_01 <- renderPlot({shinipsum::random_ggplot(type = "density")})
    #output$sessions_plot <- renderPlot({shinipsum::random_ggplot(type = "bar")})
    output$plot_03 <- renderPlot({shinipsum::random_ggplot(type = "bar")})

    output$table_1       <- DT::renderDT({shinipsum::random_DT(nrow = 5, ncol = 3, type = "numeric")})
    output$table_2       <- DT::renderDT({shinipsum::random_DT(nrow = 5, ncol = 3, type = "numeric")})
    output$table_3       <- DT::renderDT({shinipsum::random_DT(nrow = 5, ncol = 3, type = "numeric")})


  })
}

## To be copied in the UI
# mod_report_22_ui("report_22_ui_1")

## To be copied in the server
# mod_report_22_server("report_22_ui_1")
