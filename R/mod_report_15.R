#' report_15 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_report_15_ui <- function(id){
  ns <- NS(id)
  tagList(

    h2("Report 15: Daily Active Profiles"),

    #data subsert and reference plot
    fluidRow(

        column(width = 4, shinydashboard::box(
          width = 12, title = "Data Subset", collapsible = TRUE, footer = "This box can be closed by minus sign above", status = "primary",
          shinyWidgets::airMonthpickerInput(inputId = ns("year_month_selected"), label = "Month to analyse:", value = (lubridate::today()), minDate = "2020-12-01", maxDate = lubridate::today()),
          )), #-- column

        column(width = 8, shinydashboard::box(
          width = 12, title = "Overview chart as a reference", status = "primary", collapsible = TRUE, plotOutput(outputId = ns("plot_01")) %>% withSpinner(color = "#0dc5c1"))
        ) #-- column
    ), #-- fluidRow

    fluidRow(

      shinydashboard::tabBox(title = "First tabBox", id = "tabset1", height = "250px", width = 12,
          shiny::tabPanel("Debug",
              fluidPage(
                  fluidRow(
                      shiny::verbatimTextOutput(outputId = ns("year_month_selected")),
                      shiny::verbatimTextOutput(outputId = ns("year_month_selected_1"))
                  )
              )
          )
      )
    )

  ) #-- tagList
}

#' report_15 Server Functions
#'
#' @noRd
mod_report_15_server <- function(id, aws_buffer){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    dt <- aws_buffer$profiles
#    default_analysis_period <- fd.dashboard::get_default_analysis_period()
#    profiles_daily_dt <- fd.dashboard::get_active_profiles_daily(dt, default_analysis_period)

    output$year_month_selected   <- shiny::renderText(glue::glue("Month Selected: {input$year_month_selected}"))
    output$year_month_selected_1 <- shiny::renderText(glue::glue("Month Selected: {input$year_month_selected[1]}"))


    profiles_daily_dt <- reactive({

      # Sat Jan 23 12:44:03 2021 ------------------------------
      # whatever is greater - today(), if current month, or end of month if previous month
      end_date <- ifelse(lubridate::floor_date(lubridate::today(), unit = "month") == input$year_month_selected[1],
                         lubridate::today(),
                         lubridate::ceiling_date(input$year_month_selected[1], unit = "months") - lubridate::days(1)) %>%
        as.Date(origin = "1970-01-01")

      analysis_period <- input$year_month_selected[1] %--% end_date
      profiles_daily_dt <- fd.dashboard::get_active_profiles_daily(dt, analysis_period)
      return(profiles_daily_dt)
    })

    #-- charting
    plots_basic_list <- function(){

      plot.color       <- config$plot_color
      line_thickness   <- config$line_thickness

      plot_01 <- profiles_daily_dt()[status == "Active", .(profiles = .N), .(date)] %>%
        ggplot(aes(date, profiles)) +
        geom_line(size = line_thickness, color = config$plot.color) +
        theme_bw() +
        scale_color_brewer(palette = "Set1", direction = 1) +
        theme(legend.position = "bottom") +
        labs(title = glue("Number of Active Profiles"), subtitle = glue("Daily numbers. All Websites"), y = "", x = "")

      plots_basic_list <- list(plot_01 = plot_01)
      return(plots_basic_list)
    }
    output$plot_01 <- renderPlot({plots_basic_list()["plot_01"]})


  })
}

## To be copied in the UI
# mod_report_15_ui("report_15_ui_1")

## To be copied in the server
# mod_report_15_server("report_15_ui_1")
