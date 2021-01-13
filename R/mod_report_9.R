#' report_9 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom lubridate month year today %--% %within%
#' @importFrom shinycssloaders withSpinner
#' @import R6
mod_report_9_ui <- function(id){
  ns <- NS(id)
  tagList(

    #-- 1.0 UI interface
    h2("Report 9: Daily Lead Flow"),

    #-- 1.3.1.1 1st row - controls and overall chart
    fluidRow(
      column(width = 4,
             shinydashboard::box(width = 12, title = "Data Subset", collapsible = TRUE, footer = "This box can be closed by minus sign above", status = "primary",
                 shinyWidgets::airMonthpickerInput(inputId = ns("year_month_selected"), label = "Month to analyse:", value = "2020-12-01", minDate = "2020-10-01", maxDate = lubridate::today()),
                 shiny::selectInput(inputId = ns("countries_selected"), label = "Countries (sorted by volume in a given month):", choices = c("US", "GB"), selected = "US", multiple = TRUE)),
             shinydashboard::box(width = 12, title = "Charting Options", collapsible = TRUE, status = "primary", shiny::sliderInput(ns("rolling_window"), "Rolling Window [days]:", 1, 7, 3, step = 1, ticks = T))
      ),
      column(width = 8,
             shinydashboard::box(width = 12, title = "Overview chart as a reference", status = "primary", collapsible = TRUE, plotOutput(outputId = ns("plot_02")) %>% withSpinner(color = "#0dc5c1"))
      )
    ), #-- fluidRow

    #-- 1.3.1.2 2nd row - detailed chart
    fluidRow(
      shinydashboard::tabBox(title = "First tabBox", id = "tabset1", height = "250px", width = 12,
             shiny::tabPanel("Y-Y analysis",
                      fluidRow(shinydashboard::box(width = 6, plotOutput(outputId = ns("plot_04"))), shinydashboard::box(width = 6, plotOutput(outputId = ns("plot_05")))),
                      fluidRow(shinydashboard::box(width = 6, plotOutput(outputId = ns("plot_03"))), shinydashboard::box(width = 6, plotOutput(outputId = ns("plot_07"))))
             ),
             shiny::tabPanel("All Markets",
                      fluidRow(shinydashboard::box(width = 6, plotOutput(outputId = ns("plot_08"))), shinydashboard::box(width = 6, plotOutput(outputId = ns("plot_09")))),
                      fluidRow(shinydashboard::box(width = 12, plotOutput(outputId = ns("plot_10"))))
             ),
             shiny::tabPanel("Tabular",
                      fluidRow(shinydashboard::box(width = 4, shiny::sliderInput(inputId = ns("month_bakwards"), "Current Month + Months backwards:", 1, 6, 3, step = 1, ticks = T))),
                      fluidRow(shinydashboard::box(width = 6, title = "Number of leads per month",  DT::DTOutput(outputId = ns("leads_sum"))), shinydashboard::box(width = 6, title = "Daily average number of leads", DT::DTOutput(outputId = ns("leads_avg")))),
                      fluidRow(shinydashboard::box(width = 6, title = "Number of leads per month, % change", DT::DTOutput(outputId = ns("leads_sum_pct"))), shinydashboard::box(width = 6, title = "Daily average number of leads, % change", DT::DTOutput(outputId = ns("leads_avg_pct"))))
             ),
             shiny::tabPanel("Debug",
                      fluidPage(
                        fluidRow(
                          shiny::verbatimTextOutput(outputId = ns("countries_selected")),
                          shiny::verbatimTextOutput(outputId = ns("month_selected")),
                          #shiny::verbatimTextOutput(outputId = ns("line_thickness")),
                          shiny::verbatimTextOutput(outputId = ns("line_thickness_config")),
                          shiny::verbatimTextOutput(outputId = ns("plot.color")),
                          shiny::verbatimTextOutput(outputId = ns("host")),
                          shiny::verbatimTextOutput(outputId = ns("websites_sorted"))
                        ),
                        DT::DTOutput(outputId = ns("dt_subset")),
                        DT::DTOutput(outputId = ns("dt_subset_rollmean")),
                        DT::DTOutput(outputId = ns("dt_equivalent_points")),
                        DT::DTOutput(outputId = ns("dt_subset_all_time")),
                        DT::DTOutput(outputId = ns("dt_subset_all_time_countries")),
                        DT::DTOutput(outputId = ns("dt_aggregated"))
                      ))
      )
    ) #-- fluidRow
  )
}

#' report_9 Server Functions
#'
#' @noRd
#'
#' @import data.table
#' @import R6
#' @importFrom lubridate %--% %within%
#' @importFrom glue glue
#'
mod_report_9_server <- function(id, aws_buffer){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    dt <- aws_buffer$leads
    charting_period <- aws_buffer$leads_dates_range[1] %--% aws_buffer$leads_dates_range[2]

    #-- 0.4.3 layout definitions
    plot.color       <- config$plot_color
    plot.color.light <- config$plot_light_color
    line_thickness   <- config$line_thickness
    #line_thickness   <- 1
    #output$line_thickness <- shiny::renderText(glue::glue("Line Thikness: {line_thickness}"))
    output$line_thickness_config <- shiny::renderText(glue::glue("Line Thikness Config: {config$line_thickness}"))
    output$plot.color <- shiny::renderText(glue::glue("Plot Color Config: {config$plot.color}"))
    output$host <- shiny::renderText(glue::glue("Host Config: {config$host}"))

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
    dt_subset <- reactive({
      countries_selected <- input$countries_selected
      countries_selected_str <- countries_selected %>% stringr::str_c() %>% paste(collapse = ", ")
      output$countries_selected <- shiny::renderText(glue::glue("Countries Selected: {countries_selected_str}"))

      month_selected <- input$year_month_selected %>% lubridate::month(label = TRUE, abbr = TRUE)
      output$month_selected <- shiny::renderText(glue::glue("Month Selected: {month_selected}"))

      dt <- dt[website_iso2c %in% countries_selected & month == month_selected & Date %within% charting_period, .(leads = .N), .(Date_y, year)]
      return(dt)
    })
    #output$dt_subset <- DT::renderDT({dt_subset()})

    dt_subset_test <- reactive({
      countries_selected <- input$countries_selected
      countries_selected <- c("US", "UK")
      month_selected <- input$year_month_selected %>% lubridate::month(label = TRUE, abbr = TRUE)
      month_selected <- c("Dec")

      dt = data.table::copy(dt[website_iso2c %in% countries_selected & Date %within% charting_period & month == month_selected & year == 2020, .(leads = .N), .(Date, year, Country, Country_original, website_iso2c)])
      return(dt)
    })
    output$dt_subset <- DT::renderDT({dt_subset_test()})

    #-- 2.1.2 include rollmean
    dt_subset_rollmean <- reactive({
      rolling_window <- input$rolling_window
      dt <- dt_subset()[, .(Date_y, leads_rm = frollmean(leads, n = rolling_window, align = "right")), .(year)]
      return(dt)
    })
    output$dt_subset_rollmean <- DT::renderDT({dt_subset_rollmean()})


    #-- 2.1.3 find points, when cumsum of leads equals
    dt_equivalent_points <- reactive({

      x <- dt_subset()[, `:=` (leads_cs = cumsum(leads)), keyby = .(year)]

      year_with_less_leads <- x[, .(leads_sum = max(leads_cs)), .(year)][leads_sum == min(leads_sum), year]
      year_with_more_leads <- x[, .(leads_sum = max(leads_cs)), .(year)][leads_sum == max(leads_sum), year]

      #-- for lower leads, it is alway the end of the line, so it is straightforward
      lower_leads_point <- x[year == year_with_less_leads, ][leads_cs == max(leads_cs), ]

      #-- for the higher leads, we find the date, the first day, when exceeds the max of lower leads
      x_2019 <- x[year == 2019] %>% setkey(leads_cs)
      x_2020 <- x[year == 2020] %>% setkey(leads_cs)
      d_2019_intercept <- x_2019[x_2020, roll = -Inf][.N, .(Date_y)][, Date_y ]
      d_2020_intercept <- x_2020[x_2019, roll = -Inf][.N, .(Date_y)][, Date_y]

      #-- for higher leads, firnd the first day, with a prevailing number of leads_cs
      higher_leads_point <- rbindlist(list(x[Date_y %in% d_2020_intercept & year == year_with_more_leads],
                                           x[Date_y %in% d_2019_intercept & year == year_with_more_leads]))

      y <- rbindlist(list(lower_leads_point, higher_leads_point))
      return(y)
    })
    output$dt_equivalent_points <- DT::renderDT({dt_equivalent_points()})

    #-- 2.1.4 all time, but selecting countries
    dt_subset_all_time <- reactive({
      countries_selected <- input$countries_selected
      dt <- dt[website_iso2c %in% countries_selected , .(leads = .N), keyby = .(Date_y, year, month)]
      return(dt)
    })
    output$dt_subset_all_time <- DT::renderDT({dt_subset_all_time()})

    #-- 2.1.5 all time, selected countries as a separate var
    dt_subset_all_time_countries <- reactive({
      countries_selected <- input$countries_selected
      dt <- dt[website_iso2c %in% countries_selected , .(leads = sum(.N)), keyby = .(year, month, country = website_iso2c)]
      return(dt)
    })
    output$dt_subset_all_time_countries <- DT::renderDT({dt_subset_all_time_countries()})

    #-- 2.1.6 all time, aggregated
    dt_aggregated <- reactive({
      dt <- dt[, .(leads_daily = .N), .(website_iso2c, month, year, Date)][, .(leads_avg = mean(leads_daily) %>% round(), leads_sum = sum(leads_daily) ), .(website_iso2c, year, month)]
      return(dt)
    })
    output$dt_aggregated <- DT::renderDT({dt_aggregated()})


    #-- 2.2 set of charts charts
    #-- 2.2.1 overview plot and possibly others, where we only need data subset
    plots_basic_list <- function(){
      month_selected <- input$year_month_selected %>% lubridate::month(label = TRUE, abbr = FALSE)
      points_equivalent <- dt_equivalent_points()

      countries_selected <- input$countries_selected
      selected_countries_string <- str_flatten(countries_selected, collapse = ", ")

      year_with_less_leads <- points_equivalent[leads_cs == min(leads_cs), year]

      # dt[, .(leads = sum(leads)), .(Date, year)] %>%
      #   ggplot(aes(Date, leads, color = year)) +
      #   geom_line(size = line_thickness) + theme_bw()

      #-- 2.2.1.1 number of leads, basic plot
      plot_02 <- dt_subset()[, .(leads = sum(leads)), .(Date_y, year)] %>%
        ggplot(aes(Date_y, leads, color = year)) +
        geom_line(size = line_thickness) + theme_bw() +
        scale_color_brewer(palette = "Set1", direction = -1) + theme(legend.position = "bottom") +
        labs(title = glue("Number of Leads in {month_selected}"), subtitle = glue("Daily numbers. Geos: {selected_countries_string}"), y = "", x = "")

      #--2.2.1.2 cumulative numbers,
      plot_03 <- dt_subset()[, `:=` (leads_cs = cumsum(leads)), .(year)] %>%
        ggplot(aes(Date_y, leads_cs, color = year)) +
        geom_line(size = line_thickness) + theme_bw() + scale_color_brewer(palette = "Set1", direction = -1) + theme(legend.position = "bottom") +
        labs(title = glue("Number of Leads in {month_selected}"), subtitle = "Cumulative numbers", y = "", x = "") +

        geom_point(data = points_equivalent, aes(x = Date_y, y = leads_cs, color = year)) +
        geom_point(data = points_equivalent, aes(x = Date_y, y = leads_cs, color = year), size = 6, shape = 21) +
        geom_vline(data = points_equivalent, aes(xintercept = Date_y), color = "grey", linetype = "dotted") +
        geom_hline(data = points_equivalent[year == year_with_less_leads], aes(yintercept = leads_cs), color = "grey", linetype = "dotted")


      plots_basic_list <- list(plot_02 = plot_02, plot_03 = plot_03)
      return(plots_basic_list)
    }
    output$plot_02 <- renderPlot({plots_basic_list()["plot_02"]})
    output$plot_03 <- renderPlot({plots_basic_list()["plot_03"]})

    #-- 2.2.2 plots, where we have both dataset and rolling window changing
    plots_list <- function(){
      month_selected <- input$year_month_selected %>% lubridate::month(label = TRUE, abbr = FALSE)
      rolling_window <- input$rolling_window

      #-- 2.2.2.1 daily numbers, rollmean = 1...7
      plot_04 <- dt_subset_rollmean() %>%
        ggplot(aes(Date_y, leads_rm, color = year)) +
        geom_line(size = line_thickness) + theme_bw() + scale_color_brewer(palette = "Set1", direction = -1) + theme(legend.position = "bottom") +
        labs(title = glue("Number of Leads in {month_selected}"), subtitle = glue("Daily. Rolling average, with a {rolling_window}-days window"), y = "", x = "")

      #-- 2.2.2.2 daily numbers, rollmean = 1...7, ratio vs previous period
      plot_05 <- dt_subset_rollmean()[, .(ratio_pct = leads_rm[2]/leads_rm[1], leads_20 = leads_rm[2], leads_19 = leads_rm[1]), .(Date_y)] %>%
        ggplot(aes(Date_y, ratio_pct, color = plot.color)) + geom_line(size = line_thickness) + theme_bw() + geom_hline(yintercept = 1, color = "grey", linetype = "dashed") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + scale_color_brewer(palette = "Set1") + theme(legend.position = "none") +
        labs(title = glue("Number of Leads in {month_selected} as a % of previous period"),
             subtitle = glue("Daily. Rolling average, with a {rolling_window}-days window"), y = "", x = "")

      #-- 2.2.2.3 cumulative numbers, ratio
      plot_07 <- dt_subset()[, `:=` (leads_cs = cumsum(leads)), keyby = .(year)
      ][, .(Date_y, leads_rm = frollmean(leads_cs, n = rolling_window, align = "right")), keyby = .(year)
      ][, .(ratio_pct = leads_rm[2]/leads_rm[1], leads_20 = leads_rm[2], leads_19 = leads_rm[1]), .(Date_y)] %>%
        ggplot(aes(Date_y, ratio_pct, color = plot.color)) +
        geom_line(size = line_thickness) +
        theme_bw() +
        geom_hline(yintercept = 1, color = "grey", linetype = "dashed") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        scale_color_brewer(palette = "Set1") +
        theme(legend.position = "none") +
        labs(title = glue("Number of Leads in {month_selected} as a % of previous period"), subtitle = glue("Cumulative numbers. Rolling average, with a {rolling_window}-days window"),
             y = "", x = "")


      plots_list <- list(plot_04 = plot_04, plot_05 = plot_05, plot_07 = plot_07)
      return(plots_list)
    }
    output$plot_04 <- renderPlot({plots_list()["plot_04"]})
    output$plot_05 <- renderPlot({plots_list()["plot_05"]})
    output$plot_07 <- renderPlot({plots_list()["plot_07"]})

    #-- 2.2.3 plots, where we have all time data, but selected countries
    plots_all_time <- function(){
      countries_selected <- input$countries_selected
      selected_countries_string <- str_flatten(countries_selected, collapse = ", ")

      #-- 2.2.3.1 dots and boxplot
      plot_08 <- dt_subset_all_time() %>%
        ggplot(aes(month, leads, color = year)) +
        geom_point(position = "jitter", alpha = 0.3) +
        geom_boxplot(alpha = 0.3, position = position_dodge(width = 0.6, preserve = "single")) +
        theme_bw() +
        scale_color_brewer(palette = "Set1", direction = -1) +
        geom_smooth(se = F) +
        theme(legend.position = "bottom") +
        labs(title = glue("Geos: {selected_countries_string}"), subtitle = "Daily Numbers of Leads",
             y = "", x = "")

      #-- 2.2.3.2 barplot
      plot_09 <- dt_subset_all_time()[, .(leads = sum(leads)), .(month, year)] %>%
        ggplot(aes(month, leads, color = year, fill = year)) +
        geom_col(alpha = 0.3, position = position_dodge(width = 0.6, preserve = "single")) +
        theme_bw() +
        scale_color_brewer(palette = "Set1", direction = -1) +
        scale_fill_brewer(palette = "Set1", direction = -1) +
        geom_smooth(se = F) +
        theme(legend.position = "bottom") +
        labs(title = glue("Geos: {selected_countries_string}"), subtitle = "Monthly Numbers of Leads",
             y = "", x = "")

      plots_all_time <- list(plot_08 = plot_08, plot_09 = plot_09)
      return(plots_all_time)
    }
    output$plot_08 <- renderPlot({plots_all_time()["plot_08"]})
    output$plot_09 <- renderPlot({plots_all_time()["plot_09"]})

    #-- 2.2.4 plots all time data, facets by countries
    plots_all_time_countries <- function(){
      countries_selected <- input$countries_selected
      selected_countries_string <- str_flatten(countries_selected, collapse = ", ")

      plot_10 <- dt_subset_all_time_countries() %>%
        ggplot(aes(month, leads, color = year, fill = year)) +
        geom_col(alpha = 0.3, position = position_dodge(width = 0.6, preserve = "single")) +
        theme_bw() +
        scale_color_brewer(palette = "Set1", direction = -1) +
        scale_fill_brewer(palette = "Set1", direction = -1) +
        geom_smooth(se = F) +
        theme(legend.position = "bottom") +
        labs(title = glue("Number of Leads in {selected_countries_string}"), subtitle = "Monthly numbers",
             y = "", x = "") +
        facet_wrap(facets = "country", scales = "free_y")

      plots_all_time_countries <- list(plot_10 = plot_10)
      return(plots_all_time_countries)
    }
    output$plot_10 <- renderPlot({plots_all_time_countries()["plot_10"]})

    #-- 2.3 tables
    tables_numeric <- function(){

      months_lookback <- input$month_bakwards + 1
      countries_selected <- input$countries_selected
      x2 <- dt_aggregated()

      #-- trim to countries subset
      x2 <- x2[website_iso2c %in% countries_selected]

      #-- leads monthly
      x3 <- x2[, dcast.data.table(.SD, year + month ~ website_iso2c, value.var = "leads_sum")][order(-year, -month)][1:months_lookback]
      months_to_show <- x3[, month]

      #-- leads daily avg
      x4 <- x2[, dcast.data.table(.SD, year + month ~ website_iso2c, value.var = "leads_avg")][order(-year, -month)][1:months_lookback]

      setkey(x2, year)
      current_month <- lubridate::month(lubridate::today(), label = TRUE)
      #-- pct change of monthly sum
      x5 <- x2[month %in% months_to_show, .(leads_sum_pct = 100 * round(leads_sum[2] / leads_sum[1], 2)), .(website_iso2c, month)][, dcast.data.table(.SD, month ~ website_iso2c)][month <= current_month][order(-month)]

      #-- pct change of daily averages
      x6 <- x2[month %in% months_to_show, .(leads_avg_pct = 100 * round(leads_avg[2] / leads_avg[1], 2)), .(website_iso2c, month)][, dcast.data.table(.SD, month ~ website_iso2c)][month <= current_month][order(-month)]

      tables_numeric <- list(x3 = x3, x4 = x4, x5 = x5, x6 = x6)

      return(tables_numeric)
    }
    output$leads_sum <- DT::renderDT({tables_numeric()$x3})
    output$leads_avg <- DT::renderDT({tables_numeric()$x4})
    output$leads_sum_pct <- DT::renderDT({tables_numeric()$x5})
    output$leads_avg_pct <- DT::renderDT({tables_numeric()$x6})


    #-- lorem ipsum below
    #output$plot_02       <- renderPlot({shinipsum::random_ggplot(type = "bar")})
    #output$plot_04       <- renderPlot({shinipsum::random_ggplot(type = "random")})
    #output$plot_05       <- renderPlot({shinipsum::random_ggplot(type = "random")})
    #output$plot_03       <- renderPlot({shinipsum::random_ggplot(type = "random")})
    #output$plot_07       <- renderPlot({shinipsum::random_ggplot(type = "random")})
    #output$plot_08       <- renderPlot({shinipsum::random_ggplot(type = "random")})
    #output$plot_09       <- renderPlot({shinipsum::random_ggplot(type = "random")})
    #output$plot_10       <- renderPlot({shinipsum::random_ggplot(type = "random")})

    # output$leads_sum     <- DT::renderDT({shinipsum::random_DT(nrow = 5, ncol = 3, type = "numeric")})
    # output$leads_avg     <- DT::renderDT({shinipsum::random_DT(nrow = 5, ncol = 3, type = "numeric")})
    # output$leads_sum_pct <- DT::renderDT({shinipsum::random_DT(nrow = 5, ncol = 3, type = "numeric")})
    # output$leads_avg_pct <- DT::renderDT({shinipsum::random_DT(nrow = 5, ncol = 3, type = "numeric")})

  })
}

## To be copied in the UI
# mod_report_9_ui("report_9_ui_1")

## To be copied in the server
# mod_report_9_server("report_9_ui_1")
