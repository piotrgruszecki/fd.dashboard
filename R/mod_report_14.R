#' report_14 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_report_14_ui <- function(id){
  ns <- NS(id)
  tagList(

    h2("Report 14: Investment Level"),

    # data subset & reference plot
    fluidRow(

      column(width = 4, shinydashboard::box(
        width = 12, title = "Data Subset", collapsible = TRUE, footer = "This box can be closed by minus sign above", status = "primary",
        shinyWidgets::airMonthpickerInput(inputId = ns("year_month_selected"), label = "Month to analyse:", value = lubridate::today(), minDate = "2020-10-01", maxDate = lubridate::today())
        #shiny::selectInput(inputId = ns("currencies_selected_x"), label = "Currencies (min investment):", choices = c("USD", "EUR", "GBP", "ZAR", "MXN", "MYR", "SGD"), selected = c("USD", "EUR", "GBP"), multiple = TRUE)
        )
      ),

      column(width = 8, shinydashboard::box(
        width = 12, title = "Overview chart as a reference", status = "primary", collapsible = TRUE, plotOutput(outputId = ns("plot_03")) %>% withSpinner(color = "#0dc5c1")
        )
      )

    ), #-- fluidRow

    # actual plots
    fluidRow(
      shinydashboard::tabBox(title = "Data analysis", id = "tabset1", width = 12, #height = "250px",
         shiny::tabPanel(title = "Min Inv",

              fluidRow(
                shinydashboard::box(width = 12,
                      column(width = 4, shiny::selectInput(inputId = ns("currencies_selected"), label = "Currencies (min investment):", choices = c("USD", "EUR", "GBP", "ZAR", "MXN", "MYR", "SGD"), selected = c("USD", "EUR", "GBP"), multiple = TRUE)),
                      column(width = 4, shiny::sliderInput(ns("min_investment_boundaries"), "Min/Max cuts [k]:", min = 0, max = 300, value = c(0, 150), step = 10, ticks = T)),
                      column(width = 4, shiny::sliderInput(ns("min_investment_bin"), "Bin width [k]:", min = 10, max = 60, value = c(20), step = 10, ticks = T)))
              ),
              fluidRow(shinydashboard::box(width = 12, title = "Investment level Over Time (A).",  plotOutput(outputId = ns("plot_02")))),
              fluidRow(shinydashboard::box(width = 12, title = "Investment level Snapshot (B)",  plotOutput(outputId = ns("plot_01")))),

              fluidRow(shinydashboard::box(width = 12, title = "Tabular presentation.", DT::DTOutput(outputId = ns("table_01"))))
         ), #-- tabPanel

         shiny::tabPanel(title = "& Leads",
                         h2("Under development")
                         )
      ) #-- tabBox
    ), #-- fluidRow

    fluidRow(
      shinydashboard::tabBox(title = tagList(shiny::icon("gear"), "Data quality"), id = "tabset2", width = 12, # height = "250px",
        shiny::tabPanel(title = "Outliers",
          fluidRow(shinydashboard::box(width = 8, title = "Number of outliers (TRUE) vs currency, status.", footer = "Outliers are above by the cut-off threshold.", DT::DTOutput(outputId = ns("min_investment_outliers"))))
        ), #-- tabPanel #2
        shiny::tabPanel(title = "Missing",
          fluidRow(shinydashboard::box(width = 8, title = "Missing data - no Minimum Investment level vs Sales Rep. Only Active Profiles.", DT::DTOutput(outputId = ns("missing_investment_table"))))
        ),
      # debug printouts
      shiny::tabPanel("Debug",
        fluidPage(
          fluidRow(
            shiny::verbatimTextOutput(outputId = ns("host")),
            shiny::verbatimTextOutput(outputId = ns("plot.color")),
            shiny::verbatimTextOutput(outputId = ns("key_prof_dt")),
            shiny::verbatimTextOutput(outputId = ns("key_min_investment_max_dt"))
          )
        )
      ) #-- tabPanel Debug
    )
  )
  )
}

#' report_14 Server Functions
#'
#' @noRd
mod_report_14_server <- function(id, aws_buffer){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #-- get all profiles, pulled from aws database
    prof_dt <- aws_buffer$profiles

    output$host <- shiny::renderText(glue::glue("Host: {config$host}"))
    output$plot.color <- shiny::renderText(glue::glue("Plot color: {config$plot.color}"))

    #-- outliers
    #-- identify outliers
    min_investment_max_default_dt <- data.table(min_investment_max      = c(10,    10,    10,    100,   100,   100,   100), # in millions
                                                min_investment_currency = c("USD", "EUR", "GBP", "ZAR", "MXN", "MYR", "SGD"))

    #-- threshold to define outliers
    min_investment_max_dt <- reactive({
      dt <- min_investment_max_default_dt[order(min_investment_max, min_investment_currency)]
      setkey(dt, min_investment_currency)
      return(dt)
    })
    #output$min_investment_threshold <- DT::renderDT({min_investment_max_dt()})

    #-- join tables, it also effectively cuts off missing currencies
    setkey(prof_dt, min_investment_currency)
    prof_min_set_dt <- reactive({

      #-- join
      # Thu Mar 11 06:53:57 2021 ------------------------------
      #-- debug
      prof_dt <- aws_buffer$profiles

      setkey(prof_dt, min_investment_currency)
      timestamp <- lubridate::now()
      output$key_prof_dt <- shiny::renderText(glue::glue("### {timestamp}, key of prof_dt: {key(prof_dt)} ###"))
      output$key_min_investment_max_dt <- shiny::renderText(glue::glue("### {timestamp}, key of min_investment_max_dt: {key(min_investment_max_dt())} ###"))
      # Thu Mar 11 06:54:06 2021 ------------------------------

      dt <- prof_dt[min_investment_max_dt()]

      #-- add column outlier
      dt[, `:=` (outlier = fcase(min_investment >  min_investment_max * 1e6, TRUE,
                                 min_investment <= min_investment_max * 1e6, FALSE))]
      dt[, `:=` (min_investment = min_investment / 1e3)]

      #-- take the most recent
      setkey(dt, tech_date_start)
      dt <- dt[, .SD[.N], .(profile_id)][, .N, .(profile_id, min_investment, min_investment_currency, status, outlier, min_investment_max)]

      return(dt)
    })

    #-- number of outliers, per currency and status
    outliers_table <- reactive({

      table_0 <- prof_min_set_dt()[, .N, .(outlier, min_investment_currency, status, min_investment_max)]
      table <- table_0[, dcast.data.table(.SD, min_investment_currency + min_investment_max ~ status + outlier)][order(-Active_FALSE)]
      setnames(table, c("min_investment_currency", "min_investment_max"), c("Currency", "Threshold [mil]"))

      colnames_2_sum <- colnames(table) %>% str_subset(pattern = "_")
      table_sum <- table_0[, dcast.data.table(.SD, min_investment_currency + min_investment_max ~ status + outlier, fill = 0)][order(-Active_FALSE)][, lapply(.SD, sum), .SDcols = colnames_2_sum][, `:=` (Currency = "Total", "Threshold [mil]" = "---")]
      table_fin <- data.table::rbindlist(list(table, table_sum), use.names = T)

      return(table_fin)
    })
    output$min_investment_outliers <- DT::renderDT({outliers_table()})

    #-- undefined min_investment
    missing_inv_table <- reactive({

      dt <- prof_dt
      setkey(dt, tech_date_start)

      dt <- dt[, .SD[.N], .(profile_id)][status == "Active", .N, .(m_inv = is.na(min_investment), m_inv_c = is.na(min_investment_currency), sales_rep)]

      dt <- dt[, `:=` (missing_min_inv = fcase(
        (m_inv == T | m_inv_c == T), TRUE,
        (m_inv == F & m_inv_c == F), FALSE))][, .(sales_rep, missing_min_inv, N)][, dcast.data.table(.SD, sales_rep ~ missing_min_inv, fill = 0)]

      setnames(dt, c("FALSE", "TRUE"), c("OK", "missing"))

      dt_sum <- dt[, lapply(.SD, sum), .SDcols = is.numeric][, `:=` (sales_rep = "Total")]

      table_04_fin <- data.table::rbindlist(list(dt, dt_sum), use.names = T)
      return(table_04_fin)
    })
    output$missing_investment_table <- DT::renderDT({missing_inv_table()})

    #-- minimum investment per profile, daily
    prof_inv_daily_dt <- reactive({

      #-- start with full dataset
      dt <- prof_dt

      #-- new variable, for clarity
      dt[, `:=` (min_investment_k = min_investment / 1e3)]

      analysis_period_start <- input$year_month_selected
      analysis_period_end  <- ifelse(lubridate::floor_date(lubridate::today(), unit = "month") == input$year_month_selected[1], # condition
                                     lubridate::today(), # TRUE
                                     lubridate::ceiling_date(input$year_month_selected[1], unit = "months") - lubridate::days(1)) %>% as.Date(origin = "1970-01-01") # FALSE
      analysis_period <- analysis_period_start %--% analysis_period_end

      res_dt <- get_profiles_investment_daily(dt = dt, analysis_period = analysis_period)
      res_dt[, `:=` (n = 1)]

      return(res_dt)
    })

    #-- assign bins, separately on each day
    prof_inv_daily_binned_dt <- reactive({

      #-- start with daily numbers of investment level, per profile
      dt <- prof_inv_daily_dt()

      bin_from = input$min_investment_boundaries[1]
      bin_to   = input$min_investment_boundaries[2]
      bin_by   = input$min_investment_bin
      currencies = input$currencies_selected

      dt <- dt[min_investment_currency %in% currencies, ][, `:=` (bins = cut(min_investment_k, breaks = c(-Inf, seq.int(from = bin_from, to = bin_to, by = bin_by), Inf), include.lowest = TRUE, ordered_result = TRUE)), by = .(min_investment_currency, date)]

      dt[, `:=` (min_investment_currency = fct_reorder(.f = min_investment_currency, .x = n, .fun = sum) %>% fct_rev())]

      return(dt)
    })

    #-- charting
    plot_min_investment <- function(){

      dt <- prof_min_set_dt()[!(outlier) & status == "Active", ]

      bin_from = input$min_investment_boundaries[1]
      bin_to   = input$min_investment_boundaries[2]
      bin_by   = input$min_investment_bin
      currencies = input$currencies_selected
      #currencies <- min_investment_max_dt()[, min_investment_currency]

      dt_2 <- dt[, `:=` (n = 1)][min_investment_currency %in% currencies, ][, `:=` (min_investment_currency = as.factor(min_investment_currency) %>% fct_reorder(.f = min_investment_currency, .x = n, .fun = sum) %>% fct_rev())][]

      dt_2[, `:=` (bins = cut(min_investment, breaks = c(-Inf, seq.int(from = bin_from, to = bin_to, by = bin_by), Inf), include.lowest = TRUE, ordered_result = TRUE)), by = .(min_investment_currency)]

      dt_3 <- dt_2[, .N, .(bins, min_investment_currency)][order(bins)]

      plot_01 <- dt_3 %>%
        ggplot(aes(bins, N, color = bins, fill = bins)) +
        geom_col(alpha = 0.4) +
        scale_color_viridis_d(option = "D", direction = -1) +
        scale_fill_viridis_d(option = "D", direction = -1) +
        theme_bw() +
        facet_wrap("min_investment_currency", scales = "free_y") +
        labs(title = "Number of Profiles, binned, per currency.", subtitle = stringr::str_glue("Only Active Profiles. Snapshot for today: {lubridate::today()}"), x = "", y = "") +
        theme(legend.position = "bottom") +
        guides(colour = guide_legend(nrow = 1))

      dt_4     <- dt_3[, dcast.data.table(.SD, bins ~ min_investment_currency, value.var = "N")]
      dt_4_sum <- dt_3[, dcast.data.table(.SD, bins ~ min_investment_currency, value.var = "N", fill = 0)][, purrr::map(.SD, sum), .SDcols = is.numeric][, `:=` (bins = "Total")]
      table_01 <- data.table::rbindlist(list(dt_4, dt_4_sum), use.names = T)

      #-- as a time series for the analysis period
      plot_02 <- prof_inv_daily_binned_dt()[!is.na(min_investment_currency) & min_investment_currency %in% currencies & status == "Active", .N, .(date, bins, min_investment_currency)] %>%
        ggplot(aes(date, N, color = bins, fill = bins)) +
        geom_area(alpha = 0.5) +
        scale_color_viridis_d(option = "D", direction = -1) +
        scale_fill_viridis_d(option = "D", direction = -1) +
        facet_wrap("min_investment_currency", scales = "free_y") +
        theme_bw() +
        labs(title = "Number of Profiles, binned daily, per currency.", subtitle = stringr::str_glue("Only Active Profiles."), x = "", y = "") +
        theme(legend.position = "bottom") +
        guides(colour = guide_legend(nrow = 1))


      plot_03 <- prof_inv_daily_binned_dt()[date == max(date) & !is.na(min_investment_currency) & min_investment_k <= 100 & status == "Active", .(curr = min_investment_currency, min_investment_k)] %>%
        ggpubr::ggdensity(x = "min_investment_k", y = "..density..", color = "curr", fill = "curr", alpha = 0.3, add = "median", title = "Relative density, all profiles in currency = 1.") +
        ggpubr::theme_classic2()


      plot_min_investment_list <- list(plot_01 = plot_01, plot_02 = plot_02, plot_03 = plot_03, table_01 = table_01)
      return(plot_min_investment_list)
    }
    output$plot_01  <- renderPlot({plot_min_investment()["plot_01"]})
    output$plot_02  <- renderPlot({plot_min_investment()["plot_02"]})
    output$plot_03  <- renderPlot({plot_min_investment()["plot_03"]})
    output$table_01 <- DT::renderDT({plot_min_investment()$table_01})

    #-- plot changes over time




    #output$plot_03       <- renderPlot({shinipsum::random_ggplot(type = "density")})
    #output$plot_02       <- renderPlot({shinipsum::random_ggplot(type = "bar")})

    output$table_1       <- DT::renderDT({shinipsum::random_DT(nrow = 5, ncol = 3, type = "numeric")})
    output$table_2       <- DT::renderDT({shinipsum::random_DT(nrow = 5, ncol = 3, type = "numeric")})
    output$dt_missing    <- DT::renderDT({shinipsum::random_DT(nrow = 5, ncol = 3, type = "numeric")})

  })
}

## To be copied in the UI
# mod_report_14_ui("report_14_ui_1")

## To be copied in the server
# mod_report_14_server("report_14_ui_1")
