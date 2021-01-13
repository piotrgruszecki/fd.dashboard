#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import R6
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboard::dashboardPage(

      #-- 1.1 dashboard header
      shinydashboard::dashboardHeader(title = "FD dashboard"),

      #-- 1.2 dashboard sidebar
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("3: Lead Status",     tabName = "lead_status",     icon = icon("globe-americas")),
          shinydashboard::menuItem("9: Daily Lead Flow", tabName = "daily_lead_flow", icon = icon("globe-americas")),
          shinydashboard::menuItem("N: Other Report",    tabName = "widgets",         icon = icon("dashboard"))
        )
      ),

      #-- 1.3 dashboard main body
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = "lead_status",     mod_report_3_ui("report_3_ui_1")),
          shinydashboard::tabItem(tabName = "daily_lead_flow", mod_report_9_ui("report_9_ui_1")),
          shinydashboard::tabItem(tabName = "widgets", h2("Widgets tab content"))
        )
      )

    )

    # fluidPage(
    #   h1("fd.dashboard"),
    #
    #   shiny::verbatimTextOutput(outputId = "config_host"),
    #   shiny::verbatimTextOutput(outputId = "con_prt"),
    #   shiny::verbatimTextOutput(outputId = "leads_n_rows"),
    #
    #   shiny::dataTableOutput(outputId = "leads_summary_table"),
    #
    #   #-- modules below
    #   mod_report_3_ui("report_3_ui_1")
    #
    # )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'fd.dashboard'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

