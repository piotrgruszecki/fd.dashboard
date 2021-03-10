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
          shinydashboard::menuItem("14: Minimum Investment", tabName = "minimum_investment_level", icon = icon("globe-americas")),
          shinydashboard::menuItem("15: Daily Active Profiles", tabName = "daily_active_profiles", icon = icon("globe-americas")),
          shinydashboard::menuItem("21: Industries & Categories", tabName = "industries_categories", icon = icon("globe-americas")),
          shinydashboard::menuItem("N: Other Report",    tabName = "widgets",         icon = icon("dashboard"))
        )
      ),

      #-- 1.3 dashboard main body
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = "lead_status",              mod_report_3_ui("report_3_ui_1")),
          shinydashboard::tabItem(tabName = "daily_lead_flow",          mod_report_9_ui("report_9_ui_1")),
          shinydashboard::tabItem(tabName = "minimum_investment_level", mod_report_14_ui("report_14_ui_1")),
          shinydashboard::tabItem(tabName = "daily_active_profiles",    mod_report_15_ui("report_15_ui_1")),
          shinydashboard::tabItem(tabName = "industries_categories",    mod_report_21_ui("report_21_ui_1")),
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

