# Mon Jan 11 10:05:28 2021 ------------------------------
# https://stackoverflow.com/questions/62648298/using-shinys-updateselectinput-within-nested-modules

#-- mod_observationSelector.R ----
#

observationSelectorUI <- function(id) {
    ns <- NS(id)
    fluidPage(
        selectInput(
            inputId = ns("varTypes"),
            label = h3("Variable types"),
            choices = list("Integer" = TRUE,
                           "Real" = FALSE),
            selectize = FALSE,
            multiple = FALSE
        ),

        selectInput(
            inputId = ns("selectColumn"),
            label = h4("Selected Column"),
            choices = character(0)
        )
    )
}

observationSelectorServer <- function(id, data) {
    moduleServer(id,
                 function(input, output, session) {
                     observeEvent(eventExpr = input$varTypes,
                                  handlerExpr = {
                                      all_cols <- map_lgl(.x = mtcars, ~ all(. %% 1 == 0))
                                      selected_cols <-
                                          names(all_cols[all_cols == input$varTypes])
                                      updateSelectInput(
                                          session = session,
                                          inputId = "selectColumn",
                                          label = paste(
                                              "Selected",
                                              ifelse(input$varTypes, "integer", "real"),
                                              "columns"
                                          ),
                                          choices = selected_cols
                                      )
                                  })
                 })
}

#-- app.R ----
#
library("shiny")
library("tidyverse")


ui <- fluidPage(


    titlePanel("Nested Modules"),
    observationSelectorUI("colChooser")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    observationSelectorServer("colChooser")
}

# Run the application
shinyApp(ui = ui, server = server)


#-- corrected nesting

observationSelectorUI <- function(id) {
    ns <- NS(id)

    tagList(
        selectInput(
            inputId = ns("varTypes"),
            label = h3("Variable types"),
            choices = list("Integer" = TRUE,
                           "Real" = FALSE),
            selectize = FALSE,
            multiple = FALSE
        ),

        selectInput(
            inputId = ns("selectColumn"),
            label = h4("Selected Column"),
            choices = c("cyl", "hp", "vs", "am", "gear", "carb")
        )
    )
}

observationSelectorServer <- function(id, data) {
    moduleServer(id,
                 function(input, output, session) {
                     observeEvent(eventExpr = input$varTypes,
                                  handlerExpr = {
                                      all_cols <- map_lgl(.x = mtcars, ~ all(. %% 1 == 0))
                                      selected_cols <-
                                          names(all_cols[all_cols == input$varTypes])
                                      updateSelectInput(
                                          session = session,
                                          inputId = "selectColumn",
                                          label = paste(
                                              "Selected",
                                              ifelse(input$varTypes, "integer", "real"),
                                              "columns"
                                          ),
                                          choices = selected_cols
                                      )
                                  })

                     # Return the selection result
                     return(reactive({
                         validate(need(input$selectColumn, FALSE))
                         input$selectColumn
                     }))
                 })
}


previewUI <-     function(id) {

    ns <- NS(id)

    tabPanel("Summary table",
             column(4, observationSelectorUI(ns("colChooser"))),
             column(8, tableOutput(ns('headTable'))))
}

previewServer <- function(id) {
    moduleServer(id,
                 function(input, output, session) {

                     innerResult <- observationSelectorServer("colChooser")

                     output$headTable <- renderTable(head(mtcars[, innerResult()]))
                 })
}

