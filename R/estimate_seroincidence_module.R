#' @title UI for Seroincidence Estimation
#' @importFrom shiny tabPanel sidebarLayout sidebarPanel mainPanel
#' @importFrom shiny uiOutput textOutput tableOutput helpText h4
#' @importFrom shiny NS
#'
#' @param id A string to identify a namespace
estimate_seroincidence_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Estimate Seroincidence",
    sidebarLayout(
      position = "left",
      sidebarPanel(
        width = 3,
        h4("Estimation Filters"),
        helpText("Provide parameters for filtering the seroincidence estimation:"),
        uiOutput(ns("antigen_type")),
        radioButtons(
          inputId = ns("choose_stratification"),
          label = "Choose Stratification Type:",
          choices = list("Overall" = "overall", "Stratified" = "stratified"),
          selected = "overall"
        ),
        uiOutput(ns("stratification_column")),
        uiOutput(ns("antigen_available")),
        textOutput(ns("status1")),
        textOutput(ns("result"))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Estimate Seroincidence",
            tableOutput(ns("est_incidence"))
          )
        )
      )
    )
  )
}

#' @title Server Logic for Seroincidence Estimation
#' @importFrom shiny moduleServer req renderTable showNotification observe
#' @importFrom dplyr %>%
#'
#' @param id A string to identify a namespace
#' @param pop_data Reactive expression for population data
#' @param curve_data Reactive expression for curve data
#' @param noise_data Reactive expression for noise data
estimate_seroincidence_server <- function(id,
                                          pop_data,
                                          curve_data,
                                          noise_data,
                                          imported_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ############################################################################
    # Choose antigen
    output$antigen_available <- renderUI({
      req(pop_data())
      checkboxGroupInput(
        inputId = ns("antigen_available"),
        label = "Choose Antigen",
        choices = unique(pop_data()$antigen_iso),
        selected = unique(pop_data()$antigen_iso)
      )
    })

    # Choose stratification
    output$stratification_column <- renderUI({
      req(input$choose_stratification, pop_data())
      if (input$choose_stratification == "overall") {
        return(NULL)
      } else if (input$choose_stratification == "stratified") {
        selectInput(
          ns("stratification_column"),
          label = "Choose Stratification",
          choices = isolate(pop_data()) %>%
            dplyr::select(-imported_data$selected_id()) %>%
            names()
        )
      }
    })

    ############################################################################

    pop_df <- reactive({
      req(pop_data())
      pop_data() %>%
        serocalculator::as_pop_data(
          age = imported_data$selected_age(),
          value = imported_data$selected_value(),
          id = imported_data$selected_id()
        )
    })

    curve_df <- reactive({
      req(curve_data())
      curve_data()
    })

    noise_df <- reactive({
      req(noise_data())
      noise_data()
    })

    ############################################################################

    output$est_incidence <- renderTable({
      req(input$choose_stratification, input$antigen_available)

      if (input$choose_stratification == "overall") {
        est <- serocalculator::est.incidence(
          pop_data = pop_df(),
          curve_params = curve_df(),
          noise_params = noise_df(),
          antigen_isos = input$antigen_available,
          verbose = TRUE
        )
      } else if (input$choose_stratification == "stratified") {
        req(input$stratification_column)
        est <- serocalculator::est.incidence.by(
          pop_data = pop_df(),
          curve_params = curve_df(),
          noise_params = noise_df(),
          verbose = TRUE,
          antigen_isos = input$antigen_available,
          strata = input$stratification_column
        )
      }
      summary(est)
    })
  })
}
