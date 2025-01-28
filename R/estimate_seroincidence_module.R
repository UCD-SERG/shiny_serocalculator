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
                                          imported_data = imported_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    ############################################################################

    # Reactive object to filter population data
    pop_df <- reactive({
      subset(pop_data(), pop_data()$antigen_iso == "HlyE_IgA" & pop_data()$Country == "Pakistan") %>%
        serocalculator::as_pop_data(
          age = imported_data$selected_age(),
          value = imported_data$selected_value(),
          id = imported_data$selected_id()
        )
    })

    curve_df <- reactive({
      subset(curve_data(), curve_data()$antigen_iso == "HlyE_IgA")
    })

    noise_df <- reactive({
      subset(noise_data(), noise_data()$antigen_iso == "HlyE_IgA" & noise_data()$Country == "Pakistan")
    })

    output$est_incidence <- renderTable({
      if (input$choose_stratification == "overall") {
        # Overall seroincidence estimation
        est <- serocalculator::est.incidence(
          pop_data = pop_df(),
          curve_params = curve_df(),
          noise_params = noise_df(),
          verbose = TRUE
        )
      } else if (input$choose_stratification == "stratified") {
        # Stratified seroincidence estimation
        est <- serocalculator::est.incidence.by(
          pop_data = pop_df(),
          curve_params = curve_df(),
          noise_params = noise_df(),
          verbose = TRUE,
          strata = "cluster" # replace with input
        )
      }
    })
  })
}
