#' @title ui for displaying to seroincidence estimation
#'
#' @importFrom shiny tabPanel
#' @importFrom shiny uiOutput
#' @importFrom shiny helpText
#' @importFrom shiny observeEvent
#' @importFrom shiny observe
#' @importFrom serocalculator load_noise_params
#' @importFrom shiny req
#' @importFrom shiny renderUI
#' @importFrom shiny selectInput
#' @importFrom dplyr select
#' @importFrom dplyr where
#' @importFrom shiny h4
#' @importFrom shiny renderTable
#' @importFrom shiny sidebarLayout
#' @importFrom dplyr %>%
#' @importFrom shiny NS
#' @importFrom shiny radioButtons
#' @importFrom shiny sidebarPanel
#' @importFrom shiny textOutput
#' @importFrom shiny mainPanel
#' @importFrom shiny tabsetPanel
#' @importFrom shiny tableOutput
#'
#' @param id a `string` to identify a namespace
estimate_seroincidence_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Estimate Seroincidence",
    sidebarLayout(
      position = "left",
      sidebarPanel(
        width = 3,

        # title
        h4("Estimation Filters"),

        # description
        helpText("Provide the parameters
                        for filtering estimation of seroincidence"),

        # choose antigen_iso type
        uiOutput(ns("antigen_type")),
        #
        #         # choose stratification type
        #         radioButtons(
        #           inputId = ns("stratification_type"),
        #           label = "Choose Stratification Type:",
        #           choices = list("Overall" = "overall", "Stratified" = "stratified"),
        #           selected = "overall"
        #         ),

        # choose stratification column
        uiOutput(ns("stratify_by")),

        # status text
        textOutput(ns("status1")),

        # display computation results
        textOutput(ns("result"))
      ),
      mainPanel(
        "",
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

#' @title server-side computation of seroincidence estimation
#' @importFrom shiny moduleServer
#' @importFrom dplyr %>%
#' @importFrom shiny NS
#'
#' @param id identify a namespace
#' @param pop_data an object of population data
#' @param curve_data an object of curve data with an extra antigen column
#' @param noise_data an object of noise data
#' @param antigen_iso  a vector of antigen isotype
estimate_seroincidence_server <- function(id, pop_data, curve_data, noise_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Debugging: Observe the input data
    observe({
      print("Pop Data:")
      print(pop_data())
      print("Curve Data:")
      print(curve_data())
      print("Noise Data:")
      print(noise_data())
    })

    output$est_incidence <- renderTable({
      # Validate all inputs
      req(
        pop_data(),
        curve_data(),
        noise_data(),
        cancelOutput = TRUE  # Stop rendering if any data is missing
      )

      # Check if any dataset is empty
      validate(
        need(nrow(pop_data()) > 0, "Pop Data is empty."),
        need(nrow(curve_data()) > 0, "Curve Data is empty."),
        need(nrow(noise_data()) > 0, "Noise Data is empty.")
      )

      # Try to estimate seroincidence
      tryCatch(
        {
          result <- serocalculator:::est.incidence(
            pop_data = isolate(pop_data()),
            curve_params = isolate(curve_data()),
            noise_params = isolate(noise_data()),
            antigen_isos = isolate(pop_data())$antigen_iso %>% unique(),
            verbose = TRUE
          )

          # Notify the user of success
          showNotification("Seroincidence estimated successfully.", type = "message")

          # Return a summary of the results
          summary(result)
        },
        error = function(e) {
          # Notify the user of an error
          showNotification(paste("Error estimating seroincidence:", e$message), type = "error")

          # Return the error message as a data frame
          data.frame(Error = e$message)
        }
      )
    })
  })
}
