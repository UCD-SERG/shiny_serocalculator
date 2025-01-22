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
estimate_seroincidence_server <- function(id,
                                          pop_data,
                                          curve_data,
                                          noise_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      print(pop_data())
      print(curve_data())
      print(noise_data())
    })
    #
    #   # Render seroincidence results
    #   output$est_incidence <- renderTable({
    #     # Ensure required data is available
    #     # req(pop_data(), curve_data(), noise_data())
    #
    #     tryCatch(
    #       {
    #         # Estimate seroincidence
    #         result <- serocalculator::est.incidence(
    #           pop_data = pop_df(),
    #           curve_params = curve_df(),
    #           noise_params = noise_df(),
    #           verbose = TRUE
    #         )
    #
    #         # Return a summary of the results
    #         summary(result)
    #       },
    #       error = function(e) {
    #         # Return error message in a data frame format
    #         data.frame(Error = e$message)
    #       }
    #     )
    #   })
  })
}
