# Module UI
estimate_seroincidence_ui <- function(id) {
  ns <- NS(id)  # Namespace to ensure unique IDs

  tabPanel(
    "Estimate Seroincidence",
    sidebarLayout(
      position = "left",
      sidebarPanel(
        width = 3,

        # title
        h4("Estimation Filters"),

        # description
        helpText("Provide the parameters for filtering estimation of seroincidence"),

        # choose antigen_iso type
        uiOutput(ns("antigen_type")),

        # choose stratification type
        radioButtons(
          inputId = ns("stratification_type"),
          label = "Choose Stratification Type:",
          choices = list("Overall" = "overall", "Stratified" = "stratified"),
          selected = "overall"
        ),

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
          tabPanel("Estimate Seroincidence",
                   tableOutput(ns("est_incidence"))
          )
        )
      )
    )
  )
}


# Module Server
estimate_seroincidence_server <- function(id, pop_data, curve_data, noise_data) {
  moduleServer(id, function(input, output, session) {

    # Render UI for antigen_type (you can customize based on your data)
    output$antigen_type <- renderUI({
      selectInput(session$ns("antigen_iso"),
                  "Choose Antigen Isotype",
                  choices = unique(pop_data()$antigen_iso),
                  selected = unique(pop_data()$antigen_iso)[1])
    })

    # Render UI for stratify_by (dynamic based on selected antigen_iso or other logic)
    output$stratify_by <- renderUI({
      if (input$stratification_type == "stratified") {
        selectInput(session$ns("stratify_column"),
                    "Choose Stratification Column",
                    choices = colnames(pop_data()))
      } else {
        NULL  # Hide stratification column for overall estimation
      }
    })

    # Placeholder for status text
    output$status1 <- renderText({
      paste("Selected Stratification Type:", input$stratification_type)
    })

    # Compute estimate incidence (this should use the actual logic based on your data)
    output$est_incidence <- renderTable({
      # Here you can add the logic for calculating the seroincidence estimate based on inputs
      # For example:
      if (input$stratification_type == "overall") {
        result <- data.frame(Estimate = sum(pop_data()$value))  # Example estimate
      } else {
        # Logic for stratified estimation
        result <- data.frame(Estimate = tapply(pop_data()$value, pop_data()$stratify_column, sum))
      }
      result
    })

    # Display computation result
    output$result <- renderText({
      paste("Seroincidence Estimation Completed.")
    })
  })
}
