# UI for the Inspect Data module
inspect_data_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Inspect Data",
    sidebarLayout(
      position = "left",
      sidebarPanel(
        width = 3,
        h4("Available Data"),
        helpText("Select uploaded data and visualize it."),

        # Select input for dataset
        selectInput(ns("updatedData_ext"), "Available Data to Choose", choices = NULL),

        # Dynamic UI elements
        uiOutput(ns("choose_visualization")),
        uiOutput(ns("stratification_radio")),
        uiOutput(ns("stratification")),
        uiOutput(ns("log"))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Numeric Summary", uiOutput(ns("numeric_summary"))),
          tabPanel("Visualize", plotOutput(ns("visualize")))
        )
      )
    )
  )
}

# Server logic for the Inspect Data module
inspect_data_server <- function(id, dataReactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Observe selected data type and update visualization options
    observeEvent(input$updatedData_ext, {
      req(input$updatedData_ext)

      # Choose visualization type based on selected data
      output$choose_visualization <- renderUI({
        switch(input$updatedData_ext,
          "Pop Data" = selectInput(
            ns("type_visualization"),
            "Choose Type of Visualization",
            choices = c("Density", "Age Scatter")
          ),
          "Curve Data" = selectInput(
            ns("type_visualization"),
            "Choose Type of Visualization",
            choices = c("Distribution", "Decay"),
            selected = "Distribution"
          ),
          NULL
        )
      })

      # Log checkbox for Pop Data
      output$log <- renderUI({
        if (input$updatedData_ext == "Pop Data") {
          checkboxInput(ns("check_log"), "Log Scale", value = TRUE)
        } else {
          NULL
        }
      })

      # Stratification radio buttons
      output$stratification_radio <- renderUI({
        if (input$updatedData_ext == "Pop Data") {
          radioButtons(
            ns("stratification_choice"),
            "Do you want to stratify?",
            choices = c("Yes" = "yes", "No" = "no"),
            selected = "no"
          )
        } else {
          NULL
        }
      })

      # Stratification column selector
      output$stratification <- renderUI({
        if (input$stratification_choice == "yes") {
          data <- dataReactive()
          selectInput(
            ns("choosen_stratification"),
            "Select Stratification Column",
            choices = names(data),
            multiple = TRUE
          )
        } else {
          NULL
        }
      })
    })

    # Numeric summary
    output$numeric_summary <- renderUI({
      req(dataReactive())
      renderTable({
        data <- dataReactive()
        skimr::skim(data) %>%
          skimr::yank("numeric")
      })
    })

    # Visualization output
    output$visualize <- renderPlot({
      req(input$type_visualization, dataReactive())

      data <- dataReactive()

      if (input$type_visualization == "Distribution" && input$updatedData_ext == "Curve Data") {
        ggplot(data, aes(x = value)) +
          geom_density() +
          theme_minimal()
      } else if (input$type_visualization == "Age Scatter" && input$updatedData_ext == "Pop Data") {
        serocalculator::autoplot.pop_data(
          data,
          type = "age-scatter",
          strata = if (input$stratification_choice == "yes") input$choosen_stratification else NULL,
          log = input$check_log
        )
      }
    })
  })
}
