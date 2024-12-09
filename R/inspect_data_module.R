

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

        # Description
        helpText("This section allows the selection of uploaded data and visualize."),

        # Select input widget for column selection
        selectInput(ns("updatedData_ext"), "Available Data to Choose", choices = NULL),

        # Choose type of visualization
        uiOutput(ns("choose_visualization")),

        # Choose 'yes' or 'no' for stratification
        uiOutput(ns("stratification_radio")),

        # Choose visualization stratification
        uiOutput(ns("stratification")),

        # Choose log scale
        uiOutput(ns("log"))
      ),
      mainPanel(
        "",
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

    # Reactive values to store data
    reactive_data <- reactiveValues(pop = NULL, curve = NULL, noise = NULL)

    # Observe selected data type and update UI accordingly
    observeEvent(input$updatedData_ext, {
      req(input$updatedData_ext)

      # Choose visualization type based on selected data
      output$choose_visualization <- renderUI({
        if (input$updatedData_ext == "Pop Data") {
          selectInput(
            ns("type_visualization"),
            "Choose Type of Visualization",
            choices = c("Density", "Age Scatter")
          )
        } else if (input$updatedData_ext == "Curve Data") {
          selectInput(
            ns("type_visualization"),
            "Choose Type of Visualization",
            choices = c("Distribution", "Decay"),
            selected = "Distribution"
          )
        }
      })

      # Log checkbox for Pop Data
      output$log <- renderUI({
        if (input$updatedData_ext == "Pop Data") {
          checkboxInput(ns("check_log"), "Log", value = TRUE)
        }
      })

      # Stratification radio buttons for Pop Data
      output$stratification_radio <- renderUI({
        if (input$updatedData_ext == "Pop Data") {
          radioButtons(
            ns("stratification_choice"),
            "Do you want to stratify?",
            choices = list("Yes" = "yes", "No" = "no"),
            selected = "yes"
          )
        }
      })
    })

    # Update numeric summary based on the selected file
    observeEvent(dataReactive(), {
      req(input$updatedData_ext)
      output$numeric_summary <- renderTable({
        data <- dataReactive()
        skimr::skim(data) %>%
          skimr::yank("numeric")
      })
    })

    # Update visualization based on user inputs
    observeEvent(c(
      input$type_visualization,
      input$check_log,
      input$stratification_choice,
      input$choosen_stratification
    ), {
      req(input$type_visualization)

      output$visualize <- renderPlot({
        data <- dataReactive()
        viz_type <- input$type_visualization

        if (viz_type == "Distribution") {
          ggplot2::ggplot(data, aes(x = value)) +
            geom_density() +
            theme_minimal()
        } else if (viz_type == "Age Scatter" && input$updatedData_ext == "Pop Data") {
          serocalculator:::autoplot.pop_data(
            data,
            type = "age-scatter",
            strata = if (input$stratification_choice == "yes") input$choosen_stratification else NULL,
            log = input$check_log
          )
        }
      })
    })
  })
}
