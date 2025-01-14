#' @title ui visually inspect uploaded data
#'
#' @importFrom shiny plotOutput
#' @importFrom shiny mainPanel
#' @importFrom shiny sidebarPanel
#' @importFrom shiny checkboxInput
#' @importFrom shiny renderPlot
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_density
#' @importFrom ggplot2 theme_minimal
#'
#' @param id a `string` to identify a namespace
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
        selectInput(ns("updatedData_ext"),
          "Available Data to Choose",
          choices = c("Pop Data", "Curve Data", "Noise Data"),
          selected = "Pop Data"
        ),

        # Dynamic UI elements
        uiOutput(ns("choose_visualization")),
        uiOutput(ns("stratification_radio")),
        uiOutput(ns("stratification")),
        uiOutput(ns("antigen_type")),
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

#' @title server-side data inspection
#' @param id identify namespace
#' @param value a continuous attribute
inspect_data_server <- function(id, pop_data, curve_data, noise_data, value) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Load data from the `import_data_server` module
    data <- import_data_server(
      id = "import_data",
      pop_data = pop_data,
      curve_data = curve_data,
      noise_data = noise_data
    )

    # Observe selected data type and dynamically update UI
    observeEvent(input$updatedData_ext, {
      req(input$updatedData_ext)

      # UI for visualization type
      output$choose_visualization <- renderUI({
        vis_choices <- switch(
          input$updatedData_ext,
          "Pop Data" = c("Density", "Age Scatter"),
          "Curve Data" = c("Distribution", "Decay"),
          NULL
        )
        if (!is.null(vis_choices)) {
          selectInput(
            ns("type_visualization"),
            "Choose Type of Visualization",
            choices = vis_choices,
            selected = vis_choices[1]
          )
        }
      })

      # Log scale option for Pop Data
      output$log <- renderUI({
        if (input$updatedData_ext == "Pop Data") {
          checkboxInput(ns("check_log"), "Log Scale", value = TRUE)
        } else {
          NULL
        }
      })

      # Numeric summary of the selected dataset
      output$numeric_summary <- renderUI({
        selected_data <- switch(
          input$updatedData_ext,
          "Pop Data" = data$pop_data(),
          "Curve Data" = data$curve_data(),
          "Noise Data" = data$noise_data(),
          NULL
        )

        renderTable({
          skimr::skim(selected_data) %>%
            skimr::yank("numeric") #%>%
            #dplyr::mutate(n_observations = nrow(selected_data))
        })
      })

      # Visualization
      output$visualize <- renderPlot({
        req(input$type_visualization)
        selected_data <- switch(
          input$updatedData_ext,
          "Pop Data" = data$pop_data(),
          "Curve Data" = data$curve_data(),
          "Noise Data" = data$noise_data(),
          NULL
        )
        req(selected_data)

        if (input$type_visualization == "Density") {

          ggplot(data_clean, aes(x = selected_data[[value]])) +
            geom_density() +
            theme_minimal() +
            labs(title = "Density Plot", x = "Value", y = "Density")

        } else if (input$type_visualization == "Age Scatter") {
          req(all(c("age", value) %in% names(selected_data)))

          ggplot(selected_data, aes(x = age, y = selected_data[[value]])) +
            geom_point() +
            theme_minimal() +
            labs(title = "Age vs Value", x = "Age", y = "Value")
        }
      })

      # Stratification column selection for Pop Data
      output$stratification <- renderUI({
        if (input$updatedData_ext == "Pop Data") {
          df <- isolate(pop_data()) %>%
            dplyr::select(where(~ !is.numeric(.)))  # Non-numeric columns

          valid_choices <- names(df)
          selectInput(
            ns("choosen_stratification"),
            "Select Stratification Column",
            choices = valid_choices,
            multiple = TRUE
          )
        }
      })

      # Antigen type selection
      output$antigen_type <- renderUI({
        req(pop_data())

        if ("antigen_iso" %in% names(pop_data())) {
          checkboxGroupInput(
            ns("antigen_type_ext"),
            "Antigen Type",
            choices = unique(pop_data()$antigen_iso),
            selected = unique(pop_data()$antigen_iso)
          )
        } else {
          validate(need(FALSE, "Antigen column ('antigen_iso') not present in data."))
        }
      })
    })
  })
}


