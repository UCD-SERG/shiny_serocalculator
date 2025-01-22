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
          tabPanel("Visualize", plotOutput(ns("visualize")), textOutput(outputId = ns("antigen_choosen")))
        )
      )
    )
  )
}

#' @title server-side data inspection
#' @param id identify namespace
#' @param value a continuous attribute
inspect_data_server <- function(id, pop_data, curve_data, noise_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ############################# UI Updates #################################

    # Dynamically update visualization choices
    observeEvent(input$updatedData_ext, {
      req(input$updatedData_ext)

      output$choose_visualization <- renderUI({
        vis_choices <- switch(input$updatedData_ext,
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

      # Display summary of numeric columns in dataset
      output$numeric_summary <- renderUI({
        # Dynamically select data based on the input
        selected_data <- switch(input$updatedData_ext,
          "Pop Data" = pop_data,
          "Curve Data" = curve_data,
          "Noise Data" = noise_data,
          NULL
        )

        # Convert reactive to data.frame
        selectedDF <- isolate(selected_data())

        # Check if the data is not empty and has numeric columns
        if (!is.null(selectedDF) && nrow(selectedDF) > 0) {
          renderTable({
            skimr::skim(selectedDF) %>%
              skimr::yank("numeric") %>%
              dplyr::mutate(n_observations = nrow(selectedDF))
          })
        } else {
          renderText("No data available to display summary.")
        }
      })

      ############################ Stratification UI ###########################

      output$stratification <- renderUI({
        if (input$updatedData_ext == "Pop Data") {
          df <- isolate(pop_data()) %>%
            dplyr::select(where(~ !is.numeric(.))) %>%
            dplyr::select(-antigen_iso)

          # available choices
          valid_choices <- names(df)

          selectInput(
            ns("choosen_stratification"),
            "Select Stratification Column",
            choices = valid_choices,
            multiple = FALSE
          )
        }
      })

      ############################ Antigen Choice ##############################

      output$antigen_type <- renderUI({
        df <- pop_data()
        antigen_types <- df$antigen_iso %>% unique()

        checkboxGroupInput(
          session$ns("output_antigen"),
          label = "Choose antigen Type:",
          choices = antigen_types,
          selected = antigen_types[1]
        )
      })

      observeEvent(input$output_antigen, {
        req(input$output_antigen)

        output$antigen_choosen <- renderText({
          paste("The antigen(s) in use: ", paste(input$output_antigen, collapse = ", "))
        })
      })

      ############################ Visualization ###############################

      output$visualize <- renderPlot({
        if (input$updatedData_ext == "Pop Data") {
          # Convert reactive data object to data.frame
          selectedDF <- isolate(pop_data()) %>%
            serocalculator:::as_pop_data(
              antigen_isos = NULL,
              age = "age",
              value = "value",
              id = "id"
            )

          if (!is.null(input$antigen_type) && length(input$antigen_type) > 0) {
            selectedDF <- selectedDF %>%
              filter(antigen_iso %in% input$antigen_type)
          } else {
            message("No antigen type selected; skipping filter step.")
          }


          # Check if data is available and proceed
          if (is.null(selectedDF) || nrow(selectedDF) == 0) {
            return(NULL)
          }

          if (input$type_visualization == "Density") {
            selectedDF %>%
              serocalculator:::autoplot.pop_data(
                type = "density",
                strata = input$choosen_stratification,
                log = input$check_log
              )
          } else if (input$type_visualization == "Age Scatter") {
            selectedDF %>%
              serocalculator:::autoplot.pop_data(
                type = "age-scatter",
                strata = input$choosen_stratification,
                log = input$check_log
              )
          }
        } else if (input$updatedData_ext == "Curve Data") {
          # Convert reactive data object to data.frame
          selectedDF <- isolate(curve_data()) %>%
            serocalculator:::as_curve_params()

          if (!is.null(input$antigen_type) && length(input$antigen_type) > 0) {
            selectedDF <- selectedDF %>%
              filter(antigen_iso %in% input$antigen_type)
          } else {
            message("No antigen type selected; skipping filter step.")
          }

          # Check if data is available and proceed
          if (is.null(selectedDF) || nrow(selectedDF) == 0) {
            return(NULL)
          }

          if (input$type_visualization == "Decay") {
            selectedDF %>%
              serocalculator:::plot_curve_params_one_ab()
          } else if (input$type_visualization == "Distribution") {
            selectedDF %>%
              tidyr::pivot_longer(
                cols = `y0`:`r`,
                names_to = "parameter",
                values_to = "value"
              ) %>%
              ggplot2::ggplot(aes(x = value)) +
              ggplot2::geom_density() +
              ggplot2::facet_grid(parameter ~ .) +
              ggplot2::scale_y_continuous(limits = c(0, 0.009)) +
              ggplot2::scale_x_continuous(limits = c(0, 200)) +
              theme_minimal()
          }
        } else if (input$updatedData_ext == "Noise Data") {
          showNotification("No inspection visualizations for Noise Data", type = "warning")
        }
      })
    })
  })
}
