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
        # uiOutput(ns("stratification_radio")),
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

      h3("Log Option")

      # Log checkbox for Pop Data
      output$log <- renderUI({
        if (input$updatedData_ext == "Pop Data") {

          checkboxInput(ns("check_log"), "Log Scale", value = TRUE)
        } else {
          NULL
        }
      })

      # # Stratification radio buttons
      # output$stratification_radio <- renderUI({
      #   if (input$updatedData_ext == "Pop Data") {
      #     radioButtons(
      #       ns("stratification_choice"),
      #       "Do you want to stratify?",
      #       choices = c("Yes" = "yes", "No" = "no"),
      #       selected = "yes"
      #     )
      #   } else {
      #     NULL
      #   }
      # })

      # Numeric summary
      output$numeric_summary <- renderUI({
        data <- switch(input$updatedData_ext,
          "Pop Data" = pop_data(),
          "Curve Data" = curve_data(),
          "Noise Data" = noise_data(),
          NULL
        )

        renderTable({
          skimr::skim(data) %>%
            skimr::yank("numeric") %>%
            dplyr::mutate(n_observations = nrow(data))
        })
      })

      # Visualization output
      output$visualize <- renderPlot({
        req(input$type_visualization)

        data <- switch(input$updatedData_ext,
          "Pop Data" = pop_data(),
          "Curve Data" = curve_data(),
          "Noise Data" = noise_data(),
          NULL
        )

        if (input$type_visualization == "Distribution" && input$updatedData_ext == "Curve Data") {
          ggplot(data, aes(x = value)) +
            geom_density() +
            theme_minimal()
        } else if (input$type_visualization == "Age Scatter" && input$updatedData_ext == "Pop Data") {
          serocalculator:::autoplot.pop_data(
            data,
            type = "age-scatter",
            strata = if (input$stratification_choice == "yes") input$choosen_stratification else NULL,
            log = input$check_log
          )
        }
      })
    })

    observeEvent(input$updatedData_ext, {
      output$stratification <- renderUI({

        df <- isolate(pop_data()) %>%
                  select(-antigen_iso) %>%
                  select(where(~ !is.numeric(.)))

        valid_choices <-  df %>% names()
        selectInput(
          ns("choosen_stratification"),
          "Select Stratification Column",
          choices = valid_choices,
          multiple = TRUE
        )
      })
    })

    output$antigen_type <- renderUI({
      req(pop_data())

      if ("antigen_iso" %in% names(pop_data())) {
        checkboxGroupInput(
          inputId = ns("antigen_type_ext"),
          label = "Antigen Type",
          choices = unique(pop_data()$antigen_iso),
          selected = unique(pop_data()$antigen_iso)
        )
      } else {
        # Display a message if 'antigen_iso' is not in the data
        validate(need(FALSE, "Antigen column ('antigen_iso') not present in data."))
      }
    })

    # -- END --
  })
}
