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
#' @importFrom shiny selectInput
#'
#' @param id a `string` to identify a namespace
inspect_data_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Inspect Data",
    div(
      style = "position:absolute;right:1em;",
      actionButton(
        "estimate_next_btn",
        "Next",
        , icon = icon("arrow-right"),
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      tags$head(
        tags$style(
          HTML(
            "hr {border-top: 1px solid #828994;}"
          )
        )
      ),
    ),
    div(
      style = "position:absolute;right:1em;bottom:1em;",
      actionButton(
        "inspect_back_btn",
        "Back",
        , icon = icon("arrow-left"),
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      tags$head(
        tags$style(
          HTML(
            "hr {border-top: 1px solid #828994;}"
          )
        )
      ),
    ),
    sidebarLayout(
      position = "left",
      sidebarPanel(
        width = 3,
        h4("Available Data"),
        helpText("Select uploaded data and visualize it."),

        # Select input for dataset
        selectInput(ns("updatedData_ext"),
          "Select data",
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
        uiOutput(ns("dynamic_tabset")),
        uiOutput(ns("output2"))
      )
    )
  )
}


#' @title server-side data inspection
#'
#' @importFrom shiny renderUI
#' @importFrom shiny tabsetPanel
#' @importFrom shiny selectInput
#' @importFrom shiny isolate
#' @importFrom shiny validate
#' @importFrom dplyr where
#' @importFrom shiny checkboxGroupInput
#' @importFrom dplyr filter
#' @importFrom tidyselect all_of
#'
#' @param id identify namespace
#' @param pop_data population data
#' @param curve_data curve data
#' @param noise_data noise data
#' @param imported_data data returned by import_data_module
#'
#' @param y0 curve data parameter
#' @param y1 curve data parameter
#' @param t1 curve data parameter
#' @param alpha curve data paramter
#' @param r curve data parameter
#' @param value  continious variable to visualize
inspect_data_server <- function(id,
                                pop_data,
                                curve_data,
                                noise_data,
                                imported_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ######################## INITIALIZE VALUES #################################

    antigen_iso <- NULL

    ############################################################################

    # Dynamically generate the tabset
    output$dynamic_tabset <- renderUI({
      if (input$updatedData_ext == "Noise Data") {
        tabsetPanel(
          id = ns("switcher"),
          tabPanel(
            "Numeric Summary",
            uiOutput(ns("numeric_summary"))
          )
        )
      } else {
        tabsetPanel(
          id = ns("switcher"),
          tabPanel(
            "Numeric Summary",
            uiOutput(ns("numeric_summary"))
          ),
          tabPanel(
            "Visualize",
            plotOutput(ns("visualize"))
          )
        )
      }
    })


    output$summary <- renderPrint({
      "No Visualization"
    })

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
            "Select type of visualization",
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

        selected_df <- isolate(selected_data())

        # Check if the data is not empty and has numeric columns
        if (!is.null(selected_df) && nrow(selected_df) > 0) {
          renderTable({
            skimr::skim(selected_df) %>%
              skimr::yank("numeric") %>%
              dplyr::mutate(n_observations = nrow(selected_df))
          })
        } else {
          validate(
            "Please upload a dataset
            (Pop Data, Noise Data, or Curve Data)
            to enable visualization."
          )
        }
      })

      ############################ Stratification UI ###########################

      output$stratification <- renderUI({
        req(pop_data())
        if (is.null(imported_data$selected_id())) {
          showNotification("ID variable not selected for Pop Data")
        } else if (input$updatedData_ext == "Pop Data") {
          df <- isolate(pop_data()) %>%
            dplyr::select(where(~ !is.numeric(.))) %>%
            dplyr::select(-antigen_iso) %>%
            dplyr::select(-imported_data$selected_id())

          # available choices
          valid_choices <- names(df)

          selectInput(
            ns("choosen_stratification"),
            "Select stratification variable",
            choices = valid_choices,
            multiple = FALSE
          )
        }
      })

      ############################ Antigen Choice ##############################

      output$antigen_type <- renderUI({
        req(pop_data())

        df <- pop_data()
        antigen_types <- df$antigen_iso %>% unique()

        checkboxGroupInput(
          session$ns("output_antigen"),
          label = "Select antigen-isotype:",
          choices = antigen_types,
          selected = antigen_types[1]
        )
      })

      observeEvent(input$output_antigen, {
        req(input$output_antigen)

        output$antigen_choosen <- renderText({
          paste(
            "The antigen(s) in use: ",
            paste(
              input$output_antigen,
              collapse = ", "
            )
          )
        })
      })

      ############################ Visualization ###############################

      output$visualize <- renderPlot({
        if (input$updatedData_ext == "Pop Data") {
          # Convert reactive data object to data.frame
          selected_df <- isolate(pop_data()) %>%
            serocalculator::as_pop_data(
              antigen_isos = NULL,
              age = imported_data$selected_age(),
              value = imported_data$selected_value(),
              id = imported_data$selected_id()
            )

          if (!is.null(input$antigen_type) && length(input$antigen_type) > 0) {
            selected_df <- selected_df %>%
              filter(antigen_iso %in% input$antigen_type)
          } else {
            message("No antigen type selected; skipping filter step.")
          }

          # Check if data is available and proceed
          if (is.null(selected_df) || nrow(selected_df) == 0) {
            return(NULL)
          }

          # NOTE: subset by antigen_type
          if (input$type_visualization == "Density") {
            selected_df %>%
              serocalculator::autoplot(
                type = "density",
                strata = input$choosen_stratification,
                log = input$check_log
              )
          } else if (input$type_visualization == "Age Scatter") {
            selected_df %>%
              serocalculator::autoplot(
                type = "age-scatter",
                strata = input$choosen_stratification,
                log = input$check_log # check why log on/off
              )
          }
        } else if (input$updatedData_ext == "Curve Data") {
          # Convert reactive data object to data.frame
          selected_df <- isolate({
            curve_data() %>%
              serocalculator::as_curve_params(
                antigen_isos = input$output_antigen
              )
          })


          if (!is.null(input$antigen_type) && length(input$antigen_type) > 0) {
            selected_df <- selected_df %>%
              filter(antigen_iso %in% input$antigen_type)
          } else {
            message("No antigen type selected; skipping filter step.")
          }

          if (input$type_visualization == "Decay") {
            selected_df %>%
              serocalculator::autoplot(antigen_isos = input$output_antigen)
          } else if (input$type_visualization == "Distribution") {
            selected_df %>%
              tidyr::pivot_longer(
                cols = tidyselect::all_of(c("y0", "y1", "t1", "alpha", "r")),
                names_to = "parameter",
                values_to = "value"
              ) %>%
              ggplot2::ggplot(aes(x = selected_df$value)) +
              ggplot2::geom_density() +
              ggplot2::facet_grid(parameter ~ .) +
              ggplot2::scale_y_continuous(limits = c(0, 0.009)) +
              ggplot2::scale_x_continuous(limits = c(0, 200)) +
              theme_minimal()
          }
        } else if (input$updatedData_ext == "Noise Data") {
          showNotification(
            "No inspection visualizations for Noise Data",
            type = "warning"
          )
        }
      })
    })
  })
}
