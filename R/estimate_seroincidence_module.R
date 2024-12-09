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


estimate_seroincidence_server <- function(id, pop_data, curve_data, noise_data) {
  moduleServer(id, function(input, output, session) {

    # Default noise parameters
    observe({
      default_noise_params <- serocalculator::load_noise_params("https://osf.io/download/h64cw")
      noise_data(default_noise_params)
    })

    # Load noise data from uploaded file
    observeEvent(input$file_name, {
      req(input$file_name == "Noise Data", input$upload)

      noise_df <- read_data_file(input$upload)
      noise_data(noise_df)
    })

    # Update noise data from UI input
    observeEvent(input$set_average, {
      new_row <- data.frame(
        antigen = input$antigen,
        y_low = as.numeric(input$y_low),
        y_high = as.numeric(input$y_high),
        eps = as.numeric(input$eps),
        nu = as.numeric(input$nu),
        stringsAsFactors = FALSE
      )

      updated_noise <- rbind(noise_data(), new_row)
      noise_data(updated_noise)
    })

    # Dynamic UI: Select antigen types
    observeEvent(input$updatedData_ext, {
      req(input$updatedData_ext == "Pop Data", pop_data())

      output$antigen_type <- renderUI({
        antigen_types <- unique(pop_data()$antigen_iso)

        selectInput(
          inputId = session$ns("output_antigen"),
          label = "Choose Antigen Type:",
          choices = antigen_types,
          multiple = TRUE
        )
      })
    })

    # Dynamic UI: Stratification options
    observeEvent(input$stratification_type, {
      req(input$stratification_type == "stratified", pop_data())

      output$stratify_by <- renderUI({
        # Select columns suitable for stratification
        stratifiable_columns <- pop_data() %>%
          select(where(~ !is.numeric(.))) %>%
          select(-antigen_iso) %>%
          names()

        if (length(stratifiable_columns) > 0) {
          selectInput(
            inputId = session$ns("stratify_by"),
            label = "Stratify By:",
            choices = stratifiable_columns,
            multiple = TRUE
          )
        } else {
          h4("No applicable columns for stratification found.")
        }
      })
    })

    # estimate and render seroincidence results
    observeEvent(c(
      input$stratify_by, input$stratification_type,
      input$output_antigen, input$curve_upload,
      input$pop_upload, input$noise_upload
    ), {
      req(input$stratification_type, input$output_antigen)

      # Load data files
      pop_df <- read_data_file(input$pop_upload)
      pop_data(pop_df)

      noise_df <- read_data_file(input$noise_upload)
      noise_data(noise_df)

      curve_df <- read_data_file(input$curve_upload)
      curve_data(curve_df)

      # calculate seroincidence
      output$est_incidence <- renderTable({
        if (input$stratification_type == "stratified") {
          est <- serocalculator::est.incidence.by(
            pop_data = pop_data(),
            curve_params = curve_data(),
            noise_params = noise_data(),
            strata = input$stratify_by,
            antigen_isos = input$output_antigen,
            verbose = TRUE
          )
        } else if (input$stratification_type == "overall") {
          est <- serocalculator::est.incidence(
            pop_data = pop_data(),
            curve_params = curve_data(),
            noise_params = noise_data(),
            antigen_isos = input$output_antigen,
            verbose = TRUE
          )
        }

        summary(est)
      })
    })
  })
}

