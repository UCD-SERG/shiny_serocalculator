shiny_serocalculator_app <- function(...) {

  # Define global reactive values for data
  uploaded_files <- reactiveValues(files = NULL)
  pop_data <- reactiveVal(NULL)
  curve_data <- reactiveVal(NULL)
  noise_data <- reactiveVal(NULL)
  filename <- reactiveVal(NULL)
  data_df <- reactiveVal(NULL)

  # reactive object (data frame) for default noise values
  noise_values <- reactiveValues(new_val = data.frame(
    antigen = character(),
    y_low = numeric(),
    y_high = numeric(),
    eps = numeric(),
    nu = numeric(),
    stringsAsFactors = FALSE
  ))

  # get uploaded column names ----
  column_names <- reactive({
    # ensure data is uploaded
    req(input$upload)

    # load file
    null_file <- is.null(input$upload$datapath)

    if (null_file) {
      return(NULL)
    }

    # read file
    df <- vroom::vroom(input$upload$datapath, delim = ",")

    # get column names
    df %>% names()
  })

  # Define the UI
  ui <- shinyUI(
    navbarPage(
      title = "Serocalculator",
      theme = shinythemes::shinytheme("united"),

      # Initialize shinyjs
      useShinyjs(),

      # Add the busy spinner
      header = add_busy_spinner(
        spin = "atom",
        position = "top-right",
        margins = c(200, 800),
        timeout = 100
      ),

      # Tab for project summary
      summary_tab_ui("summary"),

      # Import data tab
      import_data_ui("import_data"),

      estimate_seroincidence_ui("estimate_seroincidence"),

      report_ui("report")
    )
  )

  # Define the server
  server <- function(input, output, session) {
    # Call the summary_tab_server module
    summary_tab_server("summary")

    # Call the import_data_server module with the required arguments
    import_data_server(
      id = "import_data",
      uploaded_files = uploaded_files,
      pop_data = pop_data,
      curve_data = curve_data,
      noise_data = noise_data
    )


    # # Call the estimate_seroincidence_server module
    # estimate_seroincidence_server(
    #   id = "estimate_seroincidence",
    #   pop_data = pop_data,
    #   curve_data = curve_data,
    #   noise_data = noise_data
    # )
  }


  # Run the app
  shinyApp(ui = ui, server = server)
}
