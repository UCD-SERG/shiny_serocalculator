server <- function(input, output, session) {

  # Reactive object to hold uploaded data
  data <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)

    # Read the data based on the file extension
    switch(ext,
           "csv" = read.csv(input$upload$datapath),
           "rds" = readRDS(input$upload$datapath),
           return(NULL) # Return NULL for unsupported extensions
    )
  })

  # Define global reactive values for data
  uploaded_files <- reactiveValues(files = NULL)
  pop_data <- reactiveVal(NULL)
  curve_data <- reactiveVal(NULL)
  noise_data <- reactiveVal(NULL)
  filename <- reactiveVal(NULL)
  data_df <- reactiveVal(NULL)

  # Reactive object (data frame) for default noise values
  noise_values <- reactiveValues(new_val = data.frame(
    antigen = character(),
    y_low = numeric(),
    y_high = numeric(),
    eps = numeric(),
    nu = numeric(),
    stringsAsFactors = FALSE
  ))

  # Get uploaded column names
  column_names <- reactive({
    req(input$upload)

    null_file <- is.null(input$upload$datapath)
    if (null_file) {
      return(NULL)
    }

    df <- vroom::vroom(input$upload$datapath, delim = ",")
    names(df)
  })

  # Summary tab module
  summary_tab_server("summary")

  # Call the `import_data_server` module with the required arguments
  import_data_server(
    id = "import_data",
    uploaded_files = uploaded_files,
    pop_data = pop_data,
    curve_data = curve_data,
    noise_data = noise_data
  )

  # Call the `estimate_seroincidence_server` module
  estimate_seroincidence_server(
    id = "estimate_seroincidence",
    pop_data = pop_data,
    curve_data = curve_data,
    noise_data = noise_data
  )
}
