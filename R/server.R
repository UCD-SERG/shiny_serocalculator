#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny reactiveVal
#' @importFrom utils read.csv

server <- function(input, output, session) {
  # Reactive object to hold uploaded data
  data <- reactive({
    req(input$file_upload)
    ext <- tools::file_ext(input$upload$name)

    # Read the data based on the file extension
    switch(ext,
      "csv" = read.csv(input$upload$datapath),
      "rds" = readRDS(input$upload$datapath),
      return(NULL) # Return NULL for unsupported extensions
    )
  })

  uploaded_files <- reactiveValues(files = NULL)
  pop_data <- reactiveVal(NULL)
  curve_data <- reactiveVal(NULL)
  noise_data <- reactiveVal(NULL)

  # assign reactive objects
  observeEvent(c(
    input$noise_upload,
    input$curve_upload,
    input$pop_upload
  ), {
    if (input$data_upload_type == "Noise Data") {
      # Check if a file has been uploaded for Noise Data
      req(input$noise_upload)

      # Read the uploaded file using the helper function
      df <- read_data_file(input$noise_upload)

      # Update the reactiveVal with the new noise data
      noise_data(df)
    } else if (input$data_upload_type == "Curve Data") {
      # Check if a file has been uploaded for Curve Data
      req(input$curve_upload)

      # Read the uploaded file using the helper function
      df <- read_data_file(input$curve_upload)

      # Update the reactiveVal with the new curve data
      curve_data(df)
    } else if (input$data_upload_type == "Pop Data") {
      # Check if a file has been uploaded for Pop Data
      req(input$pop_upload)

      # Read the uploaded file using the helper function
      df <- read_data_file(input$pop_upload)

      # Update the reactiveVal with the new pop data
      pop_data(df)
    }
  })

  # Call modules
  summary_tab_server("summary")

  import_data_server(
    id = "import_data",
    uploaded_files = uploaded_files,
    pop_data = pop_data,
    curve_data = curve_data,
    noise_data = noise_data
  )

  inspect_data_server(
    id = "inspect_data",
    pop_data = pop_data,
    curve_data = curve_data,
    noise_data = noise_data
  )

  # estimate_seroincidence_server(
  #   id = "estimate_seroincidence",
  #   pop_data = pop_data,
  #   curve_data = curve_data,
  #   noise_data = noise_data
  # )
}
