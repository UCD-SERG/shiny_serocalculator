#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny reactiveVal
#' @importFrom utils read.csv

server <- function(input, output, session) {

  # Call modules
  summary_tab_server("summary")

  ############################ REACTIVE OJECTS ############################
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

  # Reactive values to store uploaded data
  pop_data <- reactiveVal(NULL)
  curve_data <- reactiveVal(NULL)
  noise_data <- reactiveVal(NULL)

  ########################### ASSIGN VALUES TO REACTIVE OBJECTS ##############

  # Observe file uploads and update reactive values
  observeEvent(c(input$noise_upload, input$curve_upload, input$pop_upload), {
    if (input$data_upload_type == "Pop Data") {
      req(input$pop_upload)
      df <- read_data_file(input$pop_upload)
      pop_data(df)
    } else if (input$data_upload_type == "Curve Data") {
      req(input$curve_upload)
      df <- read_data_file(input$curve_upload)
      curve_data(df)
    } else if (input$data_upload_type == "Noise Data") {
      req(input$noise_upload)
      df <- read_data_file(input$noise_upload)
      noise_data(df)
    }
  })

  ############################################################################

  import_data_server(
    id = "import_data",
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

  estimate_seroincidence_server(
    id = "estimate_seroincidence",
    pop_data = pop_data,
    curve_data = curve_data,
    noise_data = noise_data
  )
}
