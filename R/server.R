# Define the server
server <- function(input, output, session) {

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


  # summary tab module
  summary_tab_server("summary")

  # Call the import_data_server module with the required arguments
  import_data_server(
    id = "import_data",
    uploaded_files = uploaded_files,
    pop_data = pop_data,
    curve_data = curve_data,
    noise_data = noise_data
  )

  # inspect data
  inspect_data_server("inspect_data", reactiveData)


  # Call the estimate_seroincidence_server module
  estimate_seroincidence_server(
    id = "estimate_seroincidence",
    pop_data = pop_data,
    curve_data = curve_data,
    noise_data = noise_data
  )
}
