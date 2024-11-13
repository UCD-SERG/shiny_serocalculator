


source("libraries/load_libraries.R")

# reactive object to hold upload population data
pop_data <- reactiveVal(NULL)

# reactive object to hold curve data
curve_data <- reactiveVal(NULL)

# reactive object to hold noise data
noise_data <- reactiveVal(NULL)

# reactive object to hold list of uploaded files
uploaded_files <- reactiveValues(files = character(0))

# reactive object to hold file name of uploaded data
filename <- reactiveVal(NULL)

# reactive object (data frame) for default noise values
noise_values <- reactiveValues(new_val = data.frame(
  antigen = character(),
  y_low = numeric(),
  y_high = numeric(),
  eps = numeric(),
  nu = numeric(),
  stringsAsFactors = FALSE
))


# file name ----
filename <- reactiveVal(NULL)

## data df
data_df <- reactiveVal(NULL)

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

