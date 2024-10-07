

# reactive object to hold population data
pop_data <- reactiveVal(NULL)

# reactive object to hold curve data
curve_data <- reactiveVal(NULL)

# reactive object to hold noise data
noise_data <- reactiveVal(NULL)

# reactive object to hold list of uploaded files
uploaded_files <- reactiveValues(files = NULL)

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


## file name ----
filename <- reactiveVal(NULL)

## data df
data_df <- reactiveVal(NULL)

# reactive object to hold uploaded data
data <- reactive({
  req(input$upload)
  ext <- tools::file_ext(input$upload$name)

  # Read the data based on the file extension
  if (ext == "csv") {
    read.csv(input$upload$datapath)
  } else if (ext == "rds") {
    readRDS(input$upload$datapath)
  } else {
    return(NULL)
  }
})
