# load required libraries
library(shiny)

# load pre-defined scripts
source("values/reactive_values.R")
source("ui.R")

# helper function to classify data based on file name
get_classified_data <- function(df, file_name) {
  if (is.null(df)) {
    return(NULL)
  }

  if (file_name == "Pop Data") {
    class(df) <- c("pop_data", class(df))
  } else if (file_name == "Curve Data") {
    class(df) <- c("curve_data", class(df))
  } else if (file_name == "Noise Data") {
    class(df) <- c("noise_data", class(df))
  }

  df
}

# function to assign class to uploaded file
assign_class_to_data <- function(data, file_name) {
  observeEvent(c(data(), file_name), {
    df <- data()
    get_classified_data(df, file_name)
  })
}

# Define the reactive list to track uploaded files
uploaded_files <- reactiveValues(files = character(0))

# Use the generalized function for Curve Data upload
handle_file_upload(
  input_id = "curve_upload",
  file_label = "Curve Data",
  uploaded_files = uploaded_files,
  session = session,
  updated_input_ids = c("updatedData", "updatedData_ext")
)

# Similarly, use it for other data types
handle_file_upload(
  input_id = "pop_upload",
  file_label = "Pop Data",
  uploaded_files = uploaded_files,
  session = session,
  updated_input_ids = c("updatedData", "updatedData_ext")
)

handle_file_upload(
  input_id = "noise_upload",
  file_label = "Noise Data",
  uploaded_files = uploaded_files,
  session = session,
  updated_input_ids = c("updatedData", "updatedData_ext")
)

