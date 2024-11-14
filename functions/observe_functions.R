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
