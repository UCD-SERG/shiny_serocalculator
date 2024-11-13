# load required libraries
library(shiny)

# load pre-defined scripts
source("values/reactive_values.R")

# Function to assign class to uploaded file
assign_class_to_data <- function(data, file_name) {
  observeEvent(data(), {
    df <- data()
    if (is.null(df)) {
      return(NULL)
    }

    if (file_name() == "Pop Data") {
      class(df) <- c("pop_data", class(df))
    } else if (file_name() == "Curve Data") {
      class(df) <- c("curve_data", class(df))
    } else if (file_name() == "Noise Data") {
      class(df) <- c("noise_data", class(df))
    }
  })
}

# Function to conditionally render UI for pop data
render_pop_type_ui <- function(file_name, output) {
  observeEvent(file_name(), {
    req(file_name())

    if (file_name() == "Pop Data") {
      output$pop_type <- renderUI({
        selectInput(
          inputId = "pop_type",
          label = "Choose Type",
          choices = c("Upload", "OSF"),
          selected = "Upload"
        )
      })
    } else {
      # Clear the UI element for non "Pop Data" selections
      output$pop_type <- renderUI(NULL)
    }
  })
}


