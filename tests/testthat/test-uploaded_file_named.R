library(shinytest2)
library(testthat)

source("functions/observe_functions.R")

test_that("handle_file_upload updates uploaded_files and select inputs correctly", {
  testServer(server, {
    # Initialize the uploaded_files reactiveValues
    uploaded_files <- reactiveValues(files = character(0))

    # Call the handle_file_upload function for Curve Data
    handle_file_upload(
      input_id = "curve_upload",
      file_label = "Curve Data",
      uploaded_files = uploaded_files,
      session = session,
      updated_input_ids = c("updatedData", "updatedData_ext")
    )

    # Simulate a file upload for Curve Data
    session$setInputs(curve_upload = "curve.csv")

    # Check that the "Curve Data" has been added to uploaded_files$files
    expect_true("Curve Data" %in% uploaded_files$files)

    # Check if the select inputs are updated with the new list of files
    expect_true("Curve Data" %in% session$input$updatedData)
    expect_true("Curve Data" %in% session$input$updatedData_ext)

    # Call the handle_file_upload function for Pop Data
    handle_file_upload(
      input_id = "pop_upload",
      file_label = "Pop Data",
      uploaded_files = uploaded_files,
      session = session,
      updated_input_ids = c("updatedData", "updatedData_ext")
    )

    # Simulate a file upload for Pop Data
    session$setInputs(pop_upload = "pop.csv")

    # Check that the "Pop Data" has been added to uploaded_files$files
    expect_true("Pop Data" %in% uploaded_files$files)

    # Check if the select inputs are updated with the new list of files
    expect_true("Pop Data" %in% session$input$updatedData)
    expect_true("Pop Data" %in% session$input$updatedData_ext)

    # Call the handle_file_upload function for Noise Data
    handle_file_upload(
      input_id = "noise_upload",
      file_label = "Noise Data",
      uploaded_files = uploaded_files,
      session = session,
      updated_input_ids = c("updatedData", "updatedData_ext")
    )

    # Simulate a file upload for Noise Data
    session$setInputs(noise_upload = "noise.csv")

    # Check that the "Noise Data" has been added to uploaded_files$files
    expect_true("Noise Data" %in% uploaded_files$files)

    # Check if the select inputs are updated with the new list of files
    expect_true("Noise Data" %in% session$input$updatedData)
    expect_true("Noise Data" %in% session$input$updatedData_ext)
  })
})
