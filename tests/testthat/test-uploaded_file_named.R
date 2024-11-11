# Load necessary libraries
library(shinytest2)
library(testthat)

test_that("Uploaded file is named, saved, and added to selection dropdowns correctly", {
  # Create a temporary CSV file to simulate file upload
  temp_csv <- tempfile(fileext = ".csv")
  test_data <- data.frame(a = 1:3, b = 4:6)
  write.csv(test_data, temp_csv, row.names = FALSE)

  # Test the observeEvent that handles file upload and naming
  testServer(server, {
    # Simulate file upload with `file_name` set to "Pop Data"
    session$setInputs(upload = list(name = "testfile.csv", datapath = temp_csv))
    session$setInputs(file_name = "Pop Data")
    session$flushReact()  # Ensure all reactivity has propagated

    # Check that the file has been named and saved as "Pop Data.csv"
    expect_true(file.exists("Pop Data.csv"))

    # Verify the uploaded file list includes "Pop Data"
    expect_true("Pop Data" %in% uploaded_files$files)

    # Check the select input choices are updated correctly
    expect_true("Pop Data" %in% isolate(session$input$selectedData))
    expect_true("Pop Data" %in% isolate(session$input$updatedData))

    # Now test with another `file_name` value
    session$setInputs(file_name = "Curve Data")
    session$flushReact()

    # Check for the updated file naming
    expect_true(file.exists("testfile.csv"))

    # Verify the uploaded file list includes "Curve Data | Upload | testfile.csv"
    expect_true("Curve Data | Upload | testfile.csv" %in% uploaded_files$files)

    # Confirm the dropdown lists are updated again
    expect_true("Curve Data | Upload | testfile.csv" %in% isolate(session$input$selectedData))
    expect_true("Curve Data | Upload | testfile.csv" %in% isolate(session$input$updatedData))
  })

  # Clean up temporary files created during testing
  unlink(temp_csv)
  unlink("Pop Data.csv")
  unlink("testfile.csv")
})
