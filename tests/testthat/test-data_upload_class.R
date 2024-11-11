
library(shinytest2)
library(testthat)

test_that("Data upload assigns to correct reactiveVal with correct class", {
  # Create a temporary CSV file
  temp_csv <- tempfile(fileext = ".csv")
  test_df <- data.frame(a = 1:3, b = 4:6)
  write.csv(test_df, temp_csv, row.names = FALSE)

  pop_data <- reactiveVal(NULL)
  curve_data <- reactiveVal(NULL)
  noise_data <- reactiveVal(NULL)

  # Test Noise Data
  testServer(server, {
    # Set input to simulate uploading the CSV file
    session$setInputs(upload = list(name = "testfile.csv", datapath = temp_csv))

    # Set input to specify file type as "Noise Data"
    session$setInputs(file_name = "Noise Data")

    # Flush reactivity to ensure updates have propagated
    session$flushReact()

    # Check that noise_data() has the expected data and class
    expect_equal(noise_data(), test_df)
    expect_true("noise_data" %in% class(noise_data()))
    expect_null(pop_data())
    expect_null(test_df)
    expect_null(curve_data())
  })

  # Clean up the temporary file
  unlink(temp_csv)
})
