# Load libraries
library(shinytest2)
library(testthat)

test_that("Column names are extracted correctly from uploaded file", {
  # Create a temporary CSV file with test data
  temp_csv <- tempfile(fileext = ".csv")
  test_df <- data.frame(a = 1:3, b = 4:6)
  write.csv(test_df, temp_csv, row.names = FALSE)

  # Run the test on the server
  testServer(shinyApp, {
    # Simulate the file upload
    session$setInputs(upload = list(name = "testfile.csv", datapath = temp_csv))

    # Use flushReact to make sure the reactivity is updated
    session$flushReact()

    # Check that column_names() returns the expected column names
    expect_equal(isolate(column_names()), c("a", "b"))
  })

  # Clean up the temporary file after the test
  unlink(temp_csv)
})
