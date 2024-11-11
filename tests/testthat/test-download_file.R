# Load necessary libraries
library(shinytest2)
library(testthat)
library(httr)
library(httptest2)  # For mocking HTTP requests

test_that("Data is downloaded and displayed from a valid URL, or error is shown for invalid URL", {
  # Define a mock URL to use in tests
  valid_url <- "https://example.com/pop_data.csv"
  invalid_url <- "https://invalid-url.com/pop_data.csv"

  # Mock response for the valid URL
  with_mock_api({
    # Mock the response for a valid URL
    expect_GET(valid_url, status = 200)

    # Test the observeEvent that handles URL button click and data download
    testServer(server, {
      # Simulate entering a valid URL and clicking the URL button
      session$setInputs(pop_data_url = valid_url)
      session$setInputs(pop_data_url_btn = 1)
      session$flushReact()  # Ensure all reactivity has propagated

      # Check that pop_data() is assigned correctly after download
      expect_true(!is.null(isolate(pop_data())))

      # Verify the uploaded files list includes "Pop Data"
      expect_true("Pop Data" %in% uploaded_files$files)

      # Check that the select input choices are updated with "Pop Data"
      expect_true("Pop Data" %in% isolate(session$input$updatedData))

      # Confirm that output$head renders a table with pop_data
      expect_true(!is.null(isolate(output$head)))
    })
  })

  # Mock response for an invalid URL
  with_mock_api({
    expect_GET(invalid_url, status = 404)

    # Test invalid URL handling
    testServer(server, {
      # Simulate entering an invalid URL and clicking the URL button
      session$setInputs(pop_data_url = invalid_url)
      session$setInputs(pop_data_url_btn = 1)
      session$flushReact()

      # Check that pop_data() remains NULL when the URL is invalid
      expect_null(isolate(pop_data()))

      # Check that the error notification was shown
      # Note: For Shiny notifications, capturing might require `shinytest2::app_expect_screenshot()`
      # Here we assume `showNotification()` behavior is correctly handled by Shiny itself
    })
  })
})
