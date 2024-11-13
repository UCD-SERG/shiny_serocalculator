
# Load libraries
library(shinytest2)
library(testthat)

test_that("pop_type UI element is conditionally rendered based on file_name input", {
  testServer(server, {
    # Set file_name to "Pop Data" to check if pop_type UI is rendered
    session$setInputs(file_name = "Pop Data")

    # Flush reactivity to ensure UI updates are applied
    session$flushReact()

    # Check if pop_type UI output is rendered as expected
    expect_true("pop_type" %in% names(output$pop_type))
    expect_true(!is.null(output$pop_type))
    expect_match(output$pop_type, "Choose Type")  # Checks if the label appears in the UI

    # Case 2: Set file_name to a value other than "Pop Data" to check if pop_type UI is cleared
    session$setInputs(file_name = "Other Data")

    # Flush reactivity to apply changes
    session$flushReact()

    # Check if pop_type UI output is cleared
    expect_true("pop_type" %in% names(output))
    expect_null(output$pop_type)  # Should be NULL for non "Pop Data" values
  })
})
