
library(testthat)

source("functions/observe_functions.R")

test_that("get_classified_data assigns correct class based on file name", {
  # Sample data frame for testing
  df <- data.frame(x = 1:5, y = 6:10)

  # Test for "Pop Data"
  result <- get_classified_data(df, "Pop Data")
  expect_true(inherits(result, "pop_data"))

  # Test for "Curve Data"
  result <- get_classified_data(df, "Curve Data")
  expect_true(inherits(result, "curve_data"))

  # Test for "Noise Data"
  result <- get_classified_data(df, "Noise Data")
  expect_true(inherits(result, "noise_data"))

  # Test for an unsupported file name (no class should be added)
  result <- get_classified_data(df, "Other Data")
  expect_false(inherits(result, "pop_data") || inherits(result, "curve_data") || inherits(result, "noise_data"))

  # Test for NULL input (should return NULL)
  result <- get_classified_data(NULL, "Pop Data")
  expect_null(result)
})
