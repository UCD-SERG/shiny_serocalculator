library(testthat)

test_that("get_classified_data correctly assigns classes based on file_name", {
  # NULL input
  expect_null(get_classified_data(NULL, "Pop Data"))

  # Pop Data classification
  df <- data.frame(a = 1:3, b = 4:6)


  result <- get_classified_data(df, "Pop Data")
  expect_true(inherits(result, "pop_data"))
  expect_true(inherits(result, "data.frame"))

  # Curve Data
  result <- get_classified_data(df, "Curve Data")
  expect_true(inherits(result, "curve_data"))
  expect_true(inherits(result, "data.frame"))

  # Noise Data
  result <- get_classified_data(df, "Noise Data")
  expect_true(inherits(result, "noise_data"))
  expect_true(inherits(result, "data.frame"))

  # Unknown file_name
  result <- get_classified_data(df, "Unknown Data")
  expect_false(inherits(result, "pop_data"))
  expect_false(inherits(result, "curve_data"))
  expect_false(inherits(result, "noise_data"))
  expect_true(inherits(result, "data.frame"))

})
