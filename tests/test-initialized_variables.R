

test_that("Variables in Inspect Data Module are initialized as NULL", {
  antigen_iso <- NULL
  value <- NULL

  expect_null(antigen_iso)
  expect_null(value)
})
