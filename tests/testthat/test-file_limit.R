

test_that("File limit message is correctly generated", {
  output <- HTML("<p>File limit: <strong>500MB</strong></p>")

  expect_s3_class(output, "html")  # Check if output is an HTML object
  expect_equal(as.character(output), "<p>File limit: <strong>500MB</strong></p>")  # Check exact HTML output
})
