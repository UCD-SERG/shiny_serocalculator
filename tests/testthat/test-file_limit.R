

test_that("File limit message is correctly generated", {
  output <- HTML("<p>File limit: <strong>500MB</strong></p>")

  expect_s3_class(output, "html")
  expect_equal(as.character(output),
               "<p>File limit: <strong>500MB</strong></p>")
})
