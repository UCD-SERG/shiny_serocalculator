
library(shinytest2)
library(testthat)

# Test unsupported file extension
temp_txt <- tempfile(fileext = ".txt")
writeLines("This is a test file", temp_txt)

testServer(server, {
  # Simulate a TXT file upload, which is unsupported
  session$setInputs(upload = list(name = "testfile.txt", datapath = temp_txt))

  # Expect data() to return NULL for unsupported file extensions
  expect_null(data())
})
