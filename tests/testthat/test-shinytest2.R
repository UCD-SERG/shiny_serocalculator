library(shinytest2)

test_that("{shinytest2} recording: shiny_serocalculator", {
  app <- AppDriver$new(variant = platform_variant(), name = "shiny_serocalculator",
      height = 599, width = 1065)
  rlang::warn(paste0("`pop_upload` should be the path to the file, relative to the app's tests/testthat directory.\n",
      "Remove this warning when the file is in the correct location."))
  app$upload_file(pop_upload = "pop_data.csv")
  app$expect_screenshot()
  app$expect_screenshot()
})
