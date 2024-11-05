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


test_that("{shinytest2} recording: visualize", {
  app <- AppDriver$new(variant = platform_variant(), name = "visualize", height = 599,
      width = 1065)

  rlang::warn(paste0("`pop_upload` should be the path to the file, relative to the app's tests/testthat directory.\n",
      "Remove this warning when the file is in the correct location."))
  app$upload_file(pop_upload = "pop_data.csv")
  app$set_inputs(file_name = "Curve Data")
  rlang::warn(paste0("`curve_upload` should be the path to the file, relative to the app's tests/testthat directory.\n",
      "Remove this warning when the file is in the correct location."))
  app$upload_file(curve_upload = "curve_data.csv")
  app$set_inputs(file_name = "Noise Data")
  rlang::warn(paste0("`noise_upload` should be the path to the file, relative to the app's tests/testthat directory.\n",
      "Remove this warning when the file is in the correct location."))
  app$upload_file(noise_upload = "noise_data.csv")
  app$expect_screenshot()
  app$set_inputs(type_visualization = "Age Scatter")
  app$expect_screenshot()
  app$set_inputs(stratification_choice = "no")
  app$set_inputs(stratification_choice = "yes")
})
