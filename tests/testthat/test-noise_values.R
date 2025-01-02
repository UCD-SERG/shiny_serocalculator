# test_that("noise_values initializes with correct structure and empty data", {
#   testServer(shiny.serocalculator:::server, {
#     # Check that noise_values reactiveValues object has been initialized
#     # Isolate to access reactiveValues outside of reactive context
#     noise <- isolate(noise_values$new_val)
#
#     # Check that noise is a data frame
#     expect_true(is.data.frame(noise))
#
#     # Check that noise data frame has correct column names
#     expect_equal(names(noise), c("antigen", "y_low", "y_high", "eps", "nu"))
#
#     # Check that each column is of the expected type
#     expect_type(noise$antigen, "character")
#     expect_type(noise$y_low, "double")
#     expect_type(noise$y_high, "double")
#     expect_type(noise$eps, "double")
#     expect_type(noise$nu, "double")
#
#     # Check that noise data frame is initially empty
#     expect_equal(nrow(noise), 0)
#   })
# })
