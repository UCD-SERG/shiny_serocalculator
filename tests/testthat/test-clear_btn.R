test_that("Clear environment functionality works", {
  # Mock the reactive and input/output environment
  testServer(yourAppServer, {

    # Set initial reactive values
    session$setInputs(clear_btn = 1) # Simulate pressing the clear button
    pop_data("Some Data")
    curve_data("Some Data")
    noise_data("Some Data")

    uploaded_files$files <- c("Pop Data", "Curve Data", "Noise Data", "Extra Data")

    # Run the observeEvent for `clear_btn`
    session$flushReact()

    # Assertions after clearing the environment
    expect_null(pop_data())
    expect_null(curve_data())
    expect_null(noise_data())

    # Check if uploaded_files are cleared correctly
    expect_equal(uploaded_files$files, "Extra Data")

    # Check if select inputs are updated
    expect_equal(session$lastInputValue$selectedData, "Extra Data")
    expect_equal(session$lastInputValue$updatedData, "Extra Data")
    expect_equal(session$lastInputValue$updatedData_ext, "Extra Data")

    # Ensure outputs are reset
    expect_null(output$est_incidence())
    expect_null(output$stratify_by())
    expect_null(output$antigen_type())
    expect_null(output$visualize())
    expect_null(output$stratification())
    expect_null(output$other_head())
    expect_null(output$head())
    expect_null(output$numeric_summary())
  })
})
