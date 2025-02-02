test_that(
  "read_data_file reads CSV and RDS files correctly",
  {
    # Create a test data frame to use for writing files
    test_df <- data.frame(a = 1:3, b = 4:6)

    # 1. Test with a CSV file
    temp_csv <- tempfile(fileext = ".csv")
    write.csv(test_df, temp_csv, row.names = FALSE)

    csv_file <- list(name = "testfile.csv", datapath = temp_csv)

    # Check that the CSV file is read correctly
    result_csv <- shiny.serocalculator:::read_data_file(csv_file)
    expect_equal(result_csv, test_df)

    # Clean up CSV temporary file
    unlink(temp_csv)

    # 2. Test with an RDS file
    temp_rds <- tempfile(fileext = ".rds")
    saveRDS(test_df, temp_rds)

    rds_file <- list(name = "testfile.rds", datapath = temp_rds)

    # Check that the RDS file is read correctly
    result_rds <- shiny.serocalculator:::read_data_file(rds_file)
    expect_equal(result_rds, test_df)

    # Clean up RDS temporary file
    unlink(temp_rds)

    # 3. Test with an unsupported file type
    temp_txt <- tempfile(fileext = ".txt")
    writeLines("This is a test", temp_txt)

    txt_file <- list(name = "testfile.txt", datapath = temp_txt)

    # Check that an unsupported file type triggers an error
    expect_error(shiny.serocalculator:::read_data_file(txt_file), "Unsupported file type. Please upload a .csv or .rds file.")

    # Clean up TXT temporary file
    unlink(temp_txt)
  }
)
