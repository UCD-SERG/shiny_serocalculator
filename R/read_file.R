
# Function to determine file type and read the file
read_data_file <- function(file) {
  req(file) # Ensure the file is not NULL

  # Get the file extension
  file_ext <- tools::file_ext(file$name)

  if (file_ext == "csv") {
    return(read.csv(file$datapath))
  } else if (file_ext == "rds") {
    return(readRDS(file$datapath))
  } else {
    stop("Unsupported file type. Please upload a .csv or .rds file.")
  }
}


