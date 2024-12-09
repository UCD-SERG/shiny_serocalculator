

# Function to check if the URL is working
check_url <- function(url) {
  response <- tryCatch(
    httr::GET(url),
    error = function(e) {
      return(NULL)
    }
  )
  return(!is.null(response) && httr::status_code(response) == 200)
}
