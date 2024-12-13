file_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("file_metadata")),
    verbatimTextOutput(ns("data_summary"))
  )
}

# File Summary Module Server
file_summary_server <- function(id, uploaded_data) {
  moduleServer(id, function(input, output, session) {
    # File Metadata
    output$file_metadata <- renderPrint({
      req(uploaded_data())
      file_info <- attr(uploaded_data(), "file_info")

      if (!is.null(file_info)) {
        list(
          "File Name" = file_info$name,
          "File Size (KB)" = file_info$size / 1024,
          "File Type" = file_info$type
        )
      } else {
        "File metadata not available."
      }
    })

    # Data Summary
    output$data_summary <- renderPrint({
      req(uploaded_data())
      summary(uploaded_data())
    })
  })
}
