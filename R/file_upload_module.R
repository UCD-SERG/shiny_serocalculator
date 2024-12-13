file_upload_ui <- function(id, label = "Upload File") {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), label, accept = c(".csv", ".xlsx", ".txt")),
    textOutput(ns("file_status"))
  )
}

# Module Server
file_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reactive to store uploaded data
    uploaded_data <- reactiveVal(NULL)

    observeEvent(input$file, {
      req(input$file)

      file_ext <- tools::file_ext(input$file$name)
      file_path <- input$file$datapath

      # Process file based on its extension
      data <- switch(file_ext,
        csv = read.csv(file_path),
        xlsx = readxl::read_excel(file_path),
        txt = read.delim(file_path),
        stop("Unsupported file type")
      )

      # Save the processed data in a reactive value
      uploaded_data(data)

      # Display a success message
      output$file_status <- renderText({
        paste("File uploaded successfully:", input$file$name)
      })
    })

    # Return the uploaded data
    return(uploaded_data)
  })
}
