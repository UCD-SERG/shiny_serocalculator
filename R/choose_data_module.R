#' File Upload Management Module
#'
#' This Shiny module manages file uploads, allowing users to select a file type,
#' upload a CSV or RDS file, and store the uploaded file's data in a reactive object.
#' It also provides feedback on the success of the upload.
#'
#' @param id Module ID, a string that uniquely identifies the module instance.
#' @param uploaded_files A `reactiveValues` object to store all uploaded files.
#' @param pop_data A `reactiveVal` object to store population data.
#' @param curve_data A `reactiveVal` object to store curve data.
#' @param noise_data A `reactiveVal` object to store noise data.
#'
#' @return A list of reactive objects:
#'   - \code{uploaded_files}: A `reactiveValues` object containing uploaded file data.
#'   - \code{pop_data}: A `reactiveVal` containing population data.
#'   - \code{curve_data}: A `reactiveVal` containing curve data.
#'   - \code{noise_data}: A `reactiveVal` containing noise data.
#'
#' @details
#' ## Features:
#' - **Choose file upload type**: Users can select the type of data they want to upload (e.g., Pop Data, Curve Data, Noise Data).
#' - **Upload CSV or RDS file**: Supports file uploads in CSV or RDS format.
#' - **Store uploaded data**: Data is stored in the appropriate reactive object based on the selected type.
#' - **Feedback on upload success**: Updates the UI dynamically to indicate successful data uploaded.
choose_data_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # choose data type
    pickerInput(inputId = "data_upload_type",
                label = "Choose Type",
                choices = c("Pop Data", "Curve Data", "Noise Data"),
                selected = "Pop Data",
                choicesOpt = list(disabled = c(FALSE, TRUE, TRUE))
    ),

    # upload file
    fileInput(inputId = "file_upload",
              label = "Choose File from Computer (.csv, .rds)",
              buttonLabel = "Upload...",
              multiple = TRUE,
              accept = c(".csv", ".rds")
    ),

    # file upload Indicators
    div(
      id = "pop_data_indicator_container",
      style = "display: flex; align-items: center; margin-bottom: 10px;",
      div(
        id = "pop_data_indicator",
        style = "
            width: 25px;
            height: 25px;
            border-radius: 50%;
            background-color: Tomato;
            border: 1px solid white;
            display: inline-block;
            margin-right: 10px;
          "
      ),
      span("Pop Data", style = "font-size: 14px; font-weight: bold;")
    ),

    # file upload type indicators
    div(
      id = "curve_data_indicator_container",
      style = "display: flex; align-items: center; margin-bottom: 10px;",
      div(
        id = "curve_data_indicator",
        style = "
            width: 25px;
            height: 25px;
            border-radius: 50%;
            background-color: Tomato;
            border: 1px solid white;
            display: inline-block;
            margin-right: 10px;
          "
      ),
      span("Curve Data", style = "font-size: 14px; font-weight: bold;")
    ),

    div(
      id = "noise_data_indicator_container",
      style = "display: flex; align-items: center; margin-bottom: 10px;",
      div(
        id = "noise_data_indicator",
        style = "
            width: 25px;
            height: 25px;
            border-radius: 50%;
            background-color: Tomato;
            border: 1px solid white;
            display: inline-block;
            margin-right: 10px;
          "
      ),
      span("Noise Data", style = "font-size: 14px; font-weight: bold;")
    ),

  )

}

#' @param id identify namespace
#' @param uploaded_files list of uploaded files
#' @param pop_data population data
#' @param curve_data curve data
#' @param noise_data noise data
choose_data_server <- function(id, uploaded_files, pop_data, curve_data, noise_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # # Enable "Curve Data" after uploading "Pop Data"
    # observeEvent(input$file_upload, {
    #   req(input$data_upload_type == "Pop Data")  # Ensure "Pop Data" is selected
    #   if (!is.null(input$file_upload)) {
    #     # Enable "Curve Data"
    #     updatePickerInput(
    #       session = session,
    #       inputId = "data_upload_type",
    #       choices = c("Pop Data", "Curve Data", "Noise Data"),
    #       choicesOpt = list(
    #         disabled = c(FALSE, FALSE, TRUE)  # Enable "Curve Data", keep "Noise Data" disabled
    #       ),
    #       selected = "Pop Data"  # Ensure "Pop Data" remains selected
    #     )
    #   }
    # })

    # # Enable "Noise Data" after uploading "Curve Data"
    # observeEvent(input$file_upload, {
    #   req(input$data_upload_type == "Curve Data") # Check if "Curve Data" is selected
    #   if (!is.null(input$file_upload)) {
    #     # Enable "Noise Data"
    #     updatePickerInput(
    #       session = session,
    #       inputId = "data_upload_type",
    #       choices = c("Pop Data", "Curve Data", "Noise Data"),
    #       choicesOpt = list(
    #         disabled = c(FALSE, FALSE, FALSE) # Enable all
    #       ),
    #       selected = "Curve Data"
    #     )
    #   }
    # })

    # ReactiveValues to store uploaded data
    uploaded_files <- reactiveValues(files = list())
    pop_data <- reactiveVal(NULL)
    curve_data <- reactiveVal(NULL)
    noise_data <- reactiveVal(NULL)

    # Read uploaded data based on file type
    data <- reactive({
      req(input$file_upload)
      ext <- tools::file_ext(input$file_upload$name)

      switch(ext,
             "csv" = read.csv(input$file_upload$datapath),
             "rds" = readRDS(input$file_upload$datapath),
             stop("Unsupported file type")
      )
    })

    observeEvent(input$file_upload, {
      req(input$file_upload, input$data_upload_type)

      if (input$data_upload_type == "Pop Data") {
        updatePickerInput(session, inputId = "data_upload_type",
                          choices = c("Pop Data", "Curve Data", "Noise Data"),
                          choicesOpt = list(disabled = c(FALSE, FALSE, TRUE)),
                          selected = "Pop Data"
        )
        shinyjs::runjs(sprintf('document.getElementById("%s").style.backgroundColor = "MediumSeaGreen";', ns("pop_data_indicator")))
      } else if (input$data_upload_type == "Curve Data") {
        updatePickerInput(session, inputId = "data_upload_type",
                          choices = c("Pop Data", "Curve Data", "Noise Data"),
                          choicesOpt = list(disabled = c(FALSE, FALSE, FALSE)),
                          selected = "Curve Data"
        )
        shinyjs::runjs(sprintf('document.getElementById("%s").style.backgroundColor = "MediumSeaGreen";', ns("curve_data_indicator")))
      } else if (input$data_upload_type == "Noise Data") {
        shinyjs::runjs(sprintf('document.getElementById("%s").style.backgroundColor = "MediumSeaGreen";', ns("noise_data_indicator")))
      }
    })


    # Return reactive values for use outside the module
    return(list(
      uploaded_files = uploaded_files,
      pop_data = pop_data,
      curve_data = curve_data,
      noise_data = noise_data
    ))
  })
}
