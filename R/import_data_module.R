
#' @importFrom shiny renderText
#' @importFrom shiny HTML
#' @importFrom shiny fileInput
#' @importFrom shiny tagList
#' @importFrom shiny renderText
#' @importFrom shiny textInput
#' @importFrom shiny actionButton
#' @importFrom shiny numericInput
#' @importFrom shiny updateSelectInput
#' @importFrom shiny withProgress
#' @importFrom shiny incProgress
#' @importFrom shiny showNotification
#'
#' @param id define namespace
import_data_ui <- function(id) {
  ns <- shiny::NS(id)
  tabPanel(
    "Import Data",
    sidebarLayout(
      position = "left",
      sidebarPanel(
        width = 3,
        h4("Data Upload"),

        helpText("Use this section to upload the different types of data"),

        # choose data type
        pickerInput(ns("data_upload_type"),
                    label = "Choose Type",
                    choices = c("Pop Data", "Curve Data", "Noise Data"),
                    selected = "Pop Data",
                    choicesOpt = list(disabled = c(FALSE, TRUE, TRUE))
                    ),

        # file upload
        fileInput(ns("file_upload"),
                  "Choose File from Computer (.csv, .rds)",
                  buttonLabel = "Upload...",
                  multiple = TRUE,
                  accept = c(".csv", ".rds")
        ),

        # File Upload Indicators
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

        # clear environment button
        actionButton("clear_btn", "Clear Environment")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Data Requirements", htmlOutput(ns("data_requirement"))),
          tabPanel(
            "File Preview",
            tableOutput(ns("head")),
            DT::DTOutput(ns("other_head"))
          )
        )
      )
    )
  )
}

#' @param id identify namespace
#' @param uploaded_files list of uploaded files
#' @param pop_data population data
#' @param curve_data curve data
#' @param noise_data noise data
import_data_server <- function(id,
                               uploaded_files,
                               pop_data,
                               curve_data,
                               noise_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive object to hold uploaded data
    data <- reactive({
      req(input$file_upload)
      ext <- tools::file_ext(input$upload$name)

      # Read the data based on the file extension
      switch(ext,
             "csv" = read.csv(input$upload$datapath),
             "rds" = readRDS(input$upload$datapath),
             return(NULL) # Return NULL for unsupported extensions
      )
    })

    uploaded_files <- reactiveValues(files = NULL)
    pop_data <- reactiveVal(NULL)
    curve_data <- reactiveVal(NULL)
    noise_data <- reactiveVal(NULL)


    # MODULE 1: This should be module one (file upload)
    observeEvent(input$file_upload, {
      req(input$data_upload_type == "Pop Data") # Check if "Pop Data" is selected
      if (!is.null(input$file_upload)) {
        # Enable "Curve Data"
        updatePickerInput(
          session = session,
          inputId = "data_upload_type",
          choices = c("Pop Data", "Curve Data", "Noise Data"),
          choicesOpt = list(
            disabled = c(FALSE, FALSE, TRUE) # Enable "Curve Data"
          ),
          selected = "Pop Data"
        )
      }
    })

    # Enable "Noise Data" after uploading "Curve Data"
    observeEvent(input$file_upload, {
      req(input$data_upload_type == "Curve Data") # Check if "Curve Data" is selected
      if (!is.null(input$file_upload)) {
        # Enable "Noise Data"
        updatePickerInput(
          session = session,
          inputId = "data_upload_type",
          choices = c("Pop Data", "Curve Data", "Noise Data"),
          choicesOpt = list(
            disabled = c(FALSE, FALSE, FALSE) # Enable all
          ),
          selected = "Curve Data"
        )
      }
    })

    # MODULE 2: Coloring file upload
    observeEvent(input$file_upload, {
      req(input$file_upload)
      if (!is.null(input$file_upload) && input$data_upload_type == "Pop Data") {
        shinyjs::runjs('document.getElementById("pop_data_indicator").style.backgroundColor = "MediumSeaGreen";')
      } else if (!is.null(input$file_upload) && input$data_upload_type == "Curve Data") {
        shinyjs::runjs('document.getElementById("curve_data_indicator").style.backgroundColor = "MediumSeaGreen";')
      } else if (!is.null(input$file_upload) && input$data_upload_type == "Noise Data"){
        shinyjs::runjs('document.getElementById("noise_data_indicator").style.backgroundColor = "MediumSeaGreen";')
      } else {
        shinyjs::runjs('document.getElementById("status_circle").style.backgroundColor = "Tomato";')
      }
    })

    # MODULE 3: Clear Environment
    ## clear environment
    observeEvent(input$clear_btn, {
      req(input$clear_btn)

      # clear indicators
      shinyjs::runjs('document.getElementById("pop_data_indicator").style.backgroundColor = "Tomato";')
      shinyjs::runjs('document.getElementById("curve_data_indicator").style.backgroundColor = "Tomato";')
      shinyjs::runjs('document.getElementById("noise_data_indicator").style.backgroundColor = "Tomato";')

      # clear file upload
      updateFileInput(session, "file_upload", label = "Upload a File", value = NULL)

      # set reactive objects to NULL
      pop_data(NULL)
      curve_data(NULL)
      noise_data(NULL)

      # clear enviroment
      rm(list = ls())

      #clear dropdown files
      uploaded_files$files <- setdiff(uploaded_files$files, "Pop Data")
      uploaded_files$files <- setdiff(uploaded_files$files, "Noise Data")
      uploaded_files$files <- setdiff(uploaded_files$files, "Curve Data")

      updateSelectInput(session, "selectedData", choices = uploaded_files$files)
      updateSelectInput(session, "updatedData", choices = uploaded_files$files)
      updateSelectInput(session, "updatedData_ext", choices = uploaded_files$files)


      # clear outputs
      output$est_incidence <- renderTable({ NULL})
      output$stratify_by <- renderUI({NULL})
      output$antigen_type <- renderUI({NULL})
      output$visualize <- renderPlot({NULL})
      output$stratification <- renderUI({NULL})
      output$stratification <- renderUI({NULL})
      output$other_head <- renderTable({NULL})
      output$head <- renderTable({NULL})
      output$numeric_summary <- renderTable({NULL})


    })
  })

  # MODULE 4: Update reactive objects to hold uploaded data
  # Observe changes in updatedData and uploaded files
  observeEvent(c(
    input$updatedData,
    input$noise_upload,
    input$curve_upload,
    input$pop_upload,
    input$file_upload
  ), {
    req(input$file_upload)

    output$head <- renderDT({
      if (input$file_upload == "Noise Data") {
        df <- read_data_file(input$file_upload)
        noise_data(df)
        head(noise_data())
      } else if (input$file_upload == "Curve Data") {
        df <- read_data_file(input$file_upload)
        curve_data(df)
        head(curve_data())
      } else if (input$file_upload == "Pop Data") {
        df <- read_data_file(input$file_upload)
        pop_data(df)
        head(pop_data())
      }
    })
  })



}
