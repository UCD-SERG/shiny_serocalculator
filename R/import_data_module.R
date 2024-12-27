
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

        # how to upload pop_data ("OSF" | "File Upload")
        uiOutput(ns("pop_type")),

        # file upload
        uiOutput(ns("pop_upload_type")),

        # noise
        uiOutput(ns("average")),

        uiOutput(ns("noise_params")),

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
            DT::DTOutput(ns("head")),
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

    # MODULE 0: Choose how to get pop_data
    # Select how to get pop data
    observeEvent(input$data_upload_type, {
      req(input$data_upload_type)

      if (input$data_upload_type == "Pop Data") {
        output$pop_type <- renderUI({
          selectInput(
            inputId = ns("pop_type_ext"),
            label = "Choose Type",
            choices = c("Upload", "OSF"),
            selected = "Upload"
          )
        })
      } else {
        output$pop_type <- renderUI(NULL) # Ensures UI element is cleared for non "Pop Data" file names
      }
    })



    output$pop_upload_type <- renderUI({
      req(input$pop_type_ext) # Ensure pop_type is available
      req(input$data_upload_type) # Ensure file_name is available

      if (input$data_upload_type == "Pop Data") {
        if (input$pop_type_ext == "Upload") {
          # File upload interface for Pop Data
          fileInput(
            inputId = ns("pop_upload"),
            label = "Choose (Pop Data) File from Computer (.csv, .rds)",
            buttonLabel = "Upload...",
            multiple = TRUE,
            accept = c(".csv", ".rds")
          )
        } else if (input$pop_type_ext == "OSF") {
          # URL input and download button for OSF
          tagList(
            textInput(
              inputId = ns("pop_data_url"),
              label = "Provide OSF URL:"
            ),
            actionButton(
              inputId = ns("pop_data_url_btn"),
              label = "Download Data"
            )
          )
        }
      } else if (input$data_upload_type == "Curve Data") {
        # File upload interface for Curve Data
        fileInput(
          inputId = ns("curve_upload"),
          label = "Choose (Curve Data) File from Computer (.csv, .rds)",
          buttonLabel = "Upload...",
          multiple = TRUE,
          accept = c(".csv", ".rds")
        )
      } else if (input$data_upload_type == "Noise Data") {
        radioButtons(ns("noise_choice"),
                     "Do you want to use average values:",
                     choices = c("Yes" = "yes", "No" = "no"),
                     selected = "no")
      }
    })

    # Render UI components for Noise Data when average values are used
    observeEvent(input$noise_choice, {
      req(input$noise_choice)
      if (input$noise_choice == "yes") {
        output$noise_params <- renderUI({
          tagList(
            numericInput(ns("y_low"), "y low:", value = 0.479),
            numericInput(ns("y_high"), "y high:", value = 5000000),
            numericInput(ns("eps"), "eps:", value = 0.259),
            numericInput(ns("nu"), "nu:", value = 2.60),
            textInput(ns("antigen"), "antigen:", value = "HlyE_IgA"),
            actionButton(ns("set_average"), "Set Averages")
          )
        })
      } else {
        output$noise_params <- renderUI({
          fileInput(
            inputId = ns("noise_upload"),
            label = "Choose File from Computer (.csv, .rds)",
            buttonLabel = "Upload...",
            multiple = TRUE,
            accept = c(".csv", ".rds")
          )
        })
      }
    })

    # MODULE 1: This should be module one (file upload)
    observeEvent(input$pop_upload, {
      req(input$data_upload_type == "Pop Data") # Check if "Pop Data" is selected
      if (!is.null(input$pop_upload)) {
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
    observeEvent(input$curve_upload, {
      req(input$data_upload_type == "Curve Data")
      if (!is.null(input$pop_upload)) {
        # Enable "Noise Data"
        updatePickerInput(
          session = session,
          inputId = "data_upload_type",
          choices = c("Pop Data", "Curve Data", "Noise Data"),
          choicesOpt = list(
            disabled = c(FALSE, FALSE, FALSE)
          ),
          selected = "Curve Data"
        )
      }
    })

    # MODULE 2: Coloring file upload
    observeEvent(c(input$noise_upload,
                   input$pop_upload,
                   input$curve_upload), {
      if (!is.null(input$pop_upload) && input$data_upload_type == "Pop Data") {
        shinyjs::runjs('document.getElementById("pop_data_indicator").style.backgroundColor = "MediumSeaGreen";')
      } else if (!is.null(input$curve_upload) && input$data_upload_type == "Curve Data") {
        shinyjs::runjs('document.getElementById("curve_data_indicator").style.backgroundColor = "MediumSeaGreen";')
      } else if (!is.null(input$noise_upload) && input$data_upload_type == "Noise Data"){
        shinyjs::runjs('document.getElementById("noise_data_indicator").style.backgroundColor = "MediumSeaGreen";')
      } else {
        shinyjs::runjs('document.getElementById("status_circle").style.backgroundColor = "Tomato";')
      }
    })

  # MODULE 5: Updates reactive objects with files uploaded
    observeEvent(c(
      input$noise_upload,
      input$curve_upload,
      input$pop_upload
    ), {

      output$head <- renderDT({
        if (input$data_upload_type == "Noise Data") {
          # Check if a file has been uploaded for Noise Data
          req(input$noise_upload)

          # Read the uploaded file using the helper function
          df <- read_data_file(input$noise_upload)

          # Update the reactiveVal with the new noise data
          noise_data(df)

          datatable(
            data = noise_data(),
            editable = TRUE
          )
        } else if (input$data_upload_type == "Curve Data") {
          # Check if a file has been uploaded for Curve Data
          req(input$curve_upload)

          # Read the uploaded file using the helper function
          df <- read_data_file(input$curve_upload)

          # Update the reactiveVal with the new curve data
          curve_data(df)

          datatable(
            data = curve_data(),
            editable = TRUE
          )
        } else if (input$data_upload_type == "Pop Data") {
          # Check if a file has been uploaded for Pop Data
          req(input$pop_upload)

          # Read the uploaded file using the helper function
          df <- read_data_file(input$pop_upload)

          # Update the reactiveVal with the new pop data
          pop_data(df)

          datatable(
            data = pop_data(),
            editable = TRUE
          )
        }
      })
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
      output$other_head <- renderDT({NULL})
      output$head <- renderDT({NULL})
      output$numeric_summary <- renderTable({NULL})

  })

    output$data_requirement <- renderText({
      HTML("<p> <strong> Required datasets </strong>

    The following data is required to perform the analyis the analysis. The data sets can be uploaded from your personal computer in .csv or .rds format,
    or linked from our OSF repositories (<a href=https://osf.io/ne8pc/>https://osf.io/ne8pc/</a>). The Noise Data can be uploaded, linked or entered manually.

        <ul>
          <li> <strong>Cross-sectional Population Data (Pop Data)</strong>
                  <p>A dataset with one row per sample and columns for antigen isotype, quantitative antibody results, and age in years.</p>
 <ul>
              <li>Additional columns and variables can be included for stratification</li>
              <li>Age unit is years, decimal points are fine</li>
              <li>The scale of the antibody response variable must be the same as the longituidnal antibody decay data (curve data) </li>
              <li>Do not upload any identifying health information </li>
   </p>
            </ul>
          <li><strong>Antibody Decay Curve Data (Curve Data) </strong></li>
          <p>A data set containing antibody decay curve parameters fit using a two-phase within-host
          Bayesian hierarchical framework obtaining predictive posterior samples using Markov chain Monte Carlo sampling.
          Note that variable names <u>must</u> follow these guidelines. For more information see <a href=https://onlinelibrary.wiley.com/doi/10.1002/sim.5322>Teunis (2012)</a>.
          The scale of y0 and y1 must be the same as the antibody response variable in the population data frame</p>
            <ul>
              <li>y0: baseline antibody level</li>
              <li>y1: antibody peak level </li>
              <li>t1: time from symptom onset to peak antibody response (in days) </li>
              <li>alpha: antibody decay rate (1/days for the current longitudinal parameter sets)</li>
              <li>r: shape factor of antibody decay</li>
            </ul>
        </p>
           <li> <strong>Noise Data</strong>
                  <p>A dataset containing the following variables, specifying noise parameters for each antigen isotype.
                  Note that variable names <u>must</u> follow these guidelines. For more information see <a hfref=https://onlinelibrary.wiley.com/doi/10.1002/sim.8578>Teunis (2020)</a>.
                  <ul>
                    <li>antigen_iso: antigen isotype whose noise parameters are being specified on each row</li>
                    <li>nu: biological noise</li>
                    <li>y.low: Lower limit of detection of the antibody assay</li>
                    <li>y.high: Upper limit of detection of the antibody assay</li>
                    <li>eps: measurement noise</li>
                  </ul></p>
         <p>File limit: <strong>500MB</strong></p>")
    })
  })



}
