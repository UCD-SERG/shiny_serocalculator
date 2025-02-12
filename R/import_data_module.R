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
    div(style = "position:absolute;right:1em;",
        actionButton("inspect_next_btn", "Next", icon("paper-plane"),
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        tags$head(
          tags$style(HTML("hr {border-top: 1px solid #828994;}"))
        ),
    ),

    sidebarLayout(
      position = "left",
      sidebarPanel(
        width = 3,
        h4("Data Upload"),
        hr(),
        helpText("Use this section to upload the different types of data"),

        # choose data type
        pickerInput(ns("data_upload_type"),
          label = "Choose Type",
          choices = c("Pop Data", "Curve Data", "Noise Data"),
          selected = "Pop Data",
          choicesOpt = list(disabled = c(FALSE, TRUE, TRUE))
        ),

        # how to upload data
        uiOutput(ns("file_type")),

        # file upload
        uiOutput(ns("upload_type")),
        hr(),
        h5("POP DATA PARAMETERS",
          id = "pop_parameters",
          style = "font-weight: bold;"
        ),

        # select id
        uiOutput(ns("select_id")),

        # select age
        uiOutput(ns("select_age")),

        # select value
        uiOutput(ns("select_value")),

        # antigen type
        uiOutput(ns("antigen_type")),

        # curve antigen availability indicator
        uiOutput(ns("curve_antigen_display")),

        # noise availability indicator
        uiOutput(ns("noise_antigen_display")),

        # noise
        uiOutput(ns("average")),
        uiOutput(ns("noise_params")),
        hr(),
        h5("FILE UPLOAD INDICATOR",
          id = "dynamic_heading",
          style = "font-weight: bold;"
        ),

        # file upload indicators
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
        hr(),

        # clear environment button
        actionButton(ns("clear_btn"), "Clear Environment")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Data Requirements",
            htmlOutput(ns("data_requirement"))
          ),
          tabPanel(
            "File Preview",
            h3("Pop Data"),
            DT::DTOutput(ns("pop_preview")),
            h3("Curve Data"),
            DT::DTOutput(ns("curve_preview")),
            h3("Noise Data"),
            DT::DTOutput(ns("noise_preview")),
            DT::DTOutput(ns("other_head"))
          )
        )
      )
    )
  )
}

#' @param id identify namespace
#' @param uploaded_files list of uploaded files
import_data_server <- function(id,
                               pop_data,
                               curve_data,
                               noise_data) {
  moduleServer(id, function(input,
                            output,
                            session) {
    ns <- session$ns

    # Create a reactive value to store select_id
    selected_id <- reactiveVal(NULL)
    selected_age <- reactiveVal(NULL)
    selected_value <- reactiveVal(NULL)

    observeEvent(c(
      input$id_select,
      input$age_select,
      input$value_select
    ), {
      selected_id(input$id_select)
      selected_age(input$age_select)
      selected_value(input$value_select)
    })

    ############################## DATA SELECTION #######################################

    # select how to get data
    observeEvent(input$data_upload_type, {
      req(input$data_upload_type)
      if (input$data_upload_type == "Pop Data") {
        output$file_type <- renderUI({
          selectInput(
            inputId = ns("pop_type_ext"),
            label = "Choose Type",
            choices = c("Upload", "OSF"),
            selected = "Upload"
          )
        })
      } else if (input$data_upload_type == "Curve Data") {
        output$file_type <- renderUI({
          selectInput(
            inputId = ns("curve_type_ext"),
            label = "Choose Type",
            choices = c("Upload", "OSF"),
            selected = "Upload"
          )
        })
      } else if (input$data_upload_type == "Noise Data") {
        output$file_type <- renderUI({
          selectInput(
            inputId = ns("noise_type_ext"),
            label = "Choose Type",
            choices = c("Upload", "OSF", "Manual"),
            selected = "Upload"
          )
        })
      }
    })

    ############################### GET DATA ############################################

    # UPLOAD
    ## pop_data (upload)
    observeEvent(c(input$pop_upload), {
      if (!is.null(input$pop_upload)) {
        df <- read_data_file(input$pop_upload)
        pop_data(df)
      }
    })

    ## curve_data (upload)
    observeEvent(c(input$curve_upload), {
      if (!is.null(input$curve_upload)) {
        df <- read_data_file(input$curve_upload)
        curve_data(df)
      }
    })

    ## noise_data (upload)
    observeEvent(c(input$noise_upload), {
      if (!is.null(input$noise_upload)) {
        df <- read_data_file(input$noise_upload)
        noise_data(df)
      }
    })

    # OSF
    ## pop_data (OSF)
    observeEvent(input$pop_data_url_btn, {
      if (check_url(input$pop_data_url)) {
        withProgress(message = "Downloading...", value = 0, {
          incProgress(0.5)
          pop_data_result <- tryCatch(
            {
              serocalculator::load_pop_data(file_path = input$pop_data_url)
            },
            error = function(e) {
              showNotification("Error loading population data", type = "error")
              return(NULL)
            }
          )

          incProgress(1)

          if (!is.null(pop_data_result)) {
            pop_data(pop_data_result)
          }
        })
      } else {
        showNotification("Invalid URL. Please check and try again.", type = "error")
      }
    })

    ## curve_data (OSF)
    observeEvent(input$curve_data_url_btn, {
      if (check_url(input$curve_data_url)) {
        withProgress(message = "Downloading...", value = 0, {
          incProgress(0.5)
          curve_data_result <- tryCatch(
            {
              serocalculator::load_curve_params(file_path = input$curve_data_url)
            },
            error = function(e) {
              showNotification("Error loading curve data", type = "error")
              return(NULL)
            }
          )

          incProgress(1)

          if (!is.null(curve_data_result)) {
            curve_data(curve_data_result)
          }
        })
      } else {
        showNotification("Invalid URL. Please check and try again.", type = "error")
      }
    })

    ## noise_data (OSF)
    observeEvent(input$noise_data_url_btn, {
      if (check_url(input$noise_data_url)) {
        withProgress(message = "Downloading...", value = 0, {
          incProgress(0.5)
          noise_data_result <- tryCatch(
            {
              serocalculator::load_noise_params(file_path = input$noise_data_url)
            },
            error = function(e) {
              showNotification("Error loading noise data", type = "error")
              return(NULL)
            }
          )

          incProgress(1)

          if (!is.null(noise_data_result)) {
            noise_data(noise_data_result)
          }
        })
      } else {
        showNotification("Invalid URL. Please check and try again.", type = "error")
      }
    })

    # MANUAL
    ## noise_data (MANUAL)

    observeEvent(input$set_average, {
      req(input$y_low, input$y_high, input$eps, input$nu)
      df <- data.frame(
        y_low = input$y_low,
        y_high = input$y_high,
        eps = input$eps,
        nu = input$nu,
        stringsAsFactors = FALSE
      )

      noise_data(df)
    })

    ############################### PREVIEW DATA ########################################

    output$pop_preview <- renderDT({
      datatable(data = pop_data(), editable = TRUE)
    })


    output$curve_preview <- renderDT({
      datatable(data = curve_data(), editable = TRUE)
    })


    output$noise_preview <- renderDT({
      datatable(data = pop_data(), editable = TRUE)
    })

    ############################# DATA CAPTURE ###########################################
    output$upload_type <- renderUI({
      req(input$data_upload_type)

      if (input$data_upload_type == "Pop Data") {
        req(input$pop_type_ext)
        if (input$pop_type_ext == "Upload") {
          fileInput(
            inputId = ns("pop_upload"),
            label = "Choose (Pop Data) File from Computer (.csv, .rds)",
            buttonLabel = "Import Data",
            multiple = TRUE,
            accept = c(".csv", ".rds")
          )
        } else if (input$pop_type_ext == "OSF") {
          tagList(
            textInput(
              inputId = ns("pop_data_url"),
              label = "Provide OSF URL:"
            ),
            actionButton(
              inputId = ns("pop_data_url_btn"),
              label = "Import Data"
            )
          )
        }
      } else if (input$data_upload_type == "Curve Data") {
        req(input$curve_type_ext)
        if (input$curve_type_ext == "Upload") {
          fileInput(
            inputId = ns("curve_upload"),
            label = "Choose (Curve Data) File from Computer (.csv, .rds)",
            buttonLabel = "Import Data",
            multiple = TRUE,
            accept = c(".csv", ".rds")
          )
        } else if (input$curve_type_ext == "OSF") {
          tagList(
            textInput(
              inputId = ns("curve_data_url"),
              label = "Provide OSF URL:"
            ),
            actionButton(
              inputId = ns("curve_data_url_btn"),
              label = "Import Data"
            )
          )
        }
      } else if (input$data_upload_type == "Noise Data") {
        req(input$noise_type_ext)
        if (input$noise_type_ext == "Upload") {
          fileInput(
            inputId = ns("noise_upload"),
            label = "Choose (Noise Data) File from Computer (.csv, .rds)",
            buttonLabel = "Import Data",
            multiple = TRUE,
            accept = c(".csv", ".rds")
          )
        } else if (input$noise_type_ext == "OSF") {
          tagList(
            textInput(
              inputId = ns("noise_data_url"),
              label = "Provide OSF URL:"
            ),
            actionButton(
              inputId = ns("noise_data_url_btn"),
              label = "Import Data"
            )
          )
        } else if (input$noise_type_ext == "Manual") {
          tagList(
            numericInput(ns("y_low"), "y low:", value = 0.479),
            numericInput(ns("y_high"), "y high:", value = 5000000),
            numericInput(ns("eps"), "eps:", value = 0.259),
            numericInput(ns("nu"), "nu:", value = 2.60),
            actionButton(ns("set_average"), "Import Data")
          )
        }
      }
    })


    #########################################################################
    # file upload
    observeEvent(input$pop_upload, {
      req(input$data_upload_type == "Pop Data")
      if (!is.null(input$pop_upload)) {
        updatePickerInput(
          session = session,
          inputId = "data_upload_type",
          choices = c(
            "Pop Data",
            "Curve Data",
            "Noise Data"
          ),
          choicesOpt = list(
            disabled = c(FALSE, FALSE, TRUE)
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
          choices = c(
            "Pop Data",
            "Curve Data",
            "Noise Data"
          ),
          choicesOpt = list(
            disabled = c(FALSE, FALSE, FALSE)
          ),
          selected = "Curve Data"
        )
      }
    })
    ######################### FILE UPLOAD INDICATOR #############################################################
    observeEvent(
      c(
        pop_data(),
        curve_data(),
        noise_data()
      ),
      {
        if (!is.null(pop_data())) {
          shinyjs::runjs('document.getElementById("pop_data_indicator").style.backgroundColor = "MediumSeaGreen";')
        }
        if (!is.null(curve_data())) {
          shinyjs::runjs('document.getElementById("curve_data_indicator").style.backgroundColor = "MediumSeaGreen";')
        }
        if (!is.null(noise_data())) {
          shinyjs::runjs('document.getElementById("noise_data_indicator").style.backgroundColor = "MediumSeaGreen";')
        }
        if (is.null(pop_data()) &&
          is.null(curve_data()) &&
          is.null(noise_data())) {
          shinyjs::runjs('document.getElementById("status_circle").style.backgroundColor = "Tomato";')
        }
      }
    )

    ################################################################################################################

    # get antigen in pop_data
    observeEvent(input$pop_upload, {
      output$antigen_type <- renderUI({
        req(pop_data())

        if ("antigen_iso" %in% names(pop_data())) {
          checkboxGroupInput(
            inputId = ns("antigen_type_ext"),
            label = "Antigen Type",
            choices = unique(pop_data()$antigen_iso),
            selected = unique(pop_data()$antigen_iso)
          )
        } else {
          # Display a message if 'antigen_iso' is not in the data
          validate(need(FALSE, "Antigen-isotype variable ('antigen_iso') not present in data."))
        }
      })
    })

    observeEvent(input$noise_upload, {
      if (!is.null(input$noise_upload)) {
        output$noise_antigen_display <- renderUI({
          # Extract unique antigens
          antigens <- unique(pop_data()$antigen_iso)

          # Compare antigen sets and determine colors
          colors <- ifelse(
            antigens %in% unique(noise_data()$antigen_iso),
            "MediumSeaGreen",
            "Tomato"
          )

          # Call create_indicators
          create_indicators(
            n = length(antigens),
            colors = colors,
            label = "Noise Data",
            antigen_name = antigens
          )
        })
      }
    })


    observeEvent(input$curve_upload, {
      if (!is.null(input$curve_upload)) {
        output$curve_antigen_display <- renderUI({
          # Extract unique antigens
          antigens <- unique(pop_data()$antigen_iso)

          # Compare antigen sets and determine colors
          colors <- ifelse(
            antigens %in% unique(curve_data()$antigen_iso),
            "MediumSeaGreen",
            "Tomato"
          )

          # Call create_indicators
          create_indicators(
            n = length(antigens),
            colors = colors,
            label = "Curve Data",
            antigen_name = antigens
          )
        })
      }
    })
    #########################################################


    # UI to select age column in drop-down
    output$select_age <- renderUI({
      if (input$data_upload_type == "Pop Data") {
        req(input$pop_upload)

        # Read the uploaded file using the helper function
        df <- read_data_file(input$pop_upload)

        # Update the reactiveVal with the new pop data
        pop_data(df)

        # Get column names from pop_data
        cols <- names(pop_data())

        # Add a blank entry at the top for display
        cols <- c("", cols)

        # Dynamically create drop-down list of column names
        selectInput(
          inputId = ns("age_select"),
          "Select age variable:",
          choices = cols
        )
      }
    })

    # UI to select value column in drop-down
    output$select_value <- renderUI({
      if (input$data_upload_type == "Pop Data") {
        # Read the uploaded file using the helper function
        df <- read_data_file(input$pop_upload)

        # Update the reactiveVal with the new pop data
        pop_data(df)

        # Get column names from pop_data
        cols <- names(pop_data())

        # Add a blank entry at the top for display
        cols <- c("", cols)

        # Dynamically create drop-down list of column names
        selectInput(
          inputId = ns("value_select"),
          "Select quantitative antibody response variable:",
          choices = cols
        )
      }
    })

    # UI to select age column in drop-down
    output$select_id <- renderUI({
      if (input$data_upload_type == "Pop Data") {
        req(input$pop_upload)

        # Read the uploaded file using the helper function
        df <- read_data_file(input$pop_upload)

        # Update the reactiveVal with the new pop data
        pop_data(df)

        # Get column names from pop_data
        cols <- names(pop_data())

        # Add a blank entry at the top for display
        cols <- c("", cols)

        # Ensure there are valid choices
        validate(
          need(length(cols) > 0, "No valid columns available in the uploaded file.")
        )

        # Dynamically create drop-down list of column names
        selectInput(
          inputId = ns("id_select"),
          "Select participant ID variable:",
          choices = cols
        )
      } else if (input$data_upload_type == "Curve Data") {
        req(input$curve_upload)

        # Read the uploaded file using the helper function
        df <- read_data_file(input$curve_upload)

        # Update the reactiveVal with the new pop data
        curve_data(df)
      } else if (input$data_upload_type == "Noise Data") {
        req(input$noise_upload)

        # Read the uploaded file using the helper function
        df <- read_data_file(input$noise_upload)

        # Update the reactiveVal with the new pop data
        noise_data(df)
      }
    })

    ## Clear environment with confirmation
    observeEvent(input$clear_btn, {
      shinyalert::shinyalert(
        title = "Confirm Clear",
        text = "Are you sure you want to clear the environment? This action cannot be undone.",
        type = "warning",
        showCancelButton = TRUE,
        confirmButtonText = "Yes, Clear",
        cancelButtonText = "Cancel",
        callbackR = function(x) {
          if (x) {
            # clear indicators
            shinyjs::runjs('document.getElementById("pop_data_indicator").style.backgroundColor = "Tomato";')
            shinyjs::runjs('document.getElementById("curve_data_indicator").style.backgroundColor = "Tomato";')
            shinyjs::runjs('document.getElementById("noise_data_indicator").style.backgroundColor = "Tomato";')

            # set reactive objects to NULL
            pop_data(NULL)
            curve_data(NULL)
            noise_data(NULL)

            # clear environment
            rm(list = ls())

            # clear outputs
            output$est_incidence <- renderTable({
              NULL
            })
            output$stratify_by <- renderUI({
              NULL
            })
            output$antigen_type <- renderUI({
              NULL
            })
            output$visualize <- renderPlot({
              NULL
            })
            output$stratification <- renderUI({
              NULL
            })
            output$other_head <- renderDT({
              NULL
            })
            output$pop_preview <- NULL
            output$curve_preview <- NULL
            output$noise_preview <- NULL
            output$numeric_summary <- renderTable({
              NULL
            })
            output$noise_antigen_display <- renderUI({
              NULL
            })
            output$curve_antigen_display <- renderUI({
              NULL
            })
          }
        }
      )
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
        <p>
          <li><strong>Biological noise</strong>
              <p>typically comes from cross-reactivity with other molecules or other pathogens causing an overestimation
              of antibody concentration. In this case, biological noise needs to be pre-estimated using negative controls,
              typically using the 95th percentile of the distribution of antibody responses to the antigen-isotype in a
              population with no exposure.</p>
          </li>
          <li><strong>Measurement noise</strong>
            <p>epresents measurement error from the laboratory testing process (e.g. user differences in pipetting technique,
            random ELISA plate effects). It is defined by a CV (coefficient of variation) as the ratio of the standard deviation
            to the mean for replicates. Note that the CV should ideally be measured across plates rather than within the same plate.
            Measurement noise can over- or under-estimate antibody concentration. </p>
          </li>
        </p>
         <p>File limit: <strong>500MB</strong></p>")
    })



    return(list(
      selected_id = selected_id,
      selected_age = selected_age,
      selected_value = selected_value
    ))
  })
}
