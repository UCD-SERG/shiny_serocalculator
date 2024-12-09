import_data_ui <- function(id) {
  ns <- NS(id) # Namespace to differentiate inputs/outputs
  tabPanel(
    "Import Data",
    sidebarLayout(
      position = "left",
      sidebarPanel(
        width = 3,
        h4("Data Upload"),
        helpText("Use this section to upload the different types of data"),
        selectInput(
          ns("file_name"),
          "Choose Data:",
          choices = c("Pop Data", "Curve Data", "Noise Data"),
          selected = "Pop Data"
        ),
        uiOutput(ns("pop_type")),
        uiOutput(ns("pop_upload_type")),
        uiOutput(ns("curve_upload")),
        uiOutput(ns("average")),
        uiOutput(ns("antigen")),
        uiOutput(ns("y_low")),
        uiOutput(ns("y_high")),
        uiOutput(ns("eps")),
        uiOutput(ns("nu")),
        uiOutput(ns("provide_averages")),
        uiOutput(ns("select_age")),
        uiOutput(ns("age_selected")),
        uiOutput(ns("select_value")),
        uiOutput(ns("select_id")),
        uiOutput(ns("progress_bar")),
        selectInput(ns("updatedData"), "Uploaded Data", choices = NULL),
        actionButton(ns("clear_btn"), "Clear Environment"),
        textOutput(ns("status"))
      ),
      mainPanel(
        "",
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

import_data_server <- function(id,
                               uploaded_files,
                               pop_data,
                               curve_data,
                               noise_data)
  {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Observe changes in updatedData and uploaded files
    observeEvent(c(
      input$updatedData,
      input$noise_upload,
      input$curve_upload,
      input$pop_upload
    ), {
      # Ensure updatedData is available
      req(input$updatedData)
      output$head <- renderTable({
        if (input$updatedData == "Noise Data") {
          # Check if a file has been uploaded for Noise Data
          req(input$noise_upload)

          # Read the uploaded file using the helper function
          df <- read_data_file(input$noise_upload)

          # Update the reactiveVal with the new noise data
          noise_data(df)

          head(noise_data()) # Display the head of the noise data
        } else if (input$updatedData == "Curve Data") {
          # Check if a file has been uploaded for Curve Data
          req(input$curve_upload)

          # Read the uploaded file using the helper function
          df <- read_data_file(input$curve_upload)

          # Update the reactiveVal with the new curve data
          curve_data(df)

          head(curve_data()) # Display the head of the curve data
        } else if (input$updatedData == "Pop Data") {
          # Check if a file has been uploaded for Pop Data
          req(input$pop_upload)

          # Read the uploaded file using the helper function
          df <- read_data_file(input$pop_upload)

          # Update the reactiveVal with the new pop data
          pop_data(df)

          head(pop_data()) # Display the head of the pop data
        }
      })
    })


    output$data_requirement <- renderText({
      HTML("<p>Required datasets:

        <ul>
          <li> <strong>Cross-sectional Population Data (Pop Data)</strong>
                  <p>A dataset with one row per sample and columns for antigen isotype, quantitative antibody results, and age in years. Additional columns and variables can be included for stratification.</p>
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
          <li><strong>Antibody Decay Curve Data</strong></li>
          <p>A data set containing antibody decay curve parameters fit using a Bayesian hierarchical framework obtaining predictive posterior samples using Markov chain Monte Carlo sampling. Note that variable names <u>must</u> follow these guidelines. For more information see <a href=https://onlinelibrary.wiley.com/doi/10.1002/sim.5322>Teunis (2012)</a></p>
            <ul>
              <li>y0: baseline antibody level</li>
              <li>y1: antibody peak level (ELISA units)</li>
              <li>t1: duration of infection</li>
              <li>alpha: antibody decay rate (1/days for the current longitudinal parameter sets)</li>
              <li>r: shape factor of antibody decay</li>
            </ul>
        </ul>
        </p>
         <p>File limit: <strong>500MB</strong></p>")
    })

    # Display the data in the table
    output$head <- renderTable({
      if (!is.null(pop_data())) {
        head(pop_data())
      }
    })



    # Dynamic UI for "pop_type"
    observeEvent(input$file_name, {
      if (input$file_name == "Pop Data") {
        output$pop_type <- renderUI({
          selectInput(
            ns("pop_type"),
            "Choose Type",
            choices = c("Upload", "OSF"),
            selected = "Upload"
          )
        })
      } else {
        output$pop_type <- renderUI(NULL)
      }
    })

    # Dynamic file input or URL-based data selection
    output$pop_upload_type <- renderUI({
      req(input$file_name, input$pop_type)
      if (input$file_name == "Pop Data") {
        if (input$pop_type == "Upload") {
          fileInput(
            ns("pop_upload"),
            "Choose File from Computer (.csv, .rds)",
            buttonLabel = "Upload...",
            multiple = TRUE,
            accept = c(".csv", ".rds")
          )
        } else {
          tagList(
            textInput(ns("pop_data_url"), "Provide OSF URL:"),
            actionButton(ns("pop_data_url_btn"), "Download Data")
          )
        }
      }
    })

    output$curve_upload <- renderUI({
      req(input$file_name)

      if(input$file_name == "Curve Data"){
        fileInput(
          ns("curve_upload"),
          "Choose File from Computer (.csv, .rds)",
          buttonLabel = "Upload...",
          multiple = TRUE,
          accept = c(".csv", ".rds")
        )
      }
    })

    ## ------------------ NOISE DATA -------------------------------------------


  observeEvent(input$file_name, {
      req(input$file_name)

      if (input$file_name == "Noise Data") {
        output$average <- renderUI({
          radioButtons(ns("noise_choice"),
                       "Do you want to use average values:",
                       choices = c(
                         "Yes" = "yes",
                         "No" = "no"
                       ),
                       selected = "no"
          )
        })
      } else if (input$file_name == "Curve Data") {
        output$average <- renderUI({
          NULL
        })
      } else if (input$file_name == "Pop Data") {
        output$average <- renderUI({
          NULL
        })
      }
    })

    output$y_low <- renderUI({
      req(input$file_name, input$noise_choice)

      if (input$file_name == "Noise Data") {
        if (input$noise_choice == "yes") {
          numericInput("y_low", "y low:", value = 0.479)
        } else if (input$noise_choice == "no") {
          fileInput(
            "upload",
            "Choose File from Computer (.csv, .rds)",
            buttonLabel = "Upload...",
            multiple = TRUE,
            accept = c(".csv", ".rds")
          )
        }
      } else {
        NULL  # Render nothing if file name isn't "Noise Data"
      }
    })

    output$y_high <- renderUI({
      req(input$file_name, input$noise_choice)

      if (input$file_name == "Noise Data" && input$noise_choice == "yes") {
        numericInput("y_high", "y high:", value = 5000000)
      } else {
        NULL  # Render nothing for other cases
      }
    })

    output$eps <- renderUI({
      req(input$file_name, input$noise_choice)

      if (input$file_name == "Noise Data" && input$noise_choice == "yes") {
          numericInput("eps", "eps:", value = 0.259)
      } else {
        NULL  # Render nothing for other cases
      }
    })

    output$nu <- renderUI({
      req(input$file_name, input$noise_choice)

      if (input$file_name == "Noise Data" && input$noise_choice == "yes") {
          numericInput("nu", "nu:", value = 2.60)
      } else {
        NULL  # Render nothing for other cases
      }
    })

    output$antigen <- renderUI({
      req(input$file_name, input$noise_choice)

      if (input$file_name == "Noise Data" && input$noise_choice == "yes") {
          textInput("antigen", "antigen:", value = "HlyE_IgA")
      } else {
        NULL  # Render nothing for other cases
      }
    })

    output$provide_averages <- renderUI({
      req(input$file_name, input$noise_choice)

      if (input$file_name == "Noise Data" && input$noise_choice == "yes") {
          actionButton("set_average", "Set Averages")
      } else {
        NULL  # Render nothing for other cases
      }
    })

    #------------------------------------------------------------------------------
    #                     BUSY SPINNER
    #------------------------------------------------------------------------------

    observeEvent(input$stratify_by, {
      # Simulate a long-running task
      Sys.sleep(3)

      # Update the output
      output$result <- renderText("Task completed")
    })

    # Handle file uploads and assign data
    observeEvent(input$pop_upload, {
      req(input$pop_upload)
      uploaded_files$files <- c(uploaded_files$files, "Pop Data")
      updateSelectInput(session, "updatedData", choices = uploaded_files$files)
    })

    observeEvent(input$curve_upload, {
      req(input$curve_upload)

      uploaded_files$files <- c(uploaded_files$files, "Curve Data")
      updateSelectInput(session, "updatedData", choices = uploaded_files$files)
    })

    observeEvent(input$noise_upload, {
      req(input$noise_upload)

      uploaded_files$files <- c(uploaded_files$files, "Noise Data")
      updateSelectInput(session, "updatedData", choices = uploaded_files$files)
    })

    observeEvent(input$pop_data_url_btn, {
      req(input$pop_data_url)
      if (check_url(input$pop_data_url)) {
        withProgress(message = "Downloading...", value = 0.1, {
          pop_data(serocalculator::load_pop_data(input$pop_data_url))
          incProgress(1)
        })
        uploaded_files$files <- c(uploaded_files$files, "Pop Data")
        updateSelectInput(session, "updatedData", choices = uploaded_files$files)
        output$head <- renderTable({
          head(pop_data())
        })
      } else {
        showNotification("URL provided is not valid", type = "error")
      }
    })

    # Dynamic file preview
    observeEvent(input$updatedData, {
      output$head <- renderTable({
        if (input$updatedData == "Pop Data") {
          head(pop_data())
        } else if (input$updatedData == "Curve Data") {
          head(curve_data())
        } else if (input$updatedData == "Noise Data") {
          head(noise_data())
        }
      })
    })

    # Clear environment logic
    observeEvent(input$clear_btn, {
      uploaded_files$files <- NULL
      pop_data(NULL)
      curve_data(NULL)
      noise_data(NULL)
      updateSelectInput(session, "updatedData", choices = NULL)
    })
  })
}
