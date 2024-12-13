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
        selectInput(
          ns("file_name"),
          "Choose Data:",
          choices = c("Pop Data", "Curve Data", "Noise Data"),
          selected = "Pop Data"
        ),
        uiOutput(ns("pop_type")),
        uiOutput(ns("pop_upload_type")),
        uiOutput(ns("pop_upload")),
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
                               noise_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Observe changes in updatedData and uploaded files
    observeEvent(c(
      input$updatedData,
      input$noise_upload,
      input$curve_upload,
      input$pop_upload
    ), {
      req(input$updatedData)

      output$head <- renderTable({
        if (input$updatedData == "Noise Data") {
          req(input$noise_upload)
          df <- read_data_file(input$noise_upload)
          noise_data(df)
          head(noise_data())
        } else if (input$updatedData == "Curve Data") {
          req(input$curve_upload)
          df <- read_data_file(input$curve_upload)
          curve_data(df)
          head(curve_data())
        } else if (input$updatedData == "Pop Data") {
          req(input$pop_upload)
          df <- read_data_file(input$pop_upload)
          pop_data(df)
          head(pop_data())
        }
      })
    })

    # UI for displaying data requirements
    output$data_requirement <- renderText({
      HTML("<p>Required datasets:
        <ul>
          <li> <strong>Cross-sectional Population Data (Pop Data)</strong>
              <p>A dataset with one row per sample and
              columns for antigen isotype,
              quantitative antibody results,
              and age in years. Additional columns and variables
              can be included for stratification.</p>
          </li>
          <li> <strong>Noise Data</strong>
              <p>A dataset containing the following variables,
              specifying noise parameters for each
              antigen isotype. Note that variable
              names <u>must</u> follow these guidelines.
              For more
              information see
              <a href=https://onlinelibrary.wiley.com/doi/10.1002/sim.8578>Teunis
              (2020)</a>.
              <ul>
                <li>antigen_iso: antigen isotype whose noise
                parameters are being specified on each
                row</li>
                <li>nu: biological noise</li>
                <li>y.low: Lower limit of detection of the antibody assay</li>
                <li>y.high: Upper limit of detection of the antibody assay</li>
                <li>eps: measurement noise</li>
              </ul></p>
          </li>
          <li><strong>Antibody Decay Curve Data</strong>
              <p>A dataset containing antibody decay curve parameters fit using a Bayesian hierarchical
              framework obtaining predictive posterior samples using Markov chain Monte Carlo
              sampling. Note that variable names <u>must</u> follow these guidelines. For more
              information see <a href=https://onlinelibrary.wiley.com/doi/10.1002/sim.5322>Teunis (2012)</a></p>
              <ul>
                <li>y0: baseline antibody level</li>
                <li>y1: antibody peak level (ELISA units)</li>
                <li>t1: duration of infection</li>
                <li>alpha: antibody decay rate (1/days for the current longitudinal parameter sets)</li>
                <li>r: shape factor of antibody decay</li>
              </ul>
          </li>
        </ul>
        </p>
        <p>File limit: <strong>500MB</strong></p>")
    })

    # UI for displaying uploaded data head
    output$head <- renderTable({
      req(input$updatedData) # Ensure an updated data type is selected
      if (input$updatedData == "Pop Data") {
        head(pop_data())
      } else if (input$updatedData == "Curve Data") {
        head(curve_data())
      } else if (input$updatedData == "Noise Data") {
        head(noise_data())
      }
    })

    # Dynamic UI for "pop_type"
    observeEvent(input$file_name, {
      req(input$file_name)
      if (input$file_name == "Pop Data") {
        output$pop_type <- renderUI({
          selectInput(ns("pop_typ"), "Choose Type", choices = c("Upload", "OSF"), selected = "Upload")
        })
      } else {
        output$pop_type <- renderUI(NULL)
      }
    })

    # Dynamic UI for Pop Data upload type
    output$pop_upload_type <- renderUI({
      req(input$file_name, input$pop_type)
      if (input$file_name == "Pop Data") {
        if (input$pop_type == "Upload") {
          fileInput(ns("pop_upload"), "Choose File from Computer (.csv, .rds)", buttonLabel = "Upload...", multiple = TRUE, accept = c(".csv", ".rds"))
        } else {
          tagList(
            textInput(ns("pop_data_url"), "Provide OSF URL:"),
            actionButton(ns("pop_data_url_btn"), "Download Data")
          )
        }
      }
    })

    # Dynamic UI for Curve Data upload
    output$curve_upload <- renderUI({
      req(input$file_name)
      if (input$file_name == "Curve Data") {
        fileInput(ns("curve_upload"), "Choose File from Computer (.csv, .rds)", buttonLabel = "Upload...", multiple = TRUE, accept = c(".csv", ".rds"))
      }
    })

    # Dynamic UI for Noise Data options
    observeEvent(input$file_name, {
      req(input$file_name)
      if (input$file_name == "Noise Data") {
        output$average <- renderUI({
          radioButtons(ns("noise_choice"), "Do you want to use average values:", choices = c("Yes" = "yes", "No" = "no"), selected = "no")
        })
      } else {
        output$average <- renderUI(NULL)
      }
    })

    observeEvent(input$noise_choice, {
      req(input$file_name == "Noise Data", input$noise_choice)
      if (input$noise_choice == "yes") {
        output$y_low <- renderUI({
          numericInput(ns("y_low"), "y low:", value = 0.479)
        })
        output$y_high <- renderUI({
          numericInput(ns("y_high"), "y high:", value = 5000000)
        })
        output$eps <- renderUI({
          numericInput(ns("eps"), "eps:", value = 0.259)
        })
        output$nu <- renderUI({
          numericInput(ns("nu"), "nu:", value = 2.60)
        })
        output$antigen <- renderUI({
          textInput(ns("antigen"), "antigen:", value = "HlyE_IgA")
        })
        output$provide_averages <- renderUI({
          actionButton(ns("set_average"), "Set Averages")
        })
      } else {
        output$y_low <- renderUI(NULL)
        output$y_high <- renderUI(NULL)
        output$eps <- renderUI(NULL)
        output$nu <- renderUI(NULL)
        output$antigen <- renderUI(NULL)
        output$provide_averages <- renderUI(NULL)
      }
    })

    # Busy spinner for long-running task
    observeEvent(input$stratify_by, {
      Sys.sleep(3)
      output$result <- renderText("Task completed")
    })

    # Handle file uploads and assign data
    observeEvent(input$pop_upload, {
      req(input$pop_upload)
      uploaded_files$files <- c(uploaded_files$files, "Pop Data")
      updateSelectInput(session, "updatedData", choices = uploaded_files$files)
    })

    # Observe the file upload for Curve Data
    observeEvent(input$curve_upload, {
      req(input$curve_upload)
      uploaded_files$files <- c(uploaded_files$files, "Curve Data")
      updateSelectInput(session, "updatedData", choices = uploaded_files$files)
      updateSelectInput(session, "updatedData_ext", choices = uploaded_files$files)
    })

    # Observe the file upload for Noise Data
    observeEvent(input$noise_upload, {
      req(input$noise_upload)
      uploaded_files$files <- c(uploaded_files$files, "Noise Data")
      updateSelectInput(session, "updatedData", choices = uploaded_files$files)
      updateSelectInput(session, "updatedData_ext", choices = uploaded_files$files)
    })

    # Handle OSF URL for Pop Data
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
  })
}
