# load required packages
library(ggplot2)
library(ggthemes)
library(readxl)
library(skimr)
library(stringr)
library(serocalculator)
library(shinyWidgets)
library(tidyr)
library(scales)
library(ggthemes)
library(DT)
library(dplyr)
library(shinyjs)

server <- function(input, output, session) {

  # INFORMATION VARIABLES ----

  ## uploaded files ----
  uploaded_files <- reactiveValues(files = NULL)

  ## uploaded noise data (averages)
  noise_values <- reactiveValues(new_val = data.frame(
    antigen = character(),
    y_low = numeric(),
    y_high = numeric(),
    eps = numeric(),
    nu = numeric(),
    stringsAsFactors = FALSE
  ))

  ## file name ----
  filename <- reactiveVal(NULL)

  ## data df
  data_df <- reactiveVal(NULL)

  ## choose upload data ----
  data <- reactive({
    # ensure file is uploaded
    req(input$upload)
    req(input$file_name)

    # get file extension
    ext <- tools::file_ext(input$upload$name)

    if (ext %in% c("csv", "rds")) {
      if (ext == "csv") {
        df <- read.csv(input$upload$datapath)
      } else if (extension == "rds") {
        df <- readRDS(input$upload$datapath)
      }

      if (input$file_name == "Pop Data") {
        class(df) <-
          c("pop_data", class(df))

        # set age attribute
        attr(df, "age_var") <- input$age_select

        # set value attribute
        attr(df, "value_var") <- input$value_select

        # set id attribute
        attr(df, "id_var") <- input$id_select
      } else if (input$file_name == "Curve Data") {
        class(df) <-
          c("curve_data", class(df))

        # set age attribute
        attr(df, "age_var") <- input$age_select

        # set value attribute
        attr(df, "value_var") <- input$value_select

        # set id attribute
        attr(df, "id_var") <- input$id_select
      } else if (input$file_name == "Noise Data") {
        class(df) <-
          c("noise_data", class(df))

        # set age attribute
        attr(df, "age_var") <- input$age_select

        # set value attribute
        attr(df, "value_var") <- input$value_select

        # set id attribute
        attr(df, "id_var") <- input$id_select
      }

      return(df)
    } else {
      # Return NULL for unsupported file types
      return(NULL)
    }
  })

  ## uploaded column names ----
  column_names <- reactive({
    # ensure data is uploaded
    req(input$upload)

    # load file
    null_file <- is.null(input$upload$datapath)

    if (null_file) {
      return(NULL)
    }

    # read file
    df <- vroom::vroom(input$upload$datapath, delim = ",")

    # get column names
    df %>% names()
  })

  ## upload data via URL ----
  url_data <- reactive({
    # ensure data is uploaded
    req(input$url_input)

    if (!is.null(input$url_input) && input$url_input != "") {
      # download the file
      data <- serocalculator::load_pop_data(file_path = input$url_input)
    } else {
      "Please enter a valid URL."
    }
  })

  # PROCESSING FUNCTIONS ----

  # load uploaded data
  get_uploaded_data <- function(file_input) {
    # split
    name_of_file <- file_input %>%
      strsplit(split = " | ")

    # get file name
    extracted_file_name <- name_of_file[[1]][6]

    # file name sans extension
    file_sans_ext <- strsplit(
      x = extracted_file_name,
      split = "\\."
    )[[1]][1]

    # read data
    available_data <- read.csv(paste0(file_sans_ext, ".csv"))

    return(data.frame(available_data))
  }

  #  PROCESS INPUTS ----

  observeEvent(input$file_name, {
    req(input$file_name)

    output$get_files <- renderUI({
      if (input$file_name == "Pop Data" | input$file_name == "Curve Data") {
        # upload file
        fileInput(
          label = "Choose File from Computer (.csv, .rds)",
          "upload",
          NULL,
          buttonLabel = "Upload...",
          multiple = TRUE,
          accept = c(".csv", "rds")
        )
      }
    })
  })

  ## update list of uploaded files ----
  observeEvent(input$upload, {
    # ensure file is uploaded
    req(input$upload)

    # create list of uploaded files
    uploaded_files$files <- c(
      uploaded_files$files,
      paste0(input$file_name, " | Upload | ", input$upload$name)
    )

    # get file name without extension
    filename(tools::file_path_sans_ext(input$upload$name))

    # store uploaded file on server as CSV
    write.csv(data(), file = paste0(filename(), ".csv"), row.names = FALSE)

    # update drop down list of uploaded files
    updateSelectInput(session, "selectedData",
      choices = uploaded_files$files
    )

    # update drop down for uploaded data on secondary tab
    update_data <- updateSelectInput(session, "updatedData", choices = uploaded_files$files)
    null_update_data <- updateSelectInput(session, "updatedData", choices = NULL)

    # ------------------------------------------------------------------------------
    #                         updateData
    # ------------------------------------------------------------------------------
    if (input$file_name == "Pop Data") {
      if (!any(is.element(data() %>% names(), c("ageCat")))) {
        output$other_head <- renderDT({
          print("Error: THE DATA PROVIDED DOES NOT HAVE THE COLUMNS CONSISTENT WITH POPULATION DATA.")
        })

        null_update_data
      } else {
        update_data
      }
    } else if (input$file_name == "Curve Data") {
      if (!any(is.element(column_names(), c("y0", "y1", "t1", "alpha")))) {
        # output error
        output$other_head <- renderDT({
          print("Error: THE DATA PROVIDED DOES NOT HAVE THE COLUMNS CONSISTENT WITH CURVE DATA.")
        })

        # don't update drop down
        null_update_data
      } else {
        update_data
      }
    } else if (input$file_name == "Noise Data") {
      if (!any(is.element(column_names(), c("y.low", "eps", "y.high")))) {
        # output error
        output$other_head <- renderDT({
          print("Error: THE DATA PROVIDED DOES NOT HAVE THE COLUMNS CONSISTENT WITH CURVE DATA.")
        })

        # don't update drop down
        null_update_data
      } else {
        update_data
      }
    }

    #----------------------------------------------------------------------------------
    #                          updatedData_ext
    #----------------------------------------------------------------------------------

    # update drop down for uploaded data on secondary tab

    update_available_data <- updateSelectInput(session, "updatedData_ext", choices = uploaded_files$files)
    null_available_data <- updateSelectInput(session, "updatedData_ext", choices = NULL)

    if (input$file_name == "Pop Data") {
      if (!any(is.element(data() %>% names(), c("age","ageCat")))) {
        null_available_data
      } else {
        update_available_data
      }
    }

    # update list of available data
    if (input$file_name == "Curve Data") {
      if (!any(is.element(column_names(), c("y0", "y1", "t1", "alpha")))) {
        null_available_data
      } else {
        update_available_data
      }
    }

    if (input$file_name == "Noise Data") {
      if (!any(is.element(column_names(), c("y.low", "eps", "y.high")))) {
        null_available_data
      } else {
        update_available_data
      }
    }
  })

  ## choose uploaded data ----
  observeEvent(input$statsSelectedData, {
    # ensure file is uploaded
    req(input$statsSelectedData)

    # split
    name_of_file <- statsSelectedData %>%
      strsplit(split = " | ")

    file_name <- name_of_file[[1]][4]

    # file sans extension
    file_sans_ext <- strsplit(x = file_name, split = "\\.")[[1]][1]

    # read data
    get_uploaded_data <- read.csv(paste0(file_sans_ext, ".csv"))
  })


  ## display data when the button is clicked ----
  observeEvent(input$action_btn, {
    # download the file with progress indication
    withProgress(message = "Downloading...", value = 0, {
      for (i in 1:10) {
        incProgress(1 / 15)
        Sys.sleep(0.25)
      }

      # create list of uploaded files
      provided_file <- strsplit(x = input$url_input, split = "/")[[1]][6]

      # append uploaded files
      uploaded_files$files <- c(
        uploaded_files$files,
        paste0(
          input$file_name, " | OSF | ",
          provided_file
        )
      )

      # update drop down for uploaded data on secondary tab
      updateSelectInput(session,
        "updatedData_ext",
        choices = uploaded_files$files
      )
    })

    # get file name
    download_name <- input$url_input %>% strsplit(split = "/")

    # download file
    download.file(input$url_input,
      paste0(download_name[[1]][6], ".csv"),
      mode = "wb"
    )

    if (input$file_name == "Pop Data") {
      data <- serocalculator::load_pop_data(file_path = input$url_input)
    } else if (input$file_name == "Curve Data") {
      data <- serocalculator::load_curve_params(file_path = input$url_input)
    } else if (input$file_name == "Noise Data") {
      data <- serocalculator::load_noise_params(file_path = input$url_input)
    }

    # update reactive values
    data_df(data)

    # update drop down for uploaded data on secondary tab
    updateSelectInput(session,
      "updatedData",
      choices = uploaded_files$files
    )

    # display data
    output$head <- renderTable({
      data %>% head()
    })
  })

  # check file uploaded
  observeEvent(c(input$updatedData_ext), {
    req(input$updatedData_ext)

    # uploaded file
    file_type <- strsplit(x = input$updatedData_ext, split = " | ")[[1]][1]

    output$choose_visualization <- renderUI({
      if (file_type == "Pop") {
        selectInput("type_visualization",
          "Choose Type of Visualization",
          choices = c("Density", "Age Scatter")
        )
      } else if (file_type == "Curve") {
        selectInput("type_visualization",
          "Choose Type of Visualization",
          choices = c("Distribution", "Decay"),
          selected = "Distribution"
        )
      }
    })
  })

  ## check correct file is uploaded (pop, curve, noise)
  observeEvent(c(
    input$updatedData_ext,
    input$updatedData
  ), {
    # ensure file is uploaded
    req(input$updatedData_ext)

    output$log <- renderUI({
      # get uploaded data
      df <- data()

      # column names
      cols <- df %>% names()

      file_type <- strsplit(x = input$updatedData_ext, split = " | ")[[1]][1]

      # dynamically create drop down list of column name
      if (file_type == "Pop") {
        checkboxInput("check_log", "Log", value = TRUE)
      }
    })

    output$stratification_radio <- renderUI({

      # get uploaded data
      df <- data()

      # column names
      cols <- df %>% names()

      file_type <- strsplit(x = input$updatedData_ext, split = " | ")[[1]][1]

      # dynamically create drop down list of column name
      if (file_type == "Pop") {

        # choose stratification type
        radioButtons(
          inputId = "stratification_choice",
          label = "Do you want to stratify?",
          choices = list("Yes" = "yes",
                         "No" = "no"),
          selected = "yes"
        )
      }



    })

    output$stratify_option <- renderUI({
      # get uploaded data
      df <- data()

      # column names
      cols <- df %>% names()

      file_type <- strsplit(x = input$updatedData_ext, split = " | ")[[1]][1]

      # dynamically create drop down list of column name
      if (file_type == "Pop") {
        checkboxInput("check_stratify",
                    "Stratify",
                    value = TRUE)
      }
    })

    ## select age column on drop down
    output$select_age <- renderUI({
      if (input$file_name == "Pop Data") {
        # get uploaded data
        df <- data()

        # column names
        cols <- names(df)

        # dynamically create drop down list of column names
        selectInput(
          "age_select",
          "Select Age Column:",
          cols,
          selected = "age"
        )
      }
    })

    ## select value column on drop down
    observeEvent(input$file_name, {
      req(input$file_name)

      output$select_value <- renderUI({
        # get data
        df <- data()

        cols <- NULL

        if (input$file_name == "Pop Data" && "pop_data" %in% class(df)) {
          cols <- names(df)

          # dynamically create drop down list of column names
          selectInput(
            "value_select",
            "Select Value Column:",
            cols,
            selected = "value"
          )
        }

      })
    })

    ## select id column on drop down
    output$select_id <- renderUI({
      if (input$file_name == "Pop Data") {
        # get data
        df <- data()

        # column names
        cols <- names(df)

        # dynamically create drop down list of column names
        selectInput(
          "id_select",
          "Select Index Column:",
          cols,
          selected = "index_id"
        )
      }
    })

    output$other_head <- renderDT({
      # population data
      if (input$file_name == "Pop Data") {
        check_age <- all(is.element(column_names(), c("Country", "age", "ageCat")))

        if (!check_age) {
          print("Error: THE DATA PROVIDED DOES NOT HAVE THE COLUMNS CONSISTENT WITH POPULATION DATA.")
        } else {
          # display data
          datatable(
            data = data(),
            editable = TRUE
          )
        }
      } else if (input$file_name == "Curve Data") {
        # curve data check
        curve_data_check <- any(is.element(column_names(), c("y0", "y1", "t1", "alpha")))

        if (!curve_data_check) {
          print("Error: THE DATA PROVIDED DOES NOT HAVE THE COLUMNS CONSISTENT WITH CURVE DATA.")
        } else {
          datatable(
            data = data(),
            editable = TRUE
          )
        }
      } else if (input$file_name == "Noise Data") {
        # noise data check
        noise_data_check <- any(is.element(column_names(), c("y.low", "eps", "y.high")))

        if (!noise_data_check) {
          print("Error: THE DATA PROVIDED DOES NOT HAVE THE COLUMNS CONSISTENT WITH NOISE DATA.")
        } else {
          datatable(
            data = data(),
            editable = TRUE
          )
        }
      }
    })
  })

  ## file numeric summary ----
  observeEvent(input$updatedData_ext, {
    req(input$updatedData_ext)

    g <- get_uploaded_data(input$updatedData_ext)

    output$numeric_summary <- renderTable({
      g %>%
        head() %>%
        skimr::skim() %>%
        yank("numeric")
    })
  })

  # stratify
  observeEvent(c(input$stratification_choice,
                 input$updatedData_ext),{
    req(input$stratification_choice)
    req(input$updatedData_ext)

    output$stratification <- renderUI({
    # get uploaded data
    df <- data()

    # column names
    cols <- df %>% names()

    file_type <- strsplit(x = input$updatedData_ext, split = " | ")[[1]][1]


    if(input$stratification_choice == 'yes' && file_type == "Pop"){

        selectInput("choosen_stratification",
                    "Stratify By:",
                    df %>% names(),
                    selected = 'Country')
      }

    })

  })

  observeEvent(c(
    input$updatedData_ext,
    input$stratify,
    input$check_log,
    input$type_visualization,
    input$choosen_stratification,
    input$stratification_choice
  ), {
    req(input$updatedData_ext)
    req(input$check_log)
    req(input$type_visualization)
    req(input$choosen_stratification)
    req(input$stratification_choice)

    # get data
    down_data <- get_uploaded_data(file_input = input$updatedData_ext)

    ## visualization
    output$visualize <- renderPlot({

      viz_type <- input$type_visualization

      #### distribution plot
      if (viz_type == "Distribution") {
        down_data %>%
          pivot_longer(
            cols = `y0`:`r`,
            names_to = "parameter",
            values_to = "value"
          ) %>%
          ggplot2::ggplot(aes(x = value)) +
          geom_density() +
          facet_grid(parameter ~ .) +
          scale_y_continuous(limits = c(0, 0.0075)) +
          scale_x_continuous(limits = c(0, 600)) +
          theme_minimal()

        #### age-scatter plot
      } else if (viz_type == "Age Scatter") {

        if(input$stratification_choice == 'yes'){
          # visualize age-scatter
          down_data %>% serocalculator:::autoplot.pop_data(
            type = "age-scatter",
            strata = input$choosen_stratification,
            log = input$check_log
          )
        }
        else {
          # visualize age-scatter
          down_data %>% serocalculator:::autoplot.pop_data(
            type = "age-scatter",
            strata = NULL,
            log = input$check_log
          )
        }




        #### density
      } else if (viz_type == "Density") {

        if(input$stratification_choice == 'yes'){
          # visualize density
          down_data %>% serocalculator:::autoplot.pop_data(
            type = "density",
            strata = input$choosen_stratification,
            log = input$check_log
          )

        } else{
          # visualize density
          down_data %>% serocalculator:::autoplot.pop_data(
            type = "density",
            strata = NULL,
            log = input$check_log
          )

        }

        #### decay
      } else if (viz_type == "Decay") {
        # visualize age-scatter
        down_data %>% serocalculator:::plot_curve_params_one_ab()
      }
    })
  })

  observeEvent(input$stratify_by, {
    # Show busy spinner by simulating a long computation
    Sys.sleep(3)
    output$result <- renderText("Calculation Complete")
  })

  #-----------------------------------------------------------------------------
  #                     NOISE
  #-----------------------------------------------------------------------------

  observeEvent(input$file_name, {
    req(input$file_name)

    if (input$file_name == "Noise Data") {
      output$average <- renderUI({
        radioButtons("noise_choice", "Do you want to use average values:",
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

  observeEvent(c(input$noise_choice, input$file_name), {
    req(input$noise_choice)
    req(input$file_name)

    output$y_low <- renderUI({
      if (input$file_name == "Noise Data") {
        if (input$noise_choice == "yes") {
          numericInput("y_low", "y low:", value = 0.479)
        } else if (input$noise_choice == "no") {
          fileInput(
            label = "Choose File from Computer (.csv, .rds)",
            "upload",
            NULL,
            buttonLabel = "Upload...",
            multiple = TRUE,
            accept = c(".csv", "rds")
          )
        }
      }
    })
  })

  observeEvent(c(input$noise_choice, input$file_name), {
    req(input$noise_choice)
    req(input$file_name)

    output$y_high <- renderUI({
      if (input$file_name == "Noise Data") {
        if (input$noise_choice == "yes") {
          numericInput("y_high", "y high:", value = 5000000)
        }
      }
    })
  })

  # eps
  observeEvent(c(input$noise_choice, input$file_name), {
    req(input$noise_choice)
    req(input$file_name)

    output$eps <- renderUI({
      if (input$file_name == "Noise Data") {
        if (input$noise_choice == "yes") {
          numericInput("eps", "eps:", value = 0.259)
        }
      }
    })
  })

  # nu
  observeEvent(c(input$noise_choice, input$file_name), {
    req(input$noise_choice)
    req(input$file_name)

    output$nu <- renderUI({
      if (input$file_name == "Noise Data") {
        if (input$noise_choice == "yes") {
          numericInput("nu", "nu:", value = 2.60)
        }
      }
    })
  })

  # antigen
  observeEvent(c(input$noise_choice, input$file_name), {
    req(input$noise_choice)
    req(input$file_name)

    output$antigen <- renderUI({
      if (input$file_name == "Noise Data") {
        if (input$noise_choice == "yes") {
          textInput("antigen", "antigen:", value = "HlyE_IgA")
        }
      }
    })
  })

  # action button
  observeEvent(c(input$noise_choice, input$file_name), {
    req(input$noise_choice)
    req(input$file_name)

    output$provide_averages <- renderUI({
      if (input$file_name == "Noise Data") {
        if (input$noise_choice == "yes") {
          actionButton("set_average", "Set Averages")
        }
      }
    })
  })

  #-----------------------------------------------------------------------------
  #                     SEROCALCULATOR SUMMARY
  #-----------------------------------------------------------------------------

  output$output_html <- renderUI({
    HTML("<p>The <strong>serocalculator </strong>R package provides a rapid and computationally simple method for calculating seroconversion rates,
         as originally published in <cite><a href=https://onlinelibrary.wiley.com/doi/10.1002/sim.3592> Simonsen (2009) </a></cite> and <cite><a href=https://onlinelibrary.wiley.com/doi/10.1002/sim.8578>Teunis (2012) </a></cite>,
         and further developed in subsequent publications by <cite><a href=https://www.sciencedirect.com/science/article/pii/S1755436514000371?via%3Dihub>de Graaf (2014)</a></cite>,
         <cite><a href=https://www.sciencedirect.com/science/article/pii/S1755436516300135?via%3Dihub>Teunis (2016)</a></cite>, and <cite><a href=https://onlinelibrary.wiley.com/doi/10.1002/sim.8578>Teunis (2020)</a>.</cite> </p>

         <p>In short, longitudinal seroresponses from confirmed cases with a known symptom onset date are
         assumed to represent the time course of human serum antibodies against a specific pathogen. Therefore, by using these longitudinal
         antibody dynamics with any cross–sectional sample of the same antibodies in a human population, an incidence estimate can be calculated.</p>

         <p>Further details on the methodology can be found on the  <a href=https://ucd-serg.github.io/serocalculator/articles/serocalculator.html> main package website.  </a></p>

         <p>This app provides a user-friendly interface to use the serocalculator methodology without the need for specialized coding knowledge.
         Users should follow the steps to: </p>


         <ul>
            <li> Import the required datasets</li>
            <li> Inspect their data</li>
            <li> Estimate seroincidence</li>
            <li> Prepare a report (optional)</li>
        </ul> </p>
         <p>Required datasets:
         <ul>
            <li> Cross-sectional population-based dataset with age and quantitative antibody results</li>
            <li> Noise parameters </li>
            <li> Longitudinal curve parameters </li>
        </ul></p>
         <p>If you need assistance or encounter a clear bug, please file an issue with a minimal reproducible example on  <a href=https://github.com/UCD-SERG/serocalculator/issues> GitHub </p>")
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

  #------------------------------------------------------------------------------
  #                     BUSY SPINNER
  #------------------------------------------------------------------------------

  observeEvent(input$stratify_by, {
    # Simulate a long-running task
    Sys.sleep(3)

    # Update the output
    output$result <- renderText("Task completed")
  })

  # -----------------------------------------------------------------------------
  #                     ESTIMATE SEROINCIDENCE
  # ----------------------------------------------------------------------------

  # defaults noise values
  noise_data_params <- serocalculator::load_noise_params("https://osf.io/download/h64cw")

  # noise value from uploaded file
  observeEvent(c(input$file_name, input$upload), {
    req(input$file_name)
    req(input$upload)

    if (input$file_name == "Noise Data") {
      noise_data_params <- data()
    }
  })

  # noise value from UI
  observeEvent(input$set_average, {

    new_row <- data.frame(
      antigen = input$antigen,
      y_low = input$y_low,
      y_high = input$y_high,
      eps = input$eps,
      nu = input$nu,
      stringsAsFactors = FALSE
    )

  noise_data_params <- noise_values$new_val

  })

  # choose antigen type
  observeEvent(input$updatedData_ext,{
    req(input$updatedData_ext)


    output$antigen_type <- renderUI({

      # uploaded file
      file_type <- strsplit(x = input$updatedData_ext, split = " | ")[[1]][1]

      df <- data()

      # column names
      antigen_types <- df$antigen_iso %>% unique()

      if(file_type == 'Pop'){
        selectInput(
          inputId = "output_antigen",
          label = "Choose antigen Type:",
          choices = antigen_types,
          multiple = TRUE
        )
      }

    })
  })

  observeEvent(c(input$updatedData_ext,
                 input$stratification_type), {

    req(input$updatedData_ext,
        input$stratification_type)

    output$stratify_by <- renderUI({
      if(input$stratification_type == 'stratified'){

        # Fetch the uploaded data
        g <- get_uploaded_data(input$updatedData_ext)

        # Column names excluding specific columns
        cols <- g %>%
          select(where(~ !is.numeric(.))) %>%
          select(-antigen_iso) %>%
          names()

        # Conditionally display the selectInput based on available columns
        if (any(is.element(cols, c("age", "ageCat")))) {
          selectInput("stratify_by",
                      "Stratify By:",
                      choices = cols,
                      multiple = TRUE
          )
        } else {
          h4("No applicable columns for stratification found.")
        }
      }
    })
  })


  observeEvent(c(input$stratify_by,
                 input$stratification_type), {
    req(input$stratify_by)
    req(input$stratification_type)

    # create empty list
    for (i in 1:length(uploaded_files$files))
    {
      g <- get_uploaded_data(file_input = (uploaded_files$files[[i]]))

      if (any(is.element(g %>% names(), c("ageCat")))) {
        pop_data <- data.frame(g)
      } else if (any(is.element(g %>% names(), c("y0", "y1", "t1", "alpha")))) {
        curve_data <- data.frame(g)
      } else if (any(is.element(g %>% names(), c("y.low", "eps", "y.high")))) {
        noise_data <- data.frame(g)
      }
    }

    output$est_incidence <- renderTable({

      if(input$stratification_type == 'stratified'){


            est <- serocalculator::est.incidence.by(
              pop_data = pop_data,
              curve_params = curve_data,
              noise_params = noise_data_params,
              strata = input$stratify_by,
              antigen_isos = input$output_antigen,
              verbose = TRUE
            )




      } else if(input$stratification_type == 'overall')
      {
            est <- serocalculator::est.incidence(
              pop_data = pop_data,
              curve_params = curve_data,
              noise_params = noise_data_params,
              antigen_isos = input$output_antigen,
              verbose = TRUE
            )

      }

      summary(est)


    })

  })
}


