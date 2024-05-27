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


server <- function(input, output, session) {
  # INFORMATION VARIABLES ----

  ## uploaded files ----
  uploaded_files <- reactiveValues(files = NULL)

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

    if (ext %in% c("csv", "tsv")) {
      if (ext == "csv") {
        df <- read.csv(input$upload$datapath)
      } else if (extension == "tsv") {
        df <- read_tsv(input$upload$datapath)
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

      # write
      # file_write = paste0(tools::file_path_sans_ext(input$upload$name,".csv"))
      # write.csv(x = file_write, file = df, row.names = F)

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

    # # population data check
    # pop_data_check <- any(is.element(
    #   names(available_data),
    #   c("ageCat")
    # ))
    #
    # if (!pop_data_check) {
    #   # set class
    #   class(available_data) <-
    #     c("pop_data", class(available_data))
    #
    #   # check curve data
    # } else if (!any(is.element(
    #   el = names(available_data),
    #   set = c("y0", "y1", "t1", "alpha")
    # ))) {
    #   # set class
    #   class(available_data) <-
    #     c("curve_data", class(available_data))
    #
    #   # noise data check
    #   noise_data_check <- any(is.element(column_names(), c("y.low", "eps", "y.high")))
    # } else if (!noise_data_check) {
    #   class(available_data) <-
    #     c("noise_data", class(available_data))
    # }

    return(data.frame(available_data))
  }

  #  PROCESS INPUTS ----

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
      if (!any(is.element(data() %>% names(), c("ageCat")))) {
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
    # req(input$file_name)
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

  ## check correct file is uploaded (pop, curve, noise) ----
  observeEvent(c(
    input$updatedData_ext,
    input$updatedData
  ), {
    # ensure file is uploaded
    req(input$updatedData_ext)

    file_type <- strsplit(x = input$updatedData_ext, split = " | ")[[1]][1]

    output$stratify <- renderUI({
      # get uploaded data
      df <- data()

      # column names
      cols <- df %>% names()

      # dynamically create drop down list of column name
      if (file_type == "Pop") {
        selectInput("stratify",
          "Stratify By:",
          cols,
          selected = "Country"
        )
      } else if (file_type == "Curve") {
        NULL
      }
    })

    output$stratify_by <- renderUI({
      # get uploaded data
      df <- data()

      # column names
      cols <- df %>%
        names()

      # dynamically create drop down list of column name
      if (file_type == "Pop") {
        selectInput("stratify_by",
          "Stratify By:",
          cols,
          selected = "Country"
        )
      } else {
        NULL
      }
    })

    output$log <- renderUI({
      # get uploaded data
      df <- data()

      # column names
      cols <- df %>% names()

      # dynamically create drop down list of column name
      if (file_type == "Pop") {
        checkboxInput("check_log", "Log", value = TRUE)
      } else if (file_type == "Curve") {
        NULL
      }
    })

    output$other_head <- renderDT({
      # population data
      if (input$file_name == "Pop Data") {
        check_age <- any(is.element(column_names(), c("ageCat")))

        if (!check_age) {
          print("Error: THE DATA PROVIDED DOES NOT HAVE THE COLUMNS CONSISTENT WITH POPULATION DATA.")
        } else {
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
            } else {
              NULL
            }
          })


          ## select value column on drop down
          output$select_value <- renderUI({
            if (input$file_name == "Pop Data") {
              # get data
              df <- data()

              # column names
              cols <- names(df)

              # dynamically create drop down list of column names
              selectInput(
                "value_select",
                "Select Value Column:",
                cols,
                selected = "value"
              )
            } else {
              NULL
            }
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
            } else {
              NULL
            }
          })

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

  observeEvent(c(
    input$updatedData_ext,
    input$stratify,
    input$check_log,
    input$type_visualization
  ), {
    req(input$updatedData_ext)
    req(input$stratify)
    req(input$check_log)
    req(input$type_visualization)

    # get data
    down_data <- get_uploaded_data(file_input = input$updatedData_ext)

    ## visualization
    output$visualize <- renderPlot({
      viz_type <- input$type_visualization

      #### density plot
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
        # visualize age-scatter
        down_data %>% serocalculator:::autoplot.pop_data(
          type = "age-scatter",
          strata = input$stratify,
          log = input$check_log
        )

        #### distribution
      } else if (viz_type == "Density") {
        # visualize density
        down_data %>% serocalculator:::autoplot.pop_data(
          type = "density",
          strata = input$stratify,
          log = input$check_log
        )

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
  #                     HTML OUTOUT
  #-----------------------------------------------------------------------------

  output$output_html <- renderUI({
    HTML("<p>The <strong>serocalculator </strong>R package provides a rapid and computationally simple method for calculating seroconversion rates,
         as originally published in <cite>Simonsen, 2009</cite> and <cite>Teunis, 2012</cite>, and further developed in subsequent publications by
         <cite>de_Graaf, 2014</cite>, <cite>Teunis_2016</cite>, and <cite>Teunis, 2020.</cite> </p>

         <p>In short, longitudinal seroresponses from confirmed cases with a known symptom onset date are
         assumed to represent the time course of human serum antibodies against a specific pathogen. Therefore, by using these longitudinal
         antibody dynamics with any cross–sectional sample of the same antibodies in a human population, an incidence estimate can be calculated.</p>

         <p>Further details on the methodology can be found on the  <a href=https://ucd-serg.github.io/serocalculator/articles/serocalculator.html>main package website.  </a></p>

         <p>This app provides a user-friendly interface to use the serocalculator methodology without the need for specialized coding knowledge.
         Users should follow the steps to: </p>


         <ul>
            <li> 1). Import the required datasets</li>
            <li> 2). Inspect their data,</li>
            <li> 3). Estimate seroincidence</li>
            <li> 4). Prepare a report (optional)</li>
        </ul> </p>
         <p>Required datasets:
         <ul>
            <li> 1). Cross-sectional dataset with age, quantitative antibody  </li>
            <li> 2). Noise parameters </li>
            <li> 3). Longitudinal curve parameters </li>
        </ul></p>
         <p>If you need assistance or encounter a clear bug, please file an issue with a minimal reproducible example on  <a href=https://github.com/UCD-SERG/serocalculator/issues> GitHub </p>")
  })

  output$data_requirement <- renderText({
    HTML("<p>Required datasets:

        <ul>
          <li> <strong>Population (Pop) Data </strong>
                  <p>A dataset with cross-sectional serology data per antibody and age, and additional columns</p>
          <li> <strong>Noise Data</strong>
                  <p>A dataset containing the following variables, specifying noise parameters for each antigen isotype:
                  <ul>
                    <li>antigen_iso: antigen isotype whose noise parameters are being specified on each row</li>
                    <li>nu: biological noise</li>
                    <li>eps: measurement noise</li>
                  </ul></p>
          <li><strong>Curve Data</strong></li>
          <p>A data setcontaining MCMC samples of antibody decay curve parameters from the Bayesian posterior distribution of a longitudinal decay curve model</p>
            <ul>
              <li>y0: baseline antibody level</li>
              <li>y1: antibody peak level (ELISA units)</li>
              <li>t1: duration of infection</li>
              <li>alpha: antibody decay rate (1/days for the current longitudinal parameter sets)</li>
              <li>r: shape factor of antibody decay</li>
            </ul>
        </ul>
        </p>")
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

  observeEvent(input$stratify_by, {
    req(input$stratify_by)

    output$est_incidence <- renderTable({
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

      est <- serocalculator::est.incidence.by(
        pop_data = pop_data,
        curve_params = curve_data,
        noise_params = noise_data,
        strata = input$stratify_by
      )


      summary(est)
    })
  })
}
