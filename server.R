
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

server <- function(input, output, session) {

  # INFORMATION VARIABLES ----

  ## uploaded files ----
  uploaded_files <- reactiveValues(files = NULL)

  ## file name ----
  filename <- reactiveVal(NULL)

  ## progress ----
  progress <- reactiveVal(0)

  ## data df
  data_df <- reactiveVal(NULL)

  ## choose upload data ----
  data <- reactive({

    # ensure file is uploaded
    req(input$upload)

    # get file extension
    ext <- tools::file_ext(input$upload$name)

    if (ext %in% c("csv", "tsv")) {
      if (ext == "csv")
      {
        df <- read.csv(input$upload$datapath)
      } else if (extension == "tsv")
      {
        df <- read_tsv(input$upload$datapath)
      }

      return(df)

    } else
    {
      # Return NULL for unsupported file types
      return(NULL)
    }

  })

  ## uploaded column names ----
  column_names <- reactive({

    # ensure data is uploaded
    req(input$upload)

    # load file
    null_file = is.null(input$upload$datapath)

    if (null_file)
    {
      return(NULL)
    }

    # read file
    df = vroom::vroom(input$upload$datapath, delim = ",")

    # get column names
    df %>% names()

  })

  ## available data ----
  available_data <- reactive({

    req(input$availableData)

    get_uploaded_data(file_input = input$availableData)

  })

  ## upload data via URL ----
  url_data <- reactive({

    # ensure data is uploaded
    req(input$url_input)

    if (!is.null(input$url_input) && input$url_input != "")
    {
      # download the file
      data <- serocalculator::load_pop_data(file_path = input$url_input)

    } else
    {
      "Please enter a valid URL."
    }

  })

  # PROCESSING FUNCTIONS ----

  # load uploaded data
  get_uploaded_data <- function(file_input){

    # split
    name_of_file = file_input %>%
      strsplit(split = " | ")

    # get file name
    extracted_file_name = name_of_file[[1]][6]

    # file name sans extension
    file_sans_ext = strsplit(x = extracted_file_name,
                              split = "\\.")[[1]][1]

    # read data
    available_data <- read.csv(paste0(file_sans_ext, ".csv"))

    # population data check
    pop_data_check = any(is.element(names(available_data),
                                    c('ageCat')))

    if(!pop_data_check)
    {
      # set class
      class(available_data) =
        c("pop_data", class(available_data))

    # check curve data
    } else if(!any(is.element(el = names(available_data),
                              set =  c('y0','y1','t1','alpha'))))
    {
      # set class
      class(available_data) =
        c("curve_data", class(available_data))

    # noise data check
    noise_data_check = any(is.element(column_names(),c('y.low','eps','y.high')))

    } else  if(!noise_data_check)
    {
      class(available_data) =
        c("noise_data", class(available_data))
    }

    return(available_data)

  }

  #  PROCESS INPUTS ----

  ## update list of uploaded files ----
  observeEvent(input$upload, {

    # ensure file is uploaded
    req(input$upload)

    # create list of uploaded files
    uploaded_files$files <- c(uploaded_files$files,
                              paste0(input$file_name," | Upload | ",input$upload$name))

    # get file name without extension
    filename(tools::file_path_sans_ext(input$upload$name))

    ## store uploaded file on server as CSV
    filename <- paste0("uploaded_data", ".csv")
    write.csv(data(), file = paste0(filename(), ".csv"),
              row.names = FALSE)

    # read data
    up_data = read.csv(file = "uploaded_data.csv")

    # update drop down list of uploaded files
    updateSelectInput(session, "selectedData",
                      choices = uploaded_files$files)

    # update drop down for uploaded data on secondary tab
    updateSelectInput(session, "updatedData",
                      choices = uploaded_files$files)

    # update drop down for uploaded data on secondary tab
    updateSelectInput(session, "updatedData_ext",
                      choices = uploaded_files$files)

    # update list of available data
    updateSelectInput(session, "availableData",
                      choices = uploaded_files$files)

  })

  ## choose uploaded data ----
  observeEvent(input$statsSelectedData, {

    # ensure file is uploaded
    req(input$statsSelectedData)

    #split
    name_of_file <- statsSelectedData %>%
      strsplit(split = " | ")

    file_name <- name_of_file[[1]][4]

    # file sans extension
    file_sans_ext <- strsplit(x = file_name,split = "\\.")[[1]][1]

    # read data
    get_uploaded_data <- read.csv(paste0(file_sans_ext,".csv"))

  })


  ## display data when the button is clicked ----
  observeEvent(input$action_btn, {

    # download the file with progress indication
    withProgress(message = 'Downloading...', value = 0, {
      for (i in 1:10) {
        incProgress(1/15)
        Sys.sleep(0.25)
      }

      # create list of uploaded files
      provided_file <- strsplit(x = input$url_input,split = '/')[[1]][6]

      # append uploaded files
      uploaded_files$files <- c(uploaded_files$files,
                                paste0(input$file_name, " | OSF | ",
                                provided_file))

      # update drop down for uploaded data on secondary tab
      updateSelectInput(session,
                        "updatedData_ext",
                        choices = uploaded_files$files)

    })

    # get file name
    download_name <- input$url_input %>% strsplit(split = "/")

    # download file
    download.file(input$url_input,
                  paste0(download_name[[1]][6],'.csv'),
                  mode = "wb")

    if(input$file_name == 'Pop Data')
    {
      data <- serocalculator::load_pop_data(file_path = input$url_input)

    } else if (input$file_name == 'Curve Data')
    {
      data <- serocalculator::load_curve_params(file_path = input$url_input)

    } else if (input$file_name == 'Noise Data')
    {
      data <- serocalculator::load_noise_params(file_path = input$url_input)
    }

    # update reactive values
    data_df(data)

    # update drop down for uploaded data on secondary tab
    updateSelectInput(session,
                      "updatedData",
                      choices = uploaded_files$files)

    # display data
    output$head <- renderTable({
      data %>% head()
    })


  })

  # check file uploaded
  observeEvent(c(input$updatedData_ext),{

    #req(input$file_name)
    req(input$updatedData_ext)

    # uploaded file
    file_type = strsplit(x = input$updatedData_ext,split = " | ")[[1]][1]

    output$choose_visualization <- renderUI({
      if (file_type == 'Pop')
      {

        selectInput("type_visualization",
                    "Choose Type of Visualization",
                    choices = c("Density", "Age Scatter"))

      } else if (file_type == 'Curve')
      {
        selectInput("type_visualization",
                    "Choose Type of Visualization",
                    choices = c("Distribution", "Decay"))
      }
    })
  })

  ## check correct file is uploaded (pop, curve, noise) ----
  observeEvent(c(input$updatedData_ext,
                 input$updatedData),{

    # ensure file is uploaded
    req(input$updatedData_ext)

    file_type = strsplit(x = input$updatedData_ext,split = " | ")[[1]][1]

    output$stratify <- renderUI({

      # get uploaded data
      df = data()

      # column names
      cols = df %>% names()

      # dynamically create drop down list of column name
      if(file_type == 'Pop')
      {
        selectInput("stratify",
                    "Stratify By:",
                    cols,
                    selected = "Country")

      } else if (file_type == 'Curve')
      {
        NULL
      }

    })

    output$log <- renderUI({

      # get uploaded data
      df = data()

      # column names
      cols = df %>% names()

      # dynamically create drop down list of column name
      if(file_type == 'Pop')
      {
        checkboxInput("check_log", "Log", value = TRUE)

      } else if (file_type == 'Curve')
      {
        NULL
      }

    })

    output$other_head <- renderDT({

      # population data
      if(input$file_name == "Pop Data")
      {

        check_age = any(is.element(column_names(),c('ageCat')))

      if(!check_age)
      {

        print('Error: THE DATA PROVIDED DOES NOT HAVE THE COLUMNS CONSISTENT WITH POPULATION DATA.')

      } else
      {

        ## select age column on drop down
        output$select_age <- renderUI({

        # get uploaded data
        df = data()

        # column names
        cols = names(df)

        # dynamically create drop down list of column names
        selectInput("age",
                    "Select Age Column:",
                    cols)
        })

        ## select value column on drop down
        output$select_value <- renderUI({

            # get data
            df = data()

            # column names
            cols = names(df)

            # dynamically create drop down list of column names
            selectInput("age",
                        "Select Value Column:",
                        cols)
          })

          ## select id column on drop down
          output$select_id <- renderUI({

            # get data
            df = data()

            # column names
            cols = names(df)

            # dynamically create drop down list of column names
            selectInput("age",
                        "Select Index Column:",
                        cols)
          })

          # display data
          datatable(data = data(),
                    editable = TRUE)
        }
      } else if(input$file_name == "Curve Data")
      {

        # curve data check
        curve_data_check = any(is.element(column_names(),c('y0','y1','t1','alpha')))

        if(!curve_data_check)
        {

          print('Error: THE DATA PROVIDED DOES NOT HAVE THE COLUMNS CONSISTENT WITH CURVE DATA.')

        } else
        {
          datatable(data = data(),
                    editable = TRUE)
        }

      } else if(input$file_name == "Noise Data")
      {
        # noise data check
        noise_data_check = any(is.element(column_names(),c('y.low','eps','y.high')))

        if(!noise_data_check)
        {

          print('Error: THE DATA PROVIDED DOES NOT HAVE THE COLUMNS CONSISTENT WITH NOISE DATA.')

        } else
        {
          datatable(data = data(),
                    editable = TRUE)
        }
      }
    })
  })

  output$data_table <- renderTable({

    data_df() %>% head()

  })

  ## file numeric summary ----
  observeEvent(input$updatedData_ext, {

    req(input$updatedData_ext)

    g = get_uploaded_data(input$updatedData_ext)

    output$numeric_summary <- renderTable({

      g %>% head() %>% skimr::skim() %>% yank("numeric")

    })
  })

  observeEvent(c(input$updatedData_ext,
                 input$stratify,
                 input$check_log,
                 input$type_visualization), {

    req(input$updatedData_ext)
    req(input$stratify)
    req(input$check_log)
    req(input$type_visualization)

    # get data
    down_data = get_uploaded_data(file_input = input$updatedData_ext)

    ## visualization
    output$visualize <- renderPlot({

      viz_type = input$type_visualization
      file_type = strsplit(x = input$updatedData_ext,split = " | ")[[1]][1]

      #### density plot
      if(viz_type == 'Distribution' & file_type == 'Curve')
      {
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
      } else if (viz_type == 'Age Scatter' & file_type == 'Pop')
      {
        # visualize age-scatter
        down_data %>% serocalculator:::autoplot.pop_data(type = 'age-scatter',
                                                         strata = input$stratify,
                                                         log = input$check_log)

      #### distribution
      } else if(viz_type == 'Density' & file_type == 'Pop')
      {

        # visualize density
        down_data %>% serocalculator:::autoplot.pop_data(type = 'density',
                                                         strata = input$stratify,
                                                         log = input$check_log)


      #### decay
      } else if (viz_type == 'Decay' & file_type == 'Curve')
      {
        # visualize age-scatter
        down_data %>% serocalculator:::plot_curve_params_one_ab()
      }

    })
  })
}





