
# load required libraries
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

  ## filename ----
  filename <- reactiveVal(NULL)

  ## progress ----
  progress <- reactiveVal(0)

  ## data_df
  data_df <- reactiveVal(NULL)

  ## choose upload data ----
  data <- reactive({

    # ensure file is uploaded
    req(input$upload)

    # get file extension
    ext <- tools::file_ext(input$upload$name)

    if (ext %in% c("csv", "tsv")) {
      if (ext == "csv") {
        df <- read.csv(input$upload$datapath)
      } else if (extension == "tsv") {
        df <- read_tsv(input$upload$datapath)
      }
      return(df)
    } else {
      return(NULL)  # Return NULL for unsupported file types
    }

  })

  ## uploaded column names ----
  column_names <- reactive({

    # ensure data is uploaded
    req(input$upload)

    # load file
    if (is.null(input$upload$datapath))
      return(NULL)

    df <- vroom::vroom(input$upload$datapath,delim = ",")
    colnames(df)
  })

  ## available data ----
  available_data <- reactive({
    req(input$availableData)

    get_uploaded_data(file_input = input$availableData)

  })

  ## url upload data ----
  url_data <- reactive({

    # ensure data is uploaded
    req(input$url_input)

    if (!is.null(input$url_input) && input$url_input != "") {
      vroom(input$url_input, delim = ",")

      # download the file
      data <- serocalculator::load_pop_data(file_path = input$url_input)
      #data %>% rename()

      # write file to disk


    } else {
      "Please enter a valid URL."
    }

  })

  # PROCESSING FUNCTIONS ----

  # load uploaded data
  get_uploaded_data <- function(file_input){

    # split
    name_of_file <- file_input %>%
      strsplit(split = " | ")

    # get file name
    extracted_file_name <- name_of_file[[1]][6]

    # file name sans extension
    file_sans_ext <- strsplit(x = extracted_file_name,split = "\\.")[[1]][1]

    # read data
    available_data <- read.csv(paste0(file_sans_ext,".csv"))

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
    write.csv(data(), file = paste0(filename(), ".csv"), row.names = FALSE)
    up_data = read.csv(file = "uploaded_data.csv")

    # update drop down list of uploaded files
    updateSelectInput(session, "selectedData", choices = uploaded_files$files)

    # update drop down for uploaded data on secondary tab
    updateSelectInput(session, "updatedData", choices = uploaded_files$files)

    # update drop down for uploaded data on secondary tab
    updateSelectInput(session, "updatedData_ext", choices = uploaded_files$files)


    # update list of available data
    updateSelectInput(session, "availableData", choices = uploaded_files$files)


    # update drop down for uploaded data on secondary tab
    updateSelectInput(session, "age", choices = names(data()))
  })

  ## choose uploaded data ----
  observeEvent(input$statsSelectedData, {
    req(input$statsSelectedData)  # Ensure file is uploaded

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
      uploaded_files$files <- c(uploaded_files$files,
                                paste0(input$file_name, " | OSF | ",provided_file))


      # update drop down for uploaded data on secondary tab
      updateSelectInput(session, "updatedData_ext", choices = uploaded_files$files)

    })

    # write the downloaded file
    download_name <- input$url_input %>% strsplit(split = "/")
    download.file(input$url_input, paste0(download_name[[1]][6],'.csv'), mode = "wb")

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


    # Update reactive values
    data_df(data)

    # update drop down for uploaded data on secondary tab
    updateSelectInput(session, "updatedData", choices = uploaded_files$files)

    output$head <- renderTable({
      data %>% head()
    })


  })

  ## check correct file is uploaded (pop, curve, noise) ----
  observeEvent(c(input$file_name,input$updatedData_ext, input$updatedData),{

    # ensure file is uploaded
    req(input$file_name)



    output$other_head <- renderDT({

      # check
      if(input$file_name == "Pop Data"){
        if(!any(is.element(column_names(),c('ageCat')))){
          print('Error: THE DATA PROVIDED DOES NOT HAVE THE COLUMNS CONSISTENT WITH POPULATION DATA.')
        } else {

          ## select age column on dropdown
          output$select_age <- renderUI({

            df = data()

            items=names(df)

            # Dynamically create dropdown list of column names
            selectInput("age","Select Age Column:", items)
          })

          ## select value column on dropdown
          # column names dropdown
          output$select_value <- renderUI({

            df = data()

            items=names(df)

            # Dynamically create drop down list of column names
            selectInput("age","Select Value Column:", items)
          })

          ## select id column on drop down
          output$select_id <- renderUI({

            df = data()

            items = names(df)

            # Dynamically create drop down list of column names
            selectInput("age","Select Index Column:", items)
          })

          datatable(data = data(), editable = TRUE)
        }
      } else if(input$file_name == "Curve Data"){
        if(!any(is.element(column_names(),c('y0','y1','t1','alpha')))){
          print('Please Upload Curve Data')
        } else {
          datatable(data = data(), editable = TRUE)
        }
      } else if(input$file_name == "Noise Data"){
        if(!any(is.element(column_names(),c('y.low','eps','y.high')))){
          print('Please Upload Noise Data')
        } else {
          datatable(data = data(), editable = TRUE)
        }
      }
    })
  })

  # update columns of uploaded data
  observeEvent(input$availableData, {
    req(input$availableData)

    # file
    input$availableData

    # get data
    available_data <- get_uploaded_data(file_input = input$availableData)

    # update columns
    updateSelectInput(session, "columns", choices = colnames(available_data))

  })

  # set attributes
  observeEvent(c(input$select_age, input$select_value, input$select_id),{

    # rename
    data()
  })

  output$sub_dropdown_ui <- renderUI({
    if (input$file_out == "Population") {
      selectInput("sub_dropdown", "Choose Type of Visualization",
                  choices = c("Density", "Age Scatter"))
    } else if (input$file_out == "Curve Param") {
      selectInput("sub_dropdown", "Choose Type of Visualization",
                  choices = c("Distribution", "Decay"))
    } else {
      # If none selected, render empty dropdown
      selectInput("sub_dropdown", "Choose Type of Visualization", choices = NULL)
    }
  })

  output$selected_options <- renderText({
    paste("Main dropdown selection:", input$file_out, "\n",
          "Sub dropdown selection:", input$sub_dropdown)
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

  observeEvent(c(input$updatedData_ext, input$file_out, input$check_stratify, input$check_log), {

    req(input$updatedData_ext)
    req(input$file_out)
    req(input$check_stratify)
    req(input$check_log)

    down_data = get_uploaded_data(file_input = input$updatedData_ext)

    ## population age scatter
    output$visualize <- renderPlot({

      viz_type <- input$sub_dropdown

      #### density plot
      if(viz_type == 'Density')
      {
        if(input$check_stratify)
        {
          down_data %>% serocalculator:::autoplot.pop_data(type = 'density',strata = 'Country',log = input$checklog)

        } else if(!input$check_stratify)
        {
          down_data %>% serocalculator:::autoplot.pop_data(type = 'density', log = input$checklog)
        }

        #### age-scatter plot
      } else if (viz_type == 'Age Scatter')
      {
        if(input$check_stratify)
        {
          if(input$checklog)
          {
            down_data %>% serocalculator:::autoplot.pop_data(type = 'age-scatter',strata = 'Country',log = input$checklog)
          } else if(!input$checklog)
          {
            down_data %>% serocalculator:::autoplot.pop_data(type = 'age-scatter',strata = 'Country')
          }

        } else if(!input$check_stratify)
        {
          down_data %>% serocalculator:::autoplot.pop_data(type = 'age-scatter')
        }

      } else if(viz_type == 'Distribution')
      {
        down_data %>%
          pivot_longer(
            cols = `y0`:`r`,
            names_to = "parameter",
            values_to = "value"
          ) %>%

          ggplot(aes(y=value)) +
          geom_density() +
          scale_y_continuous(limits = c(0, 500)) +
          facet_grid(rows = vars(parameter)) +
          theme_linedraw()

      }

    })
  })
}





