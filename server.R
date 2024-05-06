
# load required libraries
library(ggplot2)
library(ggthemes)
library(readxl)
library(skimr)
library(stringr)
library(serocalculator)
library(shinyWidgets)
library(knitrProgressBar)

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

    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           #tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           #colnames(csv),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
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
    extracted_file_name <- name_of_file[[1]][4]

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
                              paste0(input$file_name," | ",input$upload$name))

    # get file name without extension
    filename(tools::file_path_sans_ext(input$upload$name))

    ## store uploaded file on server as CSV
    filename <- paste0("uploaded_data_", Sys.Date(), ".csv")
    write.csv(data(), file = paste0(filename(), ".csv"), row.names = FALSE)

    # update drop down list of uploaded files
    updateSelectInput(session, "selectedData", choices = uploaded_files$files)

    # update drop down for uploaded data on secondary tab
    updateSelectInput(session, "updatedData", choices = uploaded_files$files)

    # update list of available data
    updateSelectInput(session, "availableData", choices = uploaded_files$files)
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
  })

  # Read the downloaded file into a data frame
  download.file(input$url_input, "downloaded_file.csv", mode = "wb")
  data <- serocalculator::load_noise_params(file_path = input$url_input)

  # Update reactive values
  data_df(data)

  })

  output$data_table <- renderTable({
    data_df() %>% head()
  })



}





