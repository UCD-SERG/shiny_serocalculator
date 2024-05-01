
# load required libraries
library(ggplot2)
library(ggthemes)
library(readxl)
library(skimr)
library(stringr)
library(serocalculator)
library(shinyWidgets)

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

  # UPLOADED DATA ----

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
  #observeEvent(input$action_btn, {

    # function to update progress
  #  update_progress <- function(value) {

  #    progress(value)

      # incrementally update the progress bar
   #   setProgress(session, value = value, detail = paste(value, "% Complete"))
    }

    # download the file with progress indication
    #withProgress(message = 'Downloading...', value = 0, {
      #for (i in 1:10) {
        #Sys.sleep(0.5)  # Simulate a delay for demonstration purposes
        #update_progress(i * 10)
      #}

      # Read the downloaded file into a data frame
      #download.file(input$file_url, "downloaded_file.csv", mode = "wb")
      #data <- serocalculator::load_noise_params(file_path = input$url_input)

      # Update reactive values
      #data_df(data)
      #update_progress(100)  # Update progress to 100% when download completes
    #})
  #})


    #update_progress(100)  # Update progress to 100% when download completes

    #output$data_table <- renderTable({
    #  data_df()
    #})

    # Progress bar UI
    #output$progress_bar <- renderUI({
      #if (!is.null(input$url_input)) {
        #progressBar(
          #id = "progress",
          #value = progress(),
          #display_pct = TRUE
        #)
      #}
    #})

  ## check correct file is uploaded (pop, curve, noise) ----
  observeEvent(input$file_name,{

    # ensure file is uploaded
    req(input$file_name)

    output$head <- renderTable({

    # check
    if(input$file_name == "Pop Data"){
      if(any(is.element(column_names(),c('catchment','antigen_iso','cluster')))){
        print('Please Upload Pop Data')
      } else{
        data() %>% head()
      }
      } else if(input$file_name == "Curve Data"){
        if(any(is.element(column_names(),c('catchment','antigen_iso','cluster')))){
          print('Please Upload Curve Data')
      } else {
        data() %>% head()
      }
      } else if(input$file_name == "Curve Data"){
        if(any(is.element(column_names(),c('catchment','antigen_iso','cluster')))){
          print('Please Upload Noise Data')
      } else {
        data() %>% head()
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

    output$plot <- renderPlot({
      ggplot(available_data(),aes(x=Age)) +
        geom_density()
    })

    #------------------------ plot
    observeEvent(c(input$columns,input$availableData), {

      output$dynamic_plot <- renderPlot({
        req(input$availableData)
        req(input$columns)

        # get data
        available_data <- get_uploaded_data(file_input = input$availableData)

        ggplot(available_data, aes_string(x = input$columns)) +
          geom_bar()
      })
    })


  # DATA OUTPUT ----
  ## file preview ----
  output$str <- renderTable({
      data() %>% head() %>% skimr::skim() %>% gt::gt()
    })

  ## file summary ----
  output$summary <- renderPrint({
    data() %>% summary()
  })

  ## file numeric summary ----
  output$numeric_summary <- renderTable({
      data() %>% head() %>% skimr::skim() %>% yank("numeric")
  })

  ## file character summary ----
  output$character_summary <- renderTable({
      data() %>% head() %>% skimr::skim() %>% yank("character")
  })



