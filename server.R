
library(ggplot2)
library(ggthemes)
library(readxl)
library(skimr)
library(stringr)
library(vroom)

server <- function(input, output, session) {

  # information of uploaded data
  uploaded_files <- reactiveValues(files = NULL)
  filename <- reactiveVal(NULL)

  # read uploaded data
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

  # get column names of uploaded data
  column_names <- reactive({

    # ensure data is uploaded
    req(input$upload)

    # load file
    if (is.null(input$upload$datapath))
      return(NULL)

    df <- vroom::vroom(input$upload$datapath,delim = ",")
    colnames(df)
  })

  # read uploaded file and add data to the list
  observeEvent(input$upload, {

    # ensure file is uploaded
    req(input$upload)

    # create list of uploaded files
    uploaded_files$files <- c(uploaded_files$files,
                              paste0(input$file_name," | ",input$upload$name))

    # get file name without extension
    filename(tools::file_path_sans_ext(input$upload$name))

    # store uploaded file on server as CSV
    filename <- paste0("uploaded_data_", Sys.Date(), ".csv")
    write.csv(data(), file = paste0(filename(), ".csv"), row.names = FALSE)

    # update dropdown list of uploaded files
    updateSelectInput(session, "selectedData", choices = uploaded_files$files)

    # update dropdown for uploaded data on secondary tab
    updateSelectInput(session, "updatedData", choices = uploaded_files$files)

    # update list of available data
    updateSelectInput(session, "availableData", choices = uploaded_files$files)
  })

  # choose uploaded data
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

  # available data
  available_data <- reactive({
    req(input$availableData)

    get_uploaded_data(file_input = input$availableData)

  })

  # check uploaded data
  observeEvent(input$file_name,{

    # ensure file is uploaded
    req(input$file_name)

    output$head <- renderTable({

      # check
      if(input$file_name == "Pop Data"){
        if(any(is.element(column_names(),c('catchment','antigen_iso','cluster')))){
          print('Please Select Pop Data')
        } else{
          data() %>% head()
        }
      } else if(input$file_name == "Curve Data"){
        if(any(is.element(column_names(),c('catchment','antigen_iso','cluster')))){
          print('Please Select Curve Data')
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

  # upload OSF URL
  observeEvent(input$action_btn,{

    url <- input$url_input

    # Check if URL is not empty
    if (!is.null(url) && url != "") {

      # read data from URL
      data <- vroom(url, delim = ",")

      # show data in table
      output$data_table <- renderTable({
        data() %>% head()
      })

    } else {

      # Show error message if URL is empty
      output$data_table <- renderTable({
        "Please enter a valid URL."
      })
    }
  })

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

  #------------------------ PLOT ----------------------------------------

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


  # ---------- DATA OUTPUT ------------------------------------------------

  #output$head <- renderTable({

  #  if(any(is.element(column_names(),c('catchment','antigen_iso','cluster')))){
  #    print('Please select a data set')
  #  } else{
  #    data() %>% head()
  #  }

  #})

  output$str <- renderTable({
    data() %>% head() %>% skimr::skim() %>% gt::gt()
  })

  output$summary <- renderPrint({
    data() %>% summary()
  })

  #-----------------------------------------------------------------------
  output$numeric_summary <- renderTable({
    data() %>% head() %>% skimr::skim() %>% yank("numeric")
  })

  output$character_summary <- renderTable({
    data() %>% head() %>% skimr::skim() %>% yank("character")
  })
  #-----------------------------------------------------------------------

  output$y0 <- renderPlot({
    ggplot(data(),aes(x=y0)) +
      geom_density() +
      theme_minimal() +
      ggtitle("Baseline Antibody Concentration") +
      xlab("") +
      ylab("")
  })

  output$y1 <- renderPlot({
    ggplot(data(),aes(x=y1)) +
      geom_density() +
      theme_minimal() +
      ggtitle("Peak Antibody Concentration") +
      xlab("") +
      ylab("")
  })

  output$t1 <- renderPlot({
    ggplot(data(),aes(x=t1)) +
      geom_density() +
      theme_minimal() +
      ggtitle("Time to peak antibody concentration") +
      xlab("") +
      ylab("")
  })

  output$alpha <- renderPlot({
    ggplot(data(),aes(x=alpha)) +
      geom_density() +
      theme_minimal() +
      ggtitle("Antibody decay rate in days") +
      xlab("") +
      ylab("")
  })

  output$r <- renderPlot({
    ggplot(data(),aes(x=r)) +
      geom_density() +
      theme_minimal() +
      ggtitle("Antibody decay shape") +
      xlab("") +
      ylab("")
  })
}
