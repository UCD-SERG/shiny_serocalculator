
library(ggplot2)
library(ggthemes)
library(readxl)
library(skimr)
library(stringr)

server <- function(input, output, session) {

  # uploaded files information
  uploaded_files <- reactiveValues(files = NULL)
  filename <- reactiveVal(NULL)

  # Read uploaded file and add data to the list
  observeEvent(input$upload, {
    req(input$upload)  # Ensure file is uploaded

    uploaded_files$files <- c(uploaded_files$files,
                              paste0(input$file_name," | ",input$upload$name))

    filename(tools::file_path_sans_ext(input$upload$name))

    # store file on server
    filename <- paste0("uploaded_data_", Sys.Date(), ".csv")
    write.csv(data(), file = paste0(filename(), ".csv"), row.names = FALSE)

    # update
    updateSelectInput(session, "selectedData", choices = uploaded_files$files)
  })

  # Read uploaded file and add data to the list
  observeEvent(input$upload, {
    req(input$upload)  # Ensure file is uploaded

    uploaded_files$files <- c(uploaded_files$files,
                              paste0(input$file_name," | ",input$upload$name))

    # update
    updateSelectInput(session, "selectedData", choices = uploaded_files$files)
    updateSelectInput(session, "updatedData", choices = uploaded_files$files)
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

  data <- reactive({
    req(input$upload)

    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
          )
  })

  # available data
  available_data <- reactive({
    req(input$availableData)

    get_uploaded_data(file_input = input$availableData)

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





  output$head <- renderTable({
    data() %>% head()
  })

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
