
library(ggplot2)
library(ggthemes)
library(readxl)
library(skimr)
library(gt)

server <- function(input, output, session) {

  # Store uploaded file information
  uploaded_files <- reactiveValues(files = NULL)

  # Read uploaded file and add data to the list
  observeEvent(input$upload, {
    req(input$upload)  # Ensure file is uploaded

    uploaded_files$files <- c(uploaded_files$files,
                              paste0(input$file_name," | ",input$upload$name))

    # update
    updateSelectInput(session, "selectedData", choices = uploaded_files$files)
    #updateSelectInput(session, "uploadedData", choices = uploaded_files$files)
  })

  # Read uploaded file and add data to the list
  observeEvent(input$upload, {
    req(input$upload)  # Ensure file is uploaded

    uploaded_files$files <- c(uploaded_files$files,
                              paste0(input$file_name," | ",input$upload$name))

    # update
    updateSelectInput(session, "selectedData", choices = uploaded_files$files)
    updateSelectInput(session, "updatedData", choices = uploaded_files$files)
  })

  # choose uploaded data
  # Read uploaded file and add data to the list
  observeEvent(input$uploadedData, {
    req(input$uploadedData)  # Ensure file is uploaded

    #split
    name_of_file <- input$uploadedData$name %>%
      strsplit(split = "|")


    #updateSelectInput(session, "selectedData", choices = uploaded_files$files)

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

  output$radio_buttons <- renderUI({
    switch(input$radio_display,
           "preview" = tableOutput("head"),
           "str" = tableOutput("str"),
           "summary" = verbatimTextOutput("summary")
    )
  })

  #-----------------------------------------------------

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
