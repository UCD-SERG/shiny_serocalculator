
library(ggplot2)
library(ggthemes)

server <- function(input, output, session) {
  data <- reactive({
    req(input$upload)

    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })

  output$head <- renderTable({
    head(data())
  })

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
      xlab("")
  })

  output$alpha <- renderPlot({
    ggplot(data(),aes(x=alpha)) +
      geom_density() +
      theme_minimal() +
      ggtitle("Antibody decay rate in days") +
      xlab("")
  })

  output$r <- renderPlot({
    ggplot(data(),aes(x=r)) +
      geom_density() +
      theme_minimal() +
      ggtitle("Antibody decay shape") +
      xlab("")
  })
}
