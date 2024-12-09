shiny_serocalculator_app <- function(...) {

  # ui
  ui <- ui()

  # server
  server <- server(input, output, session)

  # Run the app
  shinyApp(ui = ui, server = server)
}
