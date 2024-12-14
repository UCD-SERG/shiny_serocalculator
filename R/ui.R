
#' @importFrom shiny fluidPage
#' @importFrom shiny navbarPage
#'
ui <- function(...) {
  # define UI
  fluidPage(
    navbarPage(
      title = "Serocalculator App",
      theme = shinythemes::shinytheme("united"),

      # Initialize shinyjs
      shinyjs::useShinyjs(),

      # Add the busy spinner
      header = shinybusy::add_busy_spinner(
        spin = "atom",
        position = "top-right",
        margins = c(200, 800),
        timeout = 100
      ),
      tabPanel("Summary", summary_tab_ui("summary")),
      tabPanel("Import Data", import_data_ui("import_data")),
      tabPanel("Inspect Data", inspect_data_ui("inspect_data")),
      tabPanel("Estimate Seroincidence",
               estimate_seroincidence_ui("estimate_seroincidence"))
    )
  )
}
