#' @importFrom shiny fluidPage
#' @importFrom shiny navbarPage

ui <- function(...) {
  # define UI
  fluidPage(
    navbarPage(
      title = "Serocalculator App",
      id = "serocalculator_app",
      theme = shinythemes::shinytheme("sandstone"),

      # Initialize shinyjs
      shinyjs::useShinyjs(),
      shinyalert::useShinyalert(),


      # Add the busy spinner
      header = shinybusy::add_busy_spinner(
        spin = "atom",
        position = "top-right",
        margins = c(200, 800),
        timeout = 100
      ),
      tabPanel(
        title = "Start Here",
        value = "summary",
        summary_tab_ui("summary"),
        icon = icon("home")
      ),
      tabPanel(
        title = "Step 1: Import Data",
        value = "import_data",
        import_data_ui("import_data"),
        icon = icon("cog")
      ),
      tabPanel(
        title = "Step 2: Inspect Data",
        value = "inspect_data",
        inspect_data_ui("inspect_data"),
        icon = icon("table")
      ),
      tabPanel(
        title = "Step 3: Estimate Seroconversion Rate",
        value = "estimate_seroincidence",
        estimate_seroincidence_ui("estimate_seroincidence"),
        icon = icon("chart-bar")
      )
    )
  )
}
