
ui <- function(...) {
# Define the UI
ui <- shinyUI(
  navbarPage(
    title = "Serocalculator",
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

    # Tab for project summary
    summary_tab_ui("summary"),

    # Import data tab
    import_data_ui("import_data"),

    estimate_seroincidence_ui("estimate_seroincidence"),

    report_ui("report")
  )
)
}
