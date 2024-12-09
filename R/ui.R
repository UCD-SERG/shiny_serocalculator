
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

    # data summary tab
    summary_tab_ui("summary"),

    # Import data tab
    import_data_ui("import_data"),

    # Inspect data tab
    inspect_data_ui("inspect_data"),

    # Inspect incidence tab
    estimate_seroincidence_ui("estimate_seroincidence"),

    report_ui("report")
  )
)
}
