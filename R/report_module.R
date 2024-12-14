
#' @importFrom shiny navbarMenu
#' @importFrom shiny icon
report_ui <- function(id) {
  ns <- NS(id) # Namespace for unique IDs

  navbarMenu(
    title = "Report",
    tabPanel(
      title = "Rmd",
      icon = icon("file-code"),
      DT::dataTableOutput(ns("table"))
    ),
    tabPanel(
      title = "R",
      icon = icon("code")
    )
  )
}
