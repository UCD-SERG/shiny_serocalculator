#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny reactiveVal
#' @importFrom utils read.csv

server <- function(input, output, session) {

  # Call modules
  summary_tab_server("summary")

  import_data_server(
    id = "import_data",
    uploaded_files = uploaded_files,
    pop_data = pop_data,
    curve_data = curve_data,
    noise_data = noise_data
  )

  inspect_data_server(
    id = "inspect_data",
    pop_data = pop_data,
    curve_data = curve_data,
    noise_data = noise_data
  )

  estimate_seroincidence_server(
    id = "estimate_seroincidence",
    pop_data = pop_data,
    curve_data = curve_data,
    noise_data = noise_data
  )
}
