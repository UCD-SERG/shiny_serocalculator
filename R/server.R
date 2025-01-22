#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny reactiveVal
#' @importFrom utils read.csv

server <- function(input, output, session) {
  ############################ REACTIVE OBJECTS ###############################

  # Reactive values to store uploaded data
  pop_data <- reactiveVal(NULL)
  curve_data <- reactiveVal(NULL)
  noise_data <- reactiveVal(NULL)

  ######################### CALL MODULES ######################################

  # Summary module (pass data if needed)
  summary_tab_server(id = "summary")

  # Import data module
  import_data_server(
    id = "import_data",
    pop_data = pop_data,
    curve_data = curve_data,
    noise_data = noise_data
  )

  # Inspect data module
  inspect_data_server(
    id = "inspect_data",
    pop_data = pop_data,
    curve_data = curve_data,
    noise_data = noise_data
  )

  # Estimate seroincidence module
  estimate_seroincidence_server(
    id = "estimate_seroincidence",
    pop_data = pop_data,
    curve_data = curve_data,
    noise_data = noise_data
  )
}
