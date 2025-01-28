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
  session$userData$select_id <- reactiveVal()

  ######################### CALL MODULES ######################################

  # Summary module (pass data if needed)
  summary_tab_server(id = "summary")

  # Call import_data_server and get the shared reactive value
  imported_data <- import_data_server(
    "import_data",
    pop_data = pop_data,
    curve_data = curve_data,
    noise_data = noise_data
  )

  observeEvent(input$age_select, {
    print(paste("Debug: input$age_select =", input$age_select))  # Debugging
    selected_age(input$age_select)  # Update the reactive value
    print(paste("Debug: selected_id updated to:", selected_age()))  # Debugging
  })

  # Inspect data module
  inspect_data_server(
    id = "inspect_data",
    pop_data = pop_data,
    curve_data = curve_data,
    noise_data = noise_data,
    imported_data = imported_data
  )

  # Estimate seroincidence module
  estimate_seroincidence_server(
    id = "estimate_seroincidence",
    pop_data = pop_data,
    curve_data = curve_data,
    noise_data = noise_data,
    imported_data = imported_data
  )
}
