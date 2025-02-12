#' @importFrom shiny h2
#' @importFrom shiny htmlOutput
summary_tab_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Serocalculator"),
    div(style = "position:absolute;right:1em;",
        actionButton("import_next_btn", "Next", icon("paper-plane"),
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        tags$head(
          tags$style(HTML("hr {border-top: 1px solid #828994;}"))
        ),
    ),
    htmlOutput(ns("output_html"))
  )
}


# Module Server
summary_tab_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$next_btn, {
      updateNavbarPage(session, "serocalculator_app", selected = "import_data")
    })

    output$output_html <- renderUI({
      HTML("
        <p>The <strong>serocalculator</strong> R package provides a rapid and computationally simple method for calculating seroconversion rates,
        as originally published in <cite><a href='https://onlinelibrary.wiley.com/doi/10.1002/sim.3592'>Simonsen (2009)</a></cite> and <cite><a href='https://onlinelibrary.wiley.com/doi/10.1002/sim.8578'>Teunis (2012)</a></cite>,
        and further developed in subsequent publications by <cite><a href='https://www.sciencedirect.com/science/article/pii/S1755436514000371?via%3Dihub'>de Graaf (2014)</a></cite>,
        <cite><a href='https://www.sciencedirect.com/science/article/pii/S1755436516300135?via%3Dihub'>Teunis (2016)</a></cite>, and <cite><a href='https://onlinelibrary.wiley.com/doi/10.1002/sim.8578'>Teunis (2020)</a>.</cite></p>

        <p>This approach uses quantitative antibody responses to calculate the seroconversion rate (the rate at which individuals are infected in a population).
        In short, concentrations of antibodies for particular pathogens reflect how long it has been since an individual was last
        infected by that pathogen. Large numbers of individuals with high antibody concentrations in a cross-sectional
        survey indicate a high seroconversion rate, whereas small numbers of individuals with high antibody concentrations
        indicate a lower seroconversion rate.

        We used models of antibody decay dynamics to interpret the quantitative antibody responses in the population. The antibody dynamics are modeled
        separately among confirmed cases where antibody responses are measured longitudinally after infection.</p>

        <p>Further details on the methodology can be found on the <a href='https://ucd-serg.github.io/serocalculator/articles/serocalculator.html'>main package website</a>.</p>

        <p>This app provides a user-friendly interface to use the serocalculator methodology without the need for specialized coding knowledge.
        Users should click on the tabs above to follow the steps to calculate seroconversion rates:</p>

        <ul>
          <li>Step 1: Import the required datasets</li>
          <li>Step 2: Inspect their data</li>
          <li>Step 3: Estimate seroconversion rate</li>
          <li>Step 4: Prepare a report (Coming Soon)</li>
        </ul>

        <p>Required datasets:</p>
        <ul>
          <li>Cross-sectional population-based dataset with age and quantitative antibody results (Pop Data)</li>
          <li>Noise parameters (Noise Data)</li>
          <li>Longitudinal curve parameters (Curve Data)</li>
        </ul>

        <p>If you need assistance or encounter a clear bug, please file an issue with a minimal reproducible example on <a href='https://github.com/UCD-SERG/serocalculator/issues'>GitHub</a>.</p>
      ")
    })
  })
}
