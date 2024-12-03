# Module UI
summary_tab_ui <- function(id) {
  ns <- NS(id) # Namespace to avoid ID collisions
  tabPanel(
    "Summary",
    h2("Serocalculator"),
    htmlOutput(ns("output_html"))
  )
}

# Module Server
summary_tab_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$output_html <- renderUI({
      HTML("
        <p>The <strong>serocalculator </strong>R package provides a rapid and computationally simple method for calculating seroconversion rates,
        as originally published in <cite><a href='https://onlinelibrary.wiley.com/doi/10.1002/sim.3592'>Simonsen (2009)</a></cite> and <cite><a href='https://onlinelibrary.wiley.com/doi/10.1002/sim.8578'>Teunis (2012)</a></cite>,
        and further developed in subsequent publications by <cite><a href='https://www.sciencedirect.com/science/article/pii/S1755436514000371?via%3Dihub'>de Graaf (2014)</a></cite>,
        <cite><a href='https://www.sciencedirect.com/science/article/pii/S1755436516300135?via%3Dihub'>Teunis (2016)</a></cite>, and <cite><a href='https://onlinelibrary.wiley.com/doi/10.1002/sim.8578'>Teunis (2020)</a>.</cite></p>

        <p>In short, longitudinal seroresponses from confirmed cases with a known symptom onset date are
        assumed to represent the time course of human serum antibodies against a specific pathogen. Therefore, by using these longitudinal
        antibody dynamics with any cross-sectional sample of the same antibodies in a human population, an incidence estimate can be calculated.</p>

        <p>Further details on the methodology can be found on the <a href='https://ucd-serg.github.io/serocalculator/articles/serocalculator.html'>main package website</a>.</p>

        <p>This app provides a user-friendly interface to use the serocalculator methodology without the need for specialized coding knowledge.
        Users should follow the steps to:</p>

        <ul>
          <li>Import the required datasets</li>
          <li>Inspect their data</li>
          <li>Estimate seroincidence</li>
          <li>Prepare a report (optional)</li>
        </ul>

        <p>Required datasets:</p>
        <ul>
          <li>Cross-sectional population-based dataset with age and quantitative antibody results</li>
          <li>Noise parameters</li>
          <li>Longitudinal curve parameters</li>
        </ul>

        <p>If you need assistance or encounter a clear bug, please file an issue with a minimal reproducible example on <a href='https://github.com/UCD-SERG/serocalculator/issues'>GitHub</a>.</p>
      ")
    })
  })
}
