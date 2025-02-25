#' @title UI for Summary Page
#'
#' @importFrom shiny h2
#' @importFrom shiny htmlOutput
#' @importFrom shiny div
#' @importFrom shiny tagList
#' @importFrom shiny tag
#' @importFrom shiny HTML
#' @importFrom shiny h2
#'
#' @param id identify namespace
summary_tab_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "position:absolute;right:1em;",
      actionButton(
        "import_next_btn",
        label = "Next", icon = icon("arrow-right"),
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      tags$head(
        tags$style(
          HTML(
            "hr {border-top: 1px solid #828994;}"
          )
        )
      ),
    ),
    h2("Serocalculator"),
    htmlOutput(ns("output_html")),
    div(
      style = "position:absolute;right:1em;",
      uiOutput(ns("serocalculator_version"))
    ),
  )
}


# Module Server
#' @title Server-side for Summary Module
#'
#' @importFrom shiny updateNavbarPage
#' @importFrom shiny renderUI
#' @importFrom utils packageVersion
#' @importFrom shiny observeEvent
#'
#' @param id identify namespace
summary_tab_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$next_btn, {
      updateNavbarPage(session,
        "serocalculator_app",
        selected = "import_data"
      )
    })

    output$serocalculator_version <- renderUI({
      HTML(
        paste(
          "Serocalculator",
          as.character(packageVersion("serocalculator")),
          "<br>", R.version.string,
          "<br>",
          paste(
            "shiny.serocalculator",
            as.character(packageVersion("shiny.serocalculator"))
          )
        )
      )
    })



    output$output_html <- renderUI({
      tags$head(
        tags$style(HTML("
          .wrapped-text {
            max-width: 200px;  /* Adjust as needed */
            word-wrap: break-word;
            white-space: normal;
            overflow-wrap: break-word;
          }
        "))
      )

      HTML("
        <div class='wrapped-text'>
          <p>The <strong>serocalculator</strong> R package provides a rapid and
          computationally simple method for
          calculating seroconversion rates,
          as originally published in
          <cite><a href='https://onlinelibrary.wiley.com/doi/10.1002/sim.3592'>
          Simonsen (2009)</a></cite> and <cite>
          <a href='https://onlinelibrary.wiley.com/doi/10.1002/sim.8578'>
          Teunis (2012)
          </a></cite>, and further developed in subsequent publications by
          <cite>
          <a href=
          'http://bit.ly/40W7wQY'>
          de Graaf (2014)</a></cite>,
          <cite>
          <a href=
          'https://bit.ly/42UnBJv'>
          Teunis (2016)</a></cite>, and <cite>
          <a href='https://onlinelibrary.wiley.com/doi/10.1002/sim.8578'>
          Teunis (2020)</a>.</cite></p>
          <p>This approach uses quantitative antibody responses to calculate
          the seroconversion rate (the rate at which individuals are infected
          in a population).
          In short, concentrations of antibodies for particular pathogens
          reflect how long it has been since an individual was last
          infected by that pathogen. Large numbers of individuals with
          high antibody concentrations in a cross-sectional
          survey indicate a high seroconversion rate, whereas small
          numbers of individuals with high antibody concentrations
          indicate a lower seroconversion rate.

          We used models of antibody decay dynamics to interpret
          the quantitative antibody responses in the population.
          The antibody dynamics are modeled separately among confirmed
          cases where antibody responses are measured
          longitudinally after infection.</p>

          <p>Further details on the methodology can be found on the
          <a href=
          'https://bit.ly/41aaOBr'>
          main package website</a>.</p>

          <p>This app provides a user-friendly interface to use the
          serocalculator methodology without the need for specialized
          coding knowledge. Users should click on the tabs above to
          follow the steps to calculate seroconversion rates:</p>

          <ul>
            <li>Step 1: Import the required datasets</li>
            <li>Step 2: Inspect and visualize the data</li>
            <li>Step 3: Estimate the seroconversion rate</li>

          </ul>

          <p>Required datasets:</p>
          <ul>
            <li>Cross-sectional population-based dataset with age
            and quantitative antibody results (Pop Data)
            </li>
            <li>Noise parameters (Noise Data)</li>
            <li>Modeleded longitudinal seroresponse parameters
            (Seroresponse Data)</li>
          </ul>

          <p>If you need assistance or encounter a bug,
          please file an issue with a minimal reproducible example on
          <a href='https://github.com/UCD-SERG/serocalculator/issues'>
          GitHub</a>.</p>
        </div>
      ")
    })
  })
}
