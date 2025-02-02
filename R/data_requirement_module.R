data_requirement_ui <- function(id) {
  ns <- NS(id)

  htmlOutput("data_requirement")
}

data_requirement_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # UI for displaying data requirements
    output$data_requirement <- renderText({
      HTML("<p>Required datasets:
          <ul>
            <li> <strong>Cross-sectional Population Data (Pop Data)</strong>
                <p>A dataset with one row per sample and
                columns for antigen isotype,
                quantitative antibody results,
                and age in years. Additional columns and variables
                can be included for stratification.</p>
            </li>
            <li> <strong>Noise Data</strong>
                <p>A dataset containing the following variables,
                specifying noise parameters for each
                antigen isotype. Note that variable
                names <u>must</u> follow these guidelines.
                For more
                information see
                <a href=https://onlinelibrary.wiley.com/doi/10.1002/sim.8578>Teunis
                (2020)</a>.
                <ul>
                  <li>antigen_iso: antigen isotype whose noise
                  parameters are being specified on each
                  row</li>
                  <li>nu: biological noise</li>
                  <li>y.low: Lower limit of detection of the antibody assay</li>
                  <li>y.high: Upper limit of detection of the antibody assay</li>
                  <li>eps: measurement noise</li>
                </ul></p>
            </li>
            <li><strong>Antibody Decay Curve Data</strong>
                <p>A dataset containing antibody decay curve parameters fit using a Bayesian hierarchical
                framework obtaining predictive posterior samples using Markov chain Monte Carlo
                sampling. Note that variable names <u>must</u> follow these guidelines. For more
                information see <a href=https://onlinelibrary.wiley.com/doi/10.1002/sim.5322>Teunis (2012)</a></p>
                <ul>
                  <li>y0: baseline antibody level</li>
                  <li>y1: antibody peak level (ELISA units)</li>
                  <li>t1: duration of infection</li>
                  <li>alpha: antibody decay rate (1/days for the current longitudinal parameter sets)</li>
                  <li>r: shape factor of antibody decay</li>
                </ul>
            </li>
          </ul>
          </p>
          <p>File limit: <strong>500MB</strong></p>")
    })
  })
}
