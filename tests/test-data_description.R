

test_that("HTML description for nu and eps is correctly generated", {
  output <- HTML(
    "<li><code>nu</code>: biological noise</li>
     <ul>
       <li>
         <em> Typically comes from cross-reactivity with other molecules or other pathogens.
         Biological noise is defined as the 95th percentile of the distribution of antibody
         responses to the antigen-isotype in a population with no exposure.</em>
       </li>
     </ul>
     <li><code>eps</code>: measurement noise</li>
     <ul>
       <li>
         <em>Represents measurement error from the laboratory testing process.
         It is defined by a CV (coefficient of variation) as the ratio of
         the standard deviation to the mean for replicates. Note that the CV should ideally be
         measured across plates rather than within the same plate.</em>
       </li>
     </ul>"
  )

  expect_type(output, "character")  # HTML output should be a character string
  expect_match(output, "<li><code>nu</code>: biological noise</li>")  # Check for "nu" description
  expect_match(output, "<li><code>eps</code>: measurement noise</li>")  # Check for "eps" description
})
