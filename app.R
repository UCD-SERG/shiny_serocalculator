# Launch the ShinyApp (Do not remove this comment)
# To deploy to shinyapps.io, run: rsconnect::deployApp()
# Or use the blue button on top of this file
reactlog::reactlog_enable()
# Load the package
pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
shiny.serocalculator::run_app() # add parameters here (if any)
