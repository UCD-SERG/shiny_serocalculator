# Load the package
pkgload::load_all()

shinyApp(ui = ui, server = server)
