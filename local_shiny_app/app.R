# Launch the app: click "Run App" in RStudio, or run
#   shiny::runApp("local_shiny_app")
# This file is also the entry point for deploying (e.g. shinyapps.io).
# The app itself lives in R/ as an R package (see README.md).

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
run_app()
