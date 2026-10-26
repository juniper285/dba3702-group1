# Development launcher: reloads the package code, then starts the app.
# Run from the package root (local_shiny_app/).

options(shiny.autoreload = FALSE)
pkgload::load_all()
run_app(options = list(launch.browser = TRUE))
