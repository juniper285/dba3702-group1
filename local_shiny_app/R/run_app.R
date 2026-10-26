#' Run the Rental Intelligence app
#'
#' @param ... Passed to [shiny::shinyApp()], e.g. `options = list(port = 8080)`.
#' @export
run_app <- function(...) {
  # enableBookmarking = "url" powers the "Save this search" button
  shinyApp(ui = app_ui, server = app_server, enableBookmarking = "url", ...)
}
