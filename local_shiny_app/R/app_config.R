#' Path to a file shipped in the package's inst/ folder
#'
#' @param ... Path components below inst/, e.g. `"extdata", "rentals.rds"`.
#' @return The full path, or `""` if the file does not exist.
#' @noRd
app_sys <- function(...) system.file(..., package = "rentalapp")

#' Bootstrap theme shared by every page
#' @noRd
app_theme <- function() bs_theme(version = 5, bootswatch = "flatly")
