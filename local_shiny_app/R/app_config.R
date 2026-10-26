#' Path to a file shipped in the package's inst/ folder
#'
#' @param ... Path components below inst/, e.g. `"extdata", "rentals.rds"`.
#' @return The full path, or `""` if the file does not exist.
#' @noRd
app_sys <- function(...) system.file(..., package = "rentalapp")

#' Bootstrap theme shared by every page (styles in inst/app/rentlens.scss)
#'
#' The Inter font is linked from Google Fonts; offline, the system font is used.
#' @noRd
app_theme <- function() {
  font <- font_collection(
    font_link("Inter", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap"),
    "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"
  )
  bs_theme(
    version = 5,
    primary = "#0f766e", secondary = "#64748b", success = "#15803d",
    info = "#0369a1", warning = "#b45309", danger = "#e11d48",
    "body-bg" = "#f6f7f9", "body-color" = "#1f2937", "border-color" = "#e5e7eb",
    "border-radius" = "0.5rem", "border-radius-lg" = "0.75rem",
    "navbar-bg" = "#ffffff",
    base_font = font, heading_font = font
  ) |>
    bs_add_rules(sass::sass_file(app_sys("app", "rentlens.scss")))
}
