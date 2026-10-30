# ---- Formatting helpers for text and tables ----------------------------

#' Format a number as Singapore dollars, e.g. "S$4,500"
#' @noRd
money <- function(x) paste0("S$", formatC(round(x), format = "d", big.mark = ","))

#' money(), or "-" when there is no value
#' @noRd
money_or_dash <- function(x) if (is.na(x)) "-" else money(x)

sqft_per_sqm <- 10.7639

#' A floor-area range in square feet and square metres, e.g.
#' "500 - 1,200 sq ft (46 - 111 m²)"; the top of the slider (`area_max`)
#' reads as "and above"
#' @noRd
area_text <- function(area) {
  open_top <- area[2] >= area_max
  fmt <- function(x) format(round(x), big.mark = ",", trim = TRUE)
  plus <- if (open_top) "+" else ""
  paste0(fmt(area[1]), " - ", fmt(area[2]), plus, " sq ft (",
         fmt(area[1] / sqft_per_sqm), " - ", fmt(area[2] / sqft_per_sqm), plus, " m\u00B2)")
}

#' A floor-area range in both units, e.g. "500-1,200 ft² · 46-111 m²"
#' @noRd
area_units <- function(area) {
  plus <- if (area[2] >= area_max) "+" else ""
  fmt <- function(x) format(round(x), big.mark = ",", trim = TRUE)
  paste0(fmt(area[1]), "\u2013", fmt(area[2]), plus, " ft\u00B2  \u00B7  ",
         fmt(area[1] / sqft_per_sqm), "\u2013", fmt(area[2] / sqft_per_sqm), plus, " m\u00B2")
}

#' Format a date as its quarter, e.g. "2024 Q2"
#' @noRd
quarter_label <- function(d) {
  paste0(format(d, "%Y"), " Q", (as.integer(format(d, "%m")) - 1) %/% 3 + 1)
}

#' Most common value (used for the typical floor-area band)
#' @noRd
most_common <- function(x) names(which.max(table(x)))

#' Link that searches property portals for a project's current listings
#' @noRd
listing_link <- function(project) {
  q <- utils::URLencode(paste0('"', project, '" for rent site:propertyguru.com.sg OR site:99.co'),
                        reserved = TRUE)
  paste0('<a href="https://www.google.com/search?q=', q, '" target="_blank">Find listings</a>')
}

# Friendly column names for tables
nice_names <- c(
  project = "Project", district = "District", bed_group = "Bedrooms",
  size = "Typical size (sqft)", median = "Median rent", average = "Average rent",
  p25 = "25th percentile",
  p75 = "75th percentile", psf = "Median S$ psf", n = "Recent rentals",
  within = "Share within budget", last = "Last rented", fits = "Budget fit",
  mrt_station = "Nearest MRT/LRT",
  mrt_m = "Distance to MRT/LRT (m)", walk_min = "Walk to MRT/LRT (min, est.)",
  listings = "Listings"
)

#' Rename table columns to their friendly names
#' @noRd
prettify <- function(s) {
  names(s) <- ifelse(names(s) %in% names(nice_names), nice_names[names(s)], names(s))
  s
}

#' Format the money / percentage columns of a prettify()-ed table
#' @noRd
format_money_cols <- function(dt, s) {
  money_cols <- intersect(c("Median rent", "Average rent", "25th percentile", "75th percentile"),
                          names(s))
  dt <- formatCurrency(dt, money_cols, "S$", digits = 0)
  if ("Median S$ psf" %in% names(s)) dt <- formatCurrency(dt, "Median S$ psf", "S$", digits = 2)
  if ("Share within budget" %in% names(s)) dt <- formatPercentage(dt, "Share within budget", digits = 0)
  dt
}
