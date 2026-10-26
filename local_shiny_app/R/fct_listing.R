# ---- Checking one listing against past rentals -------------------------

# Region of each postal district (by where most of the district lies; the
# URA regions cut across some districts, so this is an approximation)
district_regions <- c(
  "01" = "Central", "02" = "Central", "03" = "Central", "04" = "Central",
  "05" = "West",    "06" = "Central", "07" = "Central", "08" = "Central",
  "09" = "Central", "10" = "Central", "11" = "Central", "12" = "Central",
  "13" = "Central", "14" = "East",    "15" = "East",    "16" = "East",
  "17" = "East",    "18" = "East",    "19" = "North-East", "20" = "Central",
  "21" = "West",    "22" = "West",    "23" = "West",    "24" = "West",
  "25" = "North",   "26" = "North",   "27" = "North",   "28" = "North-East"
)

#' Region ("Central", "East", ...) of postal district numbers
#' @noRd
district_region <- function(district_no) unname(district_regions[sprintf("%02d", district_no)])

#' Look up an address or postal code with the OneMap Search API (no key needed)
#'
#' Answers are kept for the R session, so repeating a search makes no new call.
#' @return Data frame with `address`, `building`, `postal`, `lat`, `lng`
#'   (no rows if nothing matched or the API could not be reached).
#' @noRd
onemap_search <- function(query) {
  query <- trimws(query)
  none <- data.frame(address = character(), building = character(),
                     postal = character(), lat = numeric(), lng = numeric())
  if (!nzchar(query)) return(none)
  cached(paste0("onemap:", toupper(query)), {
    res <- tryCatch(
      httr2::request("https://www.onemap.gov.sg/api/common/elastic/search") |>
        httr2::req_url_query(searchVal = query, returnGeom = "Y",
                             getAddrDetails = "Y", pageNum = 1) |>
        httr2::req_timeout(5) |>
        httr2::req_perform() |>
        httr2::resp_body_json(),
      error = function(e) NULL)
    hits <- res$results
    if (length(hits) == 0) return(none)
    data.frame(
      address  = vapply(hits, `[[`, "", "ADDRESS"),
      building = vapply(hits, `[[`, "", "BUILDING"),
      postal   = vapply(hits, `[[`, "", "POSTAL"),
      lat      = as.numeric(vapply(hits, `[[`, "", "LATITUDE")),
      lng      = as.numeric(vapply(hits, `[[`, "", "LONGITUDE"))
    )
  })
}

#' The rental locations nearest a point
#'
#' @param locs Data frame with `project`, `district_no`, `lat`, `lng`
#'   (one row per location).
#' @return `locs` with a `dist_m` column, nearest first.
#' @noRd
nearest_locations <- function(locs, lat, lng) {
  locs$dist_m <- 1000 * haversine_km(lat, lng, locs$lat, locs$lng)
  locs[order(locs$dist_m), ]
}

#' Past rentals to compare a place with, at four levels
#'
#' @param d Contracts to compare with (already narrowed to bedrooms and period).
#' @param place List with `lat`, `lng` and optionally `building`.
#' @param radius_m Radius for the "nearby" level, in metres.
#' @return A named list of contract subsets: "Same project", "Within <r> m",
#'   "Same district", "Same region". The project level is empty when the place
#'   is not a project in the data.
#' @noRd
comparable_sets <- function(d, place, radius_m = 500) {
  # Distances only use contracts placed at their building, not street-level matches
  placed <- !is.na(d$lat) & !grepl("^Street only", d$geo_match %||% "")
  locs <- distinct(d[placed, ], project, district_no, lat, lng)
  near <- nearest_locations(locs, place$lat, place$lng)

  # Same project: the OneMap building name, or a project within 50 m
  building <- toupper(trimws(place$building %||% ""))
  project <- if (nzchar(building) && building %in% d$project) building
             else if (nrow(near) && near$dist_m[1] <= 50) near$project[1]
             else NA_character_

  # District and region of the place = those of the nearest rental location
  district_no <- if (nrow(near)) near$district_no[1] else NA
  in_radius <- which(placed & 1000 * haversine_km(place$lat, place$lng, d$lat, d$lng) <= radius_m)

  sets <- list(
    d[d$project %in% project, ],
    d[in_radius, ],
    d[d$district_no %in% district_no, ],
    d[district_region(d$district_no) %in% district_region(district_no), ]
  )
  names(sets) <- c("Same project", paste0("Within ", format(radius_m, big.mark = ","), " m"),
                   "Same district", "Same region")
  attr(sets, "project")  <- project
  attr(sets, "district") <- d$district[match(district_no, d$district_no)]
  attr(sets, "region")   <- district_region(district_no)
  sets
}

#' Count, median, average and where an asking rent falls, per comparison level
#'
#' @param sets Output of comparable_sets().
#' @param asking Asking rent (NA if none given).
#' @param area_sqft Floor area of the listing (NA if not given).
#' @return One row per level: `level`, `n`, `median`, `mean`, `p25`, `p75`,
#'   `psf`, `pct_below` (percentile of the asking rent among past rents),
#'   `pct_psf_below` (same, for rent per sqft), and `diff_pct` /
#'   `diff_psf_pct` (% the asking rent / psf is above (+) or below (-) the median).
#' @noRd
compare_levels <- function(sets, asking = NA, area_sqft = NA) {
  asking_psf <- asking / area_sqft
  rows <- lapply(names(sets), function(level) {
    s <- sets[[level]]
    has <- nrow(s) > 0
    data.frame(
      level  = level,
      n      = nrow(s),
      median = if (has) median(s$rent) else NA,
      mean   = if (has) mean(s$rent) else NA,
      p25    = if (has) unname(quantile(s$rent, 0.25)) else NA,
      p75    = if (has) unname(quantile(s$rent, 0.75)) else NA,
      psf    = if (has) median(s$psf, na.rm = TRUE) else NA,
      pct_below     = if (has && !is.na(asking)) pct_below(s$rent, asking) else NA,
      pct_psf_below = if (has && !is.na(asking_psf)) pct_below(s$psf, asking_psf) else NA
    )
  })
  t <- do.call(rbind, rows)
  t$diff_pct     <- 100 * (asking / t$median - 1)
  t$diff_psf_pct <- 100 * (asking_psf / t$psf - 1)
  t
}

#' Signed % difference as text, e.g. "+4.2% above", "-3.1% below", "-" if NA
#' @noRd
signed_pct <- function(x) {
  ifelse(is.na(x), "-",
         ifelse(round(x, 1) == 0, "0.0% (at median)",
                sprintf("%+.1f%% %s", x, ifelse(x > 0, "above", "below"))))
}

#' Percentile of a value among past values (ties count half, so a value
#' equal to the median scores 50)
#' @noRd
pct_below <- function(x, value) {
  x <- x[!is.na(x)]
  100 * (mean(x < value) + mean(x == value) / 2)
}

#' One-sentence verdict on an asking rent, e.g.
#' "S$4,800 is higher than 72% of 2-bedroom rentals within 500 m (143 rentals)."
#'
#' @param row One row of compare_levels().
#' @param beds Bedroom group label, e.g. "2" (NULL or "Any" = all bedrooms).
#' @noRd
asking_verdict <- function(row, asking, beds = NULL) {
  what <- if (length(beds) == 1 && beds != "Any") paste0(beds, "-bedroom rentals") else "rentals"
  where <- if (startsWith(row$level, "Within")) tolower(row$level)
           else paste("in the", tolower(row$level))
  paste0(money(asking), " is ", higher_lower(row$pct_below), " of ", what, " ", where,
         " (", format(row$n, big.mark = ","), " rentals).")
}

#' "higher than 72%" / "lower than 60%", from the % of past rents below a value
#' @noRd
higher_lower <- function(pct_below) {
  pct <- round(pct_below)
  if (pct >= 50) paste0("higher than ", pct, "%") else paste0("lower than ", 100 - pct, "%")
}

#' One row per building: median rent and count (for the listing-check map)
#' @noRd
summarise_buildings <- function(d) {
  d |>
    group_by(project, lat, lng) |>
    summarise(median = median(rent), n = n(), .groups = "drop")
}
