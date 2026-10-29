# ---- Personal settings: what the renter is looking for ----------------
#
# Set up by the first-visit wizard and the "My settings" dialog, kept in the
# browser's localStorage (settings.js) and turned into filters by
# mod_filters_server().

place_km_max <- 30   # "max distance to my places" slider's top end means "any distance"
max_places   <- 6

#' Valid choices for each setting, from the rental data
#' @noRd
settings_choices <- function(rentals = rentals_data()) {
  list(districts = sort(unique(rentals$district)),
       types     = sort(unique(rentals$type)),
       stations  = sort(mrt_stations()$station),
       dates     = range(rentals$lease_date))
}

#' Settings for someone who has not set anything up yet
#' @noRd
default_settings <- function(choices = settings_choices()) {
  list(
    budget    = c(3000, 5000),
    places    = key_place("NUS (Kent Ridge)"),
    districts = character(),
    beds      = bed_levels,
    types     = choices$types,
    area      = c(0, area_max),
    period    = choices$dates,
    recent    = 12,
    mrt_pick  = "",
    mrt_dist  = mrt_max,
    place_km  = place_km_max
  )
}

#' Settings as JSON, for the browser's localStorage
#' @noRd
settings_to_json <- function(s) {
  s$period <- format(s$period)
  s$version <- 1
  as.character(jsonlite::toJSON(s, auto_unbox = TRUE, digits = NA))
}

#' Read settings saved by settings_to_json()
#'
#' Each field is checked against `choices`; a missing or invalid field falls
#' back to its default, so old or damaged saved settings can't break the app.
#' @return A settings list, or NULL if `json` is empty or not JSON at all.
#' @noRd
settings_from_json <- function(json, choices = settings_choices()) {
  if (is.null(json) || !nzchar(json) || identical(json, "none")) return(NULL)
  x <- tryCatch(jsonlite::fromJSON(json, simplifyVector = TRUE), error = function(e) NULL)
  if (!is.list(x)) return(NULL)
  s <- default_settings(choices)

  num_range <- function(v, lo, hi) {
    v <- suppressWarnings(as.numeric(v))
    if (length(v) == 2 && !anyNA(v) && v[1] <= v[2] && v[1] >= lo && v[2] <= hi) v else NULL
  }
  one_num <- function(v, lo, hi) {
    v <- suppressWarnings(as.numeric(v))
    if (length(v) == 1 && !is.na(v) && v >= lo && v <= hi) v else NULL
  }
  pick <- function(v, ok, allow_empty = FALSE) {
    v <- intersect(as.character(unlist(v)), ok)
    if (length(v) || allow_empty) v else NULL
  }
  keep <- function(name, value) if (!is.null(value)) s[[name]] <<- value

  keep("budget",    num_range(x$budget, 0, 1e5))
  keep("districts", pick(x$districts, choices$districts, allow_empty = TRUE))
  keep("beds",      pick(x$beds, bed_levels))
  keep("types",     pick(x$types, choices$types))
  keep("area",      num_range(x$area, 0, area_max))
  keep("recent",    if (isTRUE(as.numeric(x$recent) %in% c(6, 12, 24))) as.numeric(x$recent))
  keep("mrt_pick",  if (identical(x$mrt_pick, "") || isTRUE(x$mrt_pick %in% choices$stations)) x$mrt_pick)
  keep("mrt_dist",  one_num(x$mrt_dist, 100, mrt_max))
  keep("place_km",  one_num(x$place_km, 1, place_km_max))

  # Period: clamp to the data we have
  p <- tryCatch(as.Date(unlist(x$period)), error = function(e) NULL)
  if (length(p) == 2 && !anyNA(p)) {
    p <- pmin(pmax(p, choices$dates[1]), choices$dates[2])
    if (p[1] <= p[2]) s$period <- p
  }

  # Places: name + a location in Singapore
  pl <- x$places
  if (is.data.frame(pl) && all(c("name", "lat", "lng") %in% names(pl))) {
    pl <- data.frame(name = as.character(pl$name), lat = as.numeric(pl$lat),
                     lng = as.numeric(pl$lng))
    pl <- pl[!is.na(pl$lat) & !is.na(pl$lng) & nzchar(pl$name) &
               pl$lat > 1.1 & pl$lat < 1.5 & pl$lng > 103.5 & pl$lng < 104.1, ]
    s$places <- utils::head(pl[!duplicated(pl$name), ], max_places)
    rownames(s$places) <- NULL
  } else if (is.list(pl) && length(pl) == 0) {
    s$places <- s$places[0, ]
  }
  s
}

#' Short description of the settings, one phrase per setting
#' @noRd
settings_summary <- function(s, choices = settings_choices()) {
  c(
    paste0(money(s$budget[1]), " - ", money(s$budget[2])),
    if (setequal(s$beds, bed_levels)) "any bedrooms"
    else paste(paste(s$beds, collapse = ", "), "bedrooms"),
    if (!setequal(s$types, choices$types)) paste(s$types, collapse = ", "),
    if (s$area[1] > 0 || s$area[2] < area_max)
      paste0(format(s$area[1], big.mark = ","), "-", format(s$area[2], big.mark = ","), " sqft"),
    if (length(s$districts) == 0) "anywhere in Singapore"
    else if (length(s$districts) <= 2) paste(sub(" .*", "", s$districts), collapse = ", ")
    else paste(length(s$districts), "districts"),
    if (nrow(s$places)) paste("near", paste(s$places$name, collapse = ", ")),
    if (nrow(s$places) && s$place_km < place_km_max) paste0("within ", s$place_km, " km of them"),
    if (nzchar(s$mrt_pick)) paste0("within ", s$mrt_dist, " m of ", s$mrt_pick)
    else if (s$mrt_dist < mrt_max) paste0("within ", s$mrt_dist, " m of an MRT/LRT"),
    paste0(format(s$period[1], "%b %Y"), " - ", format(s$period[2], "%b %Y"))
  )
}

#' Script that loads and saves the settings in the browser (inst/app/www/settings.js)
#' @noRd
settings_dependency <- function() {
  htmltools::htmlDependency("rentlens-settings", "1.0.0", src = app_sys("app", "www"),
                            script = "settings.js")
}
