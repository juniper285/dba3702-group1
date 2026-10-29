# ---- Personal settings: what the renter is looking for ----------------
#
# Set up by the first-visit wizard and the "My settings" dialog, kept in the
# browser's localStorage (settings.js) and turned into filters by
# mod_filters_server().

place_km_max <- 30   # "max distance to my places" slider's top end means "any distance"
max_places   <- 6

# How far back to look, in months: one window for every page (the data
# covers five years)
recent_choices <- c("Last 6 months" = 6, "Last 12 months" = 12, "Last 2 years" = 24,
                    "Last 3 years" = 36, "Last 5 years" = 60)

#' Lease-start dates covered by the last `months` months of data
#'
#' Matches latest_months(): rentals after the cutoff, up to the latest one.
#' @param dates First and last lease date in the data.
#' @noRd
period_for <- function(months, dates) {
  cutoff <- seq(dates[2], by = paste0("-", months, " months"), length.out = 2)[2]
  c(max(cutoff + 1, dates[1]), dates[2])
}

# Icons a renter can give a place: `icon` for the app (Font Awesome 6),
# `map_icon` for the map's pins (leaflet's Font Awesome 4 names)
place_kinds <- data.frame(
  kind     = c("work", "study", "home", "family", "leisure", "other"),
  label    = c("Work", "School", "Home", "Family", "Leisure", "Other"),
  icon     = c("briefcase", "graduation-cap", "house", "heart", "dumbbell", "location-dot"),
  map_icon = c("briefcase", "graduation-cap", "home", "heart", "bicycle", "map-marker")
)

#' A table of places (name, kind, label, address, lat, lng); no rows by default
#'
#' `label` is the kind of place in words: the icon's label ("Work"), or what
#' the renter typed for "Other" ("Church").
#' @noRd
new_places <- function(name = character(), kind = character(), address = character(),
                       lat = numeric(), lng = numeric(), label = place_label(kind)) {
  data.frame(name = name, kind = kind, label = label, address = address, lat = lat, lng = lng)
}

#' The kind of place in words: the icon's label, or `custom` (cleaned up)
#' for "Other" when one was typed
#' @noRd
place_label <- function(kind, custom = "") {
  label <- place_kinds$label[match(kind, place_kinds$kind)]
  label[is.na(label)] <- "Other"
  custom <- substr(gsub("\\s+", " ", trimws(rep_len(as.character(custom), length(kind)))), 1, 30)
  own <- kind == "other" & !is.na(custom) & nzchar(custom)
  label[own] <- paste0(toupper(substr(custom[own], 1, 1)), substring(custom[own], 2))
  label
}

#' The place chosen in the place search box, as one onemap_search() row
#'
#' A suggestion carries its OneMap result as JSON; anything else is text the
#' renter typed without picking a suggestion, which is looked up with
#' onemap_search() (first match).
#' @return Data frame with `name`, `address`, `building`, `postal`, `lat`,
#'   `lng` (no rows if nothing was found).
#' @noRd
chosen_place <- function(value, search = onemap_search) {
  value <- trimws(if (is.null(value)) "" else value)
  if (!nzchar(value)) return(data.frame())
  x <- if (startsWith(value, "{")) tryCatch(jsonlite::fromJSON(value), error = function(e) NULL)
  if (is.list(x) && all(c("name", "address", "lat", "lng") %in% names(x))) {
    return(data.frame(name = as.character(x$name), address = as.character(x$address),
                      building = if (is.null(x$building)) "NIL" else as.character(x$building),
                      postal = if (is.null(x$postal)) "NIL" else as.character(x$postal),
                      lat = as.numeric(x$lat), lng = as.numeric(x$lng)))
  }
  hit <- search(value)
  if (nrow(hit) == 0) return(data.frame())
  hit <- hit[1, ]
  hit$name <- if (nzchar(hit$building) && hit$building != "NIL") hit$building else hit$address
  hit
}

#' Street address of a OneMap search result, e.g. "1 Fusionopolis Way, Singapore 138632"
#'
#' OneMap's ADDRESS ends with the building name, which can be a shop inside
#' the building, so the building name is dropped and the postal code added.
#' @param hit One row of onemap_search().
#' @noRd
place_address <- function(hit) {
  a <- hit$address
  if (nzchar(hit$building) && hit$building != "NIL") a <- sub(hit$building, "", a, fixed = TRUE)
  a <- trimws(sub("SINGAPORE \\d{6}$", "", trimws(a)))
  # Some places' address is just their name: keep it rather than nothing
  if (!nzchar(a)) a <- trimws(sub("SINGAPORE \\d{6}$", "", trimws(hit$address)))
  a <- tools::toTitleCase(tolower(a))
  if (grepl("^\\d{6}$", hit$postal)) paste0(a, ", Singapore ", hit$postal) else a
}

#' A name for a new place when none was typed: its label, numbered if
#' taken ("Work", "Work 2", "Church", ...)
#' @noRd
default_place_name <- function(kind, taken, label = place_label(kind)) {
  base <- if (kind %in% place_kinds$kind) label else "Place"
  name <- base; i <- 1
  while (name %in% taken) { i <- i + 1; name <- paste(base, i) }
  name
}

#' Font Awesome icon name for a place kind ("other" when unknown)
#' @noRd
place_icon <- function(kind, column = "icon") {
  i <- match(kind, place_kinds$kind)
  i[is.na(i)] <- nrow(place_kinds)
  place_kinds[[column]][i]
}

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
    places    = new_places(),
    districts = character(),
    beds      = bed_levels,
    types     = choices$types,
    area      = c(0, area_max),
    period    = period_for(12, choices$dates),
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
  keep("recent",    if (isTRUE(as.numeric(x$recent) %in% recent_choices)) as.numeric(x$recent))
  keep("mrt_pick",  if (identical(x$mrt_pick, "") || isTRUE(x$mrt_pick %in% choices$stations)) x$mrt_pick)
  keep("mrt_dist",  one_num(x$mrt_dist, 100, mrt_max))
  keep("place_km",  one_num(x$place_km, 1, place_km_max))

  # The period always follows the window, up to the latest data
  s$period <- period_for(s$recent, choices$dates)

  # Places: name + a location in Singapore (kind and address are optional)
  pl <- x$places
  if (is.data.frame(pl) && all(c("name", "lat", "lng") %in% names(pl))) {
    kind <- if (is.null(pl$kind)) "other" else as.character(pl$kind)
    kind <- ifelse(kind %in% place_kinds$kind, kind, "other")
    pl <- new_places(name = as.character(pl$name), kind = kind,
                     label = place_label(kind, if (is.null(pl$label)) "" else pl$label),
                     address = if (is.null(pl$address)) "" else as.character(pl$address),
                     lat = as.numeric(pl$lat), lng = as.numeric(pl$lng))
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
    tolower(names(recent_choices)[match(s$recent, recent_choices)])
  )
}

#' Script that loads and saves the settings in the browser (inst/app/www/settings.js)
#' @noRd
settings_dependency <- function() {
  htmltools::htmlDependency("rentlens-settings", "1.0.0", src = app_sys("app", "www"),
                            script = "settings.js")
}
