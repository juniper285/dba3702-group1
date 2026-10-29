# ---- Distances to MRT/LRT stations and destinations ------------------------

# Key places for distances and the map's "Key places" layers.
# Add your own as "Name" = c(latitude, longitude) in the right group.
# Places next to an MRT station use that station's coordinates; campuses and
# hospitals are approximate (within a few hundred metres).
destination_groups <- list(
  "Universities" = list(
    "NUS (Kent Ridge)"                 = c(1.2966, 103.7764),
    "NUS (Bukit Timah campus)"         = c(1.3189, 103.8177),
    "NTU"                              = c(1.3483, 103.6831),
    "SMU"                              = c(1.2963, 103.8502),
    "SUTD"                             = c(1.3413, 103.9638),
    "SUSS (Clementi Road)"             = c(1.3292, 103.7760),
    "SIM (Clementi Road)"              = c(1.3290, 103.7766),
    "SIT (Punggol campus)"             = c(1.4134, 103.9095),
    "University of the Arts (LASALLE)" = c(1.3026, 103.8520),
    "James Cook University Singapore"  = c(1.3161, 103.8774)
  ),
  "Polytechnics" = list(
    "Ngee Ann Polytechnic"  = c(1.3331, 103.7759),
    "Singapore Polytechnic" = c(1.3099, 103.7776),
    "Temasek Polytechnic"   = c(1.3454, 103.9326),
    "Nanyang Polytechnic"   = c(1.3801, 103.8492),
    "Republic Polytechnic"  = c(1.4433, 103.7852)
  ),
  "Business districts" = list(
    "CBD (Raffles Place)"                = c(1.2841, 103.8515),
    "Marina Bay Financial Centre"        = c(1.2795, 103.8540),
    "Tanjong Pagar"                      = c(1.2766, 103.8460),
    "Orchard Road"                       = c(1.3040, 103.8322),
    "one-north"                          = c(1.2998, 103.7875),
    "Singapore Science Park"             = c(1.2927, 103.7851),
    "Mapletree Business City"            = c(1.2745, 103.7995),
    "Jurong Lake District (Jurong East)" = c(1.3332, 103.7423),
    "Paya Lebar Quarter"                 = c(1.3172, 103.8923),
    "Changi Business Park"               = c(1.3343, 103.9625),
    "Woodlands Regional Centre"          = c(1.4370, 103.7865)
  ),
  "Transport" = list(
    "Changi Airport (T2/T3)"          = c(1.3575, 103.9879),
    "HarbourFront / VivoCity"         = c(1.2654, 103.8215),
    "Tanah Merah Ferry Terminal"      = c(1.3156, 103.9903),
    "Woodlands Checkpoint (to Johor)" = c(1.4452, 103.7690)
  ),
  "Hospitals" = list(
    "Singapore General Hospital"         = c(1.2797, 103.8352),
    "National University Hospital"       = c(1.2942, 103.7832),
    "Tan Tock Seng Hospital"             = c(1.3215, 103.8458),
    "Changi General Hospital"            = c(1.3402, 103.9497),
    "KK Women's and Children's Hospital" = c(1.3105, 103.8466),
    "Mount Elizabeth Hospital"           = c(1.3053, 103.8355)
  )
)
destinations <- do.call(c, unname(destination_groups))          # one flat named list
destination_choices <- lapply(destination_groups, names)        # grouped dropdown

# The key places as a table (name, group, lat, lng), for the map
key_places <- data.frame(
  name  = names(destinations),
  group = rep(names(destination_groups), lengths(destination_groups)),
  lat   = vapply(destinations, `[`, numeric(1), 1),
  lng   = vapply(destinations, `[`, numeric(1), 2),
  row.names = NULL
)

#' Straight-line distance in km between two points
#' @noRd
haversine_km <- function(lat1, lng1, lat2, lng2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 +
       cos(lat1 * r) * cos(lat2 * r) * sin((lng2 - lng1) * r / 2)^2
  6371 * 2 * asin(sqrt(a))
}

walk_m_per_min <- 80   # walking speed for the "minutes to MRT" estimates
mrt_max        <- 2000 # MRT-distance slider's top end means "any distance"

#' MRT/LRT stations (inst/extdata/mrt_stations.csv: station, lat, lng, kind)
#'
#' @return The stations, or NULL if the file is missing.
#' @noRd
mrt_stations <- function() {
  cached("mrt", {
    f <- app_sys("extdata", "mrt_stations.csv")
    if (nzchar(f)) read.csv(f) else NULL
  })
}

#' Schools (inst/extdata/schools.csv: name, lat, lng, optionally level)
#'
#' @return The schools, or NULL if the file is missing or lacks those columns.
#' @noRd
schools_data <- function() {
  cached("schools", {
    f <- app_sys("extdata", "schools.csv")
    s <- if (nzchar(f)) read.csv(f) else NULL
    if (!is.null(s)) names(s) <- tolower(names(s))
    if (is.null(s) || !all(c("name", "lat", "lng") %in% names(s))) NULL
    else s[!is.na(s$lat) & !is.na(s$lng), ]
  })
}

#' Location (lat, lng) of a station by name
#' @noRd
station_location <- function(station, mrt = mrt_stations()) {
  st <- mrt[mrt$station == station, ][1, ]
  c(st$lat, st$lng)
}

#' Nearest MRT/LRT station to each point (straight line)
#'
#' @param mrt Data frame with columns station, lat, lng.
#' @return Data frame with `mrt_station` and `mrt_m` (metres), one row per point.
#' @noRd
nearest_mrt <- function(lat, lng, mrt) {
  best <- rep(Inf, length(lat))
  station <- rep(NA_character_, length(lat))
  for (j in seq_len(nrow(mrt))) {
    d <- haversine_km(lat, lng, mrt$lat[j], mrt$lng[j]) * 1000
    closer <- !is.na(d) & d < best
    best[closer] <- d[closer]
    station[closer] <- mrt$station[j]
  }
  data.frame(mrt_station = station, mrt_m = ifelse(is.finite(best), round(best), NA_real_))
}

#' Estimated walking time in minutes for a straight-line distance in metres
#' @noRd
walk_minutes <- function(m) round(m / walk_m_per_min)

#' Add each contract's nearest MRT/LRT station (`mrt_station`, `mrt_m`)
#'
#' Worked out once per map location. Both columns are NA when the contract
#' has no coordinates or `mrt` is NULL.
#' @noRd
add_location_mrt <- function(d, mrt) {
  d$mrt_station <- NA_character_
  d$mrt_m <- NA_real_
  if (is.null(mrt)) return(d)
  first <- !is.na(d$loc_id) & !duplicated(d$loc_id)
  near <- nearest_mrt(d$lat[first], d$lng[first], mrt)
  i <- match(d$loc_id, d$loc_id[first])
  d$mrt_station <- near$mrt_station[i]
  d$mrt_m <- near$mrt_m[i]
  d
}

#' One location per project (the location with the most contracts), with
#' its nearest MRT/LRT station
#' @noRd
project_coords <- function() {
  cached("coords", {
    rentals_data() |>
      filter(!is.na(loc_id)) |>
      count(project, loc_id, lat, lng, mrt_station, mrt_m) |>
      group_by(project) |>
      slice_max(n, n = 1, with_ties = FALSE) |>
      ungroup() |>
      select(project, lat, lng, mrt_station, mrt_m) |>
      as.data.frame()
  })
}

#' Key places as a places table (name, lat, lng), in the order given
#' @noRd
key_place <- function(names) {
  p <- key_places[match(names, key_places$name), c("name", "lat", "lng")]
  p <- p[!is.na(p$name), ]
  rownames(p) <- NULL
  p
}

#' Name of the distance column for a place, e.g. "To NTU (km)"
#' @noRd
place_column <- function(name) paste0("To ", name, " (km)", recycle0 = TRUE)   # none for no places

#' Add distance columns to a project-level table
#'
#' @param s Data frame with a `project` column.
#' @param places Data frame with `name`, `lat`, `lng`: one "To <name> (km)"
#'   column is added per place.
#' @noRd
add_distance <- function(s, places, coords = project_coords()) {
  keep <- intersect(c("project", "lat", "lng", "mrt_station", "mrt_m"), names(coords))
  s <- left_join(s, coords[, keep], by = "project")
  for (i in seq_len(nrow(places))) {
    s[[place_column(places$name[i])]] <-
      round(haversine_km(s$lat, s$lng, places$lat[i], places$lng[i]), 1)
  }
  if ("mrt_m" %in% names(s)) s$walk_min <- walk_minutes(s$mrt_m)
  select(s, -lat, -lng)
}
