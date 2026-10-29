# ---- Distances to MRT/LRT stations and destinations ------------------------

# Preferred destinations (add your own: "Name" = c(latitude, longitude))
destinations <- list(
  "NUS (Kent Ridge)"       = c(1.2966, 103.7764),
  "NTU"                    = c(1.3483, 103.6831),
  "SMU"                    = c(1.2963, 103.8502),
  "SUTD"                   = c(1.3413, 103.9638),
  "Raffles Place (CBD)"    = c(1.2840, 103.8514),
  "one-north"              = c(1.2995, 103.7873),
  "Changi Business Park"   = c(1.3343, 103.9625)
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

#' Add distance columns to a project-level table
#'
#' @param s Data frame with a `project` column.
#' @param destination A name from `destinations`.
#' @noRd
add_distance <- function(s, destination, coords = project_coords()) {
  keep <- intersect(c("project", "lat", "lng", "mrt_station", "mrt_m"), names(coords))
  s <- left_join(s, coords[, keep], by = "project")
  dest <- destinations[[destination]]
  s$dest_km <- round(haversine_km(s$lat, s$lng, dest[1], dest[2]), 1)
  if ("mrt_m" %in% names(s)) s$walk_min <- walk_minutes(s$mrt_m)
  select(s, -lat, -lng)
}
