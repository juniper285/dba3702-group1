# ---- Distances to MRT stations and destinations ------------------------

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

#' Add each project's nearest MRT station and its distance
#'
#' @param coords Data frame with columns project, lat, lng.
#' @param mrt Data frame with columns station, lat, lng.
#' @noRd
add_nearest_mrt <- function(coords, mrt) {
  coords$mrt_station <- NA_character_
  coords$mrt_km      <- NA_real_
  for (i in seq_len(nrow(coords))) {
    d <- haversine_km(coords$lat[i], coords$lng[i], mrt$lat, mrt$lng)
    if (all(is.na(d))) next
    j <- which.min(d)
    coords$mrt_station[i] <- mrt$station[j]
    coords$mrt_km[i]      <- round(d[j], 1)
  }
  coords
}

#' One location per project (median of its geocoded addresses)
#'
#' Nearest-MRT columns are added when inst/extdata/mrt_stations.csv exists
#' (columns station, lat, lng).
#' @noRd
project_coords <- function() {
  cached("coords", {
    coords <- rentals_data() |>
      filter(!is.na(lat)) |>
      group_by(project) |>
      summarise(lat = median(lat), lng = median(lng), .groups = "drop") |>
      as.data.frame()
    mrt_file <- app_sys("extdata", "mrt_stations.csv")
    if (nzchar(mrt_file)) coords <- add_nearest_mrt(coords, read.csv(mrt_file))
    coords
  })
}

#' Add distance columns to a project-level table
#'
#' @param s Data frame with a `project` column.
#' @param destination A name from `destinations`.
#' @noRd
add_distance <- function(s, destination, coords = project_coords()) {
  keep <- intersect(c("project", "lat", "lng", "mrt_station", "mrt_km"), names(coords))
  s <- left_join(s, coords[, keep], by = "project")
  dest <- destinations[[destination]]
  s$dest_km <- round(haversine_km(s$lat, s$lng, dest[1], dest[2]), 1)
  select(s, -lat, -lng)
}
