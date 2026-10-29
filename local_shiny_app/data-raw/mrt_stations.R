# ==================================================================
#  Build inst/extdata/mrt_stations.csv: one row per MRT/LRT station.
#
#  Source: LTA "MRT Station Exit" (GeoJSON) on data.gov.sg. A station's
#  location is the average of its exits.
#  Run from the package root (local_shiny_app/):
#    source("data-raw/mrt_stations.R")
# ==================================================================

dataset <- "d_b39d3a0871985372d7e1637193335da5"
poll <- jsonlite::fromJSON(paste0(
  "https://api-open.data.gov.sg/v1/public/api/datasets/", dataset, "/poll-download"))
exits <- jsonlite::fromJSON(poll$data$url)$features

xy <- do.call(rbind, exits$geometry$coordinates)
exits <- data.frame(name = exits$properties$STATION_NA, lng = xy[, 1], lat = xy[, 2])

stations <- aggregate(cbind(lat, lng) ~ name, data = exits, FUN = mean)

# "BUKIT PANJANG LRT STATION" -> "Bukit Panjang LRT"
nm <- tools::toTitleCase(tolower(sub(" STATION$", "", stations$name)))
nm <- sub("Mrt$", "MRT", sub("Lrt$", "LRT", nm))

mrt <- data.frame(
  station = nm,
  lat     = round(stations$lat, 6),
  lng     = round(stations$lng, 6),
  kind    = ifelse(grepl("LRT", stations$name), "LRT", "MRT")
)
mrt <- mrt[order(mrt$station), ]

write.csv(mrt, "inst/extdata/mrt_stations.csv", row.names = FALSE)
