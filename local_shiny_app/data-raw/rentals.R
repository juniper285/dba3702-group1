# ==================================================================
#  Build inst/extdata/rentals.rds, the data set the app reads.
#
#  Source: the geocoded copy built by the map app
#  (../sg_rental_map/data/rentals.rds, from URA_Union_Cleaned_with_Address.xlsx).
#  Run from the package root (local_shiny_app/) after that file changes:
#    source("data-raw/rentals.R")
# ==================================================================

raw <- readRDS("../sg_rental_map/data/rentals.rds")

# Short column names without spaces or symbols
rentals <- data.frame(
  project     = raw$project,
  street      = raw$street,
  district_no = raw$district,
  type        = raw$type,
  bedrooms    = suppressWarnings(as.numeric(raw$bedrooms)),  # "Unknown" -> NA
  rent        = raw$rent,
  area_band   = raw$area_sqft,
  area_sqft   = raw$sqft,
  psf         = raw$psf,
  lease_date  = raw$month,
  address     = raw$address,
  lat         = raw$lat,
  lng         = raw$lng,
  geo_match   = raw$match
)

saveRDS(rentals, "inst/extdata/rentals.rds", compress = "xz")
