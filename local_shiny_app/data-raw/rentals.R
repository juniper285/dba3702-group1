# ==================================================================
#  Build inst/extdata/rentals.rds, the data set the app reads.
#
#  Source: URA_Union_Cleaned_with_Address.xlsx (URA rental contracts,
#  combined and geocoded by the team; git-ignored, about 30 MB). Put it in
#  data-raw/ and run from the package root (local_shiny_app/):
#    source("data-raw/rentals.R")
#  Needs readxl (install.packages("readxl")); takes about a minute.
# ==================================================================

raw <- readxl::read_excel("data-raw/URA_Union_Cleaned_with_Address.xlsx", guess_max = 50000)

# Short column names without spaces or symbols
rentals <- data.frame(
  project     = raw$`Project Name`,
  street      = raw$`Street Name`,
  district_no = as.integer(raw$`Postal District`),
  type        = raw$`Property Type`,
  bedrooms    = suppressWarnings(as.numeric(raw$`No of Bedroom`)),  # missing -> NA
  rent        = raw$`Monthly Rent ($)`,
  area_band   = raw$`Floor Area (SQFT)`,
  area_sqft   = raw$`Floor Area (SQFT) Midpoint`,
  psf         = raw$`Rental Price (PSF)`,
  lease_date  = as.Date(raw$`Lease Commencement Date`),
  address     = raw$Address,
  lat         = raw$Latitude,
  lng         = raw$Longitude,
  geo_match   = raw$Match_Status
)

saveRDS(rentals, "inst/extdata/rentals.rds", compress = "xz")
