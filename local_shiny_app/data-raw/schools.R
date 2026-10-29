# ==================================================================
#  Build inst/extdata/schools.csv: primary, secondary and
#  pre-university schools with their locations.
#
#  Source: MOE "General information of schools" on data.gov.sg (names,
#  levels, postal codes), geocoded by postal code with the OneMap Search
#  API (no key needed). Only the columns the app uses are kept.
#  Run from the package root (local_shiny_app/):
#    source("data-raw/schools.R")
# ==================================================================

dataset <- "d_688b934f82c1059ed0a6993d2a829089"
res <- jsonlite::fromJSON(paste0(
  "https://data.gov.sg/api/action/datastore_search?resource_id=", dataset, "&limit=1000"))
moe <- res$result$records

geocode <- function(postal) {
  r <- httr2::request("https://www.onemap.gov.sg/api/common/elastic/search") |>
    httr2::req_url_query(searchVal = postal, returnGeom = "Y", getAddrDetails = "Y", pageNum = 1) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  hit <- Filter(function(h) h$POSTAL == postal, r$results)   # exact postal code only
  Sys.sleep(0.1)                                             # be gentle with OneMap
  if (length(hit) == 0) return(c(NA, NA))
  as.numeric(c(hit[[1]]$LATITUDE, hit[[1]]$LONGITUDE))
}

postal <- sprintf("%06d", as.integer(moe$postal_code))
xy <- t(vapply(postal, geocode, numeric(2)))

schools <- data.frame(
  name   = tools::toTitleCase(tolower(moe$school_name)),
  level  = gsub("Jc", "JC", tools::toTitleCase(tolower(moe$mainlevel_code))),
  postal = postal,
  lat    = round(xy[, 1], 6),
  lng    = round(xy[, 2], 6)
)
message(sum(is.na(schools$lat)), " of ", nrow(schools), " schools could not be geocoded")
schools <- schools[!is.na(schools$lat), ]
schools <- schools[order(schools$name), ]

write.csv(schools, "inst/extdata/schools.csv", row.names = FALSE)
