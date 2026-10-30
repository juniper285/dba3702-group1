# SG Rental Map

**RentLens, live:** https://01a18a24-12d4-aa74-2e7f-2340d690bbe1.share.connect.posit.cloud/ (`local_shiny_app/`, redeployed from `main` on every push; see [its README](local_shiny_app/README.md#deploying-posit-connect-cloud)).

An interactive R Shiny map of Singapore private residential rental contracts (URA data, Sep 2021 – Aug 2026, 412,595 contracts). It's styled after [viscover](https://github.com/XiaodanLyu/viscover).

## Features

- **Map:** one circle per building, coloured by median rent psf or median monthly rent and sized by number of contracts. Includes an optional heatmap layer and Light, OneMap and Satellite basemaps.
- **Click a building:** see its contract count, median rent and psf, bedroom breakdown and monthly trend.
- **Draw a shape:** use the polygon, rectangle or circle tool to summarise every contract in that area.
- **Filters:** lease month range, property type, bedrooms, postal district, geocode quality.
- **Export:** download the contracts in the current selection as CSV.

## Run it

```r
install.packages(c("shiny", "shinydashboard", "leaflet", "leaflet.extras",
                   "sf", "dplyr", "DT", "plotly", "readxl"))
shiny::runApp("sg_rental_map")
```

## Data

The rental contracts come from URA's
[Data Service API](https://eservice.ura.gov.sg/maps/api/#introduction)
(private residential rental contracts; needs a free URA access key). Every
data source the apps use is listed under [Data sources](#data-sources) below.

`sg_rental_map/data/rentals.rds` is a cleaned, fast-loading copy of
`URA_Union_Cleaned_with_Address.xlsx`. To refresh it:

1. Put the updated `URA_Union_Cleaned_with_Address.xlsx` in `sg_rental_map/data/`.
2. Delete `rentals.rds`.
3. Start the app. It rebuilds the `.rds` from the Excel file in about 30 seconds.

The Excel file itself is git-ignored because of its size (about 30 MB).

### Notes

- **Missing coordinates:** 1,647 contracts are not shown on the map.
- **Default geocode filter:** only *Exact* and *Good* matches are shown at first. *Street only* matches sit at the street rather than the building, and you can switch them on in the sidebar.

## Data sources

Every data set RentLens (`local_shiny_app`) uses, where it comes from and the code
that pulls it. The SG Rental Map app uses the rental contracts only.
Rebuild scripts live in `local_shiny_app/data-raw/` and write to
`local_shiny_app/inst/extdata/`; run them from `local_shiny_app/`.

| Data | Source | Pulled by | Shipped as |
|------|--------|-----------|------------|
| Rental contracts | [URA Data Service API](https://eservice.ura.gov.sg/maps/api/#introduction): private residential rental contracts | team's cleaning and geocoding (see below), then `local_shiny_app/data-raw/rentals.R` | `local_shiny_app/inst/extdata/rentals.rds` |
| MRT/LRT stations | [LTA MRT Station Exit (GeoJSON)](https://data.gov.sg/datasets/d_b39d3a0871985372d7e1637193335da5/view), data.gov.sg | `local_shiny_app/data-raw/mrt_stations.R` | `local_shiny_app/inst/extdata/mrt_stations.csv` |
| Schools | [MOE General information of schools](https://data.gov.sg/datasets/d_688b934f82c1059ed0a6993d2a829089/view), data.gov.sg, geocoded with the [OneMap Search API](https://www.onemap.gov.sg/apidocs/) | `local_shiny_app/data-raw/schools.R` | `local_shiny_app/inst/extdata/schools.csv` |
| Key places (universities, business districts, ...) | typed in by hand | `destination_groups` in `local_shiny_app/R/fct_location.R` | code |
| Address lookup ("Check a listing") | [OneMap Search API](https://www.onemap.gov.sg/apidocs/), live | `onemap_search()` in `local_shiny_app/R/fct_listing.R` | cached per R session |
| Place suggestions ("Places that matter" in My settings) | [OneMap Search API](https://www.onemap.gov.sg/apidocs/), live, called from the browser as you type | `searchPlaces()` in `local_shiny_app/inst/app/www/settings.js` | not stored; the chosen place is saved in the browser with the settings |
| Basemaps | Esri World Gray Canvas and World Imagery; [OneMap](https://www.onemap.gov.sg/) tiles (Singapore Land Authority) | `base_map()` in `local_shiny_app/R/fct_map.R` | loaded live |
| Map place search | OpenStreetMap Nominatim, via `leaflet.extras::addSearchOSM()` | `base_map()` | live |

None of these sources needs an API key, except the URA API.

### Rental contracts (URA)

URA publishes private residential rental contracts through its
[Data Service API](https://eservice.ura.gov.sg/maps/api/#introduction).
The API needs a free access key (register with URA) and a token generated
daily from that key; see URA's documentation for the endpoint and
parameters.

The team combined the URA downloads into `URA_Union_Cleaned.xlsx`, then added
each project's address and coordinates (`Address`, `Latitude`, `Longitude`)
and a `Match_Status` geocode-quality flag, giving
`URA_Union_Cleaned_with_Address.xlsx`. The scripts for those steps are not in
this repo. The Excel file is git-ignored (about 30 MB).

From there:

1. `sg_rental_map/app.R` converts the Excel file to `sg_rental_map/data/rentals.rds`
   on first run (see [Data](#data) above).
2. `source("data-raw/rentals.R")` (from `local_shiny_app/`) renames the columns and writes
   `local_shiny_app/inst/extdata/rentals.rds`:

```r
raw <- readRDS("../sg_rental_map/data/rentals.rds")
rentals <- data.frame(project = raw$project, ..., lat = raw$lat, lng = raw$lng,
                      geo_match = raw$match)
saveRDS(rentals, "inst/extdata/rentals.rds", compress = "xz")
```

### MRT/LRT stations (LTA, data.gov.sg)

data.gov.sg's poll-download endpoint returns a short-lived link to the
GeoJSON of every station exit. A station's location is the average of its
exits (188 stations, 41 of them LRT):

```r
dataset <- "d_b39d3a0871985372d7e1637193335da5"
poll <- jsonlite::fromJSON(paste0(
  "https://api-open.data.gov.sg/v1/public/api/datasets/", dataset, "/poll-download"))
exits <- jsonlite::fromJSON(poll$data$url)$features   # one row per station exit
stations <- aggregate(cbind(lat, lng) ~ name, data = exits, FUN = mean)
```

### Schools (MOE, data.gov.sg + OneMap)

MOE's school list has names, levels and postal codes but no coordinates, so
each postal code is looked up with OneMap, keeping only exact postal matches
(all 337 schools matched in October 2026). Only name, level, postal code and
location are kept; contact and staff details are dropped.

```r
dataset <- "d_688b934f82c1059ed0a6993d2a829089"
moe <- jsonlite::fromJSON(paste0(
  "https://data.gov.sg/api/action/datastore_search?resource_id=", dataset, "&limit=1000"
))$result$records

httr2::request("https://www.onemap.gov.sg/api/common/elastic/search") |>
  httr2::req_url_query(searchVal = postal, returnGeom = "Y", getAddrDetails = "Y", pageNum = 1) |>
  httr2::req_perform() |>
  httr2::resp_body_json()      # results[[i]]$LATITUDE / $LONGITUDE where $POSTAL matches
```

### Refreshing everything

```r
# from local_shiny_app/
source("data-raw/rentals.R")       # after sg_rental_map/data/rentals.rds is rebuilt
source("data-raw/mrt_stations.R")  # LTA station exits
source("data-raw/schools.R")       # MOE schools + OneMap (about a minute)
```
