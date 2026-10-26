# ==================================================================
#  SG Rental Map: combined app (DBA3702 Group 1)
#
#  Tabs: Map | Budget check | Within budget | Price trends |
#        Compare areas | Shortlist | Transactions | About
#
#  Folder layout (paths are relative, so it runs on every laptop):
#    app.R
#    data/URA_Union_Cleaned_with_Address.xlsx   (contracts with Latitude/Longitude)
#    data/mrt_lrt.csv                           (MRT/LRT station locations)
#
#  First run converts the Excel file to data/rentals.rds (about a minute);
#  later runs load in a few seconds. To refresh the data, replace the Excel
#  file and delete data/rentals.rds.
#
#  One-time install (in the Console):
#  install.packages(c("shiny", "bslib", "DT", "readxl", "ggplot2", "dplyr",
#                     "leaflet", "leaflet.extras", "sf", "plotly"))
# ==================================================================

library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(plotly)

data_file <- "data/URA_Union_Cleaned_with_Address.xlsx"
mrt_file  <- "data/mrt_lrt.csv"
rds_file  <- "data/rentals.rds"


# ---- 1. LOAD CONTRACTS ---------------------------------------------

# The cache can be in this app's format (has loc_lat) or the original
# SG Rental Map format (has month, lat, lng); both are read below.
# The Excel file is only needed when there is no usable cache.
cache_cols <- if (file.exists(rds_file)) names(readRDS(rds_file)) else character()
cache_ok <- "loc_lat" %in% cache_cols || all(c("month", "lat", "lng") %in% cache_cols)

if (!cache_ok) {
  if (!file.exists(data_file)) {
    stop("Data file not found. Put URA_Union_Cleaned_with_Address.xlsx in a ",
         "folder called 'data' next to app.R.")
  }
  message("First run: reading the Excel file (about a minute)...")
  raw <- readxl::read_excel(data_file, guess_max = 50000, .name_repair = "unique_quiet")
  col <- function(name) if (name %in% names(raw)) raw[[name]] else NA   # optional columns
  rentals <- data.frame(
    project     = raw[["Project Name"]],
    street      = raw[["Street Name"]],
    district_no = as.integer(raw[["Postal District"]]),
    type        = raw[["Property Type"]],
    bedrooms    = suppressWarnings(as.numeric(raw[["No of Bedroom"]])),   # blanks -> NA
    rent        = as.numeric(raw[["Monthly Rent ($)"]]),
    area_band   = raw[["Floor Area (SQFT)"]],
    area_sqft   = as.numeric(raw[["Floor Area (SQFT) Midpoint"]]),
    psf         = as.numeric(raw[["Rental Price (PSF)"]]),
    lease_date  = as.Date(raw[["Lease Commencement Date"]]),
    address     = col("Address"),
    loc_lat     = suppressWarnings(as.numeric(col("Latitude"))),
    loc_lng     = suppressWarnings(as.numeric(col("Longitude"))),
    match       = col("Match_Status")
  )
  dir.create("data", showWarnings = FALSE)
  saveRDS(rentals, rds_file)
}
rentals <- readRDS(rds_file)

# Convert the original SG Rental Map cache format to this app's column names
if (!"loc_lat" %in% names(rentals)) {
  rentals <- data.frame(
    project     = rentals$project,
    street      = rentals$street,
    district_no = as.integer(rentals$district),
    type        = rentals$type,
    bedrooms    = suppressWarnings(as.numeric(rentals$bedrooms)),     # "Unknown" -> NA
    rent        = as.numeric(rentals$rent),
    area_band   = rentals$area_sqft,                                  # e.g. "1,300 to 1,400"
    area_sqft   = as.numeric(rentals$sqft),                           # range midpoint
    psf         = as.numeric(rentals$psf),
    lease_date  = as.Date(rentals$month),
    address     = rentals$address,
    loc_lat     = as.numeric(rentals$lat),
    loc_lng     = as.numeric(rentals$lng),
    match       = rentals$match
  )
}
rentals <- rentals[!is.na(rentals$lease_date) & !is.na(rentals$rent), ]   # drop blank rows

# Plain-English district names (international renters don't know "D10")
district_names <- c(
  "01" = "Raffles Place, Marina",      "02" = "Tanjong Pagar, Anson",
  "03" = "Queenstown, Tiong Bahru",    "04" = "Telok Blangah, Harbourfront",
  "05" = "Pasir Panjang, Clementi",    "06" = "City Hall, Beach Road",
  "07" = "Bugis, Golden Mile",         "08" = "Little India, Farrer Park",
  "09" = "Orchard, River Valley",      "10" = "Bukit Timah, Holland, Tanglin",
  "11" = "Novena, Newton, Thomson",    "12" = "Balestier, Toa Payoh",
  "13" = "Macpherson, Braddell",       "14" = "Geylang, Eunos",
  "15" = "Katong, Joo Chiat, Marine Parade", "16" = "Bedok, Upper East Coast",
  "17" = "Loyang, Changi",             "18" = "Tampines, Pasir Ris",
  "19" = "Serangoon Garden, Hougang, Punggol", "20" = "Bishan, Ang Mo Kio",
  "21" = "Upper Bukit Timah, Ulu Pandan", "22" = "Jurong, Boon Lay",
  "23" = "Bukit Panjang, Choa Chu Kang, Hillview", "24" = "Lim Chu Kang, Tengah",
  "25" = "Kranji, Woodlands",          "26" = "Upper Thomson, Springleaf",
  "27" = "Yishun, Sembawang",          "28" = "Seletar, Yio Chu Kang"
)

rentals <- rentals |>
  mutate(
    district = paste0("D", sprintf("%02d", district_no), " ",
                      district_names[sprintf("%02d", district_no)]),
    bed_group = case_when(
      is.na(bedrooms) ~ "Unknown",
      bedrooms >= 5   ~ "5+",
      TRUE            ~ as.character(bedrooms)
    ),
    quarter = as.Date(cut(lease_date, "quarter")),
    match = case_when(
      is.na(loc_lat) | is.na(loc_lng) ~ "No coordinates",
      is.na(match)                    ~ "Unknown",
      TRUE                            ~ as.character(match)
    )
  )

date_min   <- min(rentals$lease_date)
date_max   <- max(rentals$lease_date)
bed_levels <- c("1", "2", "3", "4", "5+", "Unknown")
area_max   <- 4000   # floor-area slider's top end means "4,000 sqft and above"
mrt_max    <- 2000   # MRT slider's top end means "any distance"


# ---- 2. LOCATIONS, MRT AND DISTANCES -------------------------------

haversine_km <- function(lat1, lng1, lat2, lng2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 +
       cos(lat1 * r) * cos(lat2 * r) * sin((lng2 - lng1) * r / 2)^2
  6371 * 2 * asin(sqrt(a))
}

# One row per map location; each contract keeps a loc_id pointing at it
locs <- rentals |>
  filter(!is.na(loc_lat), !is.na(loc_lng)) |>
  distinct(lat = loc_lat, lng = loc_lng) |>
  mutate(loc_id = row_number())
has_coords <- nrow(locs) > 0

# MRT/LRT stations
mrt <- NULL
if (file.exists(mrt_file)) {
  m <- read.csv(mrt_file)
  nm <- sub(" STATION$", "", m$Name)
  nm <- tools::toTitleCase(tolower(nm))
  nm <- sub("Mrt$", "MRT", sub("Lrt$", "LRT", nm))
  mrt <- data.frame(station = nm, lat = m$Latitude, lng = m$Longitude,
                    kind = ifelse(grepl("LRT", m$Name), "LRT", "MRT"))
}
station_choices <- if (is.null(mrt)) character() else sort(mrt$station)

# Schools (FR-11, optional): data/schools.csv with columns name, lat, lng
schools_file <- "data/schools.csv"
schools <- NULL
if (file.exists(schools_file)) {
  s <- read.csv(schools_file)
  names(s) <- tolower(names(s))
  if (all(c("name", "lat", "lng") %in% names(s))) schools <- s[!is.na(s$lat) & !is.na(s$lng), ]
}

# Nearest station for every location (straight line, in metres)
locs$mrt_station <- NA_character_
locs$mrt_m       <- NA_real_
if (has_coords && !is.null(mrt)) {
  best <- rep(Inf, nrow(locs))
  for (j in seq_len(nrow(mrt))) {
    d <- haversine_km(locs$lat, locs$lng, mrt$lat[j], mrt$lng[j]) * 1000
    closer <- d < best
    best[closer] <- d[closer]
    locs$mrt_station[closer] <- mrt$station[j]
  }
  locs$mrt_m <- round(best)
}

rentals <- rentals |>
  left_join(locs, by = c("loc_lat" = "lat", "loc_lng" = "lng"))

locs_sf <- if (has_coords) st_as_sf(locs, coords = c("lng", "lat"), crs = 4326, remove = FALSE)

# One point per project (the location with the most contracts), for distances
proj_coords <- rentals |>
  filter(!is.na(loc_id)) |>
  count(project, loc_id) |>
  group_by(project) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  left_join(locs, by = "loc_id") |>
  select(project, lat, lng, mrt_station, mrt_m)

# Key places for distances and the "Key places" map layer.
# Add your own as "Name" = c(latitude, longitude) in the right group.
# Places next to an MRT station use that station's coordinates; campuses and
# hospitals are approximate (within a few hundred metres).
dest_groups <- list(
  "Universities" = list(
    "NUS (Kent Ridge)"                = c(1.2966, 103.7764),
    "NUS (Bukit Timah campus)"        = c(1.3189, 103.8177),
    "NTU"                             = c(1.3483, 103.6831),
    "SMU"                             = c(1.2963, 103.8502),
    "SUTD"                            = c(1.3413, 103.9638),
    "SUSS (Clementi Road)"            = c(1.3292, 103.7760),
    "SIM (Clementi Road)"             = c(1.3290, 103.7766),
    "SIT (Punggol campus)"            = c(1.4134, 103.9095),
    "University of the Arts (LASALLE)" = c(1.3026, 103.8520),
    "James Cook University Singapore" = c(1.3161, 103.8774)
  ),
  "Polytechnics" = list(
    "Ngee Ann Polytechnic"   = c(1.3331, 103.7759),
    "Singapore Polytechnic"  = c(1.3099, 103.7776),
    "Temasek Polytechnic"    = c(1.3454, 103.9326),
    "Nanyang Polytechnic"    = c(1.3801, 103.8492),
    "Republic Polytechnic"   = c(1.4433, 103.7852)
  ),
  "Business districts" = list(
    "CBD (Raffles Place)"            = c(1.2841, 103.8515),
    "Marina Bay Financial Centre"    = c(1.2795, 103.8540),
    "Tanjong Pagar"                  = c(1.2766, 103.8460),
    "Orchard Road"                   = c(1.3040, 103.8322),
    "one-north"                      = c(1.2998, 103.7875),
    "Singapore Science Park"         = c(1.2927, 103.7851),
    "Mapletree Business City"        = c(1.2745, 103.7995),
    "Jurong Lake District (Jurong East)" = c(1.3332, 103.7423),
    "Paya Lebar Quarter"             = c(1.3172, 103.8923),
    "Changi Business Park"           = c(1.3343, 103.9625),
    "Woodlands Regional Centre"      = c(1.4370, 103.7865)
  ),
  "Transport" = list(
    "Changi Airport (T2/T3)"         = c(1.3575, 103.9879),
    "HarbourFront / VivoCity"        = c(1.2654, 103.8215),
    "Tanah Merah Ferry Terminal"     = c(1.3156, 103.9903),
    "Woodlands Checkpoint (to Johor)" = c(1.4452, 103.7690)
  ),
  "Hospitals" = list(
    "Singapore General Hospital"     = c(1.2797, 103.8352),
    "National University Hospital"   = c(1.2942, 103.7832),
    "Tan Tock Seng Hospital"         = c(1.3215, 103.8458),
    "Changi General Hospital"        = c(1.3402, 103.9497),
    "KK Women's and Children's Hospital" = c(1.3105, 103.8466),
    "Mount Elizabeth Hospital"       = c(1.3053, 103.8355)
  )
)
destinations <- do.call(c, unname(dest_groups))          # one flat named list
destination_choices <- lapply(dest_groups, names)        # grouped dropdown

key_places <- data.frame(
  name  = names(destinations),
  group = rep(names(dest_groups), lengths(dest_groups)),
  lat   = vapply(destinations, `[`, numeric(1), 1),
  lng   = vapply(destinations, `[`, numeric(1), 2)
)
# Schools and work areas show as yellow stars (always on); airport, ports and
# hospitals have their own layer with icons
star_groups <- c("Universities", "Polytechnics", "Business districts")
star_svg <- paste0(
  "<svg xmlns='http://www.w3.org/2000/svg' width='26' height='26' viewBox='0 0 24 24'>",
  "<path d='M12 1.5l3.1 6.6 7.2.9-5.3 5 1.4 7.1L12 17.6 5.6 21.1 7 14l-5.3-5 7.2-.9z' ",
  "fill='#FFD60A' stroke='#5c4a00' stroke-width='1.3' stroke-linejoin='round'/></svg>")
star_icon <- makeIcon(
  iconUrl = paste0("data:image/svg+xml;charset=UTF-8,", utils::URLencode(star_svg, reserved = TRUE)),
  iconWidth = 26, iconHeight = 26, iconAnchorX = 13, iconAnchorY = 13)
other_icons   <- c("Transport" = "plane", "Hospitals" = "plus-square")
other_colours <- c("Transport" = "green", "Hospitals" = "red")

# Add distance columns to a project-level table
add_distance <- function(s, destination) {
  if (!has_coords) return(s)
  dest <- destinations[[destination]]
  s |>
    left_join(proj_coords, by = "project") |>
    mutate(dest_km  = round(haversine_km(lat, lng, dest[1], dest[2]), 1),
           walk_min = round(mrt_m / 80)) |>        # ~80 m per minute walking
    select(-lat, -lng)
}


# ---- 3. MAP SETTINGS -----------------------------------------------

# Fixed colour bins so a colour always means the same rent, whatever the filters
metric_choices <- c("Rent per sqft" = "psf", "Monthly rent" = "rent",
                    "Vs my budget" = "budget")
bins <- list(
  psf  = c(0, 3, 3.5, 4, 4.5, 5, 5.5, 6, 7, Inf),
  rent = c(0, 2500, 3000, 3500, 4000, 5000, 6000, 8000, 12000, Inf)
)
pals <- lapply(bins, function(b) colorBin("RdYlBu", bins = b, reverse = TRUE, na.color = "#bbbbbb"))
budget_levels <- c("Below my budget", "Within my budget", "Above my budget")
budget_pal <- colorFactor(c("#4575b4", "#1a9850", "#d73027"), levels = budget_levels)

match_choices <- sort(unique(rentals$match))
match_default <- grep("^Exact|^Good", match_choices, value = TRUE)
if (length(match_default) == 0) match_default <- setdiff(match_choices, "No coordinates")

# Convert a shape drawn on the map into an sf polygon
drawn_to_sf <- function(f) {
  if (f$geometry$type == "Point") {             # circle tool
    st_buffer(st_sfc(st_point(unlist(f$geometry$coordinates)), crs = 4326),
              dist = f$properties$radius)
  } else {                                      # polygon or rectangle
    xy <- matrix(unlist(f$geometry$coordinates), ncol = 2, byrow = TRUE)
    st_sfc(st_polygon(list(xy)), crs = 4326)
  }
}

add_draw_tools <- function(map) {
  addDrawToolbar(map, polylineOptions = FALSE, markerOptions = FALSE,
                 circleMarkerOptions = FALSE, singleFeature = TRUE,
                 editOptions = editToolbarOptions())
}


# ---- 4. HELPER FUNCTIONS -------------------------------------------

money <- function(x, d = 0) paste0("S$", formatC(x, format = "f", digits = d, big.mark = ","))

quarter_label <- function(d) {
  paste0(format(d, "%Y"), " Q", (as.integer(format(d, "%m")) - 1) %/% 3 + 1)
}

# Keep only the most recent N months of a data set
latest_months <- function(d, months) {
  if (nrow(d) == 0) return(d)
  cutoff <- seq(max(d$lease_date), by = paste0("-", months, " months"), length.out = 2)[2]
  d[d$lease_date > cutoff, ]
}

# Standard rent summary. budget = c(min, max)
summarise_rent <- function(d, budget = c(NA, NA)) {
  summarise(d,
    median  = median(rent),
    average = mean(rent),
    p25     = unname(quantile(rent, 0.25)),
    p75     = unname(quantile(rent, 0.75)),
    psf     = median(psf, na.rm = TRUE),
    n       = n(),
    within  = mean(rent >= budget[1] & rent <= budget[2]),
    .groups = "drop"
  )
}

most_common <- function(x) names(which.max(table(x)))

listing_link <- function(project) {
  q <- utils::URLencode(paste0('"', project, '" for rent site:propertyguru.com.sg OR site:99.co'),
                        reserved = TRUE)
  paste0('<a href="https://www.google.com/search?q=', q, '" target="_blank">Find listings</a>')
}

nice_names <- c(
  project = "Project", district = "District", bed_group = "Bedrooms",
  size = "Typical size (sqft)", median = "Median rent", average = "Average rent",
  p25 = "25th percentile", p75 = "75th percentile", psf = "Median S$ psf",
  n = "Contracts", within = "Share within budget", last = "Last rented",
  fits = "Budget fit", dest_km = "Distance to destination (km)",
  mrt_station = "Nearest MRT/LRT", mrt_m = "Distance to MRT/LRT (m)",
  walk_min = "Walk to MRT/LRT (min, est.)", listings = "Listings"
)
prettify <- function(s) {
  names(s) <- ifelse(names(s) %in% names(nice_names), nice_names[names(s)], names(s))
  s
}
format_money_cols <- function(dt, s) {
  money_cols <- intersect(c("Median rent", "Average rent", "25th percentile", "75th percentile"),
                          names(s))
  dt <- formatCurrency(dt, money_cols, "S$", digits = 0)
  if ("Median S$ psf" %in% names(s)) dt <- formatCurrency(dt, "Median S$ psf", "S$", digits = 2)
  if ("Share within budget" %in% names(s)) dt <- formatPercentage(dt, "Share within budget", digits = 0)
  dt
}

app_theme <- bs_theme(version = 5, bootswatch = "flatly")

# Inputs that should not be saved in "Save this search" links
table_ids <- c("wb_table", "compare_table", "records_table", "short_table",
               "map_beds", "map_proj", "map_raw", "mrt_table", "cp_table", "vfm_table")
dt_suffixes <- c("_rows_selected", "_rows_all", "_rows_current", "_row_last_clicked",
                 "_cell_clicked", "_cells_selected", "_columns_selected", "_search",
                 "_search_columns", "_state", "_cell_info")
map_inputs <- paste0("map_", c("bounds", "center", "zoom", "click", "marker_click",
                               "marker_mouseover", "marker_mouseout", "shape_click",
                               "groups", "draw_new_feature", "draw_all_features",
                               "draw_deleted_features", "draw_edited_features",
                               "draw_start", "draw_stop", "draw_editstart", "draw_editstop",
                               "draw_deletestart", "draw_deletestop"))
bookmark_exclude <- c(as.vector(outer(table_ids, dt_suffixes, paste0)), map_inputs,
                      paste0("ab_table", dt_suffixes),
                      "add_short", "short_remove", "short_clear", "clear", "clear2",
                      "save_a", "save_b")


# ---- 5. UI ---------------------------------------------------------

ui <- function(request) {
  page_navbar(
    title = "SG Rental Map",
    theme = app_theme,
    fillable = FALSE,

    # Shared sidebar: every tab reacts to these filters
    sidebar = sidebar(
      width = 320,
      accordion(
        multiple = TRUE,
        open = c("Budget", "Area and project"),
        accordion_panel(
          "Budget",
          sliderInput("budget", "Monthly budget range (S$)", min = 1000, max = 15000,
                      value = c(3000, 5000), step = 100, pre = "S$")
        ),
        accordion_panel(
          "Area and project",
          selectizeInput("district", "Districts", multiple = TRUE,
                         choices = sort(unique(rentals$district)),
                         options = list(placeholder = "All districts")),
          selectizeInput("project", "Residential projects (optional)", multiple = TRUE,
                         choices = NULL,
                         options = list(placeholder = "Type to search projects"))
        ),
        accordion_panel(
          "Property",
          checkboxGroupInput("beds", "Bedrooms", choices = bed_levels,
                             selected = bed_levels, inline = TRUE),
          checkboxGroupInput("type", "Property type",
                             choices = sort(unique(rentals$type)),
                             selected = unique(rentals$type)),
          sliderInput("area", "Floor area (sqft)", min = 0, max = area_max,
                      value = c(0, area_max), step = 100),
          helpText("Top of the slider = 4,000 sqft and above.")
        ),
        accordion_panel(
          "Time period",
          dateRangeInput("period", "Lease start between", start = date_min, end = date_max,
                         min = date_min, max = date_max, format = "M yyyy"),
          selectInput("recent", "Benchmarks use the most recent",
                      choices = c("6 months" = 6, "12 months" = 12, "24 months" = 24),
                      selected = 12)
        ),
        accordion_panel(
          "Location",
          selectInput("destination", "Your university, workplace or key place",
                      choices = destination_choices, selected = "NUS (Kent Ridge)"),
          selectizeInput("mrt_pick", "Near a specific MRT/LRT station (optional)",
                         choices = c("", station_choices),
                         options = list(placeholder = "Any station")),
          sliderInput("mrt_dist", "Within this distance (m)",
                      min = 100, max = mrt_max, value = mrt_max, step = 100),
          helpText("No station chosen: distance to the nearest station, and the top of ",
                   "the slider means any distance. Station chosen: buildings within this ",
                   "distance of it (ring on the map). Straight-line; about 80 m per minute walking."),
          checkboxGroupInput("match", "Geocode quality", match_choices,
                             selected = match_default),
          helpText("Street-only matches sit on the street, not the building.")
        )
      ),
      div(
        class = "border rounded p-2 small",
        strong("Map selection: "), textOutput("sel_status", inline = TRUE),
        checkboxInput("apply_sel", "Apply map selection to all tabs", value = TRUE),
        actionLink("clear2", "Clear selection"),
        div(
          class = "mt-2",
          actionButton("save_a", "Save as Area A", class = "btn-sm btn-primary"),
          actionButton("save_b", "Save as Area B", class = "btn-sm btn-warning")
        ),
        div(class = "mt-1", textOutput("areas_status", inline = TRUE))
      ),
      bookmarkButton("Save this search",
                     title = "Get a link that restores your filters, map selection and shortlist")
    ),

    # ---- Tab: Map (FR-01 to FR-09, FR-11) ----
    nav_panel(
      "Map",
      card(
        full_screen = TRUE,
        card_header(
          class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
          span("Click a circle to inspect a building, or draw a shape (left toolbar) ",
               "to summarise an area. ", actionLink("clear", "Clear selection")),
          radioButtons("metric", NULL, metric_choices, inline = TRUE)
        ),
        leafletOutput("map", height = 620)
      ),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box("Contracts", textOutput("mv_n"), theme = "primary"),
        value_box("Projects", textOutput("mv_proj"), theme = "secondary"),
        value_box("Median monthly rent", textOutput("mv_rent"),
                  textOutput("mv_rent_avg", inline = TRUE), theme = "secondary"),
        value_box("Median rent psf", textOutput("mv_psf"),
                  "estimate: floor area is a range", theme = "secondary")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header(textOutput("sel_title")), plotlyOutput("map_trend", height = 330)),
        card(card_header("By bedrooms"), DTOutput("map_beds", fill = FALSE))
      ),
      card(card_header("By project"), DTOutput("map_proj", fill = FALSE)),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Contracts in this selection",
          downloadButton("map_dl", "Download selection (CSV)", class = "btn-sm")
        ),
        DTOutput("map_raw", fill = FALSE)
      )
    ),

    # ---- Tab: Compare two areas (FR-10) ----
    nav_panel(
      "Area A vs B",
      uiOutput("ab_intro"),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header(class = "bg-primary text-white", textOutput("a_title")),
             uiOutput("a_tiles")),
        card(card_header(class = "bg-warning text-white", textOutput("b_title")),
             uiOutput("b_tiles"))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Median monthly rent by bedrooms"),
             plotOutput("ab_beds", height = 460),
             card_footer("Numbers above bars = median monthly rent. Contract counts are in the table below.")),
        card(card_header("Median monthly rent by quarter"),
             plotOutput("ab_trend", height = 380),
             card_footer("Quarters with fewer than 3 contracts are left out."))
      ),
      card(card_header("Side by side by bedrooms"), DTOutput("ab_table", fill = FALSE))
    ),

    # ---- Tab: Budget check ----
    nav_panel(
      "Budget check",
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box("Median rent", textOutput("vb_median"),
                  textOutput("vb_average", inline = TRUE), theme = "primary"),
        value_box("Typical range", textOutput("vb_range"),
                  "middle 50% of rents", theme = "secondary"),
        value_box("Comparable rentals", textOutput("vb_n"),
                  textOutput("vb_n_note", inline = TRUE), theme = "secondary"),
        uiOutput("verdict_box")
      ),
      card(
        card_header("How your budget compares with recent comparable rents"),
        plotOutput("budget_plot", height = 380),
        card_footer("Green band = typical range (middle 50%). Red band = your budget range.")
      )
    ),

    # ---- Tab: Within budget ----
    nav_panel(
      "Within budget",
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box("Project and bedroom options", textOutput("wb_options"), theme = "success"),
        value_box("Projects", textOutput("wb_projects"), theme = "secondary"),
        value_box("Districts", textOutput("wb_districts"), theme = "secondary"),
        value_box("Bedroom types", textOutput("wb_beds_n"), textOutput("wb_beds"),
                  theme = "secondary")
      ),
      card(
        card_header("Where your budget goes: options by district and bedrooms"),
        plotOutput("wb_plot", height = 420)
      ),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Projects with a median rent inside your budget range",
          actionButton("add_short", "Add selected to shortlist", class = "btn-sm btn-success")
        ),
        layout_columns(
          col_widths = c(4, 8),
          numericInput("wb_min_n", "Minimum recent contracts per row", value = 3, min = 1),
          p(class = "text-muted mt-4", textOutput("distance_note", inline = TRUE))
        ),
        DTOutput("wb_table", fill = FALSE),
        card_footer("Click rows to select them, then add them to your shortlist. ",
                    "Click a column header to sort; use Search to filter.")
      )
    ),

    # ---- Tab: Price trends ----
    nav_panel(
      "Price trends",
      card(
        card_header(
          "Median monthly rent by quarter",
          checkboxInput("split_district", "Show selected districts separately", value = FALSE)
        ),
        plotOutput("trend_plot", height = 420),
        card_footer(textOutput("trend_summary"))
      )
    ),

    # ---- Tab: Compare areas ----
    nav_panel(
      "Compare areas",
      layout_columns(
        col_widths = c(4, 4, 4),
        radioButtons("compare_by", "Compare by", c("District", "Project"), inline = TRUE),
        numericInput("min_n", "Minimum recent contracts per row", value = 5, min = 1),
        p(class = "text-muted mt-4",
          "Highlighted rows have a median rent within your budget range.")
      ),
      conditionalPanel(
        "input.compare_by == 'District'",
        card(card_header("Median rent by district (lines = typical range)"),
             plotOutput("district_plot", height = 650))
      ),
      card(card_header("Comparison table"), DTOutput("compare_table", fill = FALSE)),
      card(
        card_header("Median rent by district and bedrooms"),
        plotlyOutput("heat_plot", height = 650),
        card_footer("Darker red = more expensive. Numbers = median monthly rent (S$ thousands); ",
                    "hover for the exact figure and contract count. ",
                    "Blank = fewer contracts than the minimum.")
      ),
      card(
        card_header("Does being near an MRT/LRT station cost more?"),
        plotlyOutput("mrt_plot", height = 450),
        card_footer("One dot per project (recent contracts); hover for details. Line = overall trend.")
      ),
      card(card_header("Projects by distance to the nearest MRT/LRT station"),
           DTOutput("mrt_table", fill = FALSE))
    ),

    # ---- Tab: Compare properties (up to three projects side by side) ----
    nav_panel(
      "Compare properties",
      card(
        card_header("Pick up to three projects"),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          selectizeInput("cp_a", "Property A", choices = NULL,
                         options = list(placeholder = "Type a project name")),
          selectizeInput("cp_b", "Property B", choices = NULL,
                         options = list(placeholder = "Type a project name")),
          selectizeInput("cp_c", "Property C", choices = NULL,
                         options = list(placeholder = "Type a project name")),
          selectInput("cp_bed", "Bedrooms", choices = c("All", bed_levels))
        ),
        div(
          actionButton("cp_from_short", "Fill from my shortlist", class = "btn-sm"),
          downloadButton("cp_csv", "Download comparison CSV", class = "btn-sm"),
          span(class = "text-muted small ms-2",
               "Uses the sidebar time period; all three use the same bedroom filter.")
        )
      ),
      uiOutput("cp_cards"),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Rent distribution"), plotlyOutput("cp_box", height = 400),
             card_footer("Box = middle 50% of contracts; line inside = median.")),
        card(card_header("Quarterly median rent"), plotlyOutput("cp_trend", height = 400),
             card_footer("Quarters with fewer than 3 contracts are left out."))
      ),
      card(card_header("Comparison table"), DTOutput("cp_table", fill = FALSE))
    ),

    # ---- Tab: Value for money ----
    nav_panel(
      "Value for money",
      layout_columns(
        col_widths = c(4, 8),
        numericInput("vfm_min_n", "Minimum recent contracts per project", value = 5, min = 1),
        p(class = "text-muted mt-4",
          "Each dot is one project (recent contracts matching the sidebar filters). ",
          "Further right and lower = more space for the money. ",
          "Green = median rent within your budget range. Hover for details; ",
          "drag to zoom, double-click to see every project.")
      ),
      card(card_header("Median monthly rent vs median floor area"),
           plotlyOutput("vfm_plot", height = 560)),
      card(card_header("Best value: lowest median rent per sqft"),
           DTOutput("vfm_table", fill = FALSE))
    ),

    # ---- Tab: Shortlist ----
    nav_panel(
      "Shortlist",
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Your shortlist",
          div(
            actionButton("short_remove", "Remove selected", class = "btn-sm"),
            actionButton("short_clear", "Clear all", class = "btn-sm"),
            downloadButton("short_download", "Download CSV", class = "btn-sm")
          )
        ),
        DTOutput("short_table", fill = FALSE),
        card_footer("Figures use each option's contracts in the most recent period, ",
                    "regardless of the sidebar filters, so your shortlist stays comparable.")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Shortlist at a glance"),
          plotOutput("short_plot", height = 420),
          card_footer("Dot = median rent, line = typical range. Red band = your budget range.")
        ),
        card(
          card_header("How their rents have moved"),
          plotOutput("short_trend", height = 420),
          card_footer("Quarterly median rent; quarters with fewer than 3 contracts are left out.")
        )
      )
    ),

    # ---- Tab: Transactions ----
    nav_panel(
      "Transactions",
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Rental contracts matching your filters",
          downloadButton("download", "Download CSV", class = "btn-sm")
        ),
        DTOutput("records_table", fill = FALSE)
      )
    ),

    # ---- Tab: About ----
    nav_panel(
      "About",
      card(
        h4("How to read this app"),
        p("All figures come from URA private residential rental contracts: ",
          format(nrow(rentals), big.mark = ","), " contracts from ",
          format(date_min, "%b %Y"), " to ", format(date_max, "%b %Y"), "."),
        p(strong("Map: "), "one circle per building, coloured by median rent on fixed ",
          "bins and sized by number of contracts. Click a circle or draw a shape to ",
          "select; with 'Apply map selection to all tabs' ticked, every other tab ",
          "uses only the selected buildings."),
        p(strong("Area A vs B: "), "select a building or draw an area, press 'Save as Area A', ",
          "then do the same for Area B to compare them side by side. ",
          strong("Near a specific station: "), "pick an MRT/LRT station under Location to keep ",
          "only buildings within the chosen distance of it. ",
          strong("Key places: "), "yellow stars mark universities, polytechnics and work areas; ",
          "pick one under Location to get distances to it."),
        p(strong("Compare properties: "), "pick up to three projects (or fill them from your ",
          "shortlist) to see their rents, sizes, trends and transport side by side. ",
          strong("Value for money: "), "each project's median rent against its median floor ",
          "area; the table ranks projects by the lowest rent per sqft."),
        p(strong("Comparable rentals"), " are the contracts matching your filters ",
          "within the most recent period you choose in the sidebar. ",
          strong("Typical range"), " is the 25th to 75th percentile."),
        p(strong("Within budget"), " lists each project and bedroom type whose median ",
          "recent rent falls inside your budget range."),
        p("Geocode quality affects the map and map selections only. ",
          format(sum(rentals$match == "No coordinates"), big.mark = ","),
          " contracts have no coordinates and never appear on the map."),
        p("These are historical contracts, not live listings. Use 'Find listings' ",
          "to search property portals for available units."),
        p("Floor area is published as a range, so rent per sqft uses the midpoint and is ",
          "an estimate. Distances and walking times are straight-line estimates ",
          "(about 80 m per minute). Records with no bedroom count show as 'Unknown'.")
      )
    )
  )
}


# ---- 6. SERVER -----------------------------------------------------

server <- function(input, output, session) {

  setBookmarkExclude(bookmark_exclude)

  # Project list follows the chosen districts
  observeEvent(input$district, {
    d <- if (length(input$district)) rentals[rentals$district %in% input$district, ] else rentals
    updateSelectizeInput(session, "project", server = TRUE,
                         choices  = sort(unique(d$project)),
                         selected = intersect(input$project, d$project))
  }, ignoreNULL = FALSE)

  budget <- reactive(input$budget)

  # Distance (m) from every location to the chosen station, indexed by loc_id
  pick_dist <- reactive({
    st <- mrt[mrt$station == input$mrt_pick, ][1, ]
    haversine_km(locs$lat, locs$lng, st$lat, st$lng) * 1000
  })

  # ---- Data layers ----
  # A) base: every sidebar filter except the project choice and map selection
  base <- reactive({
    d <- rentals
    if (length(input$district)) d <- d[d$district %in% input$district, ]
    # Some landed houses have open-ended areas (">8000", "<=1000") and no midpoint;
    # they are kept while the floor-area slider is at its full range
    full_area <- input$area[1] == 0 && input$area[2] >= area_max
    area_ok <- full_area |
      (!is.na(d$area_sqft) & d$area_sqft >= input$area[1] &
         (d$area_sqft <= input$area[2] | input$area[2] >= area_max))
    keep <- d$bed_group %in% input$beds &
      d$type %in% input$type &
      area_ok &
      d$lease_date >= input$period[1] &
      d$lease_date <= input$period[2]
    if (isTruthy(input$mrt_pick)) {
      # FR-12: within a set distance of the chosen station
      dd <- pick_dist()[d$loc_id]          # loc_id is the row number in locs
      keep <- keep & !is.na(dd) & dd <= input$mrt_dist
    } else if (input$mrt_dist < mrt_max) {
      keep <- keep & !is.na(d$mrt_m) & d$mrt_m <= input$mrt_dist
    }
    keep[is.na(keep)] <- FALSE
    d[keep, ]
  })

  # B) map data: base + geocode quality, located contracts only
  map_data <- reactive({
    d <- base()
    d[!is.na(d$loc_id) & d$match %in% input$match, ]
  })

  # Map selection: clicked building or drawn area (NULL = nothing selected)
  sel <- reactiveValues(ids = NULL, label = NULL)
  sel_active <- reactive(!is.null(sel$ids))

  # C) market: base, narrowed to the map selection when that option is on
  market <- reactive({
    d <- base()
    if (sel_active() && isTRUE(input$apply_sel)) {
      d <- d[!is.na(d$loc_id) & d$loc_id %in% sel$ids & d$match %in% input$match, ]
    }
    d
  })

  # D) selection: market narrowed to chosen projects
  selection <- reactive({
    d <- market()
    if (length(input$project)) d <- d[d$project %in% input$project, ]
    validate(need(nrow(d) > 0,
      "No contracts match these filters. Try widening the filters or clearing the map selection."))
    d
  })

  # E) comparable rentals: the selection, most recent months only
  comparables <- reactive(latest_months(selection(), as.numeric(input$recent)))


  # ==== MAP TAB ======================================================

  loc_summary <- reactive({
    b <- budget()
    map_data() |>
      group_by(loc_id) |>
      summarise(project  = paste(head(unique(project), 3), collapse = " / "),
                address  = first(address), district = first(district),
                mrt      = first(mrt_station), mrt_m = first(mrt_m),
                n = n(), rent = median(rent), psf = median(psf, na.rm = TRUE),
                .groups = "drop") |>
      left_join(locs[, c("loc_id", "lat", "lng")], by = "loc_id") |>
      mutate(budget = factor(case_when(rent < b[1] ~ budget_levels[1],
                                       rent > b[2] ~ budget_levels[3],
                                       TRUE        ~ budget_levels[2]),
                             levels = budget_levels))
  })

  output$map <- renderLeaflet({
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addProviderTiles(providers$CartoDB.Positron, group = "Light") |>
      addProviderTiles(providers$OneMapSG.Default, group = "OneMap") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
      addLayersControl(baseGroups = c("Light", "OneMap", "Satellite"),
                       overlayGroups = c("Buildings", "Schools and work areas",
                                         "Airport, ports and hospitals", "Heatmap",
                                         "MRT/LRT stations",
                                         if (!is.null(schools)) "Schools"),
                       options = layersControlOptions(collapsed = TRUE),
                       position = "bottomleft") |>
      hideGroup(c("Airport, ports and hospitals", "Heatmap", "MRT/LRT stations", "Schools")) |>
      addSearchOSM() |>
      add_draw_tools() |>
      setView(103.83, 1.33, zoom = 12)

    # Key places: yellow stars for schools and work areas, icons for the rest
    stars  <- key_places[key_places$group %in% star_groups, ]
    others <- key_places[!key_places$group %in% star_groups, ]
    m <- m |>
      addMarkers(data = stars, lng = ~lng, lat = ~lat, icon = star_icon,
                 group = "Schools and work areas", label = ~paste0(name, " (", group, ")")) |>
      addAwesomeMarkers(data = others, lng = ~lng, lat = ~lat,
                        group = "Airport, ports and hospitals",
                        label = ~paste0(name, " (", group, ")"),
                        icon = awesomeIcons(icon = unname(other_icons[others$group]),
                                            library = "fa", iconColor = "white",
                                            markerColor = unname(other_colours[others$group])))
    if (!is.null(mrt)) {
      m <- m |> addCircleMarkers(data = mrt, lng = ~lng, lat = ~lat, group = "MRT/LRT stations",
                                 radius = 4, color = "#222", weight = 1.5, fillColor = "#fff",
                                 fillOpacity = 1, label = ~station)
    }
    if (!is.null(schools)) {
      m <- m |> addCircleMarkers(data = schools, lng = ~lng, lat = ~lat, group = "Schools",
                                 radius = 4, color = "#6a3d9a", weight = 1.5, fillColor = "#cab2d6",
                                 fillOpacity = 1, label = ~name)
    }
    m
  })
  outputOptions(output, "map", suspendWhenHidden = FALSE)

  # FR-12: ring around the chosen station
  observe({
    p <- leafletProxy("map") |> clearGroup("Station radius")
    if (isTruthy(input$mrt_pick)) {
      st <- mrt[mrt$station == input$mrt_pick, ][1, ]
      p |>
        addCircles(lng = st$lng, lat = st$lat, radius = input$mrt_dist, group = "Station radius",
                   color = "#222", weight = 2, dashArray = "6", fillOpacity = 0.05,
                   label = paste0(st$station, ": ", input$mrt_dist, " m (about ",
                                  round(input$mrt_dist / 80), " min walk)"))
    }
  })
  observeEvent(input$mrt_pick, {
    req(isTruthy(input$mrt_pick))
    st <- mrt[mrt$station == input$mrt_pick, ][1, ]
    leafletProxy("map") |> flyTo(st$lng, st$lat, zoom = 15)
    # Start from a 500 m (about 6 min) walk if the slider is still at "any distance"
    if (input$mrt_dist >= mrt_max) updateSliderInput(session, "mrt_dist", value = 500)
  })

  # Redraw circles and legend when filters, colour metric or budget change
  observe({
    d <- loc_summary()
    m <- input$metric
    p <- leafletProxy("map", data = d) |>
      clearGroup("Buildings") |> clearGroup("Heatmap") |> removeControl("legend")
    if (nrow(d) == 0) return()

    if (m == "budget") {
      fill <- budget_pal(d$budget)
    } else {
      fill <- pals[[m]](d[[m]])
    }
    lab <- sprintf(
      "<b>%s</b><br>%s<br>%s<br>%s contracts<br>Median rent %s<br>Median %s psf<br>Nearest station: %s (%s m)",
      d$project, ifelse(is.na(d$address), "", d$address), d$district,
      format(d$n, big.mark = ","), money(d$rent), money(d$psf, 2),
      ifelse(is.na(d$mrt), "-", d$mrt), ifelse(is.na(d$mrt_m), "-", format(d$mrt_m, big.mark = ","))
    ) |> lapply(HTML)

    p <- p |>
      addCircleMarkers(lng = ~lng, lat = ~lat, layerId = ~as.character(loc_id),
                       group = "Buildings", radius = ~pmin(11, 2 + sqrt(n) / 3),
                       fillColor = fill, fillOpacity = 0.8,
                       color = "#333", weight = 0.5, label = lab) |>
      addHeatmap(lng = ~lng, lat = ~lat, intensity = ~n, group = "Heatmap",
                 blur = 20, radius = 12, max = unname(quantile(d$n, 0.9)))

    if (m == "budget") {
      p |> addLegend("bottomright", pal = budget_pal, values = d$budget, layerId = "legend",
                     title = "Median rent vs my budget")
    } else {
      p |> addLegend("bottomright", pal = pals[[m]], values = na.omit(d[[m]]), layerId = "legend",
                     title = names(metric_choices)[metric_choices == m],
                     labFormat = labelFormat(prefix = "S$", big.mark = ","))
    }
  })


  # Select by clicking a building
  observeEvent(input$map_marker_click, {
    id <- input$map_marker_click$id
    req(!is.null(id), id != "")
    l <- loc_summary() |> filter(loc_id == as.integer(id))
    req(nrow(l) > 0)
    sel$ids <- as.integer(id)
    sel$label <- l$project[1]
  })

  # Select by drawing a shape
  observeEvent(input$map_draw_new_feature, {
    req(has_coords)
    shape <- drawn_to_sf(input$map_draw_new_feature)
    ids <- locs_sf$loc_id[lengths(st_intersects(locs_sf, shape)) > 0]
    if (length(ids) == 0) {
      showNotification("No buildings in that area. Try drawing a larger shape.", type = "warning")
      return()
    }
    sel$ids <- ids
    sel$label <- sprintf("Drawn area (%d buildings)", length(ids))
  })

  # "Clear selection" also wipes the saved Area A and Area B
  clear_sel <- function() {
    sel$ids <- NULL
    sel$label <- NULL
    areas$a_ids <- NULL; areas$a_label <- NULL
    areas$b_ids <- NULL; areas$b_label <- NULL
    leafletProxy("map") |> clearGroup("Selected") |>
      clearGroup("Area A") |> clearGroup("Area B") |>
      removeDrawToolbar(clearFeatures = TRUE) |> add_draw_tools()
  }
  observeEvent(input$map_draw_deleted_features, { sel$ids <- NULL; sel$label <- NULL })
  observeEvent(input$clear,  clear_sel())
  observeEvent(input$clear2, clear_sel())

  # Outline selected buildings
  observe({
    ids <- sel$ids
    p <- leafletProxy("map") |> clearGroup("Selected")
    if (!is.null(ids)) {
      s <- locs[locs$loc_id %in% ids, ]
      p |> addCircleMarkers(data = s, lng = ~lng, lat = ~lat, group = "Selected",
                            radius = 9, fill = FALSE, color = "#000", weight = 2.5,
                            options = pathOptions(interactive = FALSE))
    }
  })

  output$sel_status <- renderText({
    if (!sel_active()) "none (all filtered contracts)" else sel$label
  })

  # Contracts behind the map summaries
  map_selected <- reactive({
    d <- map_data()
    if (sel_active()) d <- d[d$loc_id %in% sel$ids, ]
    d
  })

  med <- function(x, d = 0) if (length(x) && any(!is.na(x))) money(median(x, na.rm = TRUE), d) else "-"
  output$mv_n        <- renderText(format(nrow(map_selected()), big.mark = ","))
  output$mv_proj     <- renderText(format(n_distinct(map_selected()$project), big.mark = ","))
  output$mv_rent     <- renderText(med(map_selected()$rent))
  output$mv_rent_avg <- renderText({
    r <- map_selected()$rent
    if (length(r)) paste("average", money(mean(r))) else ""
  })
  output$mv_psf      <- renderText(med(map_selected()$psf, 2))

  output$sel_title <- renderText({
    paste("Median rent psf by month:",
          if (!sel_active()) "all filtered contracts" else sel$label)
  })

  output$map_trend <- renderPlotly({
    d <- map_selected() |> filter(!is.na(psf))
    validate(need(nrow(d) > 0, "No contracts in this selection."))
    d$month <- as.Date(cut(d$lease_date, "month"))
    by_bed <- d |> group_by(month, bed_group) |>
      summarise(psf = median(psf), n = n(), .groups = "drop") |> filter(n >= 3)
    overall <- d |> group_by(month) |>
      summarise(psf = median(psf), n = n(), .groups = "drop") |> filter(n >= 3)
    plot_ly() |>
      add_lines(data = by_bed, x = ~month, y = ~psf, color = ~paste(bed_group, "BR"),
                text = ~paste(n, "contracts"), opacity = 0.6) |>
      add_lines(data = overall, x = ~month, y = ~psf, name = "All",
                line = list(color = "black", width = 3), text = ~paste(n, "contracts")) |>
      layout(xaxis = list(title = ""), yaxis = list(title = "S$ psf / month"),
             legend = list(orientation = "h", y = -0.15))
  })

  output$map_beds <- renderDT({
    validate(need(nrow(map_selected()) > 0, "No contracts in this selection."))
    map_selected() |>
      group_by(Bedrooms = bed_group) |>
      summarise(Contracts = n(), `Median rent` = median(rent), `Average rent` = mean(rent),
                `Median sqft` = median(area_sqft, na.rm = TRUE),
                `Median psf` = median(psf, na.rm = TRUE), .groups = "drop") |>
      datatable(rownames = FALSE, options = list(dom = "t", pageLength = 20)) |>
      formatCurrency(c("Median rent", "Average rent"), "S$", digits = 0) |>
      formatCurrency("Median psf", "S$", digits = 2) |>
      formatRound("Median sqft", 0)
  })

  output$map_proj <- renderDT({
    validate(need(nrow(map_selected()) > 0, "No contracts in this selection."))
    map_selected() |>
      group_by(Project = project, Street = street, District = district, Type = type,
               `Nearest MRT/LRT` = mrt_station, `MRT/LRT (m)` = mrt_m) |>
      summarise(Contracts = n(), `Median rent` = median(rent), `Average rent` = mean(rent),
                `Median psf` = median(psf, na.rm = TRUE),
                `Latest lease` = max(lease_date), .groups = "drop") |>
      mutate(`Latest lease` = format(`Latest lease`, "%b %Y")) |>
      arrange(desc(Contracts)) |>
      datatable(rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) |>
      formatCurrency(c("Median rent", "Average rent"), "S$", digits = 0) |>
      formatCurrency("Median psf", "S$", digits = 2)
  })

  map_contracts <- reactive({
    map_selected() |>
      arrange(desc(lease_date)) |>
      transmute(Month = format(lease_date, "%Y-%m"), Project = project, Street = street,
                District = district, Type = type, Bedrooms = bed_group,
                `Area (sqft)` = area_band, Rent = rent, PSF = round(psf, 2),
                Address = address, `Geocode quality` = match)
  })

  output$map_raw <- renderDT({
    datatable(map_contracts(), rownames = FALSE,
              options = list(pageLength = 15, scrollX = TRUE)) |>
      formatCurrency("Rent", "S$", digits = 0)
  })

  output$map_dl <- downloadHandler(
    filename = function() paste0("sg_rentals_selection-", Sys.Date(), ".csv"),
    content  = function(f) write.csv(map_contracts(), f, row.names = FALSE)
  )


  # ==== AREA A VS B (FR-10) ==========================================

  areas <- reactiveValues(a_ids = NULL, a_label = NULL, b_ids = NULL, b_label = NULL)

  save_area <- function(which) {
    if (!sel_active()) {
      showNotification("Click a building or draw an area on the map first.", type = "warning")
      return()
    }
    areas[[paste0(which, "_ids")]]   <- sel$ids
    areas[[paste0(which, "_label")]] <- sel$label
    showNotification(paste0("Saved as Area ", toupper(which), ": ", sel$label), type = "message")
  }
  observeEvent(input$save_a, save_area("a"))
  observeEvent(input$save_b, save_area("b"))

  output$areas_status <- renderText({
    paste0("A: ", if (is.null(areas$a_label)) "not set" else areas$a_label,
           " | B: ", if (is.null(areas$b_label)) "not set" else areas$b_label)
  })

  # Outline saved areas on the map (A blue, B orange)
  observe({
    p <- leafletProxy("map") |> clearGroup("Area A") |> clearGroup("Area B")
    for (w in c("a", "b")) {
      ids <- areas[[paste0(w, "_ids")]]
      if (is.null(ids)) next
      s <- locs[locs$loc_id %in% ids, ]
      p <- p |> addCircleMarkers(data = s, lng = ~lng, lat = ~lat,
                                 group = paste("Area", toupper(w)), radius = 12, fill = FALSE,
                                 color = if (w == "a") "#2c3e50" else "#f39c12", weight = 2,
                                 options = pathOptions(interactive = FALSE))
    }
  })

  # Contracts in each saved area (sidebar filters still apply)
  area_contracts <- function(which) {
    ids <- areas[[paste0(which, "_ids")]]
    d <- map_data()
    if (is.null(ids)) d[0, ] else d[d$loc_id %in% ids, ]
  }
  a_data <- reactive(area_contracts("a"))
  b_data <- reactive(area_contracts("b"))
  both_areas <- reactive({
    a <- a_data(); b <- b_data()
    validate(need(nrow(a) > 0 && nrow(b) > 0,
      "Save two areas first: select a building or draw an area on the map, then press 'Save as Area A'; repeat for Area B."))
    bind_rows(mutate(a, area = paste("A:", areas$a_label)),
              mutate(b, area = paste("B:", areas$b_label)))
  })

  output$ab_intro <- renderUI({
    if (is.null(areas$a_ids) || is.null(areas$b_ids)) {
      p(class = "text-muted",
        "On the Map tab, click a building or draw an area, then press 'Save as Area A' ",
        "in the sidebar. Do the same for Area B. Sidebar filters apply to both areas.")
    }
  })

  output$a_title <- renderText(paste("Area A:", if (is.null(areas$a_label)) "not set" else areas$a_label))
  output$b_title <- renderText(paste("Area B:", if (is.null(areas$b_label)) "not set" else areas$b_label))

  area_tiles <- function(d) {
    if (nrow(d) == 0) return(p(class = "text-muted", "No contracts yet."))
    layout_column_wrap(
      width = 1 / 2,
      value_box("Contracts", format(nrow(d), big.mark = ",")),
      value_box("Projects", format(n_distinct(d$project), big.mark = ",")),
      value_box("Median monthly rent", money(median(d$rent)), paste("average", money(mean(d$rent)))),
      value_box("Median rent psf", money(median(d$psf, na.rm = TRUE), 2))
    )
  }
  output$a_tiles <- renderUI(area_tiles(a_data()))
  output$b_tiles <- renderUI(area_tiles(b_data()))

  ab_colours <- function(d) setNames(c("#2c3e50", "#f39c12"), sort(unique(as.character(d$area))))   # A navy, B orange

  output$ab_beds <- renderPlot({
    d <- both_areas() |>
      group_by(area, bed_group) |>
      summarise(median = median(rent), n = n(), .groups = "drop")
    # Vertical bars: bedrooms on the x axis, median rent on the y axis.
    # Labels are written vertically above each bar so A and B never overlap.
    d$bed_group <- factor(d$bed_group, levels = bed_levels)
    ggplot(d, aes(bed_group, median, fill = area)) +
      geom_col(position = position_dodge(width = 0.85), width = 0.8) +
      geom_text(aes(label = paste0("S$", formatC(round(median), format = "d", big.mark = ","))),
                position = position_dodge(width = 0.85), angle = 90, hjust = -0.1, size = 3.6) +
      scale_fill_manual(values = ab_colours(d)) +
      scale_y_continuous(labels = scales::label_dollar(prefix = "S$"),
                         expand = expansion(mult = c(0, 0.22))) +
      labs(x = "Bedrooms", y = "Median monthly rent", fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", panel.grid.major.x = element_blank())
  })

  output$ab_trend <- renderPlot({
    d <- both_areas() |>
      group_by(area, quarter) |>
      summarise(median = median(rent), n = n(), .groups = "drop") |>
      filter(n >= 3)
    validate(need(nrow(d) > 0, "Not enough contracts per quarter."))
    ggplot(d, aes(quarter, median, colour = area)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 1.8) +
      scale_colour_manual(values = ab_colours(d)) +
      scale_y_continuous(labels = scales::label_dollar(prefix = "S$")) +
      labs(x = NULL, y = "Median monthly rent", colour = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })

  output$ab_table <- renderDT({
    d <- both_areas()
    side <- function(x, tag) {
      x |> group_by(Bedrooms = bed_group) |>
        summarise(Contracts = n(), Median = median(rent), Average = mean(rent),
                  psf = median(psf, na.rm = TRUE), .groups = "drop") |>
        rename_with(~ paste(tag, .x), -Bedrooms)
    }
    t <- full_join(side(d[startsWith(d$area, "A:"), ], "A"),
                   side(d[startsWith(d$area, "B:"), ], "B"), by = "Bedrooms") |>
      mutate(`B vs A (median)` = (`B Median` - `A Median`) / `A Median`) |>
      arrange(match(Bedrooms, bed_levels))
    datatable(t, rownames = FALSE, options = list(dom = "t", pageLength = 20, scrollX = TRUE)) |>
      formatCurrency(c("A Median", "A Average", "B Median", "B Average"), "S$", digits = 0) |>
      formatCurrency(c("A psf", "B psf"), "S$", digits = 2) |>
      formatPercentage("B vs A (median)", digits = 0)
  })


  # ==== BUDGET CHECK =================================================

  output$vb_median  <- renderText(money(median(comparables()$rent)))
  output$vb_average <- renderText(paste("average", money(mean(comparables()$rent))))

  output$vb_range <- renderText({
    q <- quantile(comparables()$rent, c(0.25, 0.75))
    paste0(money(q[1]), " - ", formatC(round(q[2]), format = "d", big.mark = ","))
  })

  output$vb_n <- renderText(format(nrow(comparables()), big.mark = ","))

  output$vb_n_note <- renderText({
    if (nrow(comparables()) < 10) "Few contracts: widen the filters or radius"
    else paste("in the latest", input$recent, "months")
  })

  output$verdict_box <- renderUI({
    r <- comparables()$rent
    b <- budget()
    q <- quantile(r, c(0.25, 0.75))
    share <- round(100 * mean(r >= b[1] & r <= b[2]))

    if (b[2] < q[1]) {
      verdict <- "Below typical"; colour <- "danger"
      advice  <- "Most comparable units rent for more. Try other districts, fewer bedrooms or a smaller unit."
    } else if (b[1] > q[2]) {
      verdict <- "Above typical"; colour <- "info"
      advice  <- "Most comparable units rent for less than your minimum. You have room to negotiate."
    } else {
      verdict <- "Realistic"; colour <- "success"
      advice  <- "Your range overlaps the typical rents for these filters."
    }
    value_box("Your budget range", verdict,
              paste0(share, "% of comparable rentals fall between ",
                     money(b[1]), " and ", money(b[2]), "."),
              advice, theme = colour)
  })

  output$budget_plot <- renderPlot({
    r <- comparables()$rent
    b <- budget()
    r <- r[r <= quantile(r, 0.99)]          # hide extreme top 1% for readability
    q <- quantile(r, c(0.25, 0.75))

    ggplot(data.frame(rent = r), aes(rent)) +
      annotate("rect", xmin = q[1], xmax = q[2], ymin = -Inf, ymax = Inf,
               fill = "#18bc9c", alpha = 0.15) +
      annotate("rect", xmin = b[1], xmax = b[2], ymin = -Inf, ymax = Inf,
               fill = "#e74c3c", alpha = 0.10) +
      geom_histogram(bins = 40, fill = "#2c3e50", colour = "white") +
      geom_vline(xintercept = b, linetype = "dashed", linewidth = 0.8, colour = "#e74c3c") +
      scale_x_continuous(labels = scales::label_dollar(prefix = "S$")) +
      labs(x = "Monthly rent", y = "Number of contracts") +
      theme_minimal(base_size = 14)
  })


  # ==== WITHIN BUDGET ================================================

  within_data <- reactive({
    b <- budget()
    s <- comparables() |>
      group_by(project, district, bed_group) |>
      summarise(
        size    = most_common(area_band),
        median  = median(rent),
        average = mean(rent),
        p25     = unname(quantile(rent, 0.25)),
        p75     = unname(quantile(rent, 0.75)),
        psf     = median(psf, na.rm = TRUE),
        n       = n(),
        last    = max(lease_date),
        .groups = "drop"
      ) |>
      filter(median >= b[1], median <= b[2], n >= input$wb_min_n) |>
      arrange(median)
    add_distance(s, input$destination)
  })

  output$wb_options   <- renderText(format(nrow(within_data()), big.mark = ","))
  output$wb_projects  <- renderText(format(n_distinct(within_data()$project), big.mark = ","))
  output$wb_districts <- renderText(n_distinct(within_data()$district))
  output$wb_beds_n    <- renderText(n_distinct(within_data()$bed_group))
  output$wb_beds      <- renderText({
    beds <- intersect(bed_levels, unique(within_data()$bed_group))
    if (length(beds) == 0) "None" else paste("Bedrooms:", paste(beds, collapse = ", "))
  })

  output$distance_note <- renderText({
    if (!has_coords) "Distance columns appear once the data file has coordinates."
    else paste0("Distances are to ", input$destination,
                " and the nearest MRT/LRT station (straight-line).")
  })

  output$wb_plot <- renderPlot({
    s <- within_data()
    validate(need(nrow(s) > 0,
      "Nothing matches your budget range. Try widening the budget or the filters."))
    s$bed_group <- factor(s$bed_group, levels = rev(bed_levels))
    s$district <- factor(s$district, levels = names(sort(table(s$district))))

    ggplot(s, aes(y = district, fill = bed_group)) +
      geom_bar() +
      scale_fill_manual(values = c("1" = "#18bc9c", "2" = "#3498db", "3" = "#2c3e50",
                                   "4" = "#f39c12", "5+" = "#e74c3c", "Unknown" = "#bdc3c7"),
                        breaks = bed_levels) +
      labs(x = "Number of project and bedroom options", y = NULL, fill = "Bedrooms") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })

  output$wb_table <- renderDT({
    s <- within_data() |> mutate(last = format(last, "%b %Y"))
    s <- prettify(s)
    datatable(s, rownames = FALSE, selection = "multiple",
              options = list(pageLength = 15, order = list(), scrollX = TRUE)) |>
      format_money_cols(s)
  })


  # ==== SHORTLIST ====================================================

  shortlist <- reactiveVal(data.frame(project = character(), bed_group = character()))

  observeEvent(input$add_short, {
    rows <- input$wb_table_rows_selected
    if (length(rows) == 0) {
      showNotification("Select one or more rows in the table first.", type = "warning")
      return()
    }
    new <- as.data.frame(within_data()[rows, c("project", "bed_group")])
    shortlist(unique(rbind(shortlist(), new)))
    showNotification(paste(length(rows), "option(s) added to your shortlist."), type = "message")
  })

  # All contracts for shortlisted options within the chosen period
  short_contracts <- reactive({
    sl <- shortlist()
    validate(need(nrow(sl) > 0,
      "Your shortlist is empty. In 'Within budget', click rows and press 'Add selected to shortlist'."))
    rentals |>
      semi_join(sl, by = c("project", "bed_group")) |>
      filter(lease_date >= input$period[1], lease_date <= input$period[2])
  })

  short_data <- reactive({
    b <- budget()
    d <- latest_months(short_contracts(), as.numeric(input$recent))
    validate(need(nrow(d) > 0, "No recent contracts for your shortlist in this time period."))
    s <- d |>
      group_by(project, district, bed_group) |>
      summarise(
        size    = most_common(area_band),
        median  = median(rent),
        average = mean(rent),
        p25     = unname(quantile(rent, 0.25)),
        p75     = unname(quantile(rent, 0.75)),
        psf     = median(psf, na.rm = TRUE),
        n       = n(),
        .groups = "drop"
      ) |>
      mutate(fits = case_when(median < b[1] ~ "Below range",
                              median > b[2] ~ "Above range",
                              TRUE          ~ "Within range")) |>
      arrange(median)
    s <- add_distance(s, input$destination)
    s$listings <- listing_link(s$project)
    s
  })

  output$short_table <- renderDT({
    s <- prettify(short_data())
    datatable(s, rownames = FALSE, escape = FALSE, selection = "multiple",
              options = list(pageLength = 25, order = list(), scrollX = TRUE)) |>
      format_money_cols(s) |>
      formatStyle("Budget fit",
                  color = styleEqual(c("Within range", "Below range", "Above range"),
                                     c("#18bc9c", "#3498db", "#e74c3c")),
                  fontWeight = "bold")
  })

  observeEvent(input$short_remove, {
    rows <- input$short_table_rows_selected
    if (length(rows) == 0) {
      showNotification("Select rows in your shortlist to remove.", type = "warning")
      return()
    }
    gone <- short_data()[rows, c("project", "bed_group")]
    shortlist(anti_join(shortlist(), gone, by = c("project", "bed_group")))
  })

  observeEvent(input$short_clear, {
    shortlist(data.frame(project = character(), bed_group = character()))
  })

  output$short_download <- downloadHandler(
    filename = function() "rental_shortlist.csv",
    content  = function(file) {
      write.csv(prettify(select(short_data(), -listings)), file, row.names = FALSE)
    }
  )

  output$short_plot <- renderPlot({
    s <- short_data()
    b <- budget()
    s$label <- paste0(s$project, " (", s$bed_group, " BR)")
    ggplot(s, aes(y = reorder(label, -median))) +
      annotate("rect", xmin = b[1], xmax = b[2], ymin = -Inf, ymax = Inf,
               fill = "#e74c3c", alpha = 0.10) +
      geom_linerange(aes(xmin = p25, xmax = p75), linewidth = 1.2, colour = "grey70") +
      geom_point(aes(x = median), size = 3.5, colour = "#2c3e50") +
      scale_x_continuous(labels = scales::label_dollar(prefix = "S$")) +
      labs(x = "Monthly rent", y = NULL) +
      theme_minimal(base_size = 13)
  })

  output$short_trend <- renderPlot({
    t <- short_contracts() |>
      mutate(label = paste0(project, " (", bed_group, " BR)")) |>
      group_by(quarter, label) |>
      summarise(median = median(rent), n = n(), .groups = "drop") |>
      filter(n >= 3)
    validate(need(nrow(t) > 0, "Not enough contracts per quarter to draw trends."))
    ggplot(t, aes(quarter, median, colour = label)) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.6) +
      scale_y_continuous(labels = scales::label_dollar(prefix = "S$")) +
      labs(x = NULL, y = "Median monthly rent", colour = NULL) +
      guides(colour = guide_legend(ncol = 1)) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })

  # Save and restore the shortlist and map selection in "Save this search" links
  onBookmark(function(state) {
    state$values$shortlist <- shortlist()
    state$values$sel_ids   <- sel$ids
    state$values$sel_label <- sel$label
    state$values$areas     <- reactiveValuesToList(areas)
  })
  onRestore(function(state) {
    sl <- state$values$shortlist
    if (!is.null(sl) && length(sl$project) > 0) {
      shortlist(data.frame(project = unlist(sl$project), bed_group = unlist(sl$bed_group)))
    }
    if (length(state$values$sel_ids) > 0) {
      sel$ids   <- as.integer(unlist(state$values$sel_ids))
      sel$label <- unlist(state$values$sel_label)
    }
    a <- state$values$areas
    if (!is.null(a)) {
      for (k in c("a_ids", "b_ids")) if (length(a[[k]])) areas[[k]] <- as.integer(unlist(a[[k]]))
      for (k in c("a_label", "b_label")) if (length(a[[k]])) areas[[k]] <- unlist(a[[k]])
    }
  })
  onBookmarked(function(url) showBookmarkUrlModal(url))


  # ==== PRICE TRENDS =================================================

  trend_data <- reactive({
    d <- selection()
    split_up <- isTRUE(input$split_district) && length(input$district) > 1
    d$group <- if (split_up) d$district else "Your selection"
    d |> group_by(quarter, group) |> summarise_rent() |> filter(n >= 5)
  })

  output$trend_plot <- renderPlot({
    t <- trend_data()
    validate(need(nrow(t) > 0, "Not enough contracts per quarter to draw a trend."))
    one_line <- length(unique(t$group)) == 1
    b <- budget()

    ggplot(t, aes(quarter, median, colour = group, fill = group)) +
      annotate("rect", ymin = b[1], ymax = b[2], xmin = as.Date(-Inf), xmax = as.Date(Inf),
               fill = "#e74c3c", alpha = 0.08) +
      (if (one_line) geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.15, colour = NA)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 1.8) +
      (if (one_line) scale_colour_manual(values = "#2c3e50", aesthetics = c("colour", "fill"))) +
      scale_y_continuous(labels = scales::label_dollar(prefix = "S$")) +
      labs(x = NULL, y = "Median monthly rent", colour = NULL, fill = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = if (one_line) "none" else "bottom")
  })

  output$trend_summary <- renderText({
    t <- selection() |> group_by(quarter) |> summarise_rent() |> filter(n >= 5)
    if (nrow(t) < 2) return("Not enough data to measure the change over time.")
    first <- t[1, ]; last <- t[nrow(t), ]
    change <- round(100 * (last$median / first$median - 1))
    paste0("Median rent went from ", money(first$median), " (", quarter_label(first$quarter),
           ") to ", money(last$median), " (", quarter_label(last$quarter), "), a change of ",
           ifelse(change >= 0, "+", ""), change, "%. ",
           "Grey band = typical range. Red band = your budget range.")
  })


  # ==== COMPARE AREAS ================================================

  recent_market <- reactive({
    d <- latest_months(market(), as.numeric(input$recent))
    validate(need(nrow(d) > 0, "No contracts match these filters."))
    d
  })

  compare_data <- reactive({
    groups <- if (input$compare_by == "District") "district" else c("project", "district")
    s <- recent_market() |>
      group_by(across(all_of(groups))) |>
      summarise_rent(budget()) |>
      filter(n >= input$min_n) |>
      arrange(median)
    if (input$compare_by == "Project") s <- add_distance(s, input$destination)
    s
  })

  output$district_plot <- renderPlot({
    s <- compare_data()
    req(input$compare_by == "District", !"project" %in% names(s))
    b <- budget()
    s$fits <- ifelse(s$median >= b[1] & s$median <= b[2],
                     "Median within budget", "Median outside budget")

    ggplot(s, aes(y = reorder(district, -median))) +
      annotate("rect", xmin = b[1], xmax = b[2], ymin = -Inf, ymax = Inf,
               fill = "#e74c3c", alpha = 0.08) +
      geom_linerange(aes(xmin = p25, xmax = p75), linewidth = 1.2, colour = "grey70") +
      geom_point(aes(x = average), shape = 4, size = 2.5, colour = "#7f8c8d") +
      geom_point(aes(x = median, colour = fits), size = 3.5) +
      # Median rent written just past the end of each line
      geom_text(aes(x = pmax(p75, average),
                    label = paste0("S$", formatC(round(median), format = "d", big.mark = ","))),
                hjust = -0.15, size = 3.5, colour = "#2c3e50") +
      scale_colour_manual(values = c("Median within budget"  = "#18bc9c",
                                     "Median outside budget" = "#95a5a6")) +
      scale_x_continuous(labels = scales::label_dollar(prefix = "S$"),
                         expand = expansion(mult = c(0.02, 0.12))) +
      labs(x = "Monthly rent (dot = median, cross = average; label = median)", y = NULL, colour = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })

  output$compare_table <- renderDT({
    b <- budget()
    s <- prettify(compare_data())
    datatable(s, rownames = FALSE,
              options = list(pageLength = 15, order = list(), scrollX = TRUE)) |>
      format_money_cols(s) |>
      formatStyle("Median rent", target = "row",
                  backgroundColor = styleInterval(c(b[1] - 0.5, b[2] + 0.5),
                                                  c("white", "#e8f8f5", "white")))
  })

  output$heat_plot <- renderPlotly({
    s <- recent_market() |>
      filter(bed_group != "Unknown") |>
      group_by(district, bed_group) |>
      summarise(median = median(rent), n = n(), .groups = "drop") |>
      filter(n >= input$min_n)
    validate(need(nrow(s) > 0, "Not enough contracts to fill the grid."))
    s$tip <- paste0(s$district, "<br>", s$bed_group, " bedrooms<br>Median: ", money(s$median),
                    "<br>Contracts: ", format(s$n, big.mark = ","))
    p <- ggplot(s, aes(x = bed_group, y = reorder(district, median), fill = median, text = tip)) +
      geom_tile(colour = "white") +
      geom_text(aes(label = sprintf("%.1f", median / 1000)), size = 3.2) +
      # Cap colours at the 95th percentile so one luxury outlier doesn't wash out the rest
      scale_fill_distiller(palette = "RdYlBu", direction = -1,
                           limits = c(min(s$median), quantile(s$median, 0.95)),
                           oob = scales::squish,
                           labels = scales::label_dollar(prefix = "S$")) +
      labs(x = "Bedrooms", y = NULL, fill = "Median rent") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  # Project-level MRT data (shared by the chart and the table)
  mrt_projects <- reactive({
    validate(need(has_coords && !is.null(mrt),
                  "Needs coordinates in the data file and data/mrt_lrt.csv."))
    s <- recent_market() |>
      filter(!is.na(mrt_m)) |>
      group_by(project, district) |>
      summarise(mrt_station = first(mrt_station), mrt_m = first(mrt_m),
                psf = median(psf, na.rm = TRUE), median = median(rent), n = n(),
                .groups = "drop") |>
      filter(n >= input$min_n) |>
      mutate(walk_min = round(mrt_m / 80))
    validate(need(nrow(s) > 5, "Not enough projects to compare."))
    s
  })

  output$mrt_plot <- renderPlotly({
    s <- mrt_projects()
    s$tip <- paste0(s$project, "<br>", s$district, "<br>Nearest: ", s$mrt_station, " (",
                    format(s$mrt_m, big.mark = ","), " m, ~", s$walk_min, " min walk)",
                    "<br>Median rent: ", money(s$median), "<br>Median ", money(s$psf, 2),
                    " psf<br>Contracts: ", s$n)
    p <- ggplot(s, aes(mrt_m, psf)) +
      geom_point(aes(size = n, text = tip), alpha = 0.4, colour = "#2c3e50") +
      geom_smooth(method = if (nrow(s) >= 30) "loess" else "lm", formula = y ~ x,
                  se = FALSE, colour = "#e74c3c") +
      scale_y_continuous(labels = scales::label_dollar(prefix = "S$")) +
      scale_size_area(max_size = 6) +
      labs(x = "Distance to nearest MRT/LRT station (m)", y = "Median rent per sqft",
           size = "Contracts") +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text")
  })

  output$mrt_table <- renderDT({
    s <- mrt_projects() |>
      arrange(mrt_m) |>
      select(project, district, mrt_station, mrt_m, walk_min, median, psf, n) |>
      prettify()
    datatable(s, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) |>
      format_money_cols(s)
  })


  # ==== COMPARE PROPERTIES (up to three projects) ===================

  # Project list, most-rented first
  project_order <- rentals |> count(project, sort = TRUE) |> pull(project)
  for (id in c("cp_a", "cp_b", "cp_c")) {
    updateSelectizeInput(session, id, choices = c("", project_order), server = TRUE)
  }

  observeEvent(input$cp_from_short, {
    picks <- head(unique(shortlist()$project), 3)
    if (length(picks) == 0) {
      showNotification("Your shortlist is empty. Add projects from 'Within budget' first.",
                       type = "warning")
      return()
    }
    ids <- c("cp_a", "cp_b", "cp_c")
    for (k in seq_along(ids)) {
      updateSelectizeInput(session, ids[k], choices = c("", project_order),
                           selected = if (k <= length(picks)) picks[k] else "", server = TRUE)
    }
  })

  cp_projects <- reactive({
    x <- c(input$cp_a, input$cp_b, input$cp_c)
    x <- unique(x[!is.na(x) & nzchar(x)])
    validate(need(length(x) >= 1,
      "Pick at least one project above, or press 'Fill from my shortlist'."))
    x
  })

  cp_data <- reactive({
    d <- rentals[rentals$project %in% cp_projects() &
                 rentals$lease_date >= input$period[1] &
                 rentals$lease_date <= input$period[2], ]
    if (input$cp_bed != "All") d <- d[d$bed_group == input$cp_bed, ]
    d$project <- factor(d$project, levels = cp_projects())
    d
  })

  cp_colours <- reactive(setNames(c("#2c3e50", "#f39c12", "#18bc9c")[seq_along(cp_projects())],
                                  cp_projects()))

  cp_summary <- reactive({
    b <- budget()
    s <- cp_data() |>
      mutate(project = as.character(project)) |>
      group_by(project) |>
      summarise(district = paste(sort(unique(district)), collapse = ", "),
                n = n(), median = median(rent), average = mean(rent),
                p25 = unname(quantile(rent, 0.25)), p75 = unname(quantile(rent, 0.75)),
                psf = median(psf, na.rm = TRUE), sqft = median(area_sqft, na.rm = TRUE),
                last = format(max(lease_date), "%b %Y"), .groups = "drop")
    s <- left_join(data.frame(project = cp_projects()), s, by = "project")
    s$n[is.na(s$n)] <- 0L
    s$fits <- case_when(is.na(s$median) ~ "No contracts",
                        s$median < b[1] ~ "Below range",
                        s$median > b[2] ~ "Above range",
                        TRUE            ~ "Within range")
    add_distance(s, input$destination)
  })

  output$cp_cards <- renderUI({
    s <- cp_summary()
    cols <- cp_colours()
    cards <- lapply(seq_len(nrow(s)), function(i) {
      x <- s[i, ]
      card(
        card_header(class = "text-white", style = paste0("background:", cols[[x$project]], ";"),
                    x$project),
        if (x$n == 0) p(class = "text-muted", "No contracts for these filters.") else tagList(
          h3(money(x$median), class = "mb-0"),
          p(class = "text-muted", "median monthly rent (average ", money(x$average), ")"),
          p(strong("Typical range: "), money(x$p25), " - ", money(x$p75), br(),
            strong("Rent per sqft: "), money(x$psf, 2), br(),
            strong("Typical size: "), format(round(x$sqft), big.mark = ","), " sqft (estimated)", br(),
            strong("District: "), x$district, br(),
            strong("Contracts: "), format(x$n, big.mark = ","), " (latest ", x$last, ")", br(),
            if (!is.null(x$mrt_station) && !is.na(x$mrt_station))
              tagList(strong("Nearest MRT/LRT: "), x$mrt_station, " (~", x$walk_min, " min walk)", br()),
            if (!is.null(x$dest_km) && !is.na(x$dest_km))
              tagList(strong("To ", input$destination, ": "), x$dest_km, " km", br()),
            strong("Budget fit: "), x$fits),
          if (x$n < 5) span(class = "badge bg-warning text-dark", "Low sample size")
        )
      )
    })
    do.call(layout_columns, c(list(col_widths = rep(12 / length(cards), length(cards))), cards))
  })

  output$cp_box <- renderPlotly({
    d <- cp_data()
    validate(need(nrow(d) > 0, "No contracts match these filters."))
    p <- ggplot(d, aes(project, rent, fill = project)) +
      geom_boxplot(outlier.alpha = 0.2) +
      scale_fill_manual(values = cp_colours()) +
      scale_y_continuous(labels = scales::label_dollar(prefix = "S$")) +
      labs(x = NULL, y = "Monthly rent", fill = NULL) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_blank())
    # Start zoomed to the main range (rare luxury contracts squash the boxes);
    # double-click the chart to see everything
    top <- unname(quantile(d$rent, 0.98)) * 1.1
    ggplotly(p) |>
      layout(yaxis = list(range = c(0, top), tickmode = "auto", nticks = 8,
                          tickprefix = "S$", tickformat = ",.0f"),
             legend = list(orientation = "h", y = -0.1))
  })

  output$cp_trend <- renderPlotly({
    d <- cp_data() |>
      group_by(project, quarter) |>
      summarise(median = median(rent), n = n(), .groups = "drop") |>
      filter(n >= 3)
    validate(need(nrow(d) > 0, "No quarters with at least 3 contracts."))
    d$tip <- paste0(d$project, "<br>", quarter_label(d$quarter), "<br>Median: ",
                    money(d$median), "<br>Contracts: ", d$n)
    p <- ggplot(d, aes(quarter, median, colour = project, group = project, text = tip)) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.6) +
      scale_colour_manual(values = cp_colours()) +
      scale_y_continuous(labels = scales::label_dollar(prefix = "S$")) +
      labs(x = NULL, y = "Median monthly rent", colour = NULL) +
      theme_minimal(base_size = 12)
    ggplotly(p, tooltip = "text") |> layout(legend = list(orientation = "h", y = -0.15))
  })

  cp_table_data <- reactive({
    s <- cp_summary()
    s$sqft <- round(s$sqft)
    s$listings <- ifelse(s$n > 0, listing_link(s$project), "")
    names(s)[names(s) == "sqft"] <- "Typical size (sqft)"
    names(s)[names(s) == "last"] <- "Latest lease"
    s
  })

  output$cp_table <- renderDT({
    s <- prettify(cp_table_data())
    datatable(s, rownames = FALSE, escape = FALSE,
              options = list(dom = "t", scrollX = TRUE)) |>
      format_money_cols(s)
  })

  output$cp_csv <- downloadHandler(
    filename = function() "property_comparison.csv",
    content  = function(file) {
      write.csv(prettify(select(cp_table_data(), -listings)), file, row.names = FALSE)
    }
  )


  # ==== VALUE FOR MONEY ==============================================

  vfm_data <- reactive({
    b <- budget()
    s <- recent_market() |>
      filter(!is.na(area_sqft)) |>
      group_by(project, district) |>
      summarise(median = median(rent), sqft = median(area_sqft),
                psf = median(psf, na.rm = TRUE), n = n(), .groups = "drop") |>
      filter(n >= input$vfm_min_n) |>
      mutate(fits = case_when(median < b[1] ~ "Below range",
                              median > b[2] ~ "Above range",
                              TRUE          ~ "Within range"))
    validate(need(nrow(s) > 0, "No projects match these filters."))
    s
  })

  output$vfm_plot <- renderPlotly({
    s <- vfm_data()
    s$tip <- paste0(s$project, "<br>", s$district, "<br>Median rent: ", money(s$median),
                    "<br>Median size: ", format(round(s$sqft), big.mark = ","), " sqft",
                    "<br>Rent per sqft: ", money(s$psf, 2), "<br>Contracts: ", s$n)
    p <- ggplot(s, aes(sqft, median, colour = fits, size = n, text = tip)) +
      geom_point(alpha = 0.6) +
      guides(size = "none") +
      scale_colour_manual(values = c("Within range" = "#18bc9c", "Below range" = "#3498db",
                                     "Above range" = "#95a5a6")) +
      scale_y_continuous(labels = scales::label_dollar(prefix = "S$")) +
      scale_x_continuous(labels = scales::label_comma()) +
      scale_size_area(max_size = 8) +
      labs(x = "Median floor area (sqft, estimated)", y = "Median monthly rent",
           colour = "Budget fit", size = "Contracts") +
      theme_minimal(base_size = 12)
    # Start zoomed to 98% of projects so the main cluster is readable;
    # double-click the chart to see the luxury outliers too
    ggplotly(p, tooltip = "text") |>
      layout(xaxis = list(range = c(0, unname(quantile(s$sqft, 0.98)) * 1.05),
                          tickmode = "auto", nticks = 8, tickformat = ",.0f"),
             yaxis = list(range = c(0, unname(quantile(s$median, 0.98)) * 1.05),
                          tickmode = "auto", nticks = 8, tickprefix = "S$", tickformat = ",.0f"))
  })

  output$vfm_table <- renderDT({
    s <- vfm_data() |>
      arrange(psf) |>
      mutate(sqft = round(sqft)) |>
      add_distance(input$destination)
    names(s)[names(s) == "sqft"] <- "Typical size (sqft)"
    s <- prettify(s)
    datatable(s, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)) |>
      format_money_cols(s)
  })


  # ==== TRANSACTIONS =================================================

  records <- reactive({
    selection() |>
      arrange(desc(lease_date), project) |>
      transmute(`Lease start` = format(lease_date, "%b %Y"),
                Project = project, Street = street, District = district,
                Type = type, Bedrooms = bed_group,
                `Floor area (sqft)` = area_band,
                `Monthly rent` = rent, `S$ psf` = round(psf, 2),
                `Nearest MRT/LRT` = mrt_station)
  })

  output$records_table <- renderDT({
    datatable(records(), rownames = FALSE,
              options = list(pageLength = 20, scrollX = TRUE)) |>
      formatCurrency("Monthly rent", "S$", digits = 0)
  })

  output$download <- downloadHandler(
    filename = function() "rental_transactions.csv",
    content  = function(file) write.csv(records(), file, row.names = FALSE)
  )
}

shinyApp(ui, server, enableBookmarking = "url")
