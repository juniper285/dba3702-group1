# ---- Loading and preparing the rental contracts ----------------------

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

bed_levels <- c("1", "2", "3", "4", "5+", "Unknown")
area_max   <- 4000   # floor-area slider's top end means "4,000 sqft and above"

# Data loaded once per R session and shared by every user of the app
.cache <- new.env(parent = emptyenv())

#' Compute `expr` once and keep the result for the rest of the R session
#' @noRd
cached <- function(name, expr) {
  if (!exists(name, envir = .cache, inherits = FALSE)) assign(name, expr, envir = .cache)
  get(name, envir = .cache)
}

#' All rental contracts, ready for the app
#'
#' Reads inst/extdata/rentals.rds (built by data-raw/rentals.R) on first use.
#' @noRd
rentals_data <- function() {
  cached("rentals", prepare_rentals(readRDS(app_sys("extdata", "rentals.rds"))))
}

#' Add the columns the app uses to the raw contracts
#'
#' @param d Data frame as written by data-raw/rentals.R.
#' @return `d` plus `district` (label), `bed_group`, `quarter` and `loc_id`
#'   (one id per map location; NA when the contract has no coordinates).
#' @noRd
prepare_rentals <- function(d) {
  d <- d |>
    mutate(
      district = paste0("D", sprintf("%02d", district_no), " ",
                        district_names[sprintf("%02d", district_no)]),
      bed_group = case_when(
        is.na(bedrooms) ~ "Unknown",
        bedrooms >= 5   ~ "5+",
        TRUE            ~ as.character(bedrooms)
      ),
      quarter = as.Date(cut(lease_date, "quarter"))
    )
  has_xy <- !is.na(d$lat) & !is.na(d$lng)
  xy <- paste(d$lat, d$lng)
  d$loc_id <- ifelse(has_xy, match(xy, unique(xy[has_xy])), NA_integer_)
  d
}

#' The sidebar filters, applied to a set of contracts
#'
#' Every argument except `d` is a sidebar input. Empty `districts` = all.
#' Contracts with no floor area are dropped.
#' @noRd
filter_market <- function(d, districts, beds, types, area, period) {
  if (length(districts)) d <- d[d$district %in% districts, ]
  # which() drops rows with no floor area instead of returning NA rows
  d[which(d$bed_group %in% beds &
          d$type %in% types &
          d$area_sqft >= area[1] &
          (d$area_sqft <= area[2] | area[2] >= area_max) &
          d$lease_date >= period[1] &
          d$lease_date <= period[2]), ]
}
