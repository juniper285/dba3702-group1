#' @keywords internal
"_PACKAGE"

#' @import shiny
#' @import bslib
#' @import leaflet
#' @importFrom dplyr filter mutate group_by summarise arrange transmute select
#' @importFrom dplyr distinct semi_join anti_join left_join n n_distinct first
#' @importFrom dplyr case_when across all_of desc ungroup count slice_max
#' @importFrom DT DTOutput renderDT datatable formatCurrency formatPercentage
#' @importFrom DT formatStyle styleEqual styleInterval
#' @importFrom leaflet.extras addSearchOSM addHeatmap addDrawToolbar
#' @importFrom leaflet.extras removeDrawToolbar editToolbarOptions
#' @importFrom sf st_as_sf st_buffer st_sfc st_point st_polygon st_intersects
#' @importFrom stats median quantile reorder
#' @importFrom utils head read.csv write.csv modifyList
NULL

# Column names used inside dplyr calls
utils::globalVariables(c(
  "project", "street", "district", "district_no", "type", "bedrooms", "bed_group",
  "rent", "area_band", "area_sqft", "psf", "lease_date", "quarter", "address",
  "lat", "lng", "loc_id", "geo_match", "group", "label", "median", "p25", "p75",
  "n", "last", "fits", "listings", "mrt_station", "mrt_m", "month", "Bedrooms",
  "area", "a_median", "b_median", "walk_min", "sqft", "q"
))
