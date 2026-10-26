# ---- Map building blocks -----------------------------------------------

metric_choices <- c("Median S$ psf per month" = "psf", "Median monthly rent" = "rent")

# Fixed colour bins (from the full dataset) so colours mean the same thing
# whatever the filters are
map_bins <- list(
  psf  = c(0, 3, 3.5, 4, 4.5, 5, 5.5, 6, 7, Inf),
  rent = c(0, 2500, 3000, 3500, 4000, 5000, 6000, 8000, 12000, Inf)
)

#' Colour palette for a map metric ("psf" or "rent")
#' @noRd
map_palette <- function(metric) colorBin("RdYlBu", bins = map_bins[[metric]], reverse = TRUE)

#' One row per map location (loc_id, lat, lng)
#' @noRd
map_locations <- function() {
  cached("locs", rentals_data() |> filter(!is.na(loc_id)) |> distinct(loc_id, lat, lng))
}

#' Map locations as sf points, for finding the ones inside a drawn shape
#' @noRd
map_locations_sf <- function() {
  cached("locs_sf", st_as_sf(map_locations(), coords = c("lng", "lat"), crs = 4326, remove = FALSE))
}

#' Convert a shape drawn with the leaflet.extras toolbar into an sf polygon
#' @noRd
drawn_to_sf <- function(f) {
  if (f$geometry$type == "Point") {             # circle tool
    st_buffer(st_sfc(st_point(unlist(f$geometry$coordinates)), crs = 4326),
              dist = f$properties$radius)
  } else {                                      # polygon / rectangle
    xy <- matrix(unlist(f$geometry$coordinates), ncol = 2, byrow = TRUE)
    st_sfc(st_polygon(list(xy)), crs = 4326)
  }
}

#' Polygon / rectangle / circle drawing tools (one shape at a time)
#' @noRd
add_draw_toolbar <- function(map) {
  addDrawToolbar(map, polylineOptions = FALSE, markerOptions = FALSE,
                 circleMarkerOptions = FALSE, singleFeature = TRUE,
                 editOptions = editToolbarOptions())
}

#' Empty Singapore map with basemaps, search and drawing tools
#' @noRd
base_map <- function() {
  leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
    # Keyless basemaps: CARTO now requires an API key, and the old
    # maps-*.onemap.sg tile host bundled in leaflet.providers is offline
    addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Light") |>
    addTiles("https://www.onemap.gov.sg/maps/tiles/Default/{z}/{x}/{y}.png",
             group = "OneMap",
             options = tileOptions(minZoom = 11, maxZoom = 19),
             attribution = paste0('New OneMap | Map data &copy; contributors, ',
                                  '<a href="https://www.sla.gov.sg">Singapore Land Authority</a>')) |>
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
    addLayersControl(baseGroups = c("Light", "OneMap", "Satellite"),
                     overlayGroups = c("Buildings", "Heatmap"),
                     options = layersControlOptions(collapsed = TRUE),
                     position = "bottomleft") |>
    hideGroup("Heatmap") |>
    addSearchOSM() |>
    add_draw_toolbar() |>
    setView(103.83, 1.33, zoom = 12)
}

#' Summarise contracts to one row per map location
#' @noRd
summarise_locations <- function(d) {
  d |>
    group_by(loc_id, lat, lng) |>
    summarise(project = paste(head(unique(project), 3), collapse = " / "),
              address = first(address), district = first(district),
              n = n(), rent = median(rent), psf = median(psf, na.rm = TRUE),
              .groups = "drop")
}

#' Hover labels for summarise_locations() rows
#' @noRd
location_labels <- function(d) {
  sprintf(
    "<b>%s</b><br>%s<br>%s<br>%s rentals<br>Median rent %s<br>Median S$%.2f psf",
    d$project, ifelse(is.na(d$address), "", d$address), d$district,
    format(d$n, big.mark = ","), money(d$rent), d$psf) |>
    lapply(HTML)
}
