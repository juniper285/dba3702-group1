# ---- Map building blocks -----------------------------------------------

metric_choices <- c("Median S$ psf per month" = "psf", "Median monthly rent" = "rent",
                    "Median rent vs my budget" = "budget")

# Fixed colour bins (from the full dataset) so colours mean the same thing
# whatever the filters are
map_bins <- list(
  psf  = c(0, 3, 3.5, 4, 4.5, 5, 5.5, 6, 7, Inf),
  rent = c(0, 2500, 3000, 3500, 4000, 5000, 6000, 8000, 12000, Inf)
)

budget_levels <- c("Below my budget", "Within my budget", "Above my budget")

#' Colour palette for a map metric ("psf", "rent" or "budget")
#' @noRd
map_palette <- function(metric) {
  if (metric == "budget") {
    colorFactor(c("#4575b4", "#1a9850", "#d73027"), levels = budget_levels)
  } else {
    colorBin("RdYlBu", bins = map_bins[[metric]], reverse = TRUE)
  }
}

#' Where each rent sits against a budget range, as a factor of budget_levels
#' @noRd
budget_fit <- function(rent, budget) {
  factor(ifelse(rent < budget[1], budget_levels[1],
                ifelse(rent > budget[2], budget_levels[3], budget_levels[2])),
         levels = budget_levels)
}

#' TRUE for rentals whose building was found exactly or well enough to map
#' (the geocoding's "Exact" and "Good" matches)
#' @noRd
well_located <- function(geo_match) grepl("^Exact|^Good", geo_match)

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

# Schools and work areas show as yellow stars (always on); airport, ports and
# hospitals get their own layer with icons
star_groups <- c("Universities", "Polytechnics", "Business districts")
star_svg <- paste0(
  "<svg xmlns='http://www.w3.org/2000/svg' width='26' height='26' viewBox='0 0 24 24'>",
  "<path d='M12 1.5l3.1 6.6 7.2.9-5.3 5 1.4 7.1L12 17.6 5.6 21.1 7 14l-5.3-5 7.2-.9z' ",
  "fill='#FFD60A' stroke='#5c4a00' stroke-width='1.3' stroke-linejoin='round'/></svg>")
place_icons   <- c("Transport" = "plane", "Hospitals" = "plus-square")
place_colours <- c("Transport" = "green", "Hospitals" = "red")

#' Add the key places: stars for schools and work areas, icons for the rest
#' @noRd
add_key_places <- function(map, places = key_places) {
  stars  <- places[places$group %in% star_groups, ]
  others <- places[!places$group %in% star_groups, ]
  star <- makeIcon(
    iconUrl = paste0("data:image/svg+xml;charset=UTF-8", ",",
                     utils::URLencode(star_svg, reserved = TRUE)),
    iconWidth = 26, iconHeight = 26, iconAnchorX = 13, iconAnchorY = 13)
  map |>
    addMarkers(data = stars, lng = ~lng, lat = ~lat, icon = star,
               group = "Schools and work areas", label = ~paste0(name, " (", group, ")")) |>
    addAwesomeMarkers(data = others, lng = ~lng, lat = ~lat,
                      group = "Airport, ports and hospitals",
                      label = ~paste0(name, " (", group, ")"),
                      icon = awesomeIcons(icon = unname(place_icons[others$group]),
                                          library = "fa", iconColor = "white",
                                          markerColor = unname(place_colours[others$group])))
}

#' Add small white dots for MRT/LRT stations and purple dots for schools
#' @noRd
add_station_layers <- function(map, mrt = mrt_stations(), schools = schools_data()) {
  if (!is.null(mrt)) {
    map <- addCircleMarkers(map, data = mrt, lng = ~lng, lat = ~lat, group = "MRT/LRT stations",
                            radius = 4, color = "#222", weight = 1.5, fillColor = "#fff",
                            fillOpacity = 1, label = ~station)
  }
  if (!is.null(schools)) {
    map <- addCircleMarkers(map, data = schools, lng = ~lng, lat = ~lat, group = "Schools",
                            radius = 4, color = "#6a3d9a", weight = 1.5, fillColor = "#cab2d6",
                            fillOpacity = 1,
                            label = if (is.null(schools$level)) ~name else ~paste0(name, " (", level, ")"))
  }
  map
}

#' Empty Singapore map with basemaps, search, drawing tools and the
#' key-place and MRT/LRT layers
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
                     overlayGroups = c("Buildings", "My places", "Schools and work areas",
                                       "Airport, ports and hospitals", "Heatmap",
                                       "MRT/LRT stations",
                                       if (!is.null(schools_data())) "Schools"),
                     options = layersControlOptions(collapsed = TRUE),
                     position = "bottomleft") |>
    hideGroup(c("Airport, ports and hospitals", "Heatmap", "MRT/LRT stations", "Schools")) |>
    addSearchOSM() |>
    add_draw_toolbar() |>
    setView(103.83, 1.33, zoom = 12) |>
    add_key_places() |>
    add_station_layers()
}

#' Show the renter's places that matter as purple pins, with each place's icon
#'
#' @param map A leafletProxy().
#' @param places Data frame with `name`, `lat`, `lng` and optionally `kind`.
#' @noRd
update_my_places <- function(map, places) {
  map <- clearGroup(map, "My places")
  if (nrow(places) == 0) return(map)
  kind <- if (is.null(places$kind)) "other" else places$kind
  addAwesomeMarkers(map, data = places, lng = ~lng, lat = ~lat, group = "My places",
                    label = ~paste("My place:", name),
                    icon = awesomeIcons(icon = place_icon(kind, "map_icon"),
                                        library = "fa", iconColor = "white",
                                        markerColor = "purple"))
}

#' Draw a dashed ring of `radius` metres around a station (or remove it)
#'
#' @param map A leafletProxy().
#' @param station Station name, or NULL to remove the ring.
#' @noRd
update_station_ring <- function(map, station, radius) {
  map <- clearGroup(map, "Station radius")
  if (is.null(station)) return(map)
  st <- station_location(station)
  addCircles(map, lng = st[2], lat = st[1], radius = radius, group = "Station radius",
             color = "#222", weight = 2, dashArray = "6", fillOpacity = 0.05,
             label = paste0(station, ": ", radius, " m (about ", walk_minutes(radius), " min walk)"))
}

#' Summarise contracts to one row per map location
#' @noRd
summarise_locations <- function(d) {
  d |>
    group_by(loc_id, lat, lng) |>
    summarise(project = paste(head(unique(project), 3), collapse = " / "),
              address = first(address), district = first(district),
              mrt = first(mrt_station), mrt_m = first(mrt_m),
              n = n(), rent = median(rent), psf = median(psf, na.rm = TRUE),
              .groups = "drop")
}

#' Hover labels for summarise_locations() rows
#' @noRd
location_labels <- function(d) {
  sprintf(
    "<b>%s</b><br>%s<br>%s<br>%s rentals<br>Median rent %s<br>Median S$%.2f psf<br>Nearest station: %s",
    d$project, ifelse(is.na(d$address), "", d$address), d$district,
    format(d$n, big.mark = ","), money(d$rent), d$psf,
    ifelse(is.na(d$mrt), "-", paste0(d$mrt, " (", format(d$mrt_m, big.mark = ","), " m)"))) |>
    lapply(HTML)
}
