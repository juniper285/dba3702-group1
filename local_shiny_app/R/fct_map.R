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
    # One colour, lightest = cheapest, darkest = dearest
    colorBin(c("#c6dbef", "#08306b"), bins = map_bins[[metric]])
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

# Key-place layers on the map, built from the groups in destination_groups.
# All start hidden, so the map shows only the renter's own places until they
# turn a layer on. Universities and polytechnics are yellow stars; the others
# are pins with an icon (`colour` is an awesome-markers colour, `hex` the same
# colour for the key)
key_layers <- list(
  "Universities and polytechnics" = list(groups = c("Universities", "Polytechnics")),
  "Work areas"        = list(groups = "Business districts",
                             icon = "briefcase", colour = "blue", hex = "#38aadd"),
  "Hospitals"         = list(groups = "Hospitals",
                             icon = "plus-square", colour = "red", hex = "#d63e2a"),
  "Airport and ports" = list(groups = "Transport",
                             icon = "plane", colour = "green", hex = "#72b026")
)
star_svg <- paste0(
  "<svg xmlns='http://www.w3.org/2000/svg' width='26' height='26' viewBox='0 0 24 24'>",
  "<path d='M12 1.5l3.1 6.6 7.2.9-5.3 5 1.4 7.1L12 17.6 5.6 21.1 7 14l-5.3-5 7.2-.9z' ",
  "fill='#FFD60A' stroke='#5c4a00' stroke-width='1.3' stroke-linejoin='round'/></svg>")

#' Add the key places, one layer per entry of key_layers
#' @noRd
add_key_places <- function(map, places = key_places) {
  star <- makeIcon(
    iconUrl = paste0("data:image/svg+xml;charset=UTF-8", ",",
                     utils::URLencode(star_svg, reserved = TRUE)),
    iconWidth = 26, iconHeight = 26, iconAnchorX = 13, iconAnchorY = 13)
  for (layer in names(key_layers)) {
    k <- key_layers[[layer]]
    p <- places[places$group %in% k$groups, ]
    map <- if (is.null(k$icon)) {
      addMarkers(map, data = p, lng = ~lng, lat = ~lat, icon = star, group = layer,
                 label = ~paste0(name, " (", group, ")"))
    } else {
      addAwesomeMarkers(map, data = p, lng = ~lng, lat = ~lat, group = layer,
                        label = ~paste0(name, " (", group, ")"),
                        icon = awesomeIcons(icon = k$icon, library = "fa", iconColor = "white",
                                            markerColor = k$colour))
    }
  }
  map
}

#' The map's key: what each symbol means (shown in the top-right corner)
#' @noRd
map_key <- function() {
  row <- function(symbol, text) div(class = "map-key-row", span(class = "map-key-symbol", symbol), text)
  pin <- function(icon_name, hex) span(class = "map-key-pin", style = paste0("background:", hex),
                                       icon(icon_name))
  dot <- function(fill, line, size = 10) span(class = "map-key-dot", style = sprintf(
    "width:%dpx;height:%dpx;background:%s;border:1.5px solid %s", size, size, fill, line))
  ring <- function(line) span(class = "map-key-dot", style = sprintf(
    "width:14px;height:14px;border:2.5px solid %s", line))
  layers <- key_layers[!vapply(key_layers, function(k) is.null(k$icon), TRUE)]
  tags$details(
    class = "map-key", open = NA,
    tags$summary("Map key"),
    row(span(dot("#9ca3af", "#333", 8), dot("#9ca3af", "#333", 14)),
        "A building with rentals; bigger = more rentals"),
    row(ring("#000"), "Selected"),
    row(span(ring(area_palette[1]), ring(area_palette[2])), "Saved areas, each in its own colour"),
    row(pin("location-dot", "#d252b9"), "My places"),
    row(HTML(star_svg), "Universities and polytechnics"),
    lapply(names(layers), function(n) row(pin(layers[[n]]$icon, layers[[n]]$hex), n)),
    row(dot("#fff", "#222"), "MRT/LRT station"),
    row(dot("#cab2d6", "#6a3d9a"), "School"),
    div(class = "map-key-note", "Only buildings and your places show at first; turn the ",
        "others on with the layers button (bottom left).")
  )
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
                     overlayGroups = c("Buildings", "My places", names(key_layers),
                                       "Heatmap", "MRT/LRT stations",
                                       if (!is.null(schools_data())) "Schools"),
                     options = layersControlOptions(collapsed = TRUE),
                     position = "bottomleft") |>
    hideGroup(c(names(key_layers), "Heatmap", "MRT/LRT stations", "Schools")) |>
    addControl(map_key(), position = "topright", className = "map-key-control") |>
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

# ---- Saved areas (compared under the map) ---------------------------------

# One colour per saved area, used for its outline, list dot and charts
area_palette <- c("#1e3a8a", "#f59e0b", "#db2777", "#059669")
max_areas <- length(area_palette)

#' No saved areas yet: a list of areas, each list(id, name, colour, locs)
#' where `locs` are map location ids
#' @noRd
new_saved_areas <- function() reactiveVal(list())

#' A name for a selection of buildings
#'
#' One building: its project name. Several: the station nearest their centre,
#' e.g. "Around Clementi MRT · 59 buildings".
#' @param d Contracts in the selection (with loc_id, lat, lng, project, district).
#' @noRd
name_area <- function(d, mrt = mrt_stations()) {
  d <- d[!is.na(d$loc_id), ]
  n <- length(unique(d$loc_id))
  if (n == 0) return("Empty area")
  if (n == 1) return(paste(head(unique(d$project), 2), collapse = " / "))
  where <- if (is.null(mrt)) names(which.max(table(d$district)))
           else nearest_mrt(mean(d$lat), mean(d$lng), mrt)$mrt_station
  paste0("Around ", where, " · ", n, " buildings")
}

#' Add an area to the saved list (NULL when the list is full), giving it the
#' first colour not in use
#' @noRd
add_area <- function(areas, locs, name) {
  if (length(areas) >= max_areas) return(NULL)
  used <- vapply(areas, `[[`, "", "colour")
  ids <- vapply(areas, `[[`, 0, "id")
  c(areas, list(list(id = if (length(ids)) max(ids) + 1 else 1, name = name,
                     colour = setdiff(area_palette, used)[1], locs = as.integer(locs))))
}

#' Saved areas as JSON for the browser and "Save search" links. Buildings are
#' stored by their coordinates, which survive a rebuild of the data (the map
#' location ids don't)
#' @noRd
areas_to_json <- function(areas, locs = map_locations()) {
  keys <- paste(locs$lat, locs$lng)
  jsonlite::toJSON(lapply(areas, function(a) list(
    name = a$name, keys = I(keys[match(a$locs, locs$loc_id)]))),
    auto_unbox = TRUE)
}

#' Saved areas back from areas_to_json(); areas whose buildings are no longer
#' in the data are dropped, and anything unreadable gives no areas
#' @noRd
areas_from_json <- function(json, locs = map_locations()) {
  x <- tryCatch(jsonlite::fromJSON(json, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(x)) return(list())
  keys <- paste(locs$lat, locs$lng)
  areas <- list()
  for (a in head(x, max_areas)) {
    ids <- locs$loc_id[match(unlist(a$keys), keys)]
    ids <- ids[!is.na(ids)]
    if (length(ids) == 0 || !is.character(a$name)) next
    areas <- add_area(areas, ids, substr(a$name, 1, 80))
  }
  areas
}
