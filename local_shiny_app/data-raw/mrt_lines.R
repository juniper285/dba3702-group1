# ==================================================================
#  Build inst/extdata/mrt_lines.rds: the track of every MRT/LRT line,
#  one row per line, in its official colour.
#
#  Source: OpenStreetMap (route relations for each line, via the public
#  Overpass API). Lines under construction (Jurong Region Line, Cross
#  Island Line), the Sentosa Express and the Changi Skytrain are left out.
#  Run from the package root (local_shiny_app/):
#    source("data-raw/mrt_lines.R")
# ==================================================================

lines <- data.frame(
  ref    = c("NSL", "EWL", "NEL", "CCL", "DTL", "TEL", "BPLRT", "SKLRT", "PGLRT"),
  line   = c("North-South Line", "East-West Line", "North East Line", "Circle Line",
             "Downtown Line", "Thomson-East Coast Line", "Bukit Panjang LRT",
             "Sengkang LRT", "Punggol LRT"),
  colour = c("#d42e12", "#009645", "#9900aa", "#fa9e0d", "#005ec4", "#9d5b25",
             "#748477", "#748477", "#748477")
)

overpass <- function(query) {
  res <- httr2::request("https://overpass-api.de/api/interpreter") |>
    httr2::req_user_agent("RentLens (student project)") |>
    httr2::req_body_form(data = query) |>
    httr2::req_retry(max_tries = 5, backoff = function(i) 15 * i,
                     is_transient = function(r) httr2::resp_status(r) %in% c(429, 502, 503, 504)) |>
    httr2::req_perform()
  jsonlite::fromJSON(httr2::resp_body_string(res), simplifyVector = FALSE)$elements
}

# The track ways of one line (not its stops or platforms), as one simplified
# multi-line in longitude/latitude
line_track <- function(ref) {
  ways <- overpass(sprintf(paste0(
    '[out:json][timeout:120];',
    'rel(1.15,103.6,1.48,104.1)["type"="route"]["ref"="%s"];',
    'way(r)["railway"];out geom;'), ref))
  ways <- ways[!duplicated(vapply(ways, `[[`, 0, "id"))]
  geoms <- lapply(ways, function(w)
    sf::st_linestring(do.call(rbind, lapply(w$geometry, function(p) c(p$lon, p$lat)))))
  sf::st_sfc(geoms, crs = 4326) |>
    sf::st_transform(3414) |>          # SVY21, in metres
    sf::st_union() |>
    sf::st_line_merge() |>
    sf::st_simplify(dTolerance = 10) |>
    sf::st_transform(4326)
}

tracks <- do.call(c, lapply(lines$ref, function(r) { Sys.sleep(2); line_track(r) }))
mrt_lines <- sf::st_sf(lines, geometry = tracks)

saveRDS(mrt_lines, "inst/extdata/mrt_lines.rds")
