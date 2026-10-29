#' The app's server: wires the modules together
#' @noRd
app_server <- function(input, output, session) {
  # The building or area selected on the map; with "Apply map selection to
  # all pages" on, the filters narrow every page to it
  map_sel <- new_map_selection()
  # Map selections saved as Area A and Area B, compared on Market > Area A vs B
  areas <- new_saved_areas()

  # The renter's settings (budget, places, type of home, commute), saved in
  # the browser
  settings <- mod_settings_server("settings")

  # Filtered data sets every tab works from
  filters <- mod_filters_server("filters", settings, map_sel)

  # The shortlist is shared: "Search", "Map" and "Check a listing" add to it,
  # "Shortlist" shows and edits it
  shortlist <- reactiveVal(empty_shortlist())

  mod_budget_check_server("budget_check", filters)
  mod_listing_check_server("listing_check", filters, shortlist)
  mod_within_budget_server("within_budget", filters, shortlist)
  map <- mod_map_server("map", filters, shortlist, map_sel, areas)
  mod_area_compare_server("area_compare", map$map_data, areas)
  mod_trends_server("trends", filters)
  mod_compare_server("compare", filters)
  mod_properties_server("properties", filters, shortlist)
  mod_value_server("value", filters)
  mod_shortlist_server("shortlist", filters, shortlist)
  mod_transactions_server("transactions", filters)

  # Number of shortlisted options, shown next to "Shortlist" in the navbar
  output$shortlist_n <- renderText(if (nrow(shortlist()) > 0) nrow(shortlist()) else "")

  # "Filters in use" line on the pages that follow the renter's settings
  for (page in c("map", "market", "shortlist", "records")) {
    output[[paste0("filter_note_", page)]] <- renderText(filters$summary())
  }
  onBookmarked(function(url) showBookmarkUrlModal(url))
}
