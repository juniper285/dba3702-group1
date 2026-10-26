#' The app's server: wires the modules together
#' @noRd
app_server <- function(input, output, session) {
  # Filtered data sets every tab works from
  filters <- mod_filters_server("filters")

  # The shortlist is shared: "Within budget", "Map" and "Check a listing" add to it,
  # "Shortlist" shows and edits it
  shortlist <- reactiveVal(empty_shortlist())

  mod_budget_check_server("budget_check", filters)
  mod_listing_check_server("listing_check", filters, shortlist)
  mod_within_budget_server("within_budget", filters, shortlist)
  mod_map_server("map", filters, shortlist)
  mod_trends_server("trends", filters)
  mod_compare_server("compare", filters)
  mod_shortlist_server("shortlist", filters, shortlist)
  mod_transactions_server("transactions", filters)

  onBookmarked(function(url) showBookmarkUrlModal(url))
}
