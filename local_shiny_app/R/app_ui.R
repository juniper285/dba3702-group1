#' The app's page: shared sidebar + one tab per module
#'
#' A function of `request` so that "Save this search" links can restore it.
#' @noRd
app_ui <- function(request) {
  page_navbar(
    title = "RentLens",
    theme = app_theme(),
    fillable = FALSE,   # let each page scroll instead of squeezing content to fit

    # Shared sidebar: every tab reacts to these filters
    sidebar = sidebar(width = 320, mod_filters_ui("filters")),

    nav_panel("Budget check",  mod_budget_check_ui("budget_check")),
    nav_panel("Check a listing", mod_listing_check_ui("listing_check")),
    nav_panel("Within budget", mod_within_budget_ui("within_budget")),
    nav_panel("Map",           mod_map_ui("map")),
    nav_panel("Price trends",  mod_trends_ui("trends")),
    nav_panel("Compare areas", mod_compare_ui("compare")),
    nav_panel("Shortlist",     mod_shortlist_ui("shortlist")),
    nav_panel("Transactions",  mod_transactions_ui("transactions")),
    nav_panel("About",         mod_about_ui("about"))
  )
}
