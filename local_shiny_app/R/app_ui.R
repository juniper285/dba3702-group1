#' The app's page: one page per task; the renter's settings and quick
#' filters sit on the Search page
#'
#' A function of `request` so that "Save this search" links can restore it.
#' @noRd
app_ui <- function(request) {
  page_navbar(
    id = "nav",
    title = span(class = "brand", icon("magnifying-glass-location"), "RentLens"),
    window_title = "RentLens",
    theme = app_theme(),
    fillable = FALSE,   # let each page scroll instead of squeezing content to fit
    navbar_options = navbar_options(theme = "light"),
    header = settings_dependency(),   # loads and saves My filters in the browser

    # Every page that follows the renter's filters starts with the same
    # "My filters" box (mod_settings_summary_ui() on Search, filter_note_ui()
    # elsewhere)
    nav_panel(
      "Search", value = "budget", icon = icon("wallet"),
      page_intro("What does your budget get you?",
                 "Results follow your filters. Change them any time with 'Edit my filters'; ",
                 "the Map, Market, Shortlist and Rental records pages use them too."),
      mod_settings_summary_ui("settings"),
      mod_filters_ui("filters"),
      mod_budget_check_ui("budget_check"),
      mod_within_budget_ui("within_budget")
    ),
    nav_panel("Map", value = "map", icon = icon("map-location-dot"),
              filter_note_ui("map"), mod_map_ui("map")),
    nav_panel("Check a listing", value = "listing", icon = icon("magnifying-glass-dollar"),
              mod_listing_check_ui("listing_check")),
    nav_panel(
      "Market", value = "market", icon = icon("chart-line"),
      page_intro("How is the market moving?",
                 "How areas and properties compare, for your filters."),
      filter_note_ui("market"),
      navset_underline(
        id = "market_tab",
        nav_panel("Compare areas", value = "compare", mod_compare_ui("compare")),
        nav_panel("Area A vs B", value = "area_compare", mod_area_compare_ui("area_compare")),
        nav_panel("Compare properties", value = "properties", mod_properties_ui("properties")),
        nav_panel("Value for money", value = "value", mod_value_ui("value"))
      )
    ),
    nav_panel(
      title = span("Shortlist", textOutput("shortlist_n", inline = TRUE) |>
                     tagAppendAttributes(class = "nav-badge")),
      value = "shortlist", icon = icon("star"),
      filter_note_ui("shortlist"),
      mod_shortlist_ui("shortlist")
    ),
    nav_spacer(),
    mod_settings_nav_ui("settings"),
    nav_menu(
      "More", icon = icon("ellipsis"), align = "right",
      nav_panel("Rental records", value = "records", icon = icon("table"),
                filter_note_ui("records"), mod_transactions_ui("transactions")),
      nav_panel("About the data", value = "about", icon = icon("circle-info"),
                mod_about_ui("about"))
    )
  )
}
