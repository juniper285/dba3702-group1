#' The app's page: one page per task; the filters sit on the Budget page
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

    # The filters live on the Budget page; Map, Market, Shortlist and Rental
    # records follow them too and say so with filter_note_ui()
    nav_panel(
      "Budget", value = "budget", icon = icon("wallet"),
      layout_sidebar(
        fillable = FALSE, class = "budget-layout",
        sidebar = sidebar(width = 300, mod_filters_ui("filters")),
        page_intro("What does your budget get you?",
                   "Set your budget and filters on the left. The Map, Market, Shortlist ",
                   "and Rental records pages use the same filters."),
        mod_budget_check_ui("budget_check"),
        mod_within_budget_ui("within_budget")
      )
    ),
    nav_panel("Map", value = "map", icon = icon("map-location-dot"),
              filter_note_ui("map"), mod_map_ui("map")),
    nav_panel("Check a listing", value = "listing", icon = icon("magnifying-glass-dollar"),
              mod_listing_check_ui("listing_check")),
    nav_panel(
      "Market", value = "market", icon = icon("chart-line"),
      page_intro("How is the market moving?",
                 "Rent trends over time and how areas compare, for your filters."),
      filter_note_ui("market"),
      navset_underline(
        id = "market_tab",
        nav_panel("Price trends", value = "trends", mod_trends_ui("trends")),
        nav_panel("Compare areas", value = "compare", mod_compare_ui("compare"))
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
    nav_menu(
      "More", icon = icon("ellipsis"), align = "right",
      nav_panel("Rental records", value = "records", icon = icon("table"),
                filter_note_ui("records"), mod_transactions_ui("transactions")),
      nav_panel("About the data", value = "about", icon = icon("circle-info"),
                mod_about_ui("about"))
    )
  )
}
