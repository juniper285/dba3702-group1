#' Filters: the renter's settings plus quick search filters (Search page)
#'
#' Most filters come from "My filters" (mod_settings_server()); the Search
#' page adds quick District and Project filters. The districts are part of
#' the settings, so changing them here updates (and saves) the settings.
#'
#' @param settings The list returned by mod_settings_server().
#' @param map_sel The shared map selection (reactiveValues with `ids`,
#'   `label` and `apply`), or NULL.
#' @return The server returns a list of reactives the tabs work from:
#'   - `base`: all filters except the project choice and the map selection
#'   - `market`: `base`, narrowed to the map selection when that is applied
#'   - `selection`: the market narrowed to the chosen projects (errors if empty)
#'   - `comparables`: the selection, most recent months only
#'   - `projects`, `districts`, `budget`, `recent`, `period`, `mrt_pick`,
#'     `mrt_dist`: the settings and quick filters
#'   - `places`: the places that matter (name, lat, lng), for distance columns
#'   - `summary`: the filters as one line of text
#' @noRd
mod_filters_ui <- function(id) {
  ns <- NS(id)
  div(class = "quick-filters",
      selectizeInput(ns("district"), "Districts", multiple = TRUE,
                     choices = sort(unique(rentals_data()$district)),
                     options = list(placeholder = "Anywhere in Singapore",
                                    plugins = list("remove_button"))),
      selectizeInput(ns("project"), "Projects (optional)", multiple = TRUE,
                     choices = NULL,
                     options = list(placeholder = "Type to search projects",
                                    plugins = list("remove_button"))),
      div(class = "mb-3",
          # Not namespaced: Shiny handles the default bookmark button itself
          bookmarkButton("Save search", class = "btn-outline-secondary",
                         title = "Get a link that restores your settings, filters and shortlist")))
}

#' @noRd
mod_filters_server <- function(id, settings, map_sel = NULL) {
  moduleServer(id, function(input, output, session) {
    rentals <- rentals_data()
    s <- settings$settings
    chosen <- function(x) if (is.null(x)) character() else x

    # The quick District filter and the settings' districts stay in step
    observeEvent(s()$districts, {
      if (!setequal(s()$districts, chosen(input$district))) {
        updateSelectizeInput(session, "district", selected = s()$districts)
      }
    }, ignoreNULL = FALSE)
    observeEvent(input$district, {
      d <- chosen(input$district)
      if (!setequal(d, s()$districts)) settings$save(modifyList(s(), list(districts = d)))
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Project list follows the chosen districts
    observeEvent(s()$districts, {
      d <- if (length(s()$districts)) rentals[rentals$district %in% s()$districts, ] else rentals
      updateSelectizeInput(session, "project", server = TRUE,
                           choices  = sort(unique(d$project)),
                           selected = intersect(input$project, d$project))
    }, ignoreNULL = FALSE)

    mrt_pick <- reactive(if (nzchar(s()$mrt_pick)) s()$mrt_pick else NULL)

    base <- reactive({
      x <- s()
      near <- if (!is.null(mrt_pick())) station_location(mrt_pick())
      filter_market(rentals, x$districts, x$beds, x$types, x$area, x$period,
                    near, x$mrt_dist, x$places, x$place_km)
    })

    # Is a map selection narrowing every page?
    sel_applied <- reactive(!is.null(map_sel) && !is.null(map_sel$ids) && isTRUE(map_sel$apply))

    market <- reactive({
      d <- base()
      if (sel_applied()) d <- d[which(d$loc_id %in% map_sel$ids), ]
      d
    })

    selection <- reactive({
      d <- market()
      if (length(input$project)) d <- d[d$project %in% input$project, ]
      validate(need(nrow(d) > 0,
        "No rentals match your settings. Try widening them with 'Edit my filters', or clear the map selection."))
      d
    })

    recent <- reactive(s()$recent)

    # The filters in words, one phrase per chip, for the other pages
    summary <- reactive(c(
      settings_summary(s()),
      if (length(input$project)) paste(length(input$project), "project(s)"),
      if (sel_applied()) paste("Map selection:", map_sel$label)
    ))

    list(
      base        = base,
      market      = market,
      selection   = selection,
      comparables = reactive(latest_months(selection(), recent())),
      projects    = reactive(input$project),
      districts   = reactive(s()$districts),
      budget      = reactive(s()$budget),
      recent      = recent,
      summary     = summary,
      period      = reactive(s()$period),
      places      = reactive(s()$places),
      mrt_pick    = mrt_pick,
      mrt_dist    = reactive(s()$mrt_dist)
    )
  })
}
