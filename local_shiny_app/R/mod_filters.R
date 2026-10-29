#' Filters (sidebar of the Budget page, used by most pages)
#'
#' @param map_sel The shared map selection (reactiveValues with `ids`,
#'   `label` and `apply`), or NULL.
#' @return The server returns a list of reactives the tabs work from:
#'   - `base`: all filters except the project choice and the map selection
#'   - `market`: `base`, narrowed to the map selection when that is applied
#'   - `selection`: the market narrowed to the chosen projects (errors if empty)
#'   - `comparables`: the selection, most recent months only
#'   - `projects`, `districts`, `budget`, `recent`, `period`, `destination`,
#'     `mrt_pick`, `mrt_dist`: the sidebar inputs
#'   - `summary`: the filters as one line of text
#' @noRd
mod_filters_ui <- function(id) {
  ns <- NS(id)
  rentals <- rentals_data()
  date_min <- min(rentals$lease_date)
  date_max <- max(rentals$lease_date)

  tagList(
    div(class = "filter-section",
        div(class = "section-title", icon("wallet"), " Monthly budget"),
        sliderInput(ns("budget"), NULL, min = 1000, max = 15000,
                    value = filter_defaults$budget, step = 100, pre = "S$")),
    div(class = "filter-section",
        div(class = "section-title", icon("location-dot"), " Where"),
        selectizeInput(ns("district"), "Districts", multiple = TRUE,
                       choices = sort(unique(rentals$district)),
                       options = list(placeholder = "All districts", plugins = list("remove_button"))),
        selectizeInput(ns("project"), "Projects (optional)", multiple = TRUE,
                       choices = NULL,
                       options = list(placeholder = "Type to search projects",
                                      plugins = list("remove_button")))),
    div(class = "filter-section chips",
        div(class = "section-title", icon("bed"), " Bedrooms"),
        checkboxGroupInput(ns("beds"), NULL, choices = bed_levels,
                           selected = bed_levels, inline = TRUE)),
    accordion(
      open = FALSE,
      accordion_panel(
        "More filters", icon = icon("sliders"),
        div(class = "chips",
            checkboxGroupInput(ns("type"), "Property type",
                               choices = sort(unique(rentals$type)),
                               selected = unique(rentals$type))),
        sliderInput(ns("area"), "Floor area (sqft)", min = 0, max = area_max,
                    value = c(0, area_max), step = 100),
        helpText("Top of the slider = 4,000 sqft and above."),
        dateRangeInput(ns("period"), "Lease start between", start = date_min, end = date_max,
                       min = date_min, max = date_max, format = "M yyyy"),
        selectInput(ns("recent"), "Benchmarks use the most recent",
                    choices = c("6 months" = 6, "12 months" = 12, "24 months" = 24),
                    selected = filter_defaults$recent)
      ),
      accordion_panel(
        "Commute and MRT", icon = icon("train-subway"),
        selectInput(ns("destination"), "Your university, workplace or key place",
                    destination_choices, selected = filter_defaults$destination),
        selectizeInput(ns("mrt_pick"), "Near a specific MRT/LRT station (optional)",
                       choices = c("", sort(mrt_stations()$station)),
                       options = list(placeholder = "Any station")),
        sliderInput(ns("mrt_dist"), "Within this distance (m)",
                    min = 100, max = mrt_max, value = mrt_max, step = 100),
        helpText("No station chosen: distance to the nearest station, and the top of ",
                 "the slider means any distance. Station chosen: buildings within this ",
                 "distance of it (ring on the map). Straight-line; about 80 m per minute walking.")
      )
    ),
    div(class = "d-flex gap-2 mt-3",
        actionButton(ns("reset"), "Reset", icon = icon("rotate-left"),
                     class = "btn-outline-secondary btn-sm flex-fill"),
        # Not namespaced: Shiny handles the default bookmark button itself
        bookmarkButton("Save search", class = "btn-primary btn-sm flex-fill",
                       title = "Get a link that restores your filters and shortlist"))
  )
}

# Starting values of the filters (also used by the Reset button)
filter_defaults <- list(budget = c(3000, 5000), recent = 12, destination = "NUS (Kent Ridge)")

#' @noRd
mod_filters_server <- function(id, map_sel = NULL) {
  moduleServer(id, function(input, output, session) {
    rentals <- rentals_data()

    # Project list follows the chosen districts
    observeEvent(input$district, {
      d <- if (length(input$district)) rentals[rentals$district %in% input$district, ] else rentals
      updateSelectizeInput(session, "project", server = TRUE,
                           choices  = sort(unique(d$project)),
                           selected = intersect(input$project, d$project))
    }, ignoreNULL = FALSE)

    # Start from a 500 m (about 6 min) walk when a station is picked and the
    # slider is still at "any distance"
    observeEvent(input$mrt_pick, {
      if (isTruthy(input$mrt_pick) && input$mrt_dist >= mrt_max) {
        updateSliderInput(session, "mrt_dist", value = 500)
      }
    })

    setBookmarkExclude("reset")
    observeEvent(input$reset, {
      updateSliderInput(session, "budget", value = filter_defaults$budget)
      updateSelectizeInput(session, "district", selected = character(0))
      updateSelectizeInput(session, "project", selected = character(0))
      updateCheckboxGroupInput(session, "beds", selected = bed_levels)
      updateCheckboxGroupInput(session, "type", selected = unique(rentals$type))
      updateSliderInput(session, "area", value = c(0, area_max))
      updateDateRangeInput(session, "period", start = min(rentals$lease_date),
                           end = max(rentals$lease_date))
      updateSelectInput(session, "recent", selected = filter_defaults$recent)
      updateSelectInput(session, "destination", selected = filter_defaults$destination)
      updateSelectizeInput(session, "mrt_pick", selected = "")
      updateSliderInput(session, "mrt_dist", value = mrt_max)
    })

    mrt_pick <- reactive(if (isTruthy(input$mrt_pick)) input$mrt_pick else NULL)
    mrt_dist <- reactive(if (is.null(input$mrt_dist)) mrt_max else input$mrt_dist)

    base <- reactive({
      near <- if (!is.null(mrt_pick())) station_location(mrt_pick())
      filter_market(rentals, input$district, input$beds, input$type,
                    input$area, input$period, near, mrt_dist())
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
        "No rentals match these filters. Try widening the filters on the Budget page or clearing the map selection."))
      d
    })

    recent <- reactive(as.numeric(input$recent))

    # One-line description of the filters, for the other pages
    summary <- reactive({
      b <- input$budget
      parts <- c(
        paste0(money(b[1]), " - ", money(b[2])),
        if (length(input$district) == 0) "all districts"
        else if (length(input$district) <= 2) paste(sub(" .*", "", input$district), collapse = ", ")
        else paste(length(input$district), "districts"),
        if (length(input$project)) paste(length(input$project), "project(s)"),
        if (sel_applied()) paste("map selection:", map_sel$label),
        if (setequal(input$beds, bed_levels)) "any bedrooms"
        else if (length(input$beds)) paste(paste(input$beds, collapse = ", "), "bedrooms")
        else "no bedrooms selected",
        if (!setequal(input$type, unique(rentals$type))) paste(length(input$type), "property type(s)"),
        if (input$area[1] > 0 || input$area[2] < area_max)
          paste0(format(input$area[1], big.mark = ","), "-", format(input$area[2], big.mark = ","), " sqft"),
        if (!is.null(mrt_pick())) paste0("within ", mrt_dist(), " m of ", mrt_pick())
        else if (mrt_dist() < mrt_max) paste0("within ", mrt_dist(), " m of an MRT/LRT"),
        paste0(format(input$period[1], "%b %Y"), " - ", format(input$period[2], "%b %Y")),
        paste0("benchmarks: latest ", input$recent, " months")
      )
      paste(parts, collapse = " \u00B7 ")
    })

    list(
      base        = base,
      market      = market,
      selection   = selection,
      comparables = reactive(latest_months(selection(), recent())),
      projects    = reactive(input$project),
      districts   = reactive(input$district),
      budget      = reactive(input$budget),
      recent      = recent,
      summary     = summary,
      period      = reactive(input$period),
      destination = reactive(input$destination),
      mrt_pick    = mrt_pick,
      mrt_dist    = mrt_dist
    )
  })
}
