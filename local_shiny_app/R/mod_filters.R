#' Shared sidebar filters
#'
#' @return The server returns a list of reactives the tabs work from:
#'   - `market`: all filters except the project choice
#'   - `selection`: the market narrowed to the chosen projects (errors if empty)
#'   - `comparables`: the selection, most recent months only
#'   - `projects`, `districts`, `budget`, `recent`, `period`, `destination`:
#'     the sidebar inputs
#' @noRd
mod_filters_ui <- function(id) {
  ns <- NS(id)
  rentals <- rentals_data()
  date_min <- min(rentals$lease_date)
  date_max <- max(rentals$lease_date)

  tagList(
    accordion(
      multiple = TRUE,
      open = c("Budget", "Area and project"),
      accordion_panel(
        "Budget",
        sliderInput(ns("budget"), "Monthly budget range (S$)", min = 1000, max = 15000,
                    value = c(3000, 5000), step = 100, pre = "S$")
      ),
      accordion_panel(
        "Area and project",
        selectizeInput(ns("district"), "Districts", multiple = TRUE,
                       choices = sort(unique(rentals$district)),
                       options = list(placeholder = "All districts")),
        selectizeInput(ns("project"), "Residential projects (optional)", multiple = TRUE,
                       choices = NULL,
                       options = list(placeholder = "Type to search projects"))
      ),
      accordion_panel(
        "Property",
        checkboxGroupInput(ns("beds"), "Bedrooms", choices = bed_levels,
                           selected = bed_levels, inline = TRUE),
        checkboxGroupInput(ns("type"), "Property type",
                           choices = sort(unique(rentals$type)),
                           selected = unique(rentals$type)),
        sliderInput(ns("area"), "Floor area (sqft)", min = 0, max = area_max,
                    value = c(0, area_max), step = 100),
        helpText("Top of the slider = 4,000 sqft and above.")
      ),
      accordion_panel(
        "Time period",
        dateRangeInput(ns("period"), "Lease start between", start = date_min, end = date_max,
                       min = date_min, max = date_max, format = "M yyyy"),
        selectInput(ns("recent"), "Benchmarks use the most recent",
                    choices = c("6 months" = 6, "12 months" = 12, "24 months" = 24),
                    selected = 12)
      ),
      accordion_panel(
        "Location",
        selectInput(ns("destination"), "Your university or workplace", names(destinations)),
        helpText("Distances are straight-line estimates from each project.")
      )
    ),
    # Not namespaced: Shiny handles the default bookmark button itself
    bookmarkButton("Save this search", title = "Get a link that restores your filters and shortlist")
  )
}

#' @noRd
mod_filters_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    rentals <- rentals_data()

    # Project list follows the chosen districts
    observeEvent(input$district, {
      d <- if (length(input$district)) rentals[rentals$district %in% input$district, ] else rentals
      updateSelectizeInput(session, "project", server = TRUE,
                           choices  = sort(unique(d$project)),
                           selected = intersect(input$project, d$project))
    }, ignoreNULL = FALSE)

    market <- reactive({
      filter_market(rentals, input$district, input$beds, input$type,
                    input$area, input$period)
    })

    selection <- reactive({
      d <- market()
      if (length(input$project)) d <- d[d$project %in% input$project, ]
      validate(need(nrow(d) > 0,
        "No rentals match these filters. Try widening the filters in the sidebar."))
      d
    })

    recent <- reactive(as.numeric(input$recent))

    list(
      market      = market,
      selection   = selection,
      comparables = reactive(latest_months(selection(), recent())),
      projects    = reactive(input$project),
      districts   = reactive(input$district),
      budget      = reactive(input$budget),
      recent      = recent,
      period      = reactive(input$period),
      destination = reactive(input$destination)
    )
  })
}
