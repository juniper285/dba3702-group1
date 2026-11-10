#' Options within budget (bottom of the "Budget" page): what can I get?
#'
#' @param filters The list returned by mod_filters_server().
#' @param shortlist The shared shortlist reactiveVal.
#' @noRd
mod_within_budget_ui <- function(id) {
  ns <- NS(id)
  navset_card_underline(
    title = div(class = "card-title-text", "Options within your budget",
                div(class = "small text-muted fw-normal", textOutput(ns("summary"), inline = TRUE))),
    id = ns("tab"),
    nav_panel(
      "List", value = "list", icon = icon("list"),
      DTOutput(ns("table"), fill = FALSE),
      div(class = "d-flex flex-wrap gap-3 justify-content-between align-items-start mt-2",
          div(class = "text-muted small",
              "Each row is a project and bedroom type whose median recent rent is inside your ",
              "budget. Press the star to add one to your shortlist."),
          min_rentals_input(ns("min_n")))
    ),
    nav_panel(
      "By district", value = "district", icon = icon("chart-bar"),
      plotly::plotlyOutput(ns("plot"), height = 620)
    ),
  )
}

#' @noRd
mod_within_budget_server <- function(id, filters, shortlist) {
  moduleServer(id, function(input, output, session) {
    setBookmarkExclude(c(dt_bookmark_inputs("table"), "star"))

    # One row per project + bedroom type whose recent median rent is in range
    within_data <- reactive({
      b <- filters$budget()
      filters$comparables() |>
        summarise_options() |>
        filter(median >= b[1], median <= b[2], n >= input$min_n) |>
        arrange(median) |>
        add_distance(filters$places())
    })

    output$summary <- renderText({
      s <- within_data()
      beds <- intersect(bed_levels, unique(s$bed_group))
      paste0(format(nrow(s), big.mark = ","), " options in ",
             format(n_distinct(s$project), big.mark = ","), " projects across ",
             n_distinct(s$district), " districts",
             if (length(beds)) paste0(" · bedrooms: ", paste(beds, collapse = ", ")) else "")
    })

    output$plot <- plotly::renderPlotly({
      s <- within_data()
      validate(need(nrow(s) > 0,
        "Nothing matches your budget range. Try widening the budget or the filters."))
      counts <- s |> group_by(district, bed_group) |> summarise(n = n(), .groups = "drop")
      order <- names(sort(tapply(counts$n, counts$district, sum)))
      counts$district <- factor(counts$district, levels = order)
      counts$bed_group <- factor(counts$bed_group, levels = bed_levels)

      plotly::plot_ly(counts, y = ~district, x = ~n, color = ~bed_group, colors = bed_colours,
                      type = "bar", orientation = "h",
                      hovertemplate = "%{y}<br>%{x} options<extra>%{fullData.name} BR</extra>") |>
        rl_plotly("Number of project and bedroom options", NULL) |>
        plotly::layout(barmode = "stack", legend = list(title = list(text = "Bedrooms")))
    })

    output$table <- renderDT({
      s <- within_data() |> mutate(last = format(last, "%b %Y"))
      s <- prettify(s)
      rl_datatable(s, page_length = 15, selection = "none", star = session$ns("star")) |>
        format_money_cols(s)
    })

    # The star at the left of each row
    observeEvent(input$star, toggle_shortlist(shortlist, input$star$key))
  })
}
