#' "Value for money" (Market page): where do I get the most space for my money?
#'
#' @param filters The list returned by mod_filters_server().
#' @noRd
mod_value_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      class = "mt-3",
      card_header("Median monthly rent vs median floor area"),
      plotly::plotlyOutput(ns("plot"), height = 560),
      card_footer(
        class = "d-flex flex-wrap gap-3 justify-content-between align-items-start",
        div("One dot per project (recent rentals matching your filters). Further right ",
            "and lower = more space for the money. Green = median rent within your budget. ",
            "Starts zoomed in on 98% of projects; double-click to see them all."),
        min_rentals_input(ns("min_n"), "Leave out projects with fewer recent rentals than"))
    ),
    card(card_header("Best value: lowest median rent per sqft"),
         DTOutput(ns("table"), fill = FALSE))
  )
}

#' @noRd
mod_value_server <- function(id, filters) {
  moduleServer(id, function(input, output, session) {
    setBookmarkExclude(dt_bookmark_inputs("table"))

    value_data <- reactive({
      d <- latest_months(filters$market(), filters$recent())
      s <- d |>
        group_by(project, district) |>
        summarise(median = median(rent), sqft = median(area_sqft),
                  psf = median(psf, na.rm = TRUE), n = n(), .groups = "drop") |>
        filter(n >= input$min_n) |>
        mutate(fits = fits_budget(median, filters$budget()))
      validate(need(nrow(s) > 0, "No projects match these filters."))
      s
    })

    output$plot <- plotly::renderPlotly({
      s <- value_data()
      fit_colours <- c("Within range" = rl_colours[["typical"]], "Below range" = "#3b82f6",
                       "Above range" = rl_colours[["muted"]])
      plotly::plot_ly(s, x = ~sqft, y = ~median, color = ~fits, colors = fit_colours,
                      type = "scatter", mode = "markers",
                      marker = list(opacity = 0.6, size = dot_sizes(s$n, 6, 30)),
                      text = ~sprintf("<b>%s</b><br>%s<br>Median rent %s<br>Median size %s sqft<br>S$%.2f psf<br>%s rentals",
                                      project, district, money(median),
                                      format(round(sqft), big.mark = ","), psf, n),
                      hoverinfo = "text") |>
        rl_plotly("Median floor area (sqft, estimated)", "Median monthly rent", money_y = TRUE) |>
        plotly::layout(
          xaxis = list(range = c(0, unname(quantile(s$sqft, 0.98)) * 1.05)),
          yaxis = list(range = c(0, unname(quantile(s$median, 0.98)) * 1.05)))
    })

    output$table <- renderDT({
      s <- value_data() |>
        arrange(psf) |>
        mutate(sqft = round(sqft)) |>
        add_distance(filters$places())
      names(s)[names(s) == "sqft"] <- "Typical size (sqft)"
      s <- prettify(s)
      rl_datatable(s, page_length = 15, selection = "none") |> format_money_cols(s)
    })
  })
}
