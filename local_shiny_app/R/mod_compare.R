#' "Compare areas" (Market page): which areas fit my budget?
#'
#' @param filters The list returned by mod_filters_server().
#' @noRd
mod_compare_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      class = "mt-3",
      header_with_actions(
        "Median rent by area",
        div(class = "chips mb-0",
            radioButtons(ns("compare_by"), NULL, c("District", "Project"), inline = TRUE)),
        div(class = "d-flex align-items-center gap-2 small text-muted fw-normal",
            "Min. rentals per row",
            numericInput(ns("min_n"), NULL, value = 5, min = 1, width = "80px"))
      ),
      conditionalPanel(
        "input.compare_by == 'District'", ns = ns,
        plotly::plotlyOutput(ns("district_plot"), height = 650)
      ),
      DTOutput(ns("table"), fill = FALSE),
      card_footer("Dot = median rent, line = typical range (middle 50%). Green = median within ",
                  "your budget range. Highlighted rows have a median within your budget.")
    ),
    card(
      card_header("Median rent by district and bedrooms"),
      plotly::plotlyOutput(ns("heat"), height = 650),
      card_footer("Redder = more expensive. Numbers = median monthly rent (S$ thousands); ",
                  "hover for the exact figure and rental count. ",
                  "Blank = fewer rentals than the minimum per row.")
    )
  )
}

#' @noRd
mod_compare_server <- function(id, filters) {
  moduleServer(id, function(input, output, session) {
    setBookmarkExclude(dt_bookmark_inputs("table"))

    recent_market <- reactive({
      d <- latest_months(filters$market(), filters$recent())
      validate(need(nrow(d) > 0, "No rentals match these filters."))
      d
    })

    compare_data <- reactive({
      d <- recent_market()
      groups <- if (input$compare_by == "District") "district" else c("project", "district")
      s <- d |>
        group_by(across(all_of(groups))) |>
        summarise_rent(filters$budget()) |>
        filter(n >= input$min_n) |>
        arrange(median)
      if (input$compare_by == "Project") s <- add_distance(s, filters$destination())
      s
    })

    output$district_plot <- plotly::renderPlotly({
      s <- compare_data()
      req(input$compare_by == "District", !"project" %in% names(s))
      b <- filters$budget()
      s$label <- s$district
      s$colour <- ifelse(s$median >= b[1] & s$median <= b[2],
                         rl_colours[["typical"]], rl_colours[["muted"]])
      range_dot_chart(s, b)
    })

    output$heat <- plotly::renderPlotly({
      s <- recent_market() |>
        filter(bed_group != "Unknown") |>
        group_by(district, bed_group) |>
        summarise(median = median(rent), n = n(), .groups = "drop") |>
        filter(n >= input$min_n)
      validate(need(nrow(s) > 0, "Not enough rentals to fill the grid."))
      order <- names(sort(tapply(s$median, s$district, median)))
      s$district <- factor(s$district, levels = order)
      s$bed_group <- factor(s$bed_group, levels = setdiff(bed_levels, "Unknown"))
      # Cap colours at the 95th percentile so one luxury outlier doesn't wash out the rest
      top <- unname(quantile(s$median, 0.95))
      plotly::plot_ly(s, x = ~bed_group, y = ~district, z = ~pmin(median, top),
                      type = "heatmap", colorscale = "RdBu", reversescale = TRUE, xgap = 2, ygap = 2,
                      text = ~sprintf("%s<br>%s BR<br>Median %s<br>%s rentals", district, bed_group,
                                      money(median), format(n, big.mark = ",")),
                      hoverinfo = "text",
                      colorbar = list(title = list(text = "Median rent"), tickprefix = "S$")) |>
        plotly::add_annotations(x = ~bed_group, y = ~district, text = ~sprintf("%.1f", median / 1000),
                                showarrow = FALSE, font = list(size = 11)) |>
        rl_plotly("Bedrooms", NULL)
    })

    output$table <- renderDT({
      b <- filters$budget()
      s <- prettify(compare_data())
      rl_datatable(s, page_length = 15, selection = "none") |>
        format_money_cols(s) |>
        formatStyle("Median rent", target = "row",
                    backgroundColor = styleInterval(c(b[1] - 0.5, b[2] + 0.5),
                                                    c("transparent", "#ecfdf5", "transparent")))
    })
  })
}
