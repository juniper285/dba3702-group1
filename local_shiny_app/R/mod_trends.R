#' "Price trends" (Market page): how have prices changed?
#'
#' @param filters The list returned by mod_filters_server().
#' @noRd
mod_trends_ui <- function(id) {
  ns <- NS(id)
  card(
    class = "mt-3",
    header_with_actions(
      "Median monthly rent by quarter",
      input_switch(ns("split_district"), "Show selected districts separately", value = FALSE)
    ),
    uiOutput(ns("summary")),
    plotly::plotlyOutput(ns("plot"), height = 420),
    card_footer("Shaded area = typical range (middle 50%); red band = your budget range. ",
                "Quarters with fewer than 5 rentals are left out. To compare districts, ",
                "pick two or more on the Budget page and switch on 'Show separately'.")
  )
}

#' @noRd
mod_trends_server <- function(id, filters) {
  moduleServer(id, function(input, output, session) {
    trend_data <- reactive({
      d <- filters$selection()
      split_up <- isTRUE(input$split_district) && length(filters$districts()) > 1
      d$group <- if (split_up) d$district else "Your selection"
      d |> group_by(quarter, group) |> summarise_rent() |> filter(n >= 5)
    })

    output$plot <- plotly::renderPlotly({
      t <- trend_data()
      validate(need(nrow(t) > 0, "Not enough rentals per quarter to draw a trend."))
      b <- filters$budget()
      t$label <- quarter_label(t$quarter)
      one_line <- length(unique(t$group)) == 1

      p <- plotly::plot_ly(t, x = ~quarter)
      if (one_line) {
        p <- p |>
          plotly::add_ribbons(ymin = ~p25, ymax = ~p75, name = "Typical range", hoverinfo = "skip",
                              fillcolor = "rgba(20,184,166,0.15)", line = list(width = 0))
      }
      hover <- ~sprintf("<b>%s</b> \u00B7 %s<br>Median %s<br>Typical %s - %s<br>%s rentals",
                        group, label, money(median), money(p25), money(p75),
                        format(n, big.mark = ","))
      p <- if (one_line) {
        plotly::add_trace(p, y = ~median, type = "scatter", mode = "lines+markers", name = "Median",
                          line = list(width = 2.5, color = rl_colours[["ink"]]),
                          marker = list(size = 6, color = rl_colours[["ink"]]),
                          text = hover, hoverinfo = "text")
      } else {
        plotly::add_trace(p, y = ~median, color = ~group, type = "scatter", mode = "lines+markers",
                          line = list(width = 2.5), marker = list(size = 6),
                          text = hover, hoverinfo = "text")
      }
      p |>
        rl_plotly(NULL, "Median monthly rent", money_y = TRUE) |>
        plotly::layout(showlegend = !one_line,
                       shapes = list(band(b[1], b[2], rl_colours[["budget"]], 0.07, horizontal = TRUE)))
    })

    output$summary <- renderUI({
      t <- filters$selection() |> group_by(quarter) |> summarise_rent() |> filter(n >= 5)
      if (nrow(t) < 2) return(p(class = "text-muted", "Not enough data to measure the change over time."))
      first <- t[1, ]; last <- t[nrow(t), ]
      change <- round(100 * (last$median / first$median - 1))
      layout_columns(
        col_widths = c(4, 4, 4),
        kpi(paste("Median,", quarter_label(first$quarter)), money(first$median)),
        kpi(paste("Median,", quarter_label(last$quarter)), money(last$median)),
        kpi("Change", paste0(ifelse(change >= 0, "+", ""), change, "%"),
            if (change >= 0) "rents have risen" else "rents have fallen",
            icon = if (change >= 0) "arrow-trend-up" else "arrow-trend-down")
      )
    })
  })
}
