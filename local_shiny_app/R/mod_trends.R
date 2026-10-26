#' "Price trends" tab: how have prices changed?
#'
#' @param filters The list returned by mod_filters_server().
#' @noRd
mod_trends_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header(
      "Median monthly rent by quarter",
      checkboxInput(ns("split_district"), "Show selected districts separately", value = FALSE)
    ),
    plotOutput(ns("plot"), height = 420),
    card_footer(textOutput(ns("summary")))
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

    output$plot <- renderPlot({
      t <- trend_data()
      validate(need(nrow(t) > 0, "Not enough rentals per quarter to draw a trend."))
      one_line <- length(unique(t$group)) == 1
      b <- filters$budget()

      ggplot(t, aes(quarter, median, colour = group, fill = group)) +
        annotate("rect", ymin = b[1], ymax = b[2], xmin = as.Date(-Inf), xmax = as.Date(Inf),
                 fill = "#e74c3c", alpha = 0.08) +
        (if (one_line) geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.15, colour = NA)) +
        geom_line(linewidth = 1.1) +
        geom_point(size = 1.8) +
        (if (one_line) scale_colour_manual(values = "#2c3e50", aesthetics = c("colour", "fill"))) +
        scale_y_continuous(labels = scales::label_dollar(prefix = "S$")) +
        labs(x = NULL, y = "Median monthly rent", colour = NULL, fill = NULL) +
        theme_minimal(base_size = 14) +
        theme(legend.position = if (one_line) "none" else "bottom")
    })

    output$summary <- renderText({
      t <- filters$selection() |> group_by(quarter) |> summarise_rent() |> filter(n >= 5)
      if (nrow(t) < 2) return("Not enough data to measure the change over time.")
      first <- t[1, ]; last <- t[nrow(t), ]
      change <- round(100 * (last$median / first$median - 1))
      paste0("Median rent went from ", money(first$median), " (", quarter_label(first$quarter),
             ") to ", money(last$median), " (", quarter_label(last$quarter), "), a change of ",
             ifelse(change >= 0, "+", ""), change, "%. ",
             "Grey band = typical range. Red band = your budget range.")
    })
  })
}
