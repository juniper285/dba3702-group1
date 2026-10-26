#' "Budget check" tab: is my budget realistic?
#'
#' @param filters The list returned by mod_filters_server().
#' @noRd
mod_budget_check_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("Median rent", textOutput(ns("median")),
                "of comparable recent rentals", theme = "primary"),
      value_box("Typical range", textOutput(ns("range")),
                "middle 50% of rents", theme = "secondary"),
      value_box("Comparable rentals", textOutput(ns("n")),
                textOutput(ns("n_note"), inline = TRUE), theme = "secondary"),
      uiOutput(ns("verdict"))
    ),
    card(
      card_header("How your budget compares with recent comparable rents"),
      plotOutput(ns("plot"), height = 380),
      card_footer("Green band = typical range (middle 50%). Red band = your budget range.")
    )
  )
}

#' @noRd
mod_budget_check_server <- function(id, filters) {
  moduleServer(id, function(input, output, session) {
    comparables <- filters$comparables
    budget <- filters$budget

    output$median <- renderText(money(median(comparables()$rent)))

    output$range <- renderText({
      q <- quantile(comparables()$rent, c(0.25, 0.75))
      paste0(money(q[1]), " - ", formatC(round(q[2]), format = "d", big.mark = ","))
    })

    output$n <- renderText(format(nrow(comparables()), big.mark = ","))

    output$n_note <- renderText({
      if (nrow(comparables()) < 10) "Few rentals: treat as a rough guide"
      else paste("in the latest", filters$recent(), "months")
    })

    output$verdict <- renderUI({
      b <- budget()
      v <- budget_verdict(comparables()$rent, b)
      value_box("Your budget range", v$verdict,
                paste0(v$share, "% of comparable rentals fall between ",
                       money(b[1]), " and ", money(b[2]), "."),
                v$advice, theme = v$colour)
    })

    output$plot <- renderPlot({
      r <- comparables()$rent
      b <- budget()
      r <- r[r <= quantile(r, 0.99)]          # hide extreme top 1% for readability
      q <- quantile(r, c(0.25, 0.75))

      ggplot(data.frame(rent = r), aes(rent)) +
        annotate("rect", xmin = q[1], xmax = q[2], ymin = -Inf, ymax = Inf,
                 fill = "#18bc9c", alpha = 0.15) +
        annotate("rect", xmin = b[1], xmax = b[2], ymin = -Inf, ymax = Inf,
                 fill = "#e74c3c", alpha = 0.10) +
        geom_histogram(bins = 40, fill = "#2c3e50", colour = "white") +
        geom_vline(xintercept = b, linetype = "dashed", linewidth = 0.8, colour = "#e74c3c") +
        scale_x_continuous(labels = scales::label_dollar(prefix = "S$")) +
        labs(x = "Monthly rent", y = "Number of rentals") +
        theme_minimal(base_size = 14)
    })
  })
}
