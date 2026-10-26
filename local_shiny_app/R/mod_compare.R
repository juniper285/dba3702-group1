#' "Compare areas" tab: which areas fit my budget?
#'
#' @param filters The list returned by mod_filters_server().
#' @noRd
mod_compare_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 4, 4),
      radioButtons(ns("compare_by"), "Compare by", c("District", "Project"), inline = TRUE),
      numericInput(ns("min_n"), "Minimum recent rentals per row", value = 5, min = 1),
      p(class = "text-muted mt-4",
        "Highlighted rows have a median rent within your budget range.")
    ),
    conditionalPanel(
      "input.compare_by == 'District'", ns = ns,
      card(card_header("Median rent by district (lines = typical range)"),
           plotOutput(ns("district_plot"), height = 650))
    ),
    card(card_header("Comparison table"), DTOutput(ns("table"), fill = FALSE))
  )
}

#' @noRd
mod_compare_server <- function(id, filters) {
  moduleServer(id, function(input, output, session) {
    setBookmarkExclude(dt_bookmark_inputs("table"))

    compare_data <- reactive({
      d <- latest_months(filters$market(), filters$recent())
      validate(need(nrow(d) > 0, "No rentals match these filters."))
      groups <- if (input$compare_by == "District") "district" else c("project", "district")
      s <- d |>
        group_by(across(all_of(groups))) |>
        summarise_rent(filters$budget()) |>
        filter(n >= input$min_n) |>
        arrange(median)
      if (input$compare_by == "Project") s <- add_distance(s, filters$destination())
      s
    })

    output$district_plot <- renderPlot({
      s <- compare_data()
      req(input$compare_by == "District", !"project" %in% names(s))
      b <- filters$budget()
      s$fits <- ifelse(s$median >= b[1] & s$median <= b[2],
                       "Median within budget", "Median outside budget")

      ggplot(s, aes(y = reorder(district, -median))) +
        annotate("rect", xmin = b[1], xmax = b[2], ymin = -Inf, ymax = Inf,
                 fill = "#e74c3c", alpha = 0.08) +
        geom_linerange(aes(xmin = p25, xmax = p75), linewidth = 1.2, colour = "grey70") +
        geom_point(aes(x = median, colour = fits), size = 3.5) +
        scale_colour_manual(values = c("Median within budget"  = "#18bc9c",
                                       "Median outside budget" = "#95a5a6")) +
        scale_x_continuous(labels = scales::label_dollar(prefix = "S$")) +
        labs(x = "Monthly rent", y = NULL, colour = NULL) +
        theme_minimal(base_size = 13) +
        theme(legend.position = "top")
    })

    output$table <- renderDT({
      b <- filters$budget()
      s <- prettify(compare_data())
      datatable(s, rownames = FALSE,
                options = list(pageLength = 15, order = list(), scrollX = TRUE)) |>
        format_money_cols(s) |>
        formatStyle("Median rent", target = "row",
                    backgroundColor = styleInterval(c(b[1] - 0.5, b[2] + 0.5),
                                                    c("white", "#e8f8f5", "white")))
    })
  })
}
