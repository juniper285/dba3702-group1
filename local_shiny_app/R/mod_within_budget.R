#' "Within budget" tab: what can I get within my budget?
#'
#' @param filters The list returned by mod_filters_server().
#' @param shortlist The shared shortlist reactiveVal.
#' @noRd
mod_within_budget_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("Project and bedroom options", textOutput(ns("options")), theme = "success"),
      value_box("Projects", textOutput(ns("projects")), theme = "secondary"),
      value_box("Districts", textOutput(ns("districts")), theme = "secondary"),
      value_box("Bedroom types", textOutput(ns("beds_n")), textOutput(ns("beds")), theme = "secondary")
    ),
    card(
      card_header("Where your budget goes: options by district and bedrooms"),
      plotOutput(ns("plot"), height = 420)
    ),
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Projects with a median rent inside your budget range",
        actionButton(ns("add_short"), "Add selected to shortlist", class = "btn-sm btn-success")
      ),
      layout_columns(
        col_widths = c(4, 8),
        numericInput(ns("min_n"), "Minimum recent rentals per row", value = 3, min = 1),
        uiOutput(ns("distance_note"))
      ),
      DTOutput(ns("table"), fill = FALSE),
      card_footer("Click rows to select them, then add them to your shortlist. ",
                  "Click a column header to sort; use Search to filter.")
    )
  )
}

#' @noRd
mod_within_budget_server <- function(id, filters, shortlist) {
  moduleServer(id, function(input, output, session) {
    setBookmarkExclude(c(dt_bookmark_inputs("table"), "add_short"))

    # One row per project + bedroom type whose recent median rent is in range
    within_data <- reactive({
      b <- filters$budget()
      filters$comparables() |>
        summarise_options() |>
        filter(median >= b[1], median <= b[2], n >= input$min_n) |>
        arrange(median) |>
        add_distance(filters$destination())
    })

    output$options   <- renderText(format(nrow(within_data()), big.mark = ","))
    output$projects  <- renderText(format(n_distinct(within_data()$project), big.mark = ","))
    output$districts <- renderText(n_distinct(within_data()$district))
    output$beds_n    <- renderText(n_distinct(within_data()$bed_group))
    output$beds      <- renderText({
      beds <- intersect(bed_levels, unique(within_data()$bed_group))
      if (length(beds) == 0) "None" else paste("Bedrooms:", paste(beds, collapse = ", "))
    })

    output$distance_note <- renderUI({
      mrt <- if ("mrt_km" %in% names(project_coords())) " and the nearest MRT station" else ""
      p(class = "text-muted mt-4",
        paste0("Distances are to ", filters$destination(), mrt, "."))
    })

    output$plot <- renderPlot({
      s <- within_data()
      validate(need(nrow(s) > 0,
        "Nothing matches your budget range. Try widening the budget or the filters."))
      s$bed_group <- factor(s$bed_group, levels = rev(bed_levels))
      order <- names(sort(table(s$district)))
      s$district <- factor(s$district, levels = order)

      ggplot(s, aes(y = district, fill = bed_group)) +
        geom_bar() +
        scale_fill_manual(values = c("1" = "#18bc9c", "2" = "#3498db", "3" = "#2c3e50",
                                     "4" = "#f39c12", "5+" = "#e74c3c", "Unknown" = "#bdc3c7"),
                          breaks = bed_levels) +
        labs(x = "Number of project and bedroom options", y = NULL, fill = "Bedrooms") +
        theme_minimal(base_size = 13) +
        theme(legend.position = "top")
    })

    output$table <- renderDT({
      s <- within_data() |> mutate(last = format(last, "%b %Y"))
      s <- prettify(s)
      datatable(s, rownames = FALSE, selection = "multiple",
                options = list(pageLength = 15, order = list(), scrollX = TRUE)) |>
        format_money_cols(s)
    })

    observeEvent(input$add_short, {
      add_to_shortlist(shortlist, within_data(), input$table_rows_selected)
    })
  })
}
