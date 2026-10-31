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
            radioButtons(ns("compare_by"), NULL, c("District", "Project"), inline = TRUE))
      ),
      conditionalPanel(
        "input.compare_by == 'District'", ns = ns,
        uiOutput(ns("district_plot_box"))
      ),
      DTOutput(ns("table"), fill = FALSE),
      card_footer(
        class = "d-flex flex-wrap gap-3 justify-content-between align-items-start",
        div("Dot = median rent, line = typical range (middle 50%). Green = median within ",
            "your budget range. Highlighted rows have a median within your budget; the bar ",
            "shows the share of rentals within it."),
        min_rentals_input(ns("min_n"), "Leave out areas with fewer recent rentals than"))
    ),
    card(
      card_header("Median rent by district and bedrooms"),
      uiOutput(ns("heat_box")),
      card_footer("Green = cheaper, red = more expensive. Numbers = median monthly rent ",
                  "(S$ thousands); hover for the exact figure and rental count. ",
                  "Blank = too few rentals.")
    ),
    card(
      card_header("Does being near an MRT/LRT station cost more?"),
      plotly::plotlyOutput(ns("mrt_plot"), height = 450),
      card_footer("One dot per project (recent rentals), sized by rental count; hover for ",
                  "details. Red line = overall trend.")
    ),
    card(card_header("Projects by distance to the nearest MRT/LRT station"),
         DTOutput(ns("mrt_table"), fill = FALSE))
  )
}

#' @noRd
mod_compare_server <- function(id, filters) {
  moduleServer(id, function(input, output, session) {
    setBookmarkExclude(dt_bookmark_inputs(c("table", "mrt_table")))

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
      if (input$compare_by == "Project") s <- add_distance(s, filters$places())
      s
    })

    # The chart grows with the number of districts (about 28 px each)
    output$district_plot_box <- renderUI({
      n <- if (input$compare_by == "District") nrow(compare_data()) else 0
      plotly::plotlyOutput(session$ns("district_plot"), height = max(220, 80 + 28 * n))
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

    heat_data <- reactive({
      s <- recent_market() |>
        filter(bed_group != "Unknown") |>
        group_by(district, bed_group) |>
        summarise(median = median(rent), n = n(), .groups = "drop") |>
        filter(n >= input$min_n)
      validate(need(nrow(s) > 0, "Not enough rentals to fill the grid."))
      s
    })

    # Like the district chart, the grid grows with the number of districts
    output$heat_box <- renderUI({
      n <- tryCatch(n_distinct(heat_data()$district), error = function(e) 0)
      plotly::plotlyOutput(session$ns("heat"), height = max(260, 140 + 26 * n))
    })

    output$heat <- plotly::renderPlotly({
      s <- heat_data()
      order <- names(sort(tapply(s$median, s$district, median)))
      beds <- intersect(bed_levels, s$bed_group)
      # Cap colours at the 95th percentile so one luxury outlier doesn't wash out the rest
      top <- unname(quantile(s$median, 0.95))
      plotly::plot_ly(s, x = ~bed_group, y = ~district, z = ~pmin(median, top),
                      type = "heatmap", xgap = 2, ygap = 2,
                      # soft green (cheaper) - yellow - red (dearer)
                      colorscale = list(c(0, "#bbf7d0"), c(0.5, "#fef9c3"), c(1, "#fecaca")),
                      text = ~sprintf("%s<br>%s BR<br>Median %s<br>%s rentals", district, bed_group,
                                      money(median), format(n, big.mark = ",")),
                      hoverinfo = "text",
                      colorbar = list(title = list(text = "Median rent"), tickprefix = "S$")) |>
        # On a category axis plotly reads "1", "2", ... as positions, so place
        # the numbers by position (0 = first column)
        plotly::add_annotations(x = match(s$bed_group, beds) - 1, y = ~district, text = ~sprintf("%.1f", median / 1000),
                                showarrow = FALSE, font = list(size = 11)) |>
        rl_plotly("Bedrooms", NULL) |>
        plotly::layout(xaxis = list(type = "category", categoryorder = "array", categoryarray = beds),
                       yaxis = list(type = "category", categoryorder = "array", categoryarray = order))
    })

    # One row per project with its nearest station (chart and table)
    mrt_projects <- reactive({
      s <- recent_market() |>
        filter(!is.na(mrt_m)) |>
        group_by(project, district) |>
        summarise(mrt_station = first(mrt_station), mrt_m = first(mrt_m),
                  median = median(rent), psf = median(psf, na.rm = TRUE), n = n(),
                  .groups = "drop") |>
        filter(n >= input$min_n, !is.na(psf)) |>
        mutate(walk_min = walk_minutes(mrt_m))
      validate(need(nrow(s) > 5, "Not enough projects to compare."))
      s
    })

    output$mrt_plot <- plotly::renderPlotly({
      s <- mrt_projects()
      fit <- if (nrow(s) >= 30) stats::loess(psf ~ mrt_m, s) else stats::lm(psf ~ mrt_m, s)
      line <- data.frame(mrt_m = seq(min(s$mrt_m), max(s$mrt_m), length.out = 60))
      line$psf <- stats::predict(fit, line)
      plotly::plot_ly() |>
        plotly::add_markers(
          data = s, x = ~mrt_m, y = ~psf, name = "Projects",
          marker = list(color = rl_colours[["ink"]], opacity = 0.35, size = dot_sizes(s$n)),
          text = ~sprintf("<b>%s</b><br>%s<br>Nearest: %s (%s m, ~%s min walk)<br>Median rent %s<br>Median S$%.2f psf<br>%s rentals",
                          project, district, mrt_station, format(mrt_m, big.mark = ","),
                          walk_min, money(median), psf, n),
          hoverinfo = "text") |>
        plotly::add_trace(data = line, x = ~mrt_m, y = ~psf, type = "scatter", mode = "lines",
                          name = "Trend", hoverinfo = "skip", inherit = FALSE,
                          line = list(color = rl_colours[["trend"]], width = 3)) |>
        rl_plotly("Distance to nearest MRT/LRT station (m)", "Median rent per sqft",
                  money_y = TRUE) |>
        plotly::layout(showlegend = FALSE)
    })

    output$mrt_table <- renderDT({
      s <- mrt_projects() |>
        arrange(mrt_m) |>
        select(project, district, mrt_station, mrt_m, walk_min, median, psf, n) |>
        prettify()
      rl_datatable(s, selection = "none") |> format_money_cols(s)
    })

    output$table <- renderDT({
      b <- filters$budget()
      s <- prettify(compare_data())
      rl_datatable(s, page_length = 15, selection = "none") |>
        format_money_cols(s) |>
        formatStyle("Median rent", target = "row",
                    backgroundColor = styleInterval(c(b[1] - 0.5, b[2] + 0.5),
                                                    c("transparent", "#ecfdf5", "transparent"))) |>
        formatStyle("Share within budget", fontWeight = "bold", color = "#166534",
                    background = styleColorBar(c(0, 1), "#bbf7d0"),
                    backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
                    backgroundPosition = "center")
    })
  })
}
