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
          data = s, x = ~mrt_m, y = ~psf, size = ~n, sizes = c(5, 25), name = "Projects",
          marker = list(color = rl_colours[["ink"]], opacity = 0.35),
          text = ~sprintf("<b>%s</b><br>%s<br>Nearest: %s (%s m, ~%s min walk)<br>Median rent %s<br>Median S$%.2f psf<br>%s rentals",
                          project, district, mrt_station, format(mrt_m, big.mark = ","),
                          walk_min, money(median), psf, n),
          hoverinfo = "text") |>
        plotly::add_trace(data = line, x = ~mrt_m, y = ~psf, type = "scatter", mode = "lines",
                          name = "Trend", hoverinfo = "skip", inherit = FALSE,
                          line = list(color = rl_colours[["budget"]], width = 3)) |>
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
                                                    c("transparent", "#ecfdf5", "transparent")))
    })
  })
}
