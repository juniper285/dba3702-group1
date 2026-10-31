#' "Compare saved areas" (under the map): how do the areas I saved compare?
#'
#' Areas are saved on the map ("Save this area", up to `max_areas`); this
#' card compares all of them, with all of Singapore for the same filters as a
#' grey reference.
#'
#' @param data The rentals to compare: the filters, mapped buildings only,
#'   but ignoring the map's "Only buildings within my budget" switch (so an
#'   area's figures don't depend on it).
#' @param areas The saved areas (see new_saved_areas()).
#' @param filters The list returned by mod_filters_server(): its settings
#'   (Singapore reference), budget and places.
#' @noRd
mod_saved_areas_ui <- function(id) {
  ns <- NS(id)
  card(
    class = "saved-areas-card",
    card_header(icon("code-compare"), " Compare saved areas"),
    uiOutput(ns("body"))
  )
}

#' @noRd
mod_saved_areas_server <- function(id, data, areas, filters) {
  moduleServer(id, function(input, output, session) {
    sg_label <- "Singapore (reference)"

    # Each saved area's rentals, named after the area
    sets <- reactive({
      a <- areas()
      d <- data()
      s <- lapply(a, function(x) d[d$loc_id %in% x$locs, ])
      names(s) <- make.unique(vapply(a, `[[`, "", "name"), sep = " ")
      s
    })
    # Every rental in Singapore with the same bedrooms, type, size and period
    singapore <- reactive({
      s <- filters$settings()
      filter_market(rentals_data(), NULL, s$beds, s$types, s$area, s$period)
    })
    colours <- reactive(setNames(
      c(vapply(areas(), `[[`, "", "colour"), rl_colours[["muted"]]),
      c(names(sets()), sg_label)))

    output$body <- renderUI({
      n <- length(areas())
      if (n < 2) {
        return(div(class = "text-muted",
          icon("circle-info"), " ",
          if (n == 0) "Click a building or draw an area on the map, then press 'Save this area'. "
          else "Save one more area to compare. ",
          "Save up to ", max_areas, " areas; they're compared here side by side, ",
          "with all of Singapore for reference."))
      }
      ns <- session$ns
      tagList(
        uiOutput(ns("table")),
        layout_columns(
          col_widths = c(6, 6), class = "mt-3",
          div(div(class = "section-title", "Median monthly rent by bedrooms"),
              plotly::plotlyOutput(ns("beds"), height = 340)),
          div(div(class = "section-title", "Median monthly rent by quarter"),
              plotly::plotlyOutput(ns("trend"), height = 340))
        ),
        div(class = "small text-muted mt-2",
            "Every rental matching your filters in each area's buildings, including those ",
            "the map hides with 'Only buildings within my budget'. Distances are from the ",
            "area's centre. Quarters with fewer than 3 rentals are left out.")
      )
    })

    # Areas as columns, measures as rows
    output$table <- renderUI({
      req(length(areas()) >= 2)
      all_sets <- c(sets(), setNames(list(singapore()), sg_label))
      s <- compare_areas(all_sets, filters$budget(), filters$places())
      sg <- nrow(s)
      s$district[sg] <- "-"
      place_cols <- place_column(filters$places()$name)
      s[sg, place_cols] <- NA
      fmt_km <- function(x) ifelse(is.na(x), "-", paste(x, "km"))
      rows <- list(
        "Rentals"                  = format(s$n, big.mark = ","),
        "Projects"                 = format(s$projects, big.mark = ","),
        "Median rent"              = money(s$median),
        "Typical range"            = paste0(money(s$q25), " - ", money(s$q75)),
        "Within my budget"         = paste0(round(100 * s$within), "%"),
        "Median rent per sqft"     = sprintf("S$%.2f", s$psf),
        "Walk to nearest MRT/LRT"  = ifelse(is.na(s$walk_min), "-", paste(s$walk_min, "min")),
        "Main district"            = s$district
      )
      for (p in place_cols) rows[[sub(" \\(km\\)$", "", p)]] <- fmt_km(s[[p]])
      cols <- colours()
      tags$div(class = "table-responsive",
        tags$table(class = "table table-sm saved-areas-table",
          tags$thead(tags$tr(tags$th(""), lapply(seq_len(nrow(s)), function(i)
            tags$th(style = paste0("border-top: 4px solid ", cols[[i]]), s$area[i])))),
          tags$tbody(lapply(names(rows), function(r)
            tags$tr(tags$th(scope = "row", r), lapply(rows[[r]], tags$td))))))
    })

    output$beds <- plotly::renderPlotly({
      req(length(areas()) >= 2)
      s <- compare_areas_by_bed(c(sets(), setNames(list(singapore()), sg_label)))
      s <- s[s$bed_group %in% s$bed_group[s$area != sg_label], ]
      s$bed_group <- factor(s$bed_group, levels = bed_levels)
      plotly::plot_ly(s, x = ~bed_group, y = ~median, color = ~area, colors = colours(),
                      type = "bar", customdata = ~n, text = ~money(median),
                      hovertemplate = "%{x} BR<br>Median %{text}<br>%{customdata} rentals") |>
        rl_plotly("Bedrooms", "Median monthly rent", money_y = TRUE) |>
        plotly::layout(barmode = "group")
    })

    output$trend <- plotly::renderPlotly({
      req(length(areas()) >= 2)
      all_sets <- c(sets(), setNames(list(singapore()), sg_label))
      s <- do.call(rbind, lapply(names(all_sets), function(nm) {
        d <- all_sets[[nm]]
        if (nrow(d) == 0) return(NULL)
        d |> group_by(quarter) |> summarise(median = median(rent), n = n(), .groups = "drop") |>
          mutate(area = nm)
      }))
      start <- min(do.call(rbind, sets())$quarter)
      s <- s[s$n >= 3 & s$quarter >= start, ]
      validate(need(nrow(s) > 0, "Not enough rentals per quarter."))
      s$area <- factor(s$area, levels = names(colours()))
      plotly::plot_ly(s, x = ~quarter, y = ~median, color = ~area, colors = colours(),
                      type = "scatter", mode = "lines+markers",
                      text = ~sprintf("%s<br>Median %s<br>%s rentals",
                                      quarter_label(quarter), money(median), n),
                      hoverinfo = "text") |>
        rl_plotly(NULL, "Median monthly rent", money_y = TRUE)
    })
  })
}
