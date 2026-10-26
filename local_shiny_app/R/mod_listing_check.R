#' "Check a listing" tab: is this unit's asking rent fair?
#'
#' Type an address or postal code (checked with OneMap) or click the map to
#' drop a pin, then compare an asking rent with past rentals in the same
#' project, nearby, in the same district and in the same region.
#'
#' Uses all rentals of the chosen bedroom count and property type in the
#' sidebar's "most recent" period; the other sidebar filters are ignored so
#' every level compares like with like.
#'
#' @param filters The list returned by mod_filters_server().
#' @param shortlist The shared shortlist reactiveVal.
#' @noRd
mod_listing_check_ui <- function(id) {
  ns <- NS(id)
  rentals <- rentals_data()

  tagList(
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("The unit you are considering"),
        div(class = "d-flex gap-2 align-items-end",
            textInput(ns("query"), "Address, building or postal code",
                      placeholder = "e.g. 018987 or The Sail", width = "100%"),
            actionButton(ns("lookup"), "Find", class = "btn-primary mb-3")),
        uiOutput(ns("suggestions")),
        helpText("Or click the map to drop a pin."),
        numericInput(ns("asking"), "Asking rent (S$ per month, optional)", value = NA,
                     min = 0, step = 100),
        layout_columns(
          col_widths = c(6, 6),
          selectInput(ns("beds"), "Bedrooms", c("Any", bed_levels[bed_levels != "Unknown"]),
                      selected = "2"),
          numericInput(ns("area"), "Floor area (sqft, optional)", value = NA, min = 100, step = 50)
        ),
        selectInput(ns("type"), "Property type", c("Any", sort(unique(rentals$type))),
                    selected = "Non-Landed Properties"),
        sliderInput(ns("radius"), "Nearby means within", min = 200, max = 2000,
                    value = 500, step = 100, post = " m")
      ),
      card(
        card_header(textOutput(ns("place_label"))),
        leafletOutput(ns("map"), height = 470),
        card_footer("Circles = buildings with comparable rentals inside the radius ",
                    "(hover for their median rent).")
      )
    ),
    uiOutput(ns("verdict")),
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Comparable past rentals",
        actionButton(ns("add_short"), "Add this project to shortlist", class = "btn-sm btn-success")
      ),
      DTOutput(ns("table"), fill = FALSE),
      card_footer(textOutput(ns("table_note")))
    ),
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Where the asking rent falls",
        radioButtons(ns("level"), NULL, inline = TRUE,
                     c("Same project", "Nearby", "Same district", "Same region"),
                     selected = "Nearby")
      ),
      plotOutput(ns("plot"), height = 360)
    )
  )
}

#' @noRd
mod_listing_check_server <- function(id, filters, shortlist) {
  moduleServer(id, function(input, output, session) {
    setBookmarkExclude(c(dt_bookmark_inputs("table"), "lookup", "add_short",
                         "map_click", "map_bounds", "map_center", "map_zoom",
                         "map_shape_mouseover", "map_shape_mouseout",
                         "map_marker_mouseover", "map_marker_mouseout"))

    accent <- "#7b3294"   # the user's own pin and asking rent only

    # The place being checked: list(lat, lng, building, address), or NULL
    place <- reactiveVal(NULL)
    matches <- reactiveVal(NULL)

    observeEvent(input$lookup, {
      hits <- onemap_search(input$query)
      matches(hits)
      if (nrow(hits) == 0) {
        showNotification("No address found. Try a 6-digit postal code, or click the map.",
                         type = "warning")
      } else {
        place(as.list(hits[1, ]))
      }
    })

    # Several matches: let the user pick the right one
    output$suggestions <- renderUI({
      hits <- matches()
      req(hits, nrow(hits) > 1)
      selectInput(session$ns("pick"), paste(nrow(hits), "matches; pick one"),
                  setNames(seq_len(nrow(hits)), hits$address))
    })
    observeEvent(input$pick, {
      hits <- matches()
      req(hits, as.integer(input$pick) <= nrow(hits))
      place(as.list(hits[as.integer(input$pick), ]))
    })

    observeEvent(input$map_click, {
      p <- input$map_click
      matches(NULL)
      place(list(lat = p$lat, lng = p$lng, building = "",
                 address = sprintf("Dropped pin (%.5f, %.5f)", p$lat, p$lng)))
    })

    output$place_label <- renderText({
      if (is.null(place())) "Find an address or click the map" else place()$address
    })

    # Past rentals of the same kind, most recent period
    pool <- reactive({
      d <- latest_months(rentals_data(), filters$recent())
      if (input$beds != "Any") d <- d[d$bed_group == input$beds, ]
      if (input$type != "Any") d <- d[d$type == input$type, ]
      d
    })

    sets <- reactive({
      req(place())
      comparable_sets(pool(), place(), input$radius)
    })

    levels_table <- reactive(compare_levels(sets(), input$asking, input$area))

    # The level chosen for the chart (and the verdict), by position
    level_row <- reactive({
      i <- match(input$level, c("Same project", "Nearby", "Same district", "Same region"))
      levels_table()[i, ]
    })

    output$verdict <- renderUI({
      req(place())
      t <- levels_table()
      near <- t[2, ]
      tiles <- list(
        value_box(paste("Median rent,", tolower(near$level)), money_or_dash(near$median),
                  paste0("Average ", money_or_dash(near$mean), " · ",
                         format(near$n, big.mark = ","), " rentals"),
                  theme = "primary"),
        value_box("Typical range nearby",
                  if (near$n > 0) paste0(money(near$p25), " - ", money(near$p75)) else "-",
                  "middle 50% of rents", theme = "secondary")
      )
      if (!is.na(input$asking)) {
        r <- level_row()
        tiles[[3]] <- value_box(
          "Your asking rent", money(input$asking),
          if (r$n > 0) asking_verdict(r, input$asking, input$beds)
          else paste("No past rentals at the", tolower(input$level), "level."),
          if (!is.na(r$pct_psf_below))
            paste0("Per sqft: S$", format(round(input$asking / input$area, 2), nsmall = 2),
                   ", ", higher_lower(r$pct_psf_below), " of them."),
          theme = value_box_theme(bg = accent, fg = "white"))
      }
      do.call(layout_columns, c(list(col_widths = rep(12 / length(tiles), length(tiles))), tiles))
    })

    output$table <- renderDT({
      t <- levels_table()
      t$level[2] <- paste0(t$level[2], " (nearby)")
      t$pct_below <- t$pct_psf_below <- NULL
      if (is.na(input$asking)) t$diff_pct <- NULL else t$diff_pct <- signed_pct(t$diff_pct)
      if (all(is.na(t$diff_psf_pct))) t$diff_psf_pct <- NULL
      else t$diff_psf_pct <- signed_pct(t$diff_psf_pct)
      names(t) <- c(level = "Compared with", n = "Rentals", median = "Median rent",
                    mean = "Average rent", p25 = "25th percentile", p75 = "75th percentile",
                    psf = "Median S$ psf", diff_pct = "% difference from asking rent",
                    diff_psf_pct = "% difference from asking psf")[names(t)]
      dt <- datatable(t, rownames = FALSE, selection = "none",
                      options = list(dom = "t", ordering = FALSE, scrollX = TRUE)) |>
        format_money_cols(t) |>
        formatCurrency("Average rent", "S$", digits = 0)
      diff_cols <- intersect(c("% difference from asking rent", "% difference from asking psf"),
                             names(t))
      # Red = asking more than past rents, green = asking less
      if (length(diff_cols)) {
        dt <- formatStyle(dt, diff_cols, fontWeight = "bold",
                          color = DT::JS("value.indexOf('above') > -1 ? '#c0392b' : ",
                                         "value.indexOf('below') > -1 ? '#18bc9c' : 'inherit'"))
      }
      dt
    })

    output$table_note <- renderText({
      s <- sets()
      parts <- c(
        if (!is.na(attr(s, "project"))) paste("Project:", attr(s, "project")),
        if (!is.na(attr(s, "district"))) paste("District:", attr(s, "district")),
        if (!is.na(attr(s, "region"))) paste("Region:", attr(s, "region"))
      )
      few <- levels_table()$n[2] < 10
      paste0(paste(parts, collapse = " · "), ". Rentals from the latest ", filters$recent(),
             " months", if (input$beds != "Any") paste0(", ", input$beds, " bedroom(s)"), ". ",
             if (few) "Few rentals nearby: widen the radius or use the district figures. " else "",
             "Regions are approximated from postal districts.")
    })

    output$plot <- renderPlot({
      r <- level_row()
      s <- sets()[[match(input$level, c("Same project", "Nearby", "Same district", "Same region"))]]
      validate(need(nrow(s) > 0, paste0("No past rentals at the '", input$level,
                                        "' level. Try another level or a wider radius.")))
      rent <- s$rent[s$rent <= quantile(s$rent, 0.99)]   # hide extreme top 1%
      ggplot(data.frame(rent = rent), aes(rent)) +
        annotate("rect", xmin = r$p25, xmax = r$p75, ymin = -Inf, ymax = Inf,
                 fill = "#18bc9c", alpha = 0.15) +
        geom_histogram(bins = 30, fill = "#2c3e50", colour = "white") +
        geom_vline(xintercept = r$median, linetype = "dashed", colour = "#2c3e50") +
        (if (!is.na(input$asking))
          geom_vline(xintercept = input$asking, linewidth = 1.4, colour = accent)) +
        scale_x_continuous(labels = scales::label_dollar(prefix = "S$")) +
        labs(x = "Monthly rent", y = "Number of rentals",
             caption = paste0("Green band = typical range, dashed = median",
                              if (!is.na(input$asking)) ", purple = your asking rent", "",
                              ". n = ", format(r$n, big.mark = ","))) +
        theme_minimal(base_size = 14)
    })

    # Map: pin, radius and the comparable buildings inside it
    output$map <- renderLeaflet({
      leaflet() |>
        addProviderTiles(providers$Esri.WorldGrayCanvas) |>
        setView(103.83, 1.33, zoom = 11)
    })

    observe({
      p <- place()
      req(p)
      near <- sets()[[2]]
      bldg <- if (nrow(near)) summarise_buildings(near) else NULL
      m <- leafletProxy("map") |> clearMarkers() |> clearShapes() |>
        addCircles(lng = p$lng, lat = p$lat, radius = input$radius, color = accent,
                   weight = 1.5, fillOpacity = 0.05)
      if (!is.null(bldg)) {
        m <- addCircleMarkers(m, data = bldg, lng = ~lng, lat = ~lat,
                              radius = ~pmin(10, 3 + sqrt(n) / 2),
                              color = "#2c3e50", weight = 1, fillOpacity = 0.6,
                              label = sprintf("%s: median %s (%s rentals)",
                                              bldg$project, money(bldg$median), bldg$n))
      }
      m |>
        addCircleMarkers(lng = p$lng, lat = p$lat, radius = 8, color = "white", weight = 2,
                         fillColor = accent, fillOpacity = 1, label = p$address) |>
        setView(p$lng, p$lat, zoom = if (input$radius > 1000) 14 else 15)
    })

    observeEvent(input$add_short, {
      project <- attr(sets(), "project")
      if (is.na(project) || input$beds == "Any") {
        showNotification(
          "Pick a bedroom count and a place that is a known project to add it to your shortlist.",
          type = "warning")
        return()
      }
      add_to_shortlist(shortlist, data.frame(project = project, bed_group = input$beds), 1)
    })
  })
}
