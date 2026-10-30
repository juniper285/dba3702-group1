#' "Check a listing" tab: is this unit's asking rent fair?
#'
#' Type an address or postal code (checked with OneMap) or click the map to
#' drop a pin, then compare an asking rent with past rentals in the same
#' project, nearby, in the same district and in the same region.
#'
#' Uses all rentals of the chosen bedroom count and property type in the
#' settings' "most recent" period; the other filters are ignored so
#' every level compares like with like.
#'
#' @param filters The list returned by mod_filters_server().
#' @param shortlist The shared shortlist reactiveVal.
#' @noRd
mod_listing_check_ui <- function(id) {
  ns <- NS(id)
  rentals <- rentals_data()

  examples <- c("018987", "Tanjong Pagar", "Holland Village", "Punggol")
  example_chips <- lapply(examples, function(q) {
    tags$button(type = "button", class = "example-chip", q,
                onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                                  ns("example"), q))
  })

  tagList(
    page_intro("Is this asking rent fair?",
               "Find the unit you are viewing, enter its asking rent, and see how it ",
               "compares with past rentals in the same project, nearby, in the district ",
               "and in the region."),
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header(icon("house"), " The unit you are considering"),
        card_body(
        fillable = FALSE,
        div(class = "d-flex gap-2 align-items-end",
            textInput(ns("query"), "Address, building or postal code",
                      placeholder = "e.g. 018987 or The Sail", width = "100%") |>
              tagAppendAttributes(
                onkeydown = sprintf("if (event.key === 'Enter') document.getElementById('%s').click()",
                                    ns("lookup")),
                .cssSelector = "input"),
            actionButton(ns("lookup"), "Find", icon = icon("magnifying-glass"),
                         class = "btn-primary mb-3 text-nowrap")),
        uiOutput(ns("suggestions")),
        div(class = "small text-muted mb-3", "Or click the map to drop a pin. Try: ",
            example_chips),
        layout_columns(
          col_widths = c(6, 6),
          numericInput(ns("asking"), "Asking rent (S$ / month)", value = NA, min = 0, step = 100),
          numericInput(ns("area"), "Floor area (sqft, optional)", value = NA, min = 100, step = 50)
        ),
        layout_columns(
          col_widths = c(4, 8),
          selectInput(ns("beds"), "Bedrooms", c("Any", bed_levels[bed_levels != "Unknown"]),
                      selected = "2"),
          selectInput(ns("type"), "Property type", c("Any", sort(unique(rentals$type))),
                      selected = "Any")
        ),
        sliderInput(ns("radius"), "Nearby means within", min = 200, max = 2000,
                    value = 500, step = 100, post = " m")
        )
      ),
      card(
        card_header(icon("location-dot"), " ", textOutput(ns("place_label"), inline = TRUE)),
        leafletOutput(ns("map"), height = 520),
        card_footer("Purple = your place and radius. Dark circles = buildings with comparable ",
                    "rentals inside the radius (hover for their median rent).")
      )
    ),
    conditionalPanel(
      "!output.has_place", ns = ns,
      card(empty_state("magnifying-glass-location", "Start by finding the unit",
                       "Search for an address or postal code, or click the map. ",
                       "Add the asking rent to see whether it is fair."))
    ),
    conditionalPanel(
      "output.has_place", ns = ns,
      uiOutput(ns("verdict")),
      card(
        class = "mt-3",
        header_with_actions(
          "Comparable past rentals",
          actionButton(ns("add_short"), "Add this project to shortlist", icon = icon("star"),
                       class = "btn-sm btn-primary")
        ),
        DTOutput(ns("table"), fill = FALSE),
        card_footer(textOutput(ns("table_note")))
      ),
      card(
        header_with_actions(
          "Where the asking rent falls",
          div(class = "chips mb-0",
              radioButtons(ns("level"), NULL, inline = TRUE,
                           c("Same project", "Nearby", "Same district", "Same region"),
                           selected = "Nearby"))
        ),
        plotly::plotlyOutput(ns("plot"), height = 360),
        card_footer(
          "\u2507 Median  ",
          span(style = paste0("color:", rl_colours[["accent"]]), "\u2503"), " Your asking rent")
      )
    )
  )
}

#' @noRd
mod_listing_check_server <- function(id, filters, shortlist) {
  moduleServer(id, function(input, output, session) {
    setBookmarkExclude(c(dt_bookmark_inputs("table"), "lookup", "add_short", "example",
                         "map_click", "map_bounds", "map_center", "map_zoom",
                         "map_shape_mouseover", "map_shape_mouseout",
                         "map_marker_mouseover", "map_marker_mouseout"))

    accent <- rl_colours[["accent"]]   # the user's own pin and asking rent only

    # The place being checked: list(lat, lng, building, address), or NULL
    place <- reactiveVal(NULL)
    matches <- reactiveVal(NULL)

    lookup <- function(query) {
      hits <- onemap_search(query)
      matches(hits)
      if (nrow(hits) == 0) {
        showNotification("No address found. Try a 6-digit postal code, or click the map.",
                         type = "warning")
      } else {
        place(as.list(hits[1, ]))
      }
    }
    observeEvent(input$lookup, lookup(input$query))
    observeEvent(input$example, {
      updateTextInput(session, "query", value = input$example)
      lookup(input$example)
    })

    output$has_place <- reactive(!is.null(place()))
    outputOptions(output, "has_place", suspendWhenHidden = FALSE)

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
      district <- t[3, ]
      tiles <- layout_columns(
        col_widths = c(4, 4, 4),
        kpi(paste("Median rent,", tolower(near$level)), money_or_dash(near$median),
            paste0("average ", money_or_dash(near$mean), " \u00B7 ",
                   format(near$n, big.mark = ","), " rentals"), icon = "coins"),
        kpi("Typical range nearby",
            if (near$n > 0) paste0(money(near$p25), " - ", money(near$p75)) else "-",
            "middle 50% of rents", icon = "arrows-left-right"),
        kpi("Median rent, same district", money_or_dash(district$median),
            paste0(format(district$n, big.mark = ","), " rentals"), icon = "map")
      )
      few <- if (near$n < 10)
        p(class = "small text-muted mt-2 mb-0", icon("circle-info"),
          " Few rentals nearby: widen the radius or use the district figures.")

      if (is.na(input$asking)) {
        return(tagList(
          div(class = "verdict verdict-info",
              div(class = "verdict-icon", icon("pen-to-square")),
              div(div(class = "verdict-title", "Add the asking rent"),
                  div(class = "verdict-body",
                      "Enter the asking rent above to see how it compares with these rentals."))),
          div(class = "mt-3", tiles), few))
      }
      r <- level_row()
      if (r$n == 0) {
        return(tagList(
          div(class = "verdict verdict-info verdict-strong",
              div(class = "verdict-icon", icon("circle-question")),
              div(div(class = "verdict-title", no_rentals_message(input$level)),
                  div(class = "verdict-body", "Pick another level under the chart, or change ",
                      "the bedrooms or property type."))),
          div(class = "mt-3", tiles), few))
      }
      psf <- if (!is.na(r$pct_psf_below))
        paste0(" Per sqft that is S$", format(round(input$asking / input$area, 2), nsmall = 2),
               ", ", higher_lower(r$pct_psf_below), " of them.")
      tagList(
        div(class = "verdict verdict-accent verdict-strong",
            div(class = "verdict-icon", icon("scale-balanced")),
            div(div(class = "verdict-title", asking_verdict(r, input$asking, input$beds)),
                div(class = "verdict-body",
                    if (round(r$diff_pct, 1) == 0) paste0("That is the median, ", money(r$median), ".", psf)
                    else paste0("That is ", sprintf("%.1f%%", abs(r$diff_pct)),
                                if (r$diff_pct > 0) " above" else " below",
                                " the median of ", money(r$median), ".", psf),
                    " Change the level under the chart to compare more widely."),
                if (r$n < few_rentals)
                  div(class = "verdict-warning", icon("triangle-exclamation"),
                      sprintf(" Based on only %d past rental%s, so treat this as a rough guide.",
                              r$n, if (r$n == 1) "" else "s")))),
        div(class = "mt-3", tiles), few)
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

    output$plot <- plotly::renderPlotly({
      i <- match(input$level, c("Same project", "Nearby", "Same district", "Same region"))
      s <- sets()[[i]]
      validate(need(nrow(s) > 0, no_rentals_message(input$level)))
      rent_histogram(s$rent, mark = input$asking)
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
