#' "Map" tab: where are the rentals?
#'
#' Click a circle to inspect one building, or draw a shape to summarise an
#' area; the value boxes and table follow that selection.
#'
#' @param filters The list returned by mod_filters_server().
#' @param shortlist The shared shortlist reactiveVal.
#' @param map_sel The shared map selection: reactiveValues with `ids` (map
#'   locations, NULL = nothing selected), `label` and `apply` (narrow every
#'   page to it).
#' @param areas The shared saved areas for "Area A vs B" (see new_saved_areas()).
#' @return The server returns a list with `map_data`, the mapped rentals.
#' @noRd
mod_map_ui <- function(id) {
  ns <- NS(id)
  rentals <- rentals_data()
  geo_choices <- sort(unique(rentals$geo_match[!is.na(rentals$loc_id)]))

  tagList(
    card(
      full_screen = TRUE,
      header_with_actions(
        span(icon("hand-pointer"), " Click a building, or draw an area with the tools on the left"),
        actionButton(ns("clear"), "Clear selection", icon = icon("xmark"),
                     class = "btn-sm btn-outline-secondary")
      ),
      layout_sidebar(
        sidebar = sidebar(
          position = "right", width = 300, class = "map-panel",
          div(class = "section-title", "Selection"),
          div(class = "selection-label", textOutput(ns("sel_label"))),
          input_switch(ns("apply_sel"), "Apply map selection to all pages", value = TRUE),
          div(class = "d-flex gap-2 mb-1",
              actionButton(ns("save_a"), "Save as Area A", class = "btn-sm btn-area-a flex-fill"),
              actionButton(ns("save_b"), "Save as Area B", class = "btn-sm btn-area-b flex-fill")),
          div(class = "small text-muted mb-2", textOutput(ns("areas_status"), inline = TRUE)),
          div(class = "kpi-stack",
              kpi("Rentals", textOutput(ns("vb_n"))),
              kpi("Projects", textOutput(ns("vb_proj"))),
              kpi("Median rent", textOutput(ns("vb_rent"))),
              kpi("Median psf", textOutput(ns("vb_psf")))),
          div(class = "section-title mt-3", "Map options"),
          div(class = "chips",
              radioButtons(ns("metric"), "Colour circles by", metric_choices)),
          selectizeInput(ns("geo_match"), "Geocode quality", geo_choices,
                         selected = grep("^Exact|^Good", geo_choices, value = TRUE),
                         multiple = TRUE, options = list(plugins = list("remove_button"))),
          helpText(format(sum(is.na(rentals$loc_id)), big.mark = ","),
                   " rentals have no coordinates and are not mapped. ",
                   "Yellow stars mark universities, polytechnics and work areas. Use the ",
                   "layers button for MRT/LRT stations, the airport and hospitals, the ",
                   "heatmap, or another basemap.")
        ),
        leafletOutput(ns("map"), height = 640)
      ),
      card_footer("Circle size = number of rentals; colour bins stay the same whatever the ",
                  "filters. 'Vs my budget' colours each building by whether its median rent is ",
                  "below, within or above your budget. The map follows your settings.")
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header(textOutput(ns("trend_title"), inline = TRUE)),
           plotly::plotlyOutput(ns("trend"), height = 330),
           card_footer("Months with fewer than 3 rentals are left out.")),
      card(card_header("By bedrooms"), DTOutput(ns("beds"), fill = FALSE))
    ),
    card(
      header_with_actions(
        "Projects in the map selection",
        actionButton(ns("add_short"), "Add to shortlist", icon = icon("star"),
                     class = "btn-sm btn-primary")
      ),
      DTOutput(ns("table"), fill = FALSE),
      card_footer("Select rows, then press 'Add to shortlist'.")
    ),
    card(
      header_with_actions(
        "Rentals in the map selection",
        downloadButton(ns("download"), "Download CSV", class = "btn-sm btn-outline-secondary")
      ),
      DTOutput(ns("contracts"), fill = FALSE)
    )
  )
}

#' @noRd
mod_map_server <- function(id, filters, shortlist, map_sel = new_map_selection(),
                           areas = new_saved_areas()) {
  moduleServer(id, function(input, output, session) {
    # Leaflet adds its own inputs (map_bounds, map_marker_click, map_draw_*, ...)
    observe(setBookmarkExclude(c(dt_bookmark_inputs(c("table", "beds", "contracts")),
                                 "add_short", "clear", "save_a", "save_b",
                                 grep("^map_", names(input), value = TRUE))))

    # Mapped rentals: the filters (before the map selection) narrowed to chosen
    # projects (like filters$selection(), but without validate() so the map can be empty)
    map_data <- reactive({
      d <- filters$base()
      if (length(filters$projects())) d <- d[d$project %in% filters$projects(), ]
      d[!is.na(d$loc_id) & d$geo_match %in% input$geo_match, ]
    })

    loc_summary <- reactive(summarise_locations(map_data()))

    # Selection: a clicked building, a drawn area, or nothing (= everything mapped).
    # Shared with the filters, which apply it to every page when asked to
    sel <- map_sel
    observe(sel$apply <- isTRUE(input$apply_sel))

    observeEvent(input$map_marker_click, {
      id <- input$map_marker_click$id
      req(!is.null(id), id != "")
      l <- loc_summary() |> filter(loc_id == as.integer(id))
      sel$ids <- as.integer(id); sel$label <- l$project[1]
    })
    observeEvent(input$map_draw_new_feature, {
      shape <- drawn_to_sf(input$map_draw_new_feature)
      locs_sf <- map_locations_sf()
      # Only the buildings on show (the geocode-quality choice hides some)
      ids <- intersect(locs_sf$loc_id[lengths(st_intersects(locs_sf, shape)) > 0],
                       loc_summary()$loc_id)
      if (length(ids) == 0) {
        showNotification("No buildings in that area. Try drawing a larger shape.", type = "warning")
        return()
      }
      sel$ids <- ids
      sel$label <- sprintf("Drawn area (%d buildings)", length(ids))
    })
    clear_sel <- function() { sel$ids <- NULL; sel$label <- NULL }
    observeEvent(input$map_draw_deleted_features, clear_sel())
    # "Clear selection" also wipes the saved Area A and Area B
    observeEvent(input$clear, {
      clear_sel()
      for (k in names(areas)) areas[[k]] <- NULL
      leafletProxy("map") |> clearGroup("Selected") |>
        removeDrawToolbar(clearFeatures = TRUE) |> add_draw_toolbar()
    })

    # Save the selection as Area A or B, for "Area A vs B"
    save_area <- function(w) {
      if (is.null(sel$ids)) {
        showNotification("Click a building or draw an area on the map first.", type = "warning")
        return()
      }
      areas[[paste0(w, "_ids")]]   <- sel$ids
      areas[[paste0(w, "_label")]] <- sel$label
      showNotification(paste0("Saved as Area ", toupper(w), ": ", sel$label,
                              ". Compare them on Market > Area A vs B."), type = "message")
    }
    observeEvent(input$save_a, save_area("a"))
    observeEvent(input$save_b, save_area("b"))
    output$areas_status <- renderText({
      lab <- function(w) if (is.null(areas[[paste0(w, "_label")]])) "not set" else areas[[paste0(w, "_label")]]
      paste0("A: ", lab("a"), " | B: ", lab("b"))
    })

    selected <- reactive({
      d <- map_data()
      if (!is.null(sel$ids)) d <- d[d$loc_id %in% sel$ids, ]
      d
    })

    # Base map (drawn once)
    output$map <- renderLeaflet(base_map())

    # The map only exists once its tab has been opened; wait for it before
    # sending circles (map_zoom is set when the map first draws)
    map_ready <- reactiveVal(FALSE)
    observeEvent(input$map_zoom, map_ready(TRUE))

    # Redraw circles + legend whenever filters or the colour metric change
    observe({
      req(map_ready())
      d <- loc_summary(); m <- input$metric; pal <- map_palette(m)
      vals <- if (m == "budget") budget_fit(d$rent, filters$budget()) else d[[m]]
      p <- leafletProxy("map", data = d) |> clearGroup("Buildings") |> clearGroup("Heatmap")
      if (nrow(d) == 0) return(removeControl(p, "legend"))
      p |>
        addCircleMarkers(lng = ~lng, lat = ~lat, layerId = ~as.character(loc_id),
                         group = "Buildings",
                         radius = ~pmin(11, 2 + sqrt(n) / 3),
                         fillColor = pal(vals), fillOpacity = 0.8,
                         color = "#333", weight = 0.5, label = location_labels(d)) |>
        addHeatmap(lng = ~lng, lat = ~lat, intensity = ~n, group = "Heatmap",
                   blur = 20, radius = 12, max = quantile(d$n, 0.9)) |>
        addLegend("bottomright", pal = pal, values = vals, layerId = "legend",
                  title = names(metric_choices)[metric_choices == m],
                  labFormat = if (m == "budget") labelFormat()
                              else labelFormat(prefix = "S$", big.mark = ","))
    })

    # Ring around the station chosen in the filters; fly there when it changes
    observe({
      req(map_ready())
      leafletProxy("map") |> update_station_ring(filters$mrt_pick(), filters$mrt_dist())
    })
    observeEvent(filters$mrt_pick(), {
      req(map_ready())
      st <- station_location(filters$mrt_pick())
      leafletProxy("map") |> flyTo(st[2], st[1], zoom = 15)
    })

    # Outline the selected buildings
    observe({
      req(map_ready())
      ids <- sel$ids
      p <- leafletProxy("map") |> clearGroup("Selected")
      if (!is.null(ids)) {
        s <- map_locations() |> filter(loc_id %in% ids)
        p |> addCircleMarkers(data = s, lng = ~lng, lat = ~lat, group = "Selected",
                              radius = 9, fill = FALSE, color = "#000", weight = 2.5,
                              options = pathOptions(interactive = FALSE))
      }
    })

    # Outline the saved areas (A navy, B orange)
    observe({
      req(map_ready())
      p <- leafletProxy("map") |> clearGroup("Area A") |> clearGroup("Area B")
      for (w in c("a", "b")) {
        ids <- areas[[paste0(w, "_ids")]]
        if (is.null(ids)) next
        s <- map_locations() |> filter(loc_id %in% ids)
        p <- addCircleMarkers(p, data = s, lng = ~lng, lat = ~lat,
                              group = paste("Area", toupper(w)), radius = 12, fill = FALSE,
                              color = area_colours[[w]], weight = 2,
                              options = pathOptions(interactive = FALSE))
      }
    })

    output$sel_label <- renderText(if (is.null(sel$label)) "all mapped rentals" else sel$label)
    output$vb_n    <- renderText(format(nrow(selected()), big.mark = ","))
    output$vb_proj <- renderText(format(n_distinct(selected()$project), big.mark = ","))
    output$vb_rent <- renderText(
      if (nrow(selected()) == 0) "-" else money(median(selected()$rent)))
    output$vb_psf  <- renderText(
      if (nrow(selected()) == 0) "-"
      else paste0("S$", format(round(median(selected()$psf, na.rm = TRUE), 2), nsmall = 2)))

    # One row per project + bedroom type in the selection (recent months)
    table_data <- reactive({
      d <- latest_months(selected(), filters$recent())
      validate(need(nrow(d) > 0, "No recent rentals in this map selection."))
      summarise_options(d) |> arrange(desc(n))
    })

    output$table <- renderDT({
      s <- table_data() |> mutate(last = format(last, "%b %Y"))
      s <- prettify(s)
      rl_datatable(s) |> format_money_cols(s)
    })

    observeEvent(input$add_short, {
      add_to_shortlist(shortlist, table_data(), input$table_rows_selected)
    })

    # ---- Breakdown of the selection ----
    output$trend_title <- renderText(paste(
      "Median rent psf by month:", if (is.null(sel$label)) "all mapped rentals" else sel$label))

    output$trend <- plotly::renderPlotly({
      d <- selected()[!is.na(selected()$psf), ]
      validate(need(nrow(d) > 0, "No rentals in this selection."))
      d$month <- as.Date(cut(d$lease_date, "month"))
      by_bed <- d |> group_by(month, bed_group) |>
        summarise(psf = median(psf), n = n(), .groups = "drop") |> filter(n >= 3)
      overall <- d |> group_by(month) |>
        summarise(psf = median(psf), n = n(), .groups = "drop") |> filter(n >= 3)
      validate(need(nrow(overall) > 0, "Not enough rentals per month to draw a trend."))
      hover <- "%{x|%b %Y}<br>S$%{y:.2f} psf<br>%{text} rentals"
      plotly::plot_ly() |>
        plotly::add_lines(data = by_bed, x = ~month, y = ~psf, text = ~n,
                          color = ~factor(paste(bed_group, "BR"), paste(bed_levels, "BR")),
                          colors = unname(bed_colours), opacity = 0.6,
                          hovertemplate = paste0(hover, "<extra>%{fullData.name}</extra>")) |>
        plotly::add_lines(data = overall, x = ~month, y = ~psf, text = ~n, name = "All",
                          line = list(color = rl_colours[["ink"]], width = 3),
                          hovertemplate = paste0(hover, "<extra>All</extra>")) |>
        rl_plotly(NULL, "S$ psf per month")
    })

    output$beds <- renderDT({
      validate(need(nrow(selected()) > 0, "No rentals in this selection."))
      s <- selected() |>
        group_by(Bedrooms = bed_group) |>
        summarise(Rentals = n(), `Median rent` = median(rent),
                  `Median sqft` = median(area_sqft, na.rm = TRUE),
                  `Median psf` = median(psf, na.rm = TRUE), .groups = "drop") |>
        arrange(match(Bedrooms, bed_levels))
      datatable(s, rownames = FALSE, selection = "none", class = "compact hover row-border",
                options = list(dom = "t", pageLength = 20)) |>
        formatCurrency("Median rent", "S$", digits = 0) |>
        formatCurrency("Median psf", "S$", digits = 2) |>
        DT::formatRound("Median sqft", 0)
    })

    contracts <- reactive({
      selected() |>
        arrange(desc(lease_date)) |>
        transmute(Month = format(lease_date, "%Y-%m"), Project = project, Street = street,
                  District = district, Type = type, Bedrooms = bed_group,
                  `Area (sqft)` = area_band, Rent = rent, PSF = round(psf, 2),
                  Address = address, `Nearest MRT/LRT` = mrt_station,
                  `Geocode quality` = geo_match)
    })

    output$contracts <- renderDT({
      rl_datatable(contracts(), page_length = 15, selection = "none") |>
        formatCurrency("Rent", "S$", digits = 0)
    })

    output$download <- downloadHandler(
      filename = function() paste0("rentals_map_selection-", Sys.Date(), ".csv"),
      content  = function(file) write.csv(contracts(), file, row.names = FALSE)
    )

    # Keep the selection and saved areas in "Save search" links
    onBookmark(function(state) {
      state$values$sel_ids   <- sel$ids
      state$values$sel_label <- sel$label
      state$values$areas     <- reactiveValuesToList(areas)
    })
    onRestore(function(state) {
      if (length(state$values$sel_ids) > 0) {
        sel$ids   <- as.integer(unlist(state$values$sel_ids))
        sel$label <- unlist(state$values$sel_label)
      }
      a <- state$values$areas
      for (k in c("a_ids", "b_ids")) if (length(a[[k]])) areas[[k]] <- as.integer(unlist(a[[k]]))
      for (k in c("a_label", "b_label")) if (length(a[[k]])) areas[[k]] <- unlist(a[[k]])
    })

    list(map_data = map_data)
  })
}

#' An empty map selection (see mod_map_server())
#' @noRd
new_map_selection <- function() reactiveValues(ids = NULL, label = NULL, apply = TRUE)
