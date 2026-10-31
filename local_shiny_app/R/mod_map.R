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
  tagList(
    card(
      # Fits a laptop screen with the filter box above it; the side panel scrolls
      full_screen = TRUE, height = 640,
      header_with_actions(
        span(icon("hand-pointer"), " Click a building, or draw an area with the tools on the left"),
        actionButton(ns("help"), "How to use the map", icon = icon("lightbulb"),
                     class = "btn-sm btn-outline-primary"),
        actionButton(ns("clear"), "Clear selection", icon = icon("xmark"),
                     class = "btn-sm btn-outline-secondary")
      ),
      layout_sidebar(
        sidebar = sidebar(
          position = "right", width = 300, class = "map-panel",
          div(class = "section-title", "Selection"),
          div(class = "selection-label", textOutput(ns("sel_label"))),
          input_switch(ns("apply_sel"), "Apply map selection to all pages", value = TRUE),
          div(class = "kpi-stack",
              kpi("Rentals", textOutput(ns("vb_n"))),
              kpi("Projects", textOutput(ns("vb_proj"))),
              kpi("Median rent", textOutput(ns("vb_rent"))),
              kpi("Median psf", textOutput(ns("vb_psf")))),
          div(class = "small text-muted mt-2", textOutput(ns("unmapped"), inline = TRUE)),
          div(class = "section-title mt-3", "Saved areas"),
          actionButton(ns("save_area"), "Save this area", icon = icon("bookmark"),
                       class = "btn-sm btn-primary w-100 mb-2"),
          uiOutput(ns("saved_list")),
          div(class = "section-title mt-3", "Map options"),
          input_switch(ns("in_budget"), "Only buildings within my budget", value = TRUE),
          div(class = "chips",
              radioButtons(ns("metric"), "Colour circles by", metric_choices,
                           selected = "budget"))
        ),
        leafletOutput(ns("map"), height = "100%")
      ),
      card_footer("The map follows your filters. The key (top right) explains the symbols, ",
                  "and the colour scale (bottom right) the circle colours.")
    ),
    mod_saved_areas_ui(ns("areas")),
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
                                 "add_short", "clear", "save_area", "rename_area",
                                 "remove_area", "rename_to", "rename_ok", "stored_areas", "help",
                                 grep("^map_", names(input), value = TRUE))))

    # Mappable rentals: the filters (before the map selection) narrowed to chosen
    # projects (like filters$selection(), but without validate() so the map can
    # be empty). Saved areas are compared on these
    area_data <- reactive({
      d <- filters$base()
      if (length(filters$projects())) d <- d[d$project %in% filters$projects(), ]
      d[!is.na(d$loc_id) & well_located(d$geo_match), ]
    })
    # Mapped rentals: also only buildings whose median rent is within budget,
    # unless that's switched off
    map_data <- reactive({
      d <- area_data()
      if (!isFALSE(input$in_budget) && nrow(d)) {
        b <- filters$budget()
        med <- tapply(d$rent, d$loc_id, median)
        d <- d[d$loc_id %in% as.integer(names(med)[med >= b[1] & med <= b[2]]), ]
      }
      d
    })

    loc_summary <- reactive(summarise_locations(map_data()))
    # Rentals whose building couldn't be placed exactly: counted on every other
    # page, but not on the map
    output$unmapped <- renderText({
      d <- filters$base()
      if (length(filters$projects())) d <- d[d$project %in% filters$projects(), ]
      n <- sum(is.na(d$loc_id) | !well_located(d$geo_match))
      if (n == 0) "" else paste0(format(n, big.mark = ","), " of your ",
                                 format(nrow(d), big.mark = ","), " rentals have no exact ",
                                 "location, so they aren't on the map. Other pages still count them.")
    })

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
      # Only the buildings on show (the budget switch hides some)
      ids <- intersect(locs_sf$loc_id[lengths(st_intersects(locs_sf, shape)) > 0],
                       loc_summary()$loc_id)
      if (length(ids) == 0) {
        showNotification("No buildings in that area. Try drawing a larger shape.", type = "warning")
        return()
      }
      sel$ids <- ids
      sel$label <- name_area(map_data()[map_data()$loc_id %in% ids, ])
    })
    clear_sel <- function() { sel$ids <- NULL; sel$label <- NULL }
    observeEvent(input$map_draw_deleted_features, clear_sel())
    # Saved areas stay until removed one by one
    observeEvent(input$clear, {
      clear_sel()
      leafletProxy("map") |> clearGroup("Selected") |>
        removeDrawToolbar(clearFeatures = TRUE) |> add_draw_toolbar()
    })

    # Short guide to the map's tools
    observeEvent(input$help, showModal(modalDialog(
      title = span(icon("lightbulb"), " How to use the map"),
      tags$ol(class = "map-guide",
        tags$li(strong("Pick a building or an area. "),
                "Click a circle for one building, or use the drawing tools on the left of the ",
                "map to draw a shape around several. The figures beside the map, and the ",
                "charts and tables below it, then show just that selection."),
        tags$li(strong("Use it everywhere (optional). "),
                "With 'Apply map selection to all pages' on, the Search and Market pages ",
                "also show only your selection."),
        tags$li(strong("Compare areas. "),
                "Press 'Save this area', then pick another building or area and save it too ",
                "(up to ", max_areas, "). 'Compare saved areas', just below the map, shows them ",
                "side by side. Rename (pencil) or remove (cross) them in the panel."),
        tags$li(strong("Shortlist what you like. "),
                "Tick rows in 'Projects in the map selection' and press 'Add to shortlist'."),
        tags$li(strong("See more. "),
                "Switch off 'Only buildings within my budget' to see every building, change ",
                "what the colours show, or use the layers button (bottom left of the map) for ",
                "MRT/LRT stations, hospitals and the heatmap.")),
      easyClose = TRUE, footer = modalButton("Got it")
    )))

    # ---- Saved areas: up to max_areas, compared under the map ----
    observeEvent(input$save_area, {
      if (is.null(sel$ids)) {
        showNotification("Click a building or draw an area on the map first.", type = "warning")
        return()
      }
      a <- add_area(areas(), sel$ids, sel$label)
      if (is.null(a)) {
        showNotification(paste0("You can save up to ", max_areas, " areas. Remove one first ",
                                "(the cross next to it)."), type = "warning")
        return()
      }
      areas(a)
      showNotification(paste0("Saved: ", sel$label, ". ",
                              if (length(a) < 2) "Save one more to compare them below the map."
                              else "See 'Compare saved areas' below the map."),
                       type = "message")
    })
    observe(updateActionButton(session, "save_area", disabled = length(areas()) >= max_areas))
    observeEvent(input$remove_area, {
      areas(Filter(function(x) x$id != input$remove_area, areas()))
    })
    renaming <- reactiveVal(NULL)
    observeEvent(input$rename_area, {
      a <- Filter(function(x) x$id == input$rename_area, areas())
      req(length(a) == 1)
      renaming(a[[1]]$id)
      showModal(modalDialog(
        title = "Rename this area",
        textInput(session$ns("rename_to"), NULL, value = a[[1]]$name, width = "100%"),
        easyClose = TRUE,
        footer = tagList(modalButton("Cancel"),
                         actionButton(session$ns("rename_ok"), "Rename", class = "btn-primary"))))
    })
    observeEvent(input$rename_ok, {
      name <- trimws(input$rename_to)
      if (nzchar(name)) {
        areas(lapply(areas(), function(x) {
          if (x$id == renaming()) x$name <- substr(name, 1, 80)
          x
        }))
      }
      removeModal()
    })
    output$saved_list <- renderUI({
      a <- areas()
      if (length(a) == 0) {
        return(div(class = "small text-muted",
                   "Save a building or a drawn area to compare it with others below the map."))
      }
      send <- function(input_id, id) sprintf(
        "Shiny.setInputValue('%s', %d, {priority: 'event'}); return false;", session$ns(input_id), id)
      tagList(
        lapply(a, function(x) div(class = "saved-area-row",
          span(class = "saved-area-dot", style = paste0("background:", x$colour)),
          span(class = "saved-area-name flex-fill", x$name),
          tags$a(href = "#", class = "saved-area-edit", title = "Rename",
                 `aria-label` = paste("Rename", x$name), onclick = send("rename_area", x$id),
                 icon("pen")),
          tags$a(href = "#", class = "saved-area-remove", title = "Remove",
                 `aria-label` = paste("Remove", x$name), onclick = send("remove_area", x$id),
                 icon("xmark")))),
        if (length(a) == 1) div(class = "small text-muted mt-1", "Save one more to compare.")
      )
    })
    # Kept in this browser (settings.js), like My filters. Load once, then save
    # every change; a "Save search" link's areas win over the stored ones
    areas_loaded <- reactiveVal(FALSE)
    observeEvent(input$stored_areas, once = TRUE, {
      if (!areas_loaded() && length(areas()) == 0 && input$stored_areas != "none") {
        areas(areas_from_json(input$stored_areas))
      }
      areas_loaded(TRUE)
    })
    observeEvent(areas(), {
      if (areas_loaded()) {
        session$sendCustomMessage("rentlens-save-areas", as.character(areas_to_json(areas())))
      }
    }, ignoreInit = TRUE)
    mod_saved_areas_server("areas", area_data, areas, filters)

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
        addLegend("bottomright", pal = pal, values = vals, layerId = "legend", opacity = 0.8,
                  title = names(metric_choices)[metric_choices == m],
                  labFormat = if (m == "budget") labelFormat()
                              else labelFormat(prefix = "S$", big.mark = ","))
    })

    # The renter's places that matter
    observe({
      req(map_ready())
      leafletProxy("map") |> update_my_places(filters$places())
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

    # Outline each saved area's buildings in its colour
    observe({
      req(map_ready())
      p <- leafletProxy("map") |> clearGroup("Saved areas")
      for (x in areas()) {
        s <- map_locations() |> filter(loc_id %in% x$locs)
        p <- addCircleMarkers(p, data = s, lng = ~lng, lat = ~lat,
                              group = "Saved areas", radius = 12, fill = FALSE,
                              color = x$colour, weight = 2,
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
      rl_datatable(contracts(), page_length = 15, selection = "none", search = "rental") |>
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
      state$values$areas     <- as.character(areas_to_json(areas()))
    })
    onRestore(function(state) {
      if (length(state$values$sel_ids) > 0) {
        sel$ids   <- as.integer(unlist(state$values$sel_ids))
        sel$label <- unlist(state$values$sel_label)
      }
      if (length(state$values$areas)) {
        areas(areas_from_json(state$values$areas))
        areas_loaded(TRUE)
      }
    })

    list(map_data = map_data)
  })
}

#' An empty map selection (see mod_map_server())
#' @noRd
new_map_selection <- function() reactiveValues(ids = NULL, label = NULL, apply = TRUE)
