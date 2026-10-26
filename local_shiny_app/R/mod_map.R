#' "Map" tab: where are the rentals?
#'
#' Click a circle to inspect one building, or draw a shape to summarise an
#' area; the value boxes and table follow that selection.
#'
#' @param filters The list returned by mod_filters_server().
#' @param shortlist The shared shortlist reactiveVal.
#' @noRd
mod_map_ui <- function(id) {
  ns <- NS(id)
  rentals <- rentals_data()
  geo_choices <- sort(unique(rentals$geo_match[!is.na(rentals$loc_id)]))

  tagList(
    layout_columns(
      col_widths = c(4, 4, 4),
      radioButtons(ns("metric"), "Colour circles by", metric_choices, inline = TRUE),
      selectizeInput(ns("geo_match"), "Geocode quality", geo_choices,
                     selected = grep("^Exact|^Good", geo_choices, value = TRUE),
                     multiple = TRUE),
      p(class = "text-muted mt-4",
        format(sum(is.na(rentals$loc_id)), big.mark = ","),
        " rentals have no coordinates and are not mapped.")
    ),
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Click a circle to inspect one building, or draw a shape (left toolbar) to summarise an area",
        actionButton(ns("clear"), "Clear selection", class = "btn-sm")
      ),
      leafletOutput(ns("map"), height = 600),
      card_footer("Circle size = number of rentals. Uses all sidebar filters and time period.")
    ),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("Rentals", textOutput(ns("vb_n")), textOutput(ns("sel_label")), theme = "primary"),
      value_box("Projects", textOutput(ns("vb_proj")), theme = "secondary"),
      value_box("Median rent", textOutput(ns("vb_rent")), theme = "secondary"),
      value_box("Median S$ psf", textOutput(ns("vb_psf")), theme = "secondary")
    ),
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Projects in the map selection",
        actionButton(ns("add_short"), "Add selected to shortlist", class = "btn-sm btn-success")
      ),
      DTOutput(ns("table"), fill = FALSE),
      card_footer("Click rows to select them, then add them to your shortlist.")
    )
  )
}

#' @noRd
mod_map_server <- function(id, filters, shortlist) {
  moduleServer(id, function(input, output, session) {
    # Leaflet adds its own inputs (map_bounds, map_marker_click, map_draw_*, ...)
    observe(setBookmarkExclude(c(dt_bookmark_inputs("table"), "add_short", "clear",
                                 grep("^map_", names(input), value = TRUE))))

    # Mapped rentals: the market narrowed to chosen projects (like
    # filters$selection(), but without validate() so the map can be empty)
    map_data <- reactive({
      d <- filters$market()
      if (length(filters$projects())) d <- d[d$project %in% filters$projects(), ]
      d[!is.na(d$loc_id) & d$geo_match %in% input$geo_match, ]
    })

    loc_summary <- reactive(summarise_locations(map_data()))

    # Selection: a clicked building, a drawn area, or nothing (= everything mapped)
    sel <- reactiveValues(ids = NULL, label = NULL)

    observeEvent(input$map_marker_click, {
      id <- input$map_marker_click$id
      req(!is.null(id), id != "")
      l <- loc_summary() |> filter(loc_id == as.integer(id))
      sel$ids <- as.integer(id); sel$label <- l$project[1]
    })
    observeEvent(input$map_draw_new_feature, {
      shape <- drawn_to_sf(input$map_draw_new_feature)
      locs_sf <- map_locations_sf()
      sel$ids <- locs_sf$loc_id[lengths(st_intersects(locs_sf, shape)) > 0]
      sel$label <- sprintf("Drawn area (%d buildings)", length(sel$ids))
    })
    clear_sel <- function() { sel$ids <- NULL; sel$label <- NULL }
    observeEvent(input$map_draw_deleted_features, clear_sel())
    observeEvent(input$clear, {
      clear_sel()
      leafletProxy("map") |> clearGroup("Selected") |>
        removeDrawToolbar(clearFeatures = TRUE) |> add_draw_toolbar()
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
      vals <- d[[m]]
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
                  labFormat = labelFormat(prefix = "S$", big.mark = ","))
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
      datatable(s, rownames = FALSE, selection = "multiple",
                options = list(pageLength = 10, order = list(), scrollX = TRUE)) |>
        format_money_cols(s)
    })

    observeEvent(input$add_short, {
      add_to_shortlist(shortlist, table_data(), input$table_rows_selected)
    })
  })
}
