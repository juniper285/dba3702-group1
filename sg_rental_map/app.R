## =====================================================================
##  SG Rental Map: a viscover-style explorer for the URA private
##  residential rental contracts (URA_Union_Cleaned_with_Address.xlsx)
##
##  Map      : one circle per building, coloured by median rent, sized by
##             number of contracts (respects the filters on the left)
##  Click    : click a circle to summarise that building
##  Draw     : draw a polygon / rectangle / circle to summarise an area
##  Download : contracts in the current selection as CSV
##
##  Run with:  shiny::runApp("sg_rental_map")
## =====================================================================

library(shiny); library(shinydashboard)
library(leaflet); library(leaflet.extras)
library(sf); library(dplyr); library(DT); library(plotly)

## ---- Load data (xlsx is converted to a fast .rds cache on first run) ----
xlsx_file <- "data/URA_Union_Cleaned_with_Address.xlsx"
rds_file  <- "data/rentals.rds"

if (!file.exists(rds_file)) {
  message("First run: reading the Excel file (about a minute)...")
  raw <- readxl::read_excel(xlsx_file, guess_max = 50000)
  rentals <- raw |>
    transmute(
      project   = `Project Name`,
      street    = `Street Name`,
      district  = as.integer(`Postal District`),
      type      = `Property Type`,
      bedrooms  = ifelse(is.na(`No of Bedroom`), "Unknown", as.character(`No of Bedroom`)),
      rent      = `Monthly Rent ($)`,
      area_sqft = `Floor Area (SQFT)`,
      sqft      = `Floor Area (SQFT) Midpoint`,
      psf       = `Rental Price (PSF)`,
      month     = as.Date(`Lease Commencement Date`),
      address   = Address,
      lat       = Latitude,
      lng       = Longitude,
      match     = Match_Status
    )
  saveRDS(rentals, rds_file)
}
rentals <- readRDS(rds_file)

n_no_coords <- sum(is.na(rentals$lat))
rentals <- rentals |> filter(!is.na(lat), !is.na(lng))

## One row per map location; contracts keep a loc_id pointing at it
locs <- rentals |> distinct(lat, lng) |> mutate(loc_id = row_number())
rentals <- rentals |> left_join(locs, by = c("lat", "lng"))
locs_sf <- st_as_sf(locs, coords = c("lng", "lat"), crs = 4326, remove = FALSE)

## Fixed colour bins (from the full dataset) so colours mean the same thing
## whatever the filters are
metric_choices <- c("Median rent per sqft ($ psf / month)" = "psf",
                    "Median monthly rent ($)" = "rent")
bins <- list(
  psf  = c(0, 3, 3.5, 4, 4.5, 5, 5.5, 6, 7, Inf),
  rent = c(0, 2500, 3000, 3500, 4000, 5000, 6000, 8000, 12000, Inf)
)
pals <- lapply(bins, function(b) colorBin("RdYlBu", bins = b, reverse = TRUE))

all_months   <- sort(unique(rentals$month))
type_choices <- sort(unique(rentals$type))
bed_choices  <- c(sort(setdiff(unique(rentals$bedrooms), "Unknown")), "Unknown")
match_choices <- sort(unique(rentals$match))
match_default <- grep("^Exact|^Good", match_choices, value = TRUE)

drawn_to_sf <- function(f) {
  if (f$geometry$type == "Point") {             # circle tool
    st_buffer(st_sfc(st_point(unlist(f$geometry$coordinates)), crs = 4326),
              dist = f$properties$radius)
  } else {                                      # polygon / rectangle
    xy <- matrix(unlist(f$geometry$coordinates), ncol = 2, byrow = TRUE)
    st_sfc(st_polygon(list(xy)), crs = 4326)
  }
}
fmt_money <- function(x, d = 0) paste0("$", formatC(x, format = "f", digits = d, big.mark = ","))

## ---- UI ----
ui <- dashboardPage(
  dashboardHeader(title = "SG Rental Map", titleWidth = 260),
  dashboardSidebar(
    width = 260,
    div(style = "padding: 0 12px",
      sliderInput("months", "Lease commencement",
                  min = min(all_months), max = max(all_months),
                  value = c(min(all_months), max(all_months)),
                  timeFormat = "%b %Y", step = 31),
      checkboxGroupInput("type", "Property type", type_choices, selected = type_choices),
      checkboxGroupInput("beds", "Bedrooms", bed_choices, selected = bed_choices, inline = TRUE),
      selectInput("district", "Postal district (blank = all)",
                  sort(unique(rentals$district)), multiple = TRUE),
      checkboxGroupInput("match", "Geocode quality", match_choices, selected = match_default),
      radioButtons("metric", "Colour circles by", metric_choices),
      p(style = "font-size: 11px; opacity: .8",
        sprintf("%s contracts have no coordinates and are not shown.",
                format(n_no_coords, big.mark = ",")))
    )
  ),
  dashboardBody(
    tags$style(".content-wrapper { background: #f4f6f8 }"),
    fluidRow(
      box(width = 12,
          p("Click a circle to inspect one building, or draw a shape (left toolbar) to",
            "summarise an area. The summaries below follow your selection.",
            actionLink("clear", "Clear selection")),
          leafletOutput("map", height = 620))
    ),
    fluidRow(
      valueBoxOutput("vb_n", width = 3), valueBoxOutput("vb_proj", width = 3),
      valueBoxOutput("vb_rent", width = 3), valueBoxOutput("vb_psf", width = 3)
    ),
    fluidRow(
      box(title = textOutput("sel_title"), status = "primary", solidHeader = TRUE, width = 7,
          plotlyOutput("trend", height = 330)),
      box(title = "By bedrooms", status = "primary", solidHeader = TRUE, width = 5,
          DTOutput("tbl_beds"))
    ),
    fluidRow(
      box(title = "By project", status = "warning", solidHeader = TRUE, width = 12,
          collapsible = TRUE, DTOutput("tbl_proj")),
      box(title = "Contracts", status = "warning", solidHeader = TRUE, width = 12,
          collapsible = TRUE, collapsed = TRUE,
          DTOutput("tbl_raw"), br(),
          downloadButton("dl", "Download selection (CSV)"))
    )
  )
)

## ---- Server ----
server <- function(input, output, session) {

  ## Contracts passing the sidebar filters
  filtered <- reactive({
    d <- rentals |>
      filter(month >= input$months[1], month <= input$months[2],
             type %in% input$type, bedrooms %in% input$beds, match %in% input$match)
    if (length(input$district)) d <- d |> filter(district %in% as.integer(input$district))
    d
  })

  ## One summary row per location for the map
  loc_summary <- reactive({
    filtered() |>
      group_by(loc_id, lat, lng) |>
      summarise(project = paste(head(unique(project), 3), collapse = " / "),
                address = first(address), district = first(district),
                n = n(), rent = median(rent), psf = median(psf, na.rm = TRUE),
                .groups = "drop")
  })

  ## Selection: a clicked building, a drawn area, or nothing (= everything filtered)
  sel <- reactiveValues(ids = NULL, label = NULL)

  observeEvent(input$map_marker_click, {
    id <- input$map_marker_click$id
    req(!is.null(id), id != "")
    l <- loc_summary() |> filter(loc_id == as.integer(id))
    sel$ids <- as.integer(id); sel$label <- l$project[1]
  })
  observeEvent(input$map_draw_new_feature, {
    shape <- drawn_to_sf(input$map_draw_new_feature)
    sel$ids <- locs_sf$loc_id[lengths(st_intersects(locs_sf, shape)) > 0]
    sel$label <- sprintf("Drawn area (%d buildings)", length(sel$ids))
  })
  clear_sel <- function() { sel$ids <- NULL; sel$label <- NULL }
  observeEvent(input$map_draw_deleted_features, clear_sel())
  observeEvent(input$clear, {
    clear_sel()
    leafletProxy("map") |> clearGroup("Selected") |> removeDrawToolbar(clearFeatures = TRUE) |>
      addDrawToolbar(polylineOptions = FALSE, markerOptions = FALSE,
                     circleMarkerOptions = FALSE, singleFeature = TRUE,
                     editOptions = editToolbarOptions())
  })

  selected <- reactive({
    d <- filtered()
    if (!is.null(sel$ids)) d <- d |> filter(loc_id %in% sel$ids)
    d
  })

  ## Base map (drawn once)
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addProviderTiles(providers$CartoDB.Positron, group = "Light") |>
      addProviderTiles(providers$OneMapSG.Default, group = "OneMap") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
      addLayersControl(baseGroups = c("Light", "OneMap", "Satellite"),
                       overlayGroups = c("Buildings", "Heatmap"),
                       options = layersControlOptions(collapsed = TRUE),
                       position = "bottomleft") |>
      hideGroup("Heatmap") |>
      addSearchOSM() |>
      addDrawToolbar(polylineOptions = FALSE, markerOptions = FALSE,
                     circleMarkerOptions = FALSE, singleFeature = TRUE,
                     editOptions = editToolbarOptions()) |>
      setView(103.83, 1.33, zoom = 12)
  })

  ## Redraw circles + legend whenever filters or the colour metric change
  observe({
    d <- loc_summary(); m <- input$metric; pal <- pals[[m]]
    vals <- d[[m]]
    lab <- sprintf(
      "<b>%s</b><br>%s<br>District %s<br>%s contracts<br>Median rent %s<br>Median %s psf",
      d$project, ifelse(is.na(d$address), "", d$address), d$district,
      format(d$n, big.mark = ","), fmt_money(d$rent), fmt_money(d$psf, 2)) |>
      lapply(HTML)
    leafletProxy("map", data = d) |>
      clearGroup("Buildings") |> clearGroup("Heatmap") |>
      addCircleMarkers(lng = ~lng, lat = ~lat, layerId = ~as.character(loc_id),
                       group = "Buildings",
                       radius = ~pmin(11, 2 + sqrt(n) / 3),
                       fillColor = pal(vals), fillOpacity = 0.8,
                       color = "#333", weight = 0.5, label = lab) |>
      addHeatmap(lng = ~lng, lat = ~lat, intensity = ~n, group = "Heatmap",
                 blur = 20, radius = 12, max = quantile(d$n, 0.9)) |>
      addLegend("bottomright", pal = pal, values = vals, layerId = "legend",
                title = names(metric_choices)[metric_choices == m],
                labFormat = labelFormat(prefix = "$", big.mark = ","))
  })

  ## Outline the selected buildings
  observe({
    ids <- sel$ids
    p <- leafletProxy("map") |> clearGroup("Selected")
    if (!is.null(ids)) {
      s <- locs |> filter(loc_id %in% ids)
      p |> addCircleMarkers(data = s, lng = ~lng, lat = ~lat, group = "Selected",
                            radius = 9, fill = FALSE, color = "#000", weight = 2.5,
                            options = pathOptions(interactive = FALSE))
    }
  })

  ## ---- Summaries ----
  output$sel_title <- renderText({
    paste("Median rent psf by month:", if (is.null(sel$label)) "all filtered contracts" else sel$label)
  })

  vb <- function(value, subtitle, icon, color)
    valueBox(value, subtitle, icon = icon(icon), color = color)
  med <- function(x, d = 0) if (length(x) && any(!is.na(x))) fmt_money(median(x, na.rm = TRUE), d) else "-"
  output$vb_n    <- renderValueBox(vb(format(nrow(selected()), big.mark = ","), "Contracts", "file-signature", "blue"))
  output$vb_proj <- renderValueBox(vb(format(n_distinct(selected()$project), big.mark = ","), "Projects", "building", "purple"))
  output$vb_rent <- renderValueBox(vb(med(selected()$rent), "Median monthly rent", "dollar-sign", "green"))
  output$vb_psf  <- renderValueBox(vb(med(selected()$psf, 2), "Median rent psf", "ruler-combined", "orange"))

  output$trend <- renderPlotly({
    d <- selected()
    validate(need(nrow(d) > 0, "No contracts in this selection."))
    by_bed <- d |> filter(!is.na(psf)) |>
      group_by(month, bedrooms) |> summarise(psf = median(psf), n = n(), .groups = "drop") |>
      filter(n >= 3)
    overall <- d |> filter(!is.na(psf)) |> group_by(month) |>
      summarise(psf = median(psf), n = n(), .groups = "drop")
    plot_ly() |>
      add_lines(data = by_bed, x = ~month, y = ~psf, color = ~paste(bedrooms, "BR"),
                text = ~paste(n, "contracts"), opacity = 0.6) |>
      add_lines(data = overall, x = ~month, y = ~psf, name = "All",
                line = list(color = "black", width = 3), text = ~paste(n, "contracts")) |>
      layout(xaxis = list(title = ""), yaxis = list(title = "$ psf / month"),
             legend = list(orientation = "h", y = -0.15))
  })

  output$tbl_beds <- renderDT({
    validate(need(nrow(selected()) > 0, "No contracts in this selection."))
    selected() |> group_by(Bedrooms = bedrooms) |>
      summarise(Contracts = n(), `Median rent` = median(rent),
                `Median sqft` = median(sqft, na.rm = TRUE),
                `Median psf` = round(median(psf, na.rm = TRUE), 2), .groups = "drop") |>
      datatable(rownames = FALSE, options = list(dom = "t", pageLength = 20)) |>
      formatCurrency(c("Median rent", "Median psf"), digits = c(0, 2)) |>
      formatRound("Median sqft", 0)
  })

  output$tbl_proj <- renderDT({
    validate(need(nrow(selected()) > 0, "No contracts in this selection."))
    selected() |> group_by(Project = project, Street = street, District = district, Type = type) |>
      summarise(Contracts = n(), `Median rent` = median(rent),
                `Median psf` = round(median(psf, na.rm = TRUE), 2),
                `Latest lease` = max(month), .groups = "drop") |>
      mutate(`Latest lease` = format(`Latest lease`, "%b %Y")) |>
      arrange(desc(Contracts)) |>
      datatable(rownames = FALSE, filter = "top") |>
      formatCurrency(c("Median rent", "Median psf"), digits = c(0, 2))
  })

  output$tbl_raw <- renderDT({
    selected() |> arrange(desc(month)) |>
      transmute(Month = format(month, "%Y-%m"), Project = project, Street = street,
                District = district, Type = type, Bedrooms = bedrooms,
                `Area (sqft)` = area_sqft, Rent = rent, PSF = round(psf, 2)) |>
      datatable(rownames = FALSE, filter = "top", options = list(pageLength = 15))
  }, server = TRUE)

  output$dl <- downloadHandler(
    filename = function() paste0("sg_rentals_selection-", Sys.Date(), ".csv"),
    content  = function(f) write.csv(selected() |> select(-loc_id), f, row.names = FALSE))
}

shinyApp(ui, server)
