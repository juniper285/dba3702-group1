# ==================================================================
#  Rental Intelligence App
#  Tabs: Budget check | Within budget | Map | Price trends |
#        Compare areas | Shortlist | Transactions | About
#
#  Click "Run App". First launch reads the Excel file (~30 seconds) and
#  saves a fast copy (rentals.rds). If you change the Excel file, delete
#  rentals.rds so the app re-reads it.
#
#  One-time install (in the Console):
#  install.packages(c("shiny", "bslib", "DT", "readxl", "ggplot2", "dplyr"))
# ==================================================================

library(shiny)
library(bslib)
library(DT)
library(readxl)
library(ggplot2)
library(dplyr)


# ---- 1. LOAD DATA (runs once when the app starts) -----------------

if (file.exists("rentals.rds")) {
  rentals <- readRDS("rentals.rds")
} else {
  raw <- read_excel("/Users/gwentyn/RCourse/URA (Cleaned).xlsx", .name_repair = "unique_quiet")
  # Rename columns to short names without spaces or symbols.
  # as.numeric() guards against numbers stored as text in Excel.
  rentals <- data.frame(
    project     = raw[["Project Name"]],
    street      = raw[["Street Name"]],
    district_no = as.integer(raw[["Postal District"]]),
    type        = raw[["Property Type"]],
    bedrooms    = suppressWarnings(as.numeric(raw[["No of Bedroom"]])),  # blanks -> NA
    rent        = as.numeric(raw[["Monthly Rent ($)"]]),
    area_band   = raw[["Floor Area (SQFT)"]],
    area_sqft   = as.numeric(raw[["Floor Area (SQFT) Midpoint"]]),
    psf         = as.numeric(raw[["Rental Price (PSF)"]]),
    lease_date  = as.Date(raw[["Lease Commencement Date"]])
  )
  saveRDS(rentals, "rentals.rds")
}

# Plain-English district names (international renters don't know "D10")
district_names <- c(
  "01" = "Raffles Place, Marina",      "02" = "Tanjong Pagar, Anson",
  "03" = "Queenstown, Tiong Bahru",    "04" = "Telok Blangah, Harbourfront",
  "05" = "Pasir Panjang, Clementi",    "06" = "City Hall, Beach Road",
  "07" = "Bugis, Golden Mile",         "08" = "Little India, Farrer Park",
  "09" = "Orchard, River Valley",      "10" = "Bukit Timah, Holland, Tanglin",
  "11" = "Novena, Newton, Thomson",    "12" = "Balestier, Toa Payoh",
  "13" = "Macpherson, Braddell",       "14" = "Geylang, Eunos",
  "15" = "Katong, Joo Chiat, Marine Parade", "16" = "Bedok, Upper East Coast",
  "17" = "Loyang, Changi",             "18" = "Tampines, Pasir Ris",
  "19" = "Serangoon Garden, Hougang, Punggol", "20" = "Bishan, Ang Mo Kio",
  "21" = "Upper Bukit Timah, Ulu Pandan", "22" = "Jurong, Boon Lay",
  "23" = "Bukit Panjang, Choa Chu Kang, Hillview", "24" = "Lim Chu Kang, Tengah",
  "25" = "Kranji, Woodlands",          "26" = "Upper Thomson, Springleaf",
  "27" = "Yishun, Sembawang",          "28" = "Seletar, Yio Chu Kang"
)

# Extra columns the app uses
rentals <- rentals |>
  mutate(
    district = paste0("D", sprintf("%02d", district_no), " ",
                      district_names[sprintf("%02d", district_no)]),
    bed_group = case_when(
      is.na(bedrooms) ~ "Unknown",
      bedrooms >= 5   ~ "5+",
      TRUE            ~ as.character(bedrooms)
    ),
    quarter = as.Date(cut(lease_date, "quarter"))
  )

date_min   <- min(rentals$lease_date)
date_max   <- max(rentals$lease_date)
bed_levels <- c("1", "2", "3", "4", "5+", "Unknown")
area_max   <- 4000   # slider's top end means "4,000 sqft and above"


# ---- 2. LOCATION DATA (optional, for distances) --------------------
# Distances appear automatically once these CSV files are in this folder:
#   project_coords.csv : columns project, lat, lng   (one row per project)
#   mrt_stations.csv   : columns station, lat, lng   (one row per station)
# Until then, the distance columns are simply left out.

coords <- if (file.exists("project_coords.csv")) read.csv("project_coords.csv") else NULL
mrt    <- if (file.exists("mrt_stations.csv"))   read.csv("mrt_stations.csv")   else NULL

# Straight-line distance in km between two points
haversine_km <- function(lat1, lng1, lat2, lng2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 +
       cos(lat1 * r) * cos(lat2 * r) * sin((lng2 - lng1) * r / 2)^2
  6371 * 2 * asin(sqrt(a))
}

# Pre-compute each project's nearest MRT station
if (!is.null(coords) && !is.null(mrt)) {
  coords$mrt_station <- NA_character_
  coords$mrt_km      <- NA_real_
  for (i in seq_len(nrow(coords))) {
    d <- haversine_km(coords$lat[i], coords$lng[i], mrt$lat, mrt$lng)
    if (all(is.na(d))) next
    j <- which.min(d)
    coords$mrt_station[i] <- mrt$station[j]
    coords$mrt_km[i]      <- round(d[j], 1)
  }
}

# Preferred destinations (add your own: "Name" = c(latitude, longitude))
destinations <- list(
  "NUS (Kent Ridge)"       = c(1.2966, 103.7764),
  "NTU"                    = c(1.3483, 103.6831),
  "SMU"                    = c(1.2963, 103.8502),
  "SUTD"                   = c(1.3413, 103.9638),
  "Raffles Place (CBD)"    = c(1.2840, 103.8514),
  "one-north"              = c(1.2995, 103.7873),
  "Changi Business Park"   = c(1.3343, 103.9625)
)

# Add distance columns to a project-level table (if coordinates exist)
add_distance <- function(s, destination) {
  if (is.null(coords)) return(s)
  keep <- intersect(c("project", "lat", "lng", "mrt_station", "mrt_km"), names(coords))
  s <- left_join(s, coords[, keep], by = "project")
  dest <- destinations[[destination]]
  s$dest_km <- round(haversine_km(s$lat, s$lng, dest[1], dest[2]), 1)
  select(s, -lat, -lng)
}


# ---- 3. HELPER FUNCTIONS -------------------------------------------

money <- function(x) paste0("S$", formatC(round(x), format = "d", big.mark = ","))

quarter_label <- function(d) {
  paste0(format(d, "%Y"), " Q", (as.integer(format(d, "%m")) - 1) %/% 3 + 1)
}

# Keep only the most recent N months of a data set
latest_months <- function(d, months) {
  if (nrow(d) == 0) return(d)
  cutoff <- seq(max(d$lease_date), by = paste0("-", months, " months"), length.out = 2)[2]
  d[d$lease_date > cutoff, ]
}

# Standard rent summary used across the app.
# budget = c(min, max); "within" = share of rentals inside that range
summarise_rent <- function(d, budget = c(NA, NA)) {
  summarise(d,
    median = median(rent),
    p25    = unname(quantile(rent, 0.25)),
    p75    = unname(quantile(rent, 0.75)),
    psf    = median(psf, na.rm = TRUE),
    n      = n(),
    within = mean(rent >= budget[1] & rent <= budget[2]),
    .groups = "drop"
  )
}

# Most common value (used for the typical floor-area band)
most_common <- function(x) names(which.max(table(x)))

# Link that searches property portals for a project's current listings
listing_link <- function(project) {
  q <- utils::URLencode(paste0('"', project, '" for rent site:propertyguru.com.sg OR site:99.co'),
                        reserved = TRUE)
  paste0('<a href="https://www.google.com/search?q=', q, '" target="_blank">Find listings</a>')
}

# Friendly column names for tables
nice_names <- c(
  project = "Project", district = "District", bed_group = "Bedrooms",
  size = "Typical size (sqft)", median = "Median rent", p25 = "25th percentile",
  p75 = "75th percentile", psf = "Median S$ psf", n = "Recent rentals",
  within = "Share within budget", last = "Last rented", fits = "Budget fit",
  dest_km = "Distance to destination (km)", mrt_station = "Nearest MRT",
  mrt_km = "Distance to MRT (km)", listings = "Listings"
)
prettify <- function(s) {
  names(s) <- ifelse(names(s) %in% names(nice_names), nice_names[names(s)], names(s))
  s
}
format_money_cols <- function(dt, s) {
  money_cols <- intersect(c("Median rent", "25th percentile", "75th percentile"), names(s))
  dt <- formatCurrency(dt, money_cols, "S$", digits = 0)
  if ("Median S$ psf" %in% names(s)) dt <- formatCurrency(dt, "Median S$ psf", "S$", digits = 2)
  if ("Share within budget" %in% names(s)) dt <- formatPercentage(dt, "Share within budget", digits = 0)
  dt
}

app_theme <- bs_theme(version = 5, bootswatch = "flatly")

# DataTable inputs that should not be saved in bookmark links
table_ids <- c("wb_table", "compare_table", "records_table", "short_table")
dt_suffixes <- c("_rows_selected", "_rows_all", "_rows_current", "_row_last_clicked",
                 "_cell_clicked", "_cells_selected", "_columns_selected", "_search",
                 "_search_columns", "_state", "_cell_info")
bookmark_exclude <- c(as.vector(outer(table_ids, dt_suffixes, paste0)),
                      "add_short", "short_remove", "short_clear")


# ---- 4. UI: WHAT THE USER SEES -------------------------------------
# ui is a function(request) so that "Save this search" links can restore it

ui <- function(request) {
  page_navbar(
    title = "[App Title]",
    theme = app_theme,
    fillable = FALSE,   # let each page scroll instead of squeezing content to fit

    # Shared sidebar: every tab reacts to these filters
    sidebar = sidebar(
      width = 320,
      accordion(
        multiple = TRUE,
        open = c("Budget", "Area and project"),
        accordion_panel(
          "Budget",
          sliderInput("budget", "Monthly budget range (S$)", min = 1000, max = 15000,
                      value = c(3000, 5000), step = 100, pre = "S$")
        ),
        accordion_panel(
          "Area and project",
          selectizeInput("district", "Districts", multiple = TRUE,
                         choices = sort(unique(rentals$district)),
                         options = list(placeholder = "All districts")),
          selectizeInput("project", "Residential projects (optional)", multiple = TRUE,
                         choices = NULL,
                         options = list(placeholder = "Type to search projects"))
        ),
        accordion_panel(
          "Property",
          checkboxGroupInput("beds", "Bedrooms", choices = bed_levels,
                             selected = bed_levels, inline = TRUE),
          checkboxGroupInput("type", "Property type",
                             choices = sort(unique(rentals$type)),
                             selected = unique(rentals$type)),
          sliderInput("area", "Floor area (sqft)", min = 0, max = area_max,
                      value = c(0, area_max), step = 100),
          helpText("Top of the slider = 4,000 sqft and above.")
        ),
        accordion_panel(
          "Time period",
          dateRangeInput("period", "Lease start between", start = date_min, end = date_max,
                         min = date_min, max = date_max, format = "M yyyy"),
          selectInput("recent", "Benchmarks use the most recent",
                      choices = c("6 months" = 6, "12 months" = 12, "24 months" = 24),
                      selected = 12)
        ),
        accordion_panel(
          "Location",
          selectInput("destination", "Your university or workplace", names(destinations)),
          helpText("Distances appear once project coordinates are added.")
        )
      ),
      bookmarkButton("Save this search", title = "Get a link that restores your filters and shortlist")
    ),

    # Tab 1: Is my budget realistic?
    nav_panel(
      "Budget check",
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box("Median rent", textOutput("vb_median"),
                  "of comparable recent rentals", theme = "primary"),
        value_box("Typical range", textOutput("vb_range"),
                  "middle 50% of rents", theme = "secondary"),
        value_box("Comparable rentals", textOutput("vb_n"),
                  textOutput("vb_n_note", inline = TRUE), theme = "secondary"),
        uiOutput("verdict_box")
      ),
      card(
        card_header("How your budget compares with recent comparable rents"),
        plotOutput("budget_plot", height = 380),
        card_footer("Green band = typical range (middle 50%). Red band = your budget range.")
      )
    ),

    # Tab 2: What can I get within my budget?
    nav_panel(
      "Within budget",
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box("Project and bedroom options", textOutput("wb_options"), theme = "success"),
        value_box("Projects", textOutput("wb_projects"), theme = "secondary"),
        value_box("Districts", textOutput("wb_districts"), theme = "secondary"),
        value_box("Bedroom types", textOutput("wb_beds_n"), textOutput("wb_beds"), theme = "secondary")
      ),
      card(
        card_header("Where your budget goes: options by district and bedrooms"),
        plotOutput("wb_plot", height = 420)
      ),
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Projects with a median rent inside your budget range",
          actionButton("add_short", "Add selected to shortlist", class = "btn-sm btn-success")
        ),
        layout_columns(
          col_widths = c(4, 8),
          numericInput("wb_min_n", "Minimum recent rentals per row", value = 3, min = 1),
          uiOutput("distance_note")
        ),
        DTOutput("wb_table", fill = FALSE),
        card_footer("Click rows to select them, then add them to your shortlist. ",
                    "Click a column header to sort; use Search to filter.")
      )
    ),

    # Tab 3: Map (placeholder, to be built)
    nav_panel(
      "Map",
      card(
        card_header("Map"),
        # Put leafletOutput("map", height = 600) here when the map is ready
        div(style = "height: 600px;")
      )
    ),

    # Tab 4: How have prices changed?
    nav_panel(
      "Price trends",
      card(
        card_header(
          "Median monthly rent by quarter",
          checkboxInput("split_district", "Show selected districts separately",
                        value = FALSE)
        ),
        plotOutput("trend_plot", height = 420),
        card_footer(textOutput("trend_summary"))
      )
    ),

    # Tab 5: Which areas fit my budget?
    nav_panel(
      "Compare areas",
      layout_columns(
        col_widths = c(4, 4, 4),
        radioButtons("compare_by", "Compare by", c("District", "Project"), inline = TRUE),
        numericInput("min_n", "Minimum recent rentals per row", value = 5, min = 1),
        p(class = "text-muted mt-4",
          "Highlighted rows have a median rent within your budget range.")
      ),
      conditionalPanel(
        "input.compare_by == 'District'",
        card(card_header("Median rent by district (lines = typical range)"),
             plotOutput("district_plot", height = 650))
      ),
      card(card_header("Comparison table"), DTOutput("compare_table", fill = FALSE))
    ),

    # Tab 6: The user's saved options
    nav_panel(
      "Shortlist",
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Your shortlist",
          div(
            actionButton("short_remove", "Remove selected", class = "btn-sm"),
            actionButton("short_clear", "Clear all", class = "btn-sm"),
            downloadButton("short_download", "Download CSV", class = "btn-sm")
          )
        ),
        DTOutput("short_table", fill = FALSE),
        card_footer("Figures use each option's rentals in the most recent period, ",
                    "regardless of the sidebar filters, so your shortlist stays comparable. ",
                    "Use 'Save this search' in the sidebar to keep a link to it.")
      ),
      card(
        card_header("Shortlist at a glance"),
        plotOutput("short_plot", height = 420),
        card_footer("Dot = median rent, line = typical range. Red band = your budget range.")
      )
    ),

    # Tab 7: The underlying records
    nav_panel(
      "Transactions",
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          "Rental transactions matching your filters",
          downloadButton("download", "Download CSV", class = "btn-sm")
        ),
        DTOutput("records_table", fill = FALSE)
      )
    ),

    # Tab 8: Methodology and disclaimer
    nav_panel(
      "About",
      card(
        h4("How to read this app"),
        p("All figures come from URA private residential rental contracts: ",
          format(nrow(rentals), big.mark = ","), " records from ",
          format(date_min, "%b %Y"), " to ", format(date_max, "%b %Y"), "."),
        p(strong("Comparable rentals"), " are the transactions matching your filters ",
          "within the most recent period you choose in the sidebar."),
        p(strong("Typical range"), " is the 25th to 75th percentile: half of ",
          "comparable rentals fall inside it."),
        p(strong("Within budget"), " lists each project and bedroom type whose median ",
          "recent rent falls inside your budget range."),
        p("These are historical transactions, not live listings. Use them to check ",
          "whether rents advertised on property portals are reasonable, then use ",
          "'Find listings' to search for available units."),
        p("URA reports floor area as a range; the midpoint is used for rent per sqft. ",
          "Records with no bedroom count are shown as 'Unknown'. Distances are ",
          "straight-line estimates.")
      )
    )
  )
}


# ---- 5. SERVER: WHAT THE APP DOES ----------------------------------

server <- function(input, output, session) {

  setBookmarkExclude(bookmark_exclude)

  # Project list follows the chosen districts
  observeEvent(input$district, {
    d <- if (length(input$district)) rentals[rentals$district %in% input$district, ] else rentals
    updateSelectizeInput(session, "project", server = TRUE,
                         choices  = sort(unique(d$project)),
                         selected = intersect(input$project, d$project))
  }, ignoreNULL = FALSE)

  # A) The market: all sidebar filters EXCEPT the project choice.
  market <- reactive({
    d <- rentals
    if (length(input$district)) d <- d[d$district %in% input$district, ]
    # which() drops rows with no floor area instead of returning NA rows
    d[which(d$bed_group %in% input$beds &
      d$type %in% input$type &
      d$area_sqft >= input$area[1] &
      (d$area_sqft <= input$area[2] | input$area[2] >= area_max) &
      d$lease_date >= input$period[1] &
      d$lease_date <= input$period[2]), ]
  })

  # B) The user's selection: the market narrowed to chosen projects
  selection <- reactive({
    d <- market()
    if (length(input$project)) d <- d[d$project %in% input$project, ]
    validate(need(nrow(d) > 0,
      "No rentals match these filters. Try widening the filters in the sidebar."))
    d
  })

  # C) Comparable rentals: the selection, most recent months only
  comparables <- reactive(latest_months(selection(), as.numeric(input$recent)))

  # Budget range as c(min, max)
  budget <- reactive(input$budget)


  # ---- Tab 1: Budget check ----
  output$vb_median <- renderText(money(median(comparables()$rent)))

  output$vb_range <- renderText({
    q <- quantile(comparables()$rent, c(0.25, 0.75))
    paste0(money(q[1]), " - ", formatC(round(q[2]), format = "d", big.mark = ","))
  })

  output$vb_n <- renderText(format(nrow(comparables()), big.mark = ","))

  output$vb_n_note <- renderText({
    if (nrow(comparables()) < 10) "Few rentals: treat as a rough guide"
    else paste("in the latest", input$recent, "months")
  })

  output$verdict_box <- renderUI({
    r <- comparables()$rent
    b <- budget()
    q <- quantile(r, c(0.25, 0.75))
    share <- round(100 * mean(r >= b[1] & r <= b[2]))

    if (b[2] < q[1]) {
      verdict <- "Below typical"; colour <- "danger"
      advice  <- "Most comparable units rent for more. Try other districts, fewer bedrooms or a smaller unit."
    } else if (b[1] > q[2]) {
      verdict <- "Above typical"; colour <- "info"
      advice  <- "Most comparable units rent for less than your minimum. You have room to negotiate."
    } else {
      verdict <- "Realistic"; colour <- "success"
      advice  <- "Your range overlaps the typical rents for these filters."
    }
    value_box("Your budget range", verdict,
              paste0(share, "% of comparable rentals fall between ",
                     money(b[1]), " and ", money(b[2]), "."),
              advice, theme = colour)
  })

  output$budget_plot <- renderPlot({
    r <- comparables()$rent
    b <- budget()
    r <- r[r <= quantile(r, 0.99)]          # hide extreme top 1% for readability
    q <- quantile(r, c(0.25, 0.75))

    ggplot(data.frame(rent = r), aes(rent)) +
      annotate("rect", xmin = q[1], xmax = q[2], ymin = -Inf, ymax = Inf,
               fill = "#18bc9c", alpha = 0.15) +
      annotate("rect", xmin = b[1], xmax = b[2], ymin = -Inf, ymax = Inf,
               fill = "#e74c3c", alpha = 0.10) +
      geom_histogram(bins = 40, fill = "#2c3e50", colour = "white") +
      geom_vline(xintercept = b, linetype = "dashed", linewidth = 0.8, colour = "#e74c3c") +
      scale_x_continuous(labels = scales::label_dollar(prefix = "S$")) +
      labs(x = "Monthly rent", y = "Number of rentals") +
      theme_minimal(base_size = 14)
  })


  # ---- Tab 2: Within budget ----
  # One row per project + bedroom type whose recent median rent is in range
  within_data <- reactive({
    b <- budget()
    s <- comparables() |>
      group_by(project, district, bed_group) |>
      summarise(
        size   = most_common(area_band),
        median = median(rent),
        p25    = unname(quantile(rent, 0.25)),
        p75    = unname(quantile(rent, 0.75)),
        psf    = median(psf, na.rm = TRUE),
        n      = n(),
        last   = max(lease_date),
        .groups = "drop"
      ) |>
      filter(median >= b[1], median <= b[2], n >= input$wb_min_n) |>
      arrange(median)
    add_distance(s, input$destination)
  })

  output$wb_options   <- renderText(format(nrow(within_data()), big.mark = ","))
  output$wb_projects  <- renderText(format(n_distinct(within_data()$project), big.mark = ","))
  output$wb_districts <- renderText(n_distinct(within_data()$district))
  output$wb_beds_n    <- renderText(n_distinct(within_data()$bed_group))
  output$wb_beds      <- renderText({
    beds <- intersect(bed_levels, unique(within_data()$bed_group))
    if (length(beds) == 0) "None" else paste("Bedrooms:", paste(beds, collapse = ", "))
  })

  output$distance_note <- renderUI({
    if (is.null(coords)) {
      p(class = "text-muted mt-4",
        "Distance columns will appear here once project coordinates are added.")
    } else {
      p(class = "text-muted mt-4",
        paste("Distances are to", input$destination, "and the nearest MRT station."))
    }
  })

  output$wb_plot <- renderPlot({
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

  output$wb_table <- renderDT({
    s <- within_data() |> mutate(last = format(last, "%b %Y"))
    s <- prettify(s)
    datatable(s, rownames = FALSE, selection = "multiple",
              options = list(pageLength = 15, order = list(), scrollX = TRUE)) |>
      format_money_cols(s)
  })


  # ---- Shortlist (shared by tabs 2 and 6) ----
  shortlist <- reactiveVal(data.frame(project = character(), bed_group = character()))

  observeEvent(input$add_short, {
    rows <- input$wb_table_rows_selected
    if (length(rows) == 0) {
      showNotification("Select one or more rows in the table first.", type = "warning")
      return()
    }
    new <- as.data.frame(within_data()[rows, c("project", "bed_group")])
    shortlist(unique(rbind(shortlist(), new)))
    showNotification(paste(length(rows), "option(s) added to your shortlist."), type = "message")
  })

  short_data <- reactive({
    sl <- shortlist()
    validate(need(nrow(sl) > 0,
      "Your shortlist is empty. In 'Within budget', click rows and press 'Add selected to shortlist'."))
    b <- budget()
    d <- rentals |>
      semi_join(sl, by = c("project", "bed_group")) |>
      filter(lease_date >= input$period[1], lease_date <= input$period[2])
    d <- latest_months(d, as.numeric(input$recent))
    validate(need(nrow(d) > 0, "No recent rentals for your shortlist in this time period."))

    s <- d |>
      group_by(project, district, bed_group) |>
      summarise(
        size   = most_common(area_band),
        median = median(rent),
        p25    = unname(quantile(rent, 0.25)),
        p75    = unname(quantile(rent, 0.75)),
        psf    = median(psf, na.rm = TRUE),
        n      = n(),
        .groups = "drop"
      ) |>
      mutate(fits = case_when(median < b[1] ~ "Below range",
                              median > b[2] ~ "Above range",
                              TRUE          ~ "Within range")) |>
      arrange(median)
    s <- add_distance(s, input$destination)
    s$listings <- listing_link(s$project)
    s
  })

  output$short_table <- renderDT({
    s <- prettify(short_data())
    datatable(s, rownames = FALSE, escape = FALSE, selection = "multiple",
              options = list(pageLength = 25, order = list(), scrollX = TRUE)) |>
      format_money_cols(s) |>
      formatStyle("Budget fit",
                  color = styleEqual(c("Within range", "Below range", "Above range"),
                                     c("#18bc9c", "#3498db", "#e74c3c")),
                  fontWeight = "bold")
  })

  observeEvent(input$short_remove, {
    rows <- input$short_table_rows_selected
    if (length(rows) == 0) {
      showNotification("Select rows in your shortlist to remove.", type = "warning")
      return()
    }
    gone <- short_data()[rows, c("project", "bed_group")]
    shortlist(anti_join(shortlist(), gone, by = c("project", "bed_group")))
  })

  observeEvent(input$short_clear, {
    shortlist(data.frame(project = character(), bed_group = character()))
  })

  output$short_download <- downloadHandler(
    filename = function() "rental_shortlist.csv",
    content  = function(file) {
      write.csv(prettify(select(short_data(), -listings)), file, row.names = FALSE)
    }
  )

  output$short_plot <- renderPlot({
    s <- short_data()
    b <- budget()
    s$label <- paste0(s$project, " (", s$bed_group, " BR)")
    ggplot(s, aes(y = reorder(label, -median))) +
      annotate("rect", xmin = b[1], xmax = b[2], ymin = -Inf, ymax = Inf,
               fill = "#e74c3c", alpha = 0.10) +
      geom_linerange(aes(xmin = p25, xmax = p75), linewidth = 1.2, colour = "grey70") +
      geom_point(aes(x = median), size = 3.5, colour = "#2c3e50") +
      scale_x_continuous(labels = scales::label_dollar(prefix = "S$")) +
      labs(x = "Monthly rent", y = NULL) +
      theme_minimal(base_size = 13)
  })

  # Save and restore the shortlist inside "Save this search" links
  onBookmark(function(state) state$values$shortlist <- shortlist())
  onRestore(function(state) {
    sl <- state$values$shortlist
    if (!is.null(sl) && length(sl$project) > 0) {
      shortlist(data.frame(project = unlist(sl$project), bed_group = unlist(sl$bed_group)))
    }
  })
  onBookmarked(function(url) {
    showBookmarkUrlModal(url)
  })


  # ---- Tab 4: Price trends ----
  trend_data <- reactive({
    d <- selection()
    split_up <- isTRUE(input$split_district) && length(input$district) > 1
    d$group <- if (split_up) d$district else "Your selection"
    d |> group_by(quarter, group) |> summarise_rent() |> filter(n >= 5)
  })

  output$trend_plot <- renderPlot({
    t <- trend_data()
    validate(need(nrow(t) > 0, "Not enough rentals per quarter to draw a trend."))
    one_line <- length(unique(t$group)) == 1
    b <- budget()

    ggplot(t, aes(quarter, median, colour = group, fill = group)) +
      annotate("rect", ymin = b[1], ymax = b[2], xmin = as.Date(-Inf), xmax = as.Date(Inf),
               fill = "#e74c3c", alpha = 0.08) +
      (if (one_line) geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.15, colour = NA)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 1.8) +
      (if (one_line) scale_colour_manual(values = "#2c3e50", aesthetics = c("colour", "fill"))) +
      scale_y_continuous(labels = scales::label_dollar(prefix = "S$")) +
      labs(x = NULL, y = "Median monthly rent", colour = NULL, fill = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = if (one_line) "none" else "bottom")
  })

  output$trend_summary <- renderText({
    t <- selection() |> group_by(quarter) |> summarise_rent() |> filter(n >= 5)
    if (nrow(t) < 2) return("Not enough data to measure the change over time.")
    first <- t[1, ]; last <- t[nrow(t), ]
    change <- round(100 * (last$median / first$median - 1))
    paste0("Median rent went from ", money(first$median), " (", quarter_label(first$quarter),
           ") to ", money(last$median), " (", quarter_label(last$quarter), "), a change of ",
           ifelse(change >= 0, "+", ""), change, "%. ",
           "Grey band = typical range. Red band = your budget range.")
  })


  # ---- Tab 5: Compare areas ----
  compare_data <- reactive({
    d <- latest_months(market(), as.numeric(input$recent))
    validate(need(nrow(d) > 0, "No rentals match these filters."))
    groups <- if (input$compare_by == "District") "district" else c("project", "district")
    s <- d |>
      group_by(across(all_of(groups))) |>
      summarise_rent(budget()) |>
      filter(n >= input$min_n) |>
      arrange(median)
    if (input$compare_by == "Project") s <- add_distance(s, input$destination)
    s
  })

  output$district_plot <- renderPlot({
    s <- compare_data()
    req(input$compare_by == "District", !"project" %in% names(s))
    b <- budget()
    s$fits <- ifelse(s$median >= b[1] & s$median <= b[2],
                     "Median within budget", "Median outside budget")

    ggplot(s, aes(y = reorder(district, -median))) +
      annotate("rect", xmin = b[1], xmax = b[2], ymin = -Inf, ymax = Inf,
               fill = "#e74c3c", alpha = 0.08) +
      geom_linerange(aes(xmin = p25, xmax = p75), linewidth = 1.2, colour = "grey70") +
      geom_point(aes(x = median, colour = fits), size = 3.5) +
      scale_colour_manual(values = c("Median within budget"  = "#18bc9c",
                                     "Median outside budget" = "#95a5a6")) +
      scale_x_continuous(labels = scales::label_dollar(prefix = "S$")) +
      labs(x = "Monthly rent", y = NULL, colour = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })

  output$compare_table <- renderDT({
    b <- budget()
    s <- prettify(compare_data())
    datatable(s, rownames = FALSE,
              options = list(pageLength = 15, order = list(), scrollX = TRUE)) |>
      format_money_cols(s) |>
      formatStyle("Median rent", target = "row",
                  backgroundColor = styleInterval(c(b[1] - 0.5, b[2] + 0.5),
                                                  c("white", "#e8f8f5", "white")))
  })


  # ---- Tab 7: Transactions ----
  records <- reactive({
    selection() |>
      arrange(desc(lease_date), project) |>
      transmute(`Lease start` = format(lease_date, "%b %Y"),
                Project = project, Street = street, District = district,
                Type = type, Bedrooms = bed_group,
                `Floor area (sqft)` = area_band,
                `Monthly rent` = rent, `S$ psf` = round(psf, 2))
  })

  output$records_table <- renderDT({
    datatable(records(), rownames = FALSE,
              options = list(pageLength = 20, scrollX = TRUE)) |>
      formatCurrency("Monthly rent", "S$", digits = 0)
  })

  output$download <- downloadHandler(
    filename = function() "rental_transactions.csv",
    content  = function(file) write.csv(records(), file, row.names = FALSE)
  )
}

# enableBookmarking = "url" powers the "Save this search" button
shinyApp(ui, server, enableBookmarking = "url")
