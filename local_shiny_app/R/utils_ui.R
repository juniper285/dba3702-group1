# ---- Shared UI building blocks: tiles, empty states, charts, tables ----

# Colours used by every chart, so a colour means the same thing on each page
rl_colours <- c(
  ink     = "#1f2937",
  primary = "#0f766e",   # brand teal
  typical = "#14b8a6",   # typical range (middle 50%)
  budget  = "#16a34a",   # the user's budget range (soft green when shaded)
  accent  = "#7b3294",   # the user's own pin and asking rent only
  trend   = "#e11d48",   # fitted trend lines
  muted   = "#94a3b8",
  grid    = "#e5e7eb"
)

bed_colours <- c("1" = "#14b8a6", "2" = "#3b82f6", "3" = "#1e3a8a",
                 "4" = "#f59e0b", "5+" = "#e11d48", "Unknown" = "#cbd5e1")

rl_font <- "Inter, system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif"

#' A small headline figure: label, value and an optional note
#'
#' @param value,note Usually textOutput()s.
#' @param icon A shiny::icon() name shown before the label.
#' @noRd
kpi <- function(label, value, note = NULL, icon = NULL) {
  div(class = "kpi",
      div(class = "kpi-label", if (!is.null(icon)) shiny::icon(icon), label),
      div(class = "kpi-value", value),
      if (!is.null(note)) div(class = "kpi-note", note))
}

#' Card header with a title on the left and controls on the right
#' @noRd
header_with_actions <- function(title, ...) {
  card_header(class = "d-flex flex-wrap gap-2 justify-content-between align-items-center",
              div(class = "card-title-text", title),
              div(class = "d-flex flex-wrap gap-2 align-items-center", ...))
}

#' Friendly message shown where results will appear
#' @noRd
empty_state <- function(icon, title, ...) {
  div(class = "empty-state",
      div(class = "empty-icon", shiny::icon(icon)),
      h5(title),
      div(class = "text-muted", ...))
}

#' Short line of help text under a page title
#' @noRd
page_intro <- function(title, ...) {
  div(class = "page-intro", h4(title), p(class = "text-muted mb-0", ...))
}

#' "My filters" box: one chip per filter and an "Edit my filters" button
#'
#' The same box tops every page that follows the renter's filters.
#' @param chips Usually a uiOutput() filled with filter_chips().
#' @param button The edit button; by default it opens the My filters dialog.
#' @noRd
filter_box_ui <- function(chips, button = edit_filters_button()) {
  div(class = "settings-summary",
      div(class = "settings-icon", icon("filter")),
      div(class = "flex-fill", div(class = "section-title mb-1", "My filters"), chips),
      button)
}

#' (A click anywhere on the box presses its button too, see settings.js.)
#' "Edit my filters" button that opens the My filters dialog from any page
#' @noRd
edit_filters_button <- function() {
  tags$button(type = "button", class = "btn btn-sm btn-outline-primary edit-filters",
              onclick = "Shiny.setInputValue('settings-open_nav', Date.now(), {priority: 'event'});",
              icon("pen"), " Edit my filters")
}

#' The filter box on pages other than Search
#'
#' @param page Short page name; app_server() fills output `filter_note_<page>`.
#' @noRd
filter_note_ui <- function(page) filter_box_ui(uiOutput(paste0("filter_note_", page)))

#' One chip per filter (from settings_summary())
#' @noRd
filter_chips <- function(parts) {
  div(class = "settings-chips", lapply(parts, function(x) span(class = "settings-chip", x)))
}

#' Consistent look for every plotly chart
#' @noRd
rl_plotly <- function(p, xlab = NULL, ylab = NULL, money_x = FALSE, money_y = FALSE) {
  axis <- function(title, money) {
    list(title = list(text = title, standoff = 8), gridcolor = rl_colours[["grid"]],
         zeroline = FALSE, tickprefix = if (money) "S$" else NULL,
         separatethousands = TRUE, automargin = TRUE)
  }
  p |>
    plotly::layout(
      font = list(family = rl_font, size = 13, color = rl_colours[["ink"]]),
      xaxis = axis(xlab, money_x), yaxis = axis(ylab, money_y),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      margin = list(l = 10, r = 10, t = 10, b = 10),
      hoverlabel = list(bgcolor = "white", font = list(family = rl_font)),
      legend = list(orientation = "h", x = 0, y = 1.08)
    ) |>
    plotly::config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d",
                                              "toggleSpikelines", "hoverClosestCartesian",
                                              "hoverCompareCartesian"))
}

#' Shaded band across a plotly chart, from x0 to x1 (or y0 to y1 if `horizontal`)
#' @noRd
band <- function(from, to, colour, opacity = 0.12, horizontal = FALSE) {
  if (horizontal) {
    list(type = "rect", xref = "paper", x0 = 0, x1 = 1, y0 = from, y1 = to,
         fillcolor = colour, opacity = opacity, line = list(width = 0), layer = "below")
  } else {
    list(type = "rect", yref = "paper", y0 = 0, y1 = 1, x0 = from, x1 = to,
         fillcolor = colour, opacity = opacity, line = list(width = 0), layer = "below")
  }
}

#' Vertical line across a plotly chart at x
#' @noRd
vline <- function(x, colour, dash = "solid", width = 2) {
  list(type = "line", yref = "paper", y0 = 0, y1 = 1, x0 = x, x1 = x,
       line = list(color = colour, dash = dash, width = width))
}

#' Histogram of monthly rents with the median and an optional budget band /
#' marker line (used by Search and Check a listing)
#'
#' @param rents Monthly rents.
#' @param budget c(min, max) to shade, or NULL.
#' @param mark A single rent to mark (e.g. the asking rent), or NA.
#' @noRd
rent_histogram <- function(rents, budget = NULL, mark = NA) {
  shown <- rents[rents <= quantile(rents, 0.99)]   # hide extreme top 1%
  size <- max(100, round(diff(range(shown)) / 40, -2))
  shapes <- list(vline(median(rents), rl_colours[["ink"]], "dot", 1.5))
  if (!is.null(budget)) {
    shapes <- c(shapes, list(band(budget[1], budget[2], rl_colours[["budget"]], 0.12),
                             vline(budget[1], rl_colours[["budget"]], "dash", 1.5),
                             vline(budget[2], rl_colours[["budget"]], "dash", 1.5)))
  }
  if (!is.na(mark)) shapes <- c(shapes, list(vline(mark, rl_colours[["accent"]], width = 3)))

  plotly::plot_ly(x = shown, type = "histogram",
                  xbins = list(start = floor(min(shown) / size) * size, size = size),
                  marker = list(color = "#334155", line = list(color = "white", width = 1)),
                  hovertemplate = "S$%{x}<br>%{y:,} rentals<extra></extra>") |>
    rl_plotly("Monthly rent", "Number of rentals", money_x = TRUE) |>
    plotly::layout(shapes = shapes, bargap = 0.02,
                   # keep a marked rent in view even if it is off the histogram
                   xaxis = if (!is.na(mark) && (mark < min(shown) || mark > max(shown)))
                     list(range = range(c(shown, mark)) + c(-1, 1) * size))
}

#' Settings most people never need, in a collapsed "More options" link
#' @noRd
more_options <- function(...) {
  tags$details(class = "more-options",
               tags$summary(icon("sliders"), " More options"),
               div(class = "more-options-body", ...))
}

#' "Min. recent rentals" box: rows with fewer rentals are left out
#' @noRd
min_rentals_input <- function(id, label = "Leave out rows with fewer recent rentals than") {
  more_options(numericInput(id, label, value = 3, min = 1, width = "100%"))
}

#' Dot (median) and line (typical range) chart, one row per label
#'
#' @param s Data frame with `label`, `median`, `p25`, `p75`, `n`, and
#'   optionally `colour` (a colour per row).
#' @param budget c(min, max) to shade.
#' @noRd
range_dot_chart <- function(s, budget = NULL) {
  s <- s[order(s$median), ]
  s$label <- factor(s$label, levels = s$label)
  colour <- if (is.null(s$colour)) rl_colours[["ink"]] else s$colour
  hover <- sprintf("<b>%s</b><br>Median %s<br>Typical %s - %s<br>%s rentals",
                   s$label, money(s$median), money(s$p25), money(s$p75),
                   format(s$n, big.mark = ","))
  plotly::plot_ly(s, y = ~label) |>
    plotly::add_segments(x = ~p25, xend = ~p75, yend = ~label, hoverinfo = "skip",
                         line = list(color = "#94a3b8", width = 6)) |>
    plotly::add_markers(x = ~median, text = hover, hoverinfo = "text",
                        marker = list(color = colour, size = 13,
                                      line = list(color = "white", width = 2))) |>
    rl_plotly("Monthly rent", NULL, money_x = TRUE) |>
    plotly::layout(
      showlegend = FALSE,
      margin = list(l = 10, r = 10, t = if (is.null(budget)) 10 else 30, b = 10),
      # the budget: a green band with solid edges and a label on top
      shapes = if (!is.null(budget)) c(
        list(band(budget[1], budget[2], rl_colours[["budget"]], 0.14)),
        lapply(budget, function(x) list(type = "line", yref = "paper", y0 = 0, y1 = 1,
                                         x0 = x, x1 = x, layer = "below",
                                         line = list(color = rl_colours[["budget"]], width = 1.5)))),
      annotations = if (!is.null(budget)) list(list(
        x = mean(budget), y = 1, xref = "x", yref = "paper", yanchor = "bottom",
        text = "<b>Your budget</b>", showarrow = FALSE,
        font = list(color = rl_colours[["budget"]], size = 12))))
}

#' Marker sizes (pixels) for counts, by area: biggest count = `max` pixels
#'
#' Used instead of plotly's `size = ~n`, which warns about line widths.
#' @noRd
dot_sizes <- function(n, min = 5, max = 25) min + (max - min) * sqrt(n / max(n))

#' A shortlist star with a label, outside a table (e.g. "2 bedrooms" on the map)
#'
#' Works like the table stars: settings.js sends clicks as `input$<id>` with
#' the `shortlist_key()`, and `toggle_shortlist()` handles them.
#' @param key A `shortlist_key()`.
#' @param input_id The namespaced input id, e.g. `session$ns("star")`.
#' @noRd
star_chip <- function(key, input_id, label, on = FALSE) {
  tags$button(
    type = "button", class = paste("rl-star rl-star-chip", if (on) "is-on"),
    `data-key` = key, `data-input` = input_id, `aria-pressed` = tolower(on),
    title = if (on) "Remove from shortlist" else "Add to shortlist",
    HTML(paste0('<svg viewBox="0 0 24 24" width="16" height="16" aria-hidden="true"><path d="',
                'M12 3.2l2.7 5.6 6.1.8-4.5 4.2 1.1 6.1L12 17l-5.4 2.9 1.1-6.1-4.5-4.2 6.1-.8z"/></svg>')),
    span(label))
}

#' Columns hidden at first in every table (shown with the "Columns" button)
#'
#' Rarely needed detail: rent spread, psf, counts and dates, raw addresses.
#' A table keeps one visible by passing e.g. `hide = setdiff(detail_cols, "Recent rentals")`.
#' @noRd
detail_cols <- c("Average rent", "25th percentile", "75th percentile", "Median S$ psf",
                 "Recent rentals", "Last rented", "Distance to MRT/LRT (m)",
                 "Street", "Address", "S$ psf", "PSF")

#' A DataTable with the app's defaults
#'
#' Columns named in `hide` start hidden, and a "Columns" button lets people
#' show them (the choice is remembered in the browser, `setupTable()` in
#' settings.js). The Project column is always shown. District cells stay on
#' one line, cut short with "…" (full name on hover). Every column can be
#' made wider or narrower by dragging the edge of its header
#' (`RentLens.setupTable()`). Distance columns to the renter's own places
#' ("To Mum's (km)") are tinted light green.
#'
#' @param search What the search box looks in: "project" (the Project
#'   column only; the default when there is one), "rental" (every column,
#'   for tables of single rentals) or "table" (every column).
#' @param hide Names of columns to hide at first; `character()` hides none.
#' @param star An input id (already namespaced, `session$ns("star")`): adds a
#'   star at the left of each row that adds that option to the shortlist or
#'   takes it off. Clicks arrive as `input$star$key`, a `shortlist_key()`.
#'   Needs Project and Bedrooms columns. The star isn't a real column: no
#'   header, no sorting, not in the Columns menu.
#' @noRd
rl_datatable <- function(s, page_length = 10, selection = "multiple", escape = TRUE,
                         search = if ("Project" %in% names(s)) "project" else "table",
                         hide = detail_cols, star = NULL, ...) {
  search <- match.arg(search, c("project", "rental", "table"))
  star_col <- if (!is.null(star)) " "
  if (!is.null(star)) {
    s <- cbind(data.frame(` ` = shortlist_key(s$Project, s$Bedrooms), check.names = FALSE), s)
  }
  idx <- function(cols) which(names(s) %in% cols) - 1   # DataTables counts from 0
  # Other columns can't be searched when the box is for projects
  not_searched <- if (search == "project") idx(setdiff(names(s), "Project")) else idx(star_col)
  hidden <- idx(setdiff(hide, c("Project", star_col)))
  defs <- list()
  if (!is.null(star)) defs <- c(defs, list(list(
    targets = 0, orderable = FALSE, className = "rl-star-cell", width = "1.75rem",
    render = DT::JS(sprintf(paste(
      "function (d, type) {",
      "  if (type !== 'display') return d;",
      "  return window.RentLens ? RentLens.starButton(d, '%s') : '';",
      "}"), star)))))
  if (length(not_searched)) defs <- c(defs, list(list(searchable = FALSE, targets = not_searched)))
  if (length(hidden)) defs <- c(defs, list(list(visible = FALSE, targets = hidden)))
  # Distance to the renter's own places ("To Mum's (km)", place_column()):
  # tinted green, so their own columns stand out from the data's
  places <- idx(grep("^To .* \\(km\\)$", names(s), value = TRUE))
  if (length(places)) defs <- c(defs, list(list(targets = places, className = "dt-place")))
  if ("District" %in% names(s)) defs <- c(defs, list(list(
    targets = idx("District"), className = "dt-district",
    render = DT::JS(
      "function (d, type) {",
      "  if (type !== 'display' || d == null) return d;",
      "  return '<span class=\"dt-clip\" title=\"' + d + '\">' + d + '</span>';",
      "}"))))
  options <- list(pageLength = page_length, order = list(), scrollX = TRUE,
                  dom = '<"dt-top"f>t<"dt-bottom"ip>',
                  columnDefs = if (length(defs)) defs,
                  language = list(search = "", searchPlaceholder = paste("Search", search)))
  extensions <- character()
  if (length(hidden)) {
    extensions <- "Buttons"
    options$dom <- '<"dt-top"Bf>t<"dt-bottom"ip>'
    options$buttons <- list(list(extend = "colvis", text = "Columns", className = "dt-columns",
                                 columns = idx(setdiff(names(s), c("Project", star_col)))))
  }
  # Column widths can be dragged; widths and shown columns are remembered
  options$initComplete <- DT::JS(
    "function () { if (window.RentLens) RentLens.setupTable(this.api()); }")
  datatable(s, rownames = FALSE, selection = selection, escape = escape,
            colnames = short_colnames(names(s)),
            class = "compact hover row-border",
            extensions = extensions, options = options, ...)
}

#' Column headers short enough not to stretch a table's header row:
#' "To NUS (Ridge View Residential College (RVRC)) (km)" -> "To NUS (Ridge View… (km)"
#' @noRd
short_colnames <- function(x, width = 24) {
  long <- nchar(x) > width
  km <- long & grepl("^To .* \\(km\\)$", x)
  x[km] <- paste0(substr(x[km], 1, width - 6), "\u2026 (km)")
  x[long & !km] <- paste0(substr(x[long & !km], 1, width - 1), "\u2026")
  x
}
