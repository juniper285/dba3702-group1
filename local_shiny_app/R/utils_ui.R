# ---- Shared UI building blocks: tiles, empty states, charts, tables ----

# Colours used by every chart, so a colour means the same thing on each page
rl_colours <- c(
  ink     = "#1f2937",
  primary = "#0f766e",   # brand teal
  typical = "#14b8a6",   # typical range (middle 50%)
  budget  = "#e11d48",   # the user's budget range
  accent  = "#7b3294",   # the user's own pin and asking rent only
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

#' "Filters in use" line for pages that follow the Budget page's filters
#'
#' @param page Short page name; app_server() fills output `filter_note_<page>`.
#' @noRd
filter_note_ui <- function(page) {
  div(class = "filter-note",
      icon("filter"),
      span(class = "text-muted", "Filters:"),
      textOutput(paste0("filter_note_", page), inline = TRUE),
      tags$a(href = "#", class = "ms-auto text-nowrap",
             onclick = "Shiny.setInputValue('edit_filters', Date.now(), {priority: 'event'}); return false;",
             icon("pen"), " Edit on Budget page"))
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

#' Histogram of monthly rents with the typical range, median and optional
#' budget band / marker line (used by Budget and Check a listing)
#'
#' @param rents Monthly rents.
#' @param budget c(min, max) to shade, or NULL.
#' @param mark A single rent to mark (e.g. the asking rent), or NA.
#' @noRd
rent_histogram <- function(rents, budget = NULL, mark = NA) {
  q <- quantile(rents, c(0.25, 0.5, 0.75))
  shown <- rents[rents <= quantile(rents, 0.99)]   # hide extreme top 1%
  size <- max(100, round(diff(range(shown)) / 40, -2))
  shapes <- list(band(q[1], q[3], rl_colours[["typical"]], 0.15),
                 vline(q[2], rl_colours[["ink"]], "dot", 1.5))
  if (!is.null(budget)) {
    shapes <- c(shapes, list(band(budget[1], budget[2], rl_colours[["budget"]], 0.10),
                             vline(budget[1], rl_colours[["budget"]], "dash"),
                             vline(budget[2], rl_colours[["budget"]], "dash")))
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
                         line = list(color = "#cbd5e1", width = 5)) |>
    plotly::add_markers(x = ~median, text = hover, hoverinfo = "text",
                        marker = list(color = colour, size = 11,
                                      line = list(color = "white", width = 1.5))) |>
    rl_plotly("Monthly rent", NULL, money_x = TRUE) |>
    plotly::layout(showlegend = FALSE,
                   shapes = if (!is.null(budget)) list(band(budget[1], budget[2], rl_colours[["budget"]], 0.08)))
}

#' Marker sizes (pixels) for counts, by area: biggest count = `max` pixels
#'
#' Used instead of plotly's `size = ~n`, which warns about line widths.
#' @noRd
dot_sizes <- function(n, min = 5, max = 25) min + (max - min) * sqrt(n / max(n))

#' A DataTable with the app's defaults
#' @noRd
rl_datatable <- function(s, page_length = 10, selection = "multiple", escape = TRUE, ...) {
  datatable(s, rownames = FALSE, selection = selection, escape = escape,
            class = "compact hover row-border",
            options = list(pageLength = page_length, order = list(), scrollX = TRUE,
                           dom = '<"dt-top"f>t<"dt-bottom"ip>',
                           language = list(search = "", searchPlaceholder = "Search table")),
            ...)
}
