#' "Area A vs B" (Market page): how do two places I picked on the map compare?
#'
#' Areas are saved on the Map page ("Save as Area A / B"). The filters
#' apply to both.
#'
#' @param map_data The map's rentals (filters + geocode quality), from
#'   mod_map_server().
#' @param areas The shared saved areas: reactiveValues with `a_ids`,
#'   `a_label`, `b_ids` and `b_label`.
#' @noRd
mod_area_compare_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "mt-3", uiOutput(ns("intro"))),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header(class = "area-a", textOutput(ns("a_title"), inline = TRUE)),
           uiOutput(ns("a_tiles"))),
      card(card_header(class = "area-b", textOutput(ns("b_title"), inline = TRUE)),
           uiOutput(ns("b_tiles")))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Median monthly rent by bedrooms"),
           plotly::plotlyOutput(ns("beds"), height = 400),
           card_footer("Rental counts are in the table below.")),
      card(card_header("Median monthly rent by quarter"),
           plotly::plotlyOutput(ns("trend"), height = 400),
           card_footer("Quarters with fewer than 3 rentals are left out."))
    ),
    card(card_header("Side by side by bedrooms"), DTOutput(ns("table"), fill = FALSE))
  )
}

# Area A navy, Area B orange (also used for their outlines on the map)
area_colours <- c(a = "#1e3a8a", b = "#f59e0b")

#' @noRd
mod_area_compare_server <- function(id, map_data, areas) {
  moduleServer(id, function(input, output, session) {
    setBookmarkExclude(dt_bookmark_inputs("table"))

    area_label <- function(w) {
      l <- areas[[paste0(w, "_label")]]
      if (is.null(l)) "not set" else l
    }
    area_data <- function(w) {
      ids <- areas[[paste0(w, "_ids")]]
      d <- map_data()
      if (is.null(ids)) d[0, ] else d[d$loc_id %in% ids, ]
    }
    a_data <- reactive(area_data("a"))
    b_data <- reactive(area_data("b"))

    # Both areas in one table, with an `area` column ("A: ...", "B: ...")
    both <- reactive({
      a <- a_data(); b <- b_data()
      validate(need(nrow(a) > 0 && nrow(b) > 0,
        "Save two areas first: on the Map page, select a building or draw an area, then press 'Save as Area A'; repeat for Area B."))
      rbind(transform(a, area = paste("A:", area_label("a"))),
            transform(b, area = paste("B:", area_label("b"))))
    })
    colours <- reactive(setNames(unname(area_colours),
                                 paste(c("A:", "B:"), c(area_label("a"), area_label("b")))))

    output$intro <- renderUI({
      if (is.null(areas$a_ids) || is.null(areas$b_ids)) {
        div(class = "filter-note",
            icon("circle-info"),
            span("On the Map page, click a building or draw an area, then press ",
                 strong("Save as Area A"), ". Do the same for Area B. The filters apply to both."))
      }
    })

    output$a_title <- renderText(paste("Area A:", area_label("a")))
    output$b_title <- renderText(paste("Area B:", area_label("b")))

    tiles <- function(d) {
      if (nrow(d) == 0) return(p(class = "text-muted", "No rentals yet."))
      div(class = "kpi-stack",
          kpi("Rentals", format(nrow(d), big.mark = ",")),
          kpi("Projects", format(n_distinct(d$project), big.mark = ",")),
          kpi("Median rent", money(median(d$rent))),
          kpi("Median psf", paste0("S$", format(round(median(d$psf, na.rm = TRUE), 2), nsmall = 2))))
    }
    output$a_tiles <- renderUI(tiles(a_data()))
    output$b_tiles <- renderUI(tiles(b_data()))

    output$beds <- plotly::renderPlotly({
      s <- both() |>
        group_by(area, bed_group) |>
        summarise(median = median(rent), n = n(), .groups = "drop")
      s$bed_group <- factor(s$bed_group, levels = bed_levels)
      plotly::plot_ly(s, x = ~bed_group, y = ~median, color = ~area, colors = colours(),
                      type = "bar", text = ~money(median), textposition = "outside",
                      hovertemplate = "%{x} BR<br>Median %{text}<br>%{customdata} rentals",
                      customdata = ~n) |>
        rl_plotly("Bedrooms", "Median monthly rent", money_y = TRUE) |>
        plotly::layout(barmode = "group")
    })

    output$trend <- plotly::renderPlotly({
      s <- both() |>
        group_by(area, quarter) |>
        summarise(median = median(rent), n = n(), .groups = "drop") |>
        filter(n >= 3)
      validate(need(nrow(s) > 0, "Not enough rentals per quarter."))
      s$label <- quarter_label(s$quarter)
      plotly::plot_ly(s, x = ~quarter, y = ~median, color = ~area, colors = colours(),
                      type = "scatter", mode = "lines+markers",
                      text = ~sprintf("%s<br>Median %s<br>%s rentals", label, money(median), n),
                      hoverinfo = "text") |>
        rl_plotly(NULL, "Median monthly rent", money_y = TRUE)
    })

    output$table <- renderDT({
      d <- both()
      s <- compare_two_areas(d[startsWith(d$area, "A:"), ], d[startsWith(d$area, "B:"), ])
      names(s) <- c("Bedrooms", "A rentals", "A median", "A psf",
                    "B rentals", "B median", "B psf", "B vs A (median)")
      datatable(s, rownames = FALSE, selection = "none", class = "compact hover row-border",
                options = list(dom = "t", pageLength = 20, scrollX = TRUE)) |>
        formatCurrency(c("A median", "B median"), "S$", digits = 0) |>
        formatCurrency(c("A psf", "B psf"), "S$", digits = 2) |>
        formatPercentage("B vs A (median)", digits = 0)
    })
  })
}

#' No saved areas yet (see mod_area_compare_server())
#' @noRd
new_saved_areas <- function() reactiveValues(a_ids = NULL, a_label = NULL, b_ids = NULL, b_label = NULL)
