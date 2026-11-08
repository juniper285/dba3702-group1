#' "Shortlist" tab: the user's saved options
#'
#' @param filters The list returned by mod_filters_server().
#' @param shortlist The shared shortlist reactiveVal (columns project, bed_group).
#' @noRd
mod_shortlist_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_intro("Your shortlist",
               "Compare the options you saved from Search, Map and Check a listing."),
    conditionalPanel(
      "output.is_empty", ns = ns,
      card(empty_state("star", "Nothing shortlisted yet",
                       p(HTML(paste0("On the <b>Search</b> or <b>Map</b> page, press the star ",
                                     "next to a home. On <b>Check a listing</b>, add the ",
                                     "project you are checking.")))))
    ),
    conditionalPanel(
      "!output.is_empty", ns = ns,
      card(
        header_with_actions(
          "Saved options",
          actionButton(ns("clear"), "Clear all", icon = icon("xmark"),
                       class = "btn-sm btn-outline-secondary"),
          downloadButton(ns("download"), "Download CSV", class = "btn-sm btn-primary")
        ),
        DTOutput(ns("table"), fill = FALSE),
        card_footer("Press a star to take that option off your shortlist. ",
                    "Figures use each option's rentals in the most recent period, ",
                    "regardless of the other filters, so your shortlist stays comparable. ",
                    "Use 'Save search' on the Search page to keep a link to it.")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Shortlist at a glance"),
          plotly::plotlyOutput(ns("plot"), height = 420),
          card_footer("Dot = median rent, line = typical range (middle 50%). ",
                      "Green band = your budget range.")
        ),
        card(
          card_header("How their rents have moved"),
          plotly::plotlyOutput(ns("trend"), height = 420),
          card_footer("Quarterly median rent; quarters with fewer than 3 rentals are left out.")
        )
      )
    )
  )
}

#' @noRd
mod_shortlist_server <- function(id, filters, shortlist) {
  moduleServer(id, function(input, output, session) {
    setBookmarkExclude(c(dt_bookmark_inputs("table"), "star", "clear"))

    # Fill in the stars in every table (settings.js, "rentlens-shortlist")
    observe({
      sl <- shortlist()
      session$sendCustomMessage("rentlens-shortlist", as.list(shortlist_key(sl$project, sl$bed_group)))
    })

    # Every rental of the shortlisted options in the chosen time period
    short_contracts <- reactive({
      sl <- shortlist()
      validate(need(nrow(sl) > 0,
        "Your shortlist is empty."))
      period <- filters$period()
      rentals_data() |>
        semi_join(sl, by = c("project", "bed_group")) |>
        filter(lease_date >= period[1], lease_date <= period[2])
    })

    short_data <- reactive({
      b <- filters$budget()
      d <- latest_months(short_contracts(), filters$recent())
      validate(need(nrow(d) > 0, "No recent rentals for your shortlist in this time period."))

      s <- summarise_options(d) |>
        select(-last) |>
        mutate(fits = case_when(median < b[1] ~ "Below range",
                                median > b[2] ~ "Above range",
                                TRUE          ~ "Within range")) |>
        arrange(median) |>
        add_distance(filters$places())
      s$listings <- listing_link(s$project)
      s
    })

    output$is_empty <- reactive(nrow(shortlist()) == 0)
    outputOptions(output, "is_empty", suspendWhenHidden = FALSE)

    output$table <- renderDT({
      s <- prettify(short_data())
      rl_datatable(s, page_length = 25, escape = FALSE, selection = "none",
                   star = session$ns("star")) |>
        format_money_cols(s) |>
        formatStyle("Budget fit",
                    color = styleEqual(c("Within range", "Below range", "Above range"),
                                       c("#15803d", "#0369a1", "#e11d48")),
                    fontWeight = "bold")
    })

    # Every star here is on; pressing one takes that option off
    observeEvent(input$star, toggle_shortlist(shortlist, input$star$key))

    observeEvent(input$clear, shortlist(empty_shortlist()))

    output$download <- downloadHandler(
      filename = function() "rental_shortlist.csv",
      content  = function(file) {
        write.csv(prettify(select(short_data(), -listings)), file, row.names = FALSE)
      }
    )

    output$plot <- plotly::renderPlotly({
      s <- short_data()
      s$label <- paste0(s$project, " (", s$bed_group, " BR)")
      s$colour <- c("Within range" = "#15803d", "Below range" = "#0369a1",
                    "Above range" = "#e11d48")[s$fits] |> unname()
      range_dot_chart(s, filters$budget())
    })

    output$trend <- plotly::renderPlotly({
      t <- short_contracts() |>
        mutate(label = paste0(project, " (", bed_group, " BR)")) |>
        group_by(quarter, label) |>
        summarise(median = median(rent), n = n(), .groups = "drop") |>
        filter(n >= 3)
      validate(need(nrow(t) > 0, "Not enough rentals per quarter to draw trends."))
      t$q <- quarter_label(t$quarter)
      plotly::plot_ly(t, x = ~quarter, y = ~median, color = ~label,
                      type = "scatter", mode = "lines+markers",
                      text = ~sprintf("<b>%s</b><br>%s<br>Median %s<br>%s rentals",
                                      label, q, money(median), n),
                      hoverinfo = "text") |>
        rl_plotly(NULL, "Median monthly rent", money_y = TRUE) |>
        plotly::layout(legend = list(orientation = "h", x = 0, y = -0.15))
    })

    # Save and restore the shortlist inside "Save this search" links
    onBookmark(function(state) state$values$shortlist <- shortlist())
    onRestore(function(state) {
      sl <- state$values$shortlist
      if (!is.null(sl) && length(sl$project) > 0) {
        shortlist(data.frame(project = unlist(sl$project), bed_group = unlist(sl$bed_group)))
      }
    })
  })
}

#' Add selected table rows to the shortlist
#'
#' @param shortlist The shared shortlist reactiveVal.
#' @param options Table the rows were selected from (has project, bed_group).
#' @param rows Selected row numbers (`input$<table>_rows_selected`).
#' @noRd
add_to_shortlist <- function(shortlist, options, rows) {
  if (length(rows) == 0) {
    shortlist_toast("Select one or more rows in the table first.", added = FALSE)
    return(invisible())
  }
  new <- as.data.frame(options[rows, c("project", "bed_group")])
  shortlist(unique(rbind(shortlist(), new)))
  shortlist_toast(paste(length(rows), "option(s) added to your shortlist."))
}

#' One text key per shortlist option, used by the table stars
#' @noRd
shortlist_key <- function(project, bed_group) paste(project, bed_group, sep = " | ")

#' Star or unstar one option (a star in a table, see `rl_datatable(star =)`)
#'
#' @param key A `shortlist_key()`.
#' @return TRUE if it was added, FALSE if removed (invisibly).
#' @noRd
toggle_shortlist <- function(shortlist, key) {
  sl <- shortlist()
  have <- shortlist_key(sl$project, sl$bed_group) == key
  parts <- strsplit(key, " | ", fixed = TRUE)[[1]]
  beds <- parts[length(parts)]
  what <- paste0(paste(parts[-length(parts)], collapse = " | "), " (",
                 if (beds == "Unknown") "bedrooms unknown" else paste(beds, "bedroom"), ")")
  if (any(have)) {
    shortlist(sl[!have, , drop = FALSE])
    shortlist_toast(paste(what, "removed from your shortlist."), added = FALSE)
  } else {
    new <- data.frame(project = paste(parts[-length(parts)], collapse = " | "), bed_group = beds)
    shortlist(rbind(sl, new))
    shortlist_toast(paste(what, "added to your shortlist."))
  }
  invisible(!any(have))
}

#' A small pop-up message (top right) after the shortlist changes, with a link to it
#' @noRd
shortlist_toast <- function(text, added = TRUE) {
  go <- "document.querySelector('.navbar a[data-value=\"shortlist\"]').click(); return false;"
  show_toast(toast(
    text, if (added) tags$a(href = "#", class = "toast-link", onclick = go, "View shortlist"),
    icon = icon(if (added) "star" else "star-half-stroke"),
    type = if (added) "success" else "secondary", duration_s = 4))
}
