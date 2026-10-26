#' "Shortlist" tab: the user's saved options
#'
#' @param filters The list returned by mod_filters_server().
#' @param shortlist The shared shortlist reactiveVal (columns project, bed_group).
#' @noRd
mod_shortlist_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Your shortlist",
        div(
          actionButton(ns("remove"), "Remove selected", class = "btn-sm"),
          actionButton(ns("clear"), "Clear all", class = "btn-sm"),
          downloadButton(ns("download"), "Download CSV", class = "btn-sm")
        )
      ),
      DTOutput(ns("table"), fill = FALSE),
      card_footer("Figures use each option's rentals in the most recent period, ",
                  "regardless of the sidebar filters, so your shortlist stays comparable. ",
                  "Use 'Save this search' in the sidebar to keep a link to it.")
    ),
    card(
      card_header("Shortlist at a glance"),
      plotOutput(ns("plot"), height = 420),
      card_footer("Dot = median rent, line = typical range. Red band = your budget range.")
    )
  )
}

#' @noRd
mod_shortlist_server <- function(id, filters, shortlist) {
  moduleServer(id, function(input, output, session) {
    setBookmarkExclude(c(dt_bookmark_inputs("table"), "remove", "clear"))

    short_data <- reactive({
      sl <- shortlist()
      validate(need(nrow(sl) > 0,
        "Your shortlist is empty. In 'Within budget' or 'Map', click rows and press 'Add selected to shortlist'."))
      b <- filters$budget()
      period <- filters$period()
      d <- rentals_data() |>
        semi_join(sl, by = c("project", "bed_group")) |>
        filter(lease_date >= period[1], lease_date <= period[2])
      d <- latest_months(d, filters$recent())
      validate(need(nrow(d) > 0, "No recent rentals for your shortlist in this time period."))

      s <- summarise_options(d) |>
        select(-last) |>
        mutate(fits = case_when(median < b[1] ~ "Below range",
                                median > b[2] ~ "Above range",
                                TRUE          ~ "Within range")) |>
        arrange(median) |>
        add_distance(filters$destination())
      s$listings <- listing_link(s$project)
      s
    })

    output$table <- renderDT({
      s <- prettify(short_data())
      datatable(s, rownames = FALSE, escape = FALSE, selection = "multiple",
                options = list(pageLength = 25, order = list(), scrollX = TRUE)) |>
        format_money_cols(s) |>
        formatStyle("Budget fit",
                    color = styleEqual(c("Within range", "Below range", "Above range"),
                                       c("#18bc9c", "#3498db", "#e74c3c")),
                    fontWeight = "bold")
    })

    observeEvent(input$remove, {
      rows <- input$table_rows_selected
      if (length(rows) == 0) {
        showNotification("Select rows in your shortlist to remove.", type = "warning")
        return()
      }
      gone <- short_data()[rows, c("project", "bed_group")]
      shortlist(anti_join(shortlist(), gone, by = c("project", "bed_group")))
    })

    observeEvent(input$clear, shortlist(empty_shortlist()))

    output$download <- downloadHandler(
      filename = function() "rental_shortlist.csv",
      content  = function(file) {
        write.csv(prettify(select(short_data(), -listings)), file, row.names = FALSE)
      }
    )

    output$plot <- renderPlot({
      s <- short_data()
      b <- filters$budget()
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
    showNotification("Select one or more rows in the table first.", type = "warning")
    return(invisible())
  }
  new <- as.data.frame(options[rows, c("project", "bed_group")])
  shortlist(unique(rbind(shortlist(), new)))
  showNotification(paste(length(rows), "option(s) added to your shortlist."), type = "message")
}
