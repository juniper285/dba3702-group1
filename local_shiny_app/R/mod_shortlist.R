#' "Shortlist" tab: the user's saved options
#'
#' @param filters The list returned by mod_filters_server().
#' @param shortlist The shared shortlist reactiveVal (columns project, bed_group).
#' @noRd
mod_shortlist_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_intro("Your shortlist",
               "Compare the options you saved from Budget, Map and Check a listing."),
    conditionalPanel(
      "output.is_empty", ns = ns,
      card(empty_state("star", "Nothing shortlisted yet",
                       p(HTML(paste0("On the <b>Budget</b> or <b>Map</b> page, select rows in a ",
                                     "table and press <b>Add to shortlist</b>. On <b>Check a ",
                                     "listing</b>, add the project you are checking.")))))
    ),
    conditionalPanel(
      "!output.is_empty", ns = ns,
      card(
        header_with_actions(
          "Saved options",
          actionButton(ns("remove"), "Remove selected", icon = icon("trash-can"),
                       class = "btn-sm btn-outline-secondary"),
          actionButton(ns("clear"), "Clear all", icon = icon("xmark"),
                       class = "btn-sm btn-outline-secondary"),
          downloadButton(ns("download"), "Download CSV", class = "btn-sm btn-primary")
        ),
        DTOutput(ns("table"), fill = FALSE),
        card_footer("Figures use each option's rentals in the most recent period, ",
                    "regardless of the other filters, so your shortlist stays comparable. ",
                    "Use 'Save search' on the Budget page to keep a link to it.")
      ),
      card(
        card_header("Shortlist at a glance"),
        plotly::plotlyOutput(ns("plot"), height = 420),
        card_footer("Dot = median rent, line = typical range (middle 50%). ",
                    "Red band = your budget range.")
      )
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
        "Your shortlist is empty."))
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

    output$is_empty <- reactive(nrow(shortlist()) == 0)
    outputOptions(output, "is_empty", suspendWhenHidden = FALSE)

    output$table <- renderDT({
      s <- prettify(short_data())
      rl_datatable(s, page_length = 25, escape = FALSE) |>
        format_money_cols(s) |>
        formatStyle("Budget fit",
                    color = styleEqual(c("Within range", "Below range", "Above range"),
                                       c("#15803d", "#0369a1", "#e11d48")),
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

    output$plot <- plotly::renderPlotly({
      s <- short_data()
      s$label <- paste0(s$project, " (", s$bed_group, " BR)")
      s$colour <- c("Within range" = "#15803d", "Below range" = "#0369a1",
                    "Above range" = "#e11d48")[s$fits] |> unname()
      range_dot_chart(s, filters$budget())
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
