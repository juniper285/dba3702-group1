#' "Compare properties" (Market page): up to three projects side by side
#'
#' Uses the filters' time period and budget, plus its own bedroom choice;
#' the other filters are ignored so any project can be compared.
#'
#' @param filters The list returned by mod_filters_server().
#' @param shortlist The shared shortlist reactiveVal (for "Fill from my shortlist").
#' @noRd
mod_properties_ui <- function(id) {
  ns <- NS(id)
  pick <- function(i, label) {
    selectizeInput(ns(i), label, choices = NULL,
                   # the list opens over the page instead of scrolling inside the card
                   options = list(placeholder = "Type a project name", dropdownParent = "body"))
  }
  tagList(
    card(
      class = "mt-3",
      header_with_actions(
        "Pick up to three projects",
        actionButton(ns("from_short"), "Fill from my shortlist", icon = icon("star"),
                     class = "btn-sm btn-outline-secondary"),
        downloadButton(ns("download"), "Download CSV", class = "btn-sm btn-primary")
      ),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        pick("a", "Property A"), pick("b", "Property B"), pick("c", "Property C"),
        selectInput(ns("bed"), "Bedrooms", choices = c("All", bed_levels))
      ),
      card_footer("Uses the time period and budget from My filters; ",
                  "all three use the bedroom choice above.")
    ),
    uiOutput(ns("cards")),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Rent distribution"), plotly::plotlyOutput(ns("box"), height = 400),
           card_footer("Box = middle 50% of rentals; line inside = median. ",
                       "Starts zoomed in on the main range; double-click to see everything.")),
      card(card_header("Quarterly median rent"), plotly::plotlyOutput(ns("trend"), height = 400),
           card_footer("Quarters with fewer than 3 rentals are left out."))
    ),
    card(card_header("Comparison table"), DTOutput(ns("table"), fill = FALSE))
  )
}

# Property A navy, B orange, C teal
property_colours <- c("#1e3a8a", "#f59e0b", "#14b8a6")

#' @noRd
mod_properties_server <- function(id, filters, shortlist) {
  moduleServer(id, function(input, output, session) {
    setBookmarkExclude(c(dt_bookmark_inputs("table"), "from_short"))

    # Project list, most-rented first
    project_order <- rentals_data() |> count(project, sort = TRUE) |> dplyr::pull(project)
    for (i in c("a", "b", "c")) {
      updateSelectizeInput(session, i, choices = c("", project_order), server = TRUE)
    }

    observeEvent(input$from_short, {
      picks <- head(unique(shortlist()$project), 3)
      if (length(picks) == 0) {
        showNotification("Your shortlist is empty. Add projects from the Budget or Map page first.",
                         type = "warning")
        return()
      }
      ids <- c("a", "b", "c")
      for (k in seq_along(ids)) {
        updateSelectizeInput(session, ids[k], choices = c("", project_order), server = TRUE,
                             selected = if (k <= length(picks)) picks[k] else "")
      }
    })

    projects <- reactive({
      x <- c(input$a, input$b, input$c)
      x <- unique(x[!is.na(x) & nzchar(x)])
      validate(need(length(x) >= 1,
        "Pick at least one project above, or press 'Fill from my shortlist'."))
      x
    })

    data <- reactive({
      period <- filters$period()
      d <- rentals_data()
      d <- d[d$project %in% projects() & d$lease_date >= period[1] & d$lease_date <= period[2], ]
      if (input$bed != "All") d <- d[d$bed_group == input$bed, ]
      d$project <- factor(d$project, levels = projects())
      d
    })

    colours <- reactive(setNames(property_colours[seq_along(projects())], projects()))

    summary_data <- reactive({
      summarise_projects(mutate(data(), project = as.character(project)), projects(),
                         filters$budget()) |>
        add_distance(filters$places())
    })

    output$cards <- renderUI({
      s <- summary_data()
      cols <- colours()
      cards <- lapply(seq_len(nrow(s)), function(i) {
        x <- s[i, ]
        card(
          card_header(style = paste0("background:", cols[[x$project]], "; color: #fff;"), x$project),
          if (x$n == 0) p(class = "text-muted", "No rentals for these filters.") else tagList(
            div(class = "kpi-value", money(x$median)),
            p(class = "text-muted", "median monthly rent (average ", money(x$average), ")"),
            p(strong("Typical range: "), money(x$p25), " - ", money(x$p75), br(),
              strong("Rent per sqft: "), sprintf("S$%.2f", x$psf), br(),
              strong("Typical size: "), format(round(x$sqft), big.mark = ","), " sqft (estimated)", br(),
              strong("District: "), x$district, br(),
              strong("Rentals: "), format(x$n, big.mark = ","), " (latest ",
              format(x$last, "%b %Y"), ")", br(),
              if (!is.na(x$mrt_station))
                tagList(strong("Nearest MRT/LRT: "), x$mrt_station, " (~", x$walk_min, " min walk)", br()),
              lapply(filters$places()$name, function(place) {
                km <- x[[place_column(place)]]
                if (!is.na(km)) tagList(strong("To ", place, ": "), km, " km", br())
              }),
              strong("Budget fit: "), x$fits),
            if (x$n < 5) span(class = "badge bg-warning text-dark", "Few rentals: a rough guide")
          )
        )
      })
      do.call(layout_columns, c(list(col_widths = rep(12 / length(cards), length(cards))), cards))
    })

    output$box <- plotly::renderPlotly({
      d <- data()
      validate(need(nrow(d) > 0, "No rentals match these filters."))
      # Start zoomed to the main range (rare luxury rentals squash the boxes)
      top <- unname(quantile(d$rent, 0.98)) * 1.1
      plotly::plot_ly(d, x = ~project, y = ~rent, color = ~project, colors = colours(),
                      type = "box", boxpoints = "outliers", marker = list(opacity = 0.3)) |>
        rl_plotly(NULL, "Monthly rent", money_y = TRUE) |>
        plotly::layout(showlegend = FALSE, yaxis = list(range = c(0, top)),
                       xaxis = list(showticklabels = FALSE))
    })

    output$trend <- plotly::renderPlotly({
      s <- data() |>
        group_by(project, quarter) |>
        summarise(median = median(rent), n = n(), .groups = "drop") |>
        filter(n >= 3)
      validate(need(nrow(s) > 0, "No quarters with at least 3 rentals."))
      s$label <- quarter_label(s$quarter)
      plotly::plot_ly(s, x = ~quarter, y = ~median, color = ~project, colors = colours(),
                      type = "scatter", mode = "lines+markers",
                      text = ~sprintf("<b>%s</b><br>%s<br>Median %s<br>%s rentals",
                                      project, label, money(median), n),
                      hoverinfo = "text") |>
        rl_plotly(NULL, "Median monthly rent", money_y = TRUE)
    })

    table_data <- reactive({
      s <- summary_data()
      s$sqft <- round(s$sqft)
      s$last <- format(s$last, "%b %Y")
      s$listings <- ifelse(s$n > 0, listing_link(s$project), "")
      names(s)[names(s) == "sqft"] <- "Typical size (sqft)"
      s
    })

    output$table <- renderDT({
      s <- prettify(table_data())
      rl_datatable(s, selection = "none", escape = FALSE) |> format_money_cols(s)
    })

    output$download <- downloadHandler(
      filename = function() "property_comparison.csv",
      content  = function(file) {
        write.csv(prettify(select(table_data(), -listings)), file, row.names = FALSE)
      }
    )
  })
}
