#' Budget check (top of the "Budget" page): is my budget realistic?
#'
#' @param filters The list returned by mod_filters_server().
#' @noRd
mod_budget_check_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("verdict")),
    layout_columns(
      col_widths = c(4, 4, 4), class = "mt-3",
      kpi("Median rent", textOutput(ns("median")), textOutput(ns("average"), inline = TRUE),
          icon = "coins"),
      kpi("Typical range", textOutput(ns("range")), "middle 50% of rents",
          icon = "arrows-left-right"),
      kpi("Comparable rentals", textOutput(ns("n")), textOutput(ns("n_note"), inline = TRUE),
          icon = "file-signature")
    ),
    card(
      card_header("How your budget compares with recent comparable rents"),
      plotly::plotlyOutput(ns("plot"), height = 360),
      card_footer(
        span(style = paste0("color:", rl_colours[["budget"]]), "■"), " Your budget range  ",
        "┇ Median. Hover over a bar for the number of rentals."
      )
    )
  )
}

#' @noRd
mod_budget_check_server <- function(id, filters) {
  moduleServer(id, function(input, output, session) {
    comparables <- filters$comparables
    budget <- filters$budget

    output$median <- renderText(money(median(comparables()$rent)))
    output$average <- renderText(paste("average", money(mean(comparables()$rent))))

    output$range <- renderText({
      q <- quantile(comparables()$rent, c(0.25, 0.75))
      paste0(money(q[1]), " - ", formatC(round(q[2]), format = "d", big.mark = ","))
    })

    output$n <- renderText(format(nrow(comparables()), big.mark = ","))

    output$n_note <- renderText({
      if (nrow(comparables()) < 10) "Few rentals: treat as a rough guide"
      else paste("in the latest", filters$recent(), "months")
    })

    output$verdict <- renderUI({
      b <- budget()
      v <- budget_verdict(comparables()$rent, b)
      icons <- c(success = "circle-check", danger = "circle-exclamation", info = "circle-info")
      div(class = paste0("verdict verdict-", v$colour),
          div(class = "verdict-icon", icon(icons[[v$colour]])),
          div(div(class = "verdict-title", paste0("Your budget is ", tolower(v$verdict))),
              div(class = "verdict-body",
                  paste0(v$share, "% of comparable rentals fall between ",
                         money(b[1]), " and ", money(b[2]), ". ", v$advice))))
    })

    output$plot <- plotly::renderPlotly(rent_histogram(comparables()$rent, budget()))
  })
}
