#' "Transactions" tab: the underlying records
#'
#' @param filters The list returned by mod_filters_server().
#' @noRd
mod_transactions_ui <- function(id) {
  ns <- NS(id)
  tagList(
    page_intro("Rental records",
               "Every URA rental contract matching your filters, newest first."),
    card(
      header_with_actions(
        textOutput(ns("count"), inline = TRUE),
        downloadButton(ns("download"), "Download CSV", class = "btn-sm btn-primary")
      ),
      DTOutput(ns("table"), fill = FALSE)
    )
  )
}

#' @noRd
mod_transactions_server <- function(id, filters) {
  moduleServer(id, function(input, output, session) {
    setBookmarkExclude(dt_bookmark_inputs("table"))

    records <- reactive({
      filters$selection() |>
        arrange(desc(lease_date), project) |>
        transmute(`Lease start` = format(lease_date, "%b %Y"),
                  Project = project, Street = street, District = district,
                  Type = type, Bedrooms = bed_group,
                  `Floor area (sqft)` = area_band,
                  `Monthly rent` = rent, `S$ psf` = round(psf, 2),
                  `Nearest MRT/LRT` = mrt_station)
    })

    output$table <- renderDT({
      rl_datatable(records(), page_length = 20, selection = "none") |>
        formatCurrency("Monthly rent", "S$", digits = 0)
    })

    output$count <- renderText(paste(format(nrow(records()), big.mark = ","), "rentals"))

    output$download <- downloadHandler(
      filename = function() "rental_transactions.csv",
      content  = function(file) write.csv(records(), file, row.names = FALSE)
    )
  })
}
