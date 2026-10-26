#' "Transactions" tab: the underlying records
#'
#' @param filters The list returned by mod_filters_server().
#' @noRd
mod_transactions_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header(
      class = "d-flex justify-content-between align-items-center",
      "Rental transactions matching your filters",
      downloadButton(ns("download"), "Download CSV", class = "btn-sm")
    ),
    DTOutput(ns("table"), fill = FALSE)
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
                  `Monthly rent` = rent, `S$ psf` = round(psf, 2))
    })

    output$table <- renderDT({
      datatable(records(), rownames = FALSE,
                options = list(pageLength = 20, scrollX = TRUE)) |>
        formatCurrency("Monthly rent", "S$", digits = 0)
    })

    output$download <- downloadHandler(
      filename = function() "rental_transactions.csv",
      content  = function(file) write.csv(records(), file, row.names = FALSE)
    )
  })
}
