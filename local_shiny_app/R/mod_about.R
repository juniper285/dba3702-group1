#' "About" tab: methodology and disclaimer (static, so no server)
#' @noRd
mod_about_ui <- function(id) {
  rentals <- rentals_data()
  card(
    h4("How to read this app"),
    p("All figures come from URA private residential rental contracts: ",
      format(nrow(rentals), big.mark = ","), " records from ",
      format(min(rentals$lease_date), "%b %Y"), " to ",
      format(max(rentals$lease_date), "%b %Y"), "."),
    p(strong("Comparable rentals"), " are the transactions matching your filters ",
      "within the most recent period you choose in the sidebar."),
    p(strong("Typical range"), " is the 25th to 75th percentile: half of ",
      "comparable rentals fall inside it."),
    p(strong("Within budget"), " lists each project and bedroom type whose median ",
      "recent rent falls inside your budget range."),
    p("These are historical transactions, not live listings. Use them to check ",
      "whether rents advertised on property portals are reasonable, then use ",
      "'Find listings' to search for available units."),
    p("URA reports floor area as a range; the midpoint is used for rent per sqft. ",
      "Records with no bedroom count are shown as 'Unknown'. Distances are ",
      "straight-line estimates.")
  )
}
