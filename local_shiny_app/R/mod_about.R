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
    p(strong("Check a listing"), " compares an asking rent with past rentals of the same ",
      "bedroom count and property type in the same project, within a radius, in the same ",
      "postal district and in the same region (regions are approximated from postal ",
      "districts). The table shows how far the asking rent is above (+) or below (-) each median. ",
      "Addresses are looked up with OneMap; street-level geocodes are left out of the radius."),
    p(strong("Map"), " locations come from geocoding each project's address. ",
      "'Geocode quality' lets you hide projects whose location is less certain."),
    p("These are historical transactions, not live listings. Use them to check ",
      "whether rents advertised on property portals are reasonable, then use ",
      "'Find listings' to search for available units."),
    p("URA reports floor area as a range; the midpoint is used for rent per sqft. ",
      "Records with no bedroom count are shown as 'Unknown'. Distances are ",
      "straight-line estimates.")
  )
}
