#' "About the data" page: methodology and disclaimer (static, so no server)
#' @noRd
mod_about_ui <- function(id) {
  rentals <- rentals_data()
  tagList(
    page_intro("About the data",
               "All figures come from URA private residential rental contracts: ",
               format(nrow(rentals), big.mark = ","), " records from ",
               format(min(rentals$lease_date), "%b %Y"), " to ",
               format(max(rentals$lease_date), "%b %Y"), "."),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(icon("book-open"), " How to read RentLens"),
        p(strong("My settings"), " hold your budget, the places that matter to you, where ",
          "to search, the type of home and how far you will travel. They are saved in this ",
          "browser only (no account), and every page follows them. Change them with ",
          "'My settings' in the menu bar; 'Start over' reruns the setup."),
        p(strong("Comparable rentals"), " are the transactions matching your settings ",
          "within the most recent period you choose in My settings."),
        p(strong("Typical range"), " is the 25th to 75th percentile: half of ",
          "comparable rentals fall inside it."),
        p(strong("Options within your budget"), " (Search page) lists each project and ",
          "bedroom type whose median recent rent falls inside your budget range."),
        p(strong("Check a listing"), " compares an asking rent with past rentals of the same ",
          "bedroom count and property type in the same project, within a radius, in the same ",
          "postal district and in the same region (regions are approximated from postal ",
          "districts). The table shows how far the asking rent is above (+) or below (-) each median. ",
          "Addresses are looked up with OneMap; street-level geocodes are left out of the radius."),
        p(strong("Map"), " locations come from geocoding each project's address. ",
          "'Geocode quality' lets you hide projects whose location is less certain. ",
          "Click a building or draw an area to select it; with 'Apply map selection to all ",
          "pages' on, every other page uses only the selected buildings. Purple pins are ",
          "your places; yellow stars mark universities, polytechnics and work areas."),
        p(strong("Area A vs B"), " (Market page): select a building or draw an area on the ",
          "map, press 'Save as Area A', then do the same for Area B to compare them."),
        p(strong("Compare properties"), " puts up to three projects side by side; ",
          strong("Value for money"), " plots each project's median rent against its median ",
          "floor area and ranks projects by the lowest rent per sqft."),
        p(strong("MRT/LRT"), ": every building gets its nearest station. Filter by distance ",
          "to the nearest station, or pick a station to keep buildings within a walking ",
          "distance of it (shown as a ring on the map). Station locations come from LTA's ",
          "station exit data.")
      ),
      card(
        card_header(icon("triangle-exclamation"), " Things to keep in mind"),
        p("These are historical transactions, not live listings. Use them to check ",
          "whether rents advertised on property portals are reasonable, then use ",
          "'Find listings' on your shortlist to search for available units."),
        p("URA reports floor area as a range; the midpoint is used for rent per sqft, ",
          "so psf figures are estimates."),
        p("Records with no bedroom count are shown as 'Unknown'. Distances are ",
          "straight-line estimates; walking times assume about 80 m per minute."),
        p("Medians based on few rentals are noisy: every figure shows how many rentals ",
          "it is based on.")
      )
    )
  )
}
