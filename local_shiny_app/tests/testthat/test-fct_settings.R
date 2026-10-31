choices <- list(districts = c("D01 Raffles Place, Marina", "D10 Bukit Timah, Holland, Tanglin"),
                types = c("Landed Properties", "Non-Landed Properties"),
                stations = c("Novena MRT", "Kent Ridge MRT"),
                dates = as.Date(c("2021-09-01", "2026-08-01")),
                area_top = 24650)

test_that("settings survive a round trip through JSON", {
  s <- default_settings(choices)
  s$budget <- c(2500, 4000)
  s$places <- rbind(key_place("NTU"), data.frame(name = "My office", lat = 1.30, lng = 103.85))
  s$beds <- c("2", "3")
  s$mrt_pick <- "Novena MRT"; s$mrt_dist <- 600
  back <- settings_from_json(settings_to_json(s), choices)
  expect_equal(back$budget, s$budget)
  expect_equal(back$places$name, c("NTU", "My office"))
  expect_equal(back$places$kind, c("other", "other"))     # no icon given: "other"
  expect_equal(back$beds, c("2", "3"))
  expect_equal(back$mrt_pick, "Novena MRT")
  expect_equal(back$period, s$period)
})

test_that("nothing saved, or not JSON, gives NULL", {
  expect_null(settings_from_json("none", choices))
  expect_null(settings_from_json("", choices))
  expect_null(settings_from_json("{not json", choices))
})

test_that("invalid fields fall back to their defaults one by one", {
  json <- '{"budget": [5000, 1000], "beds": ["2", "9"], "types": ["Castle"],
            "mrt_pick": "Hogwarts MRT", "mrt_dist": 50, "recent": 7,
            "period": ["2020-01-01", "2021-01-01"],
            "places": [{"name": "London", "lat": 51.5, "lng": -0.1},
                       {"name": "Office", "lat": 1.29, "lng": 103.85}]}'
  s <- settings_from_json(json, choices)
  d <- default_settings(choices)
  expect_equal(s$budget, d$budget)          # min above max
  expect_equal(s$beds, "2")                 # unknown "9" dropped
  old <- settings_from_json('{"beds": ["2", "Unknown"]}', choices)
  expect_equal(old$beds, "2")               # "Unknown" is no longer a choice
  expect_equal(s$types, d$types)            # nothing valid left
  expect_equal(s$mrt_pick, "")
  expect_equal(s$mrt_dist, d$mrt_dist)
  expect_equal(s$recent, 12)
  expect_equal(s$period, period_for(12, choices$dates))   # follows the window, not the saved dates
  expect_equal(s$places$name, "Office")     # outside Singapore dropped
})

test_that("settings_summary() describes the settings", {
  s <- default_settings(choices)
  s$places <- key_place(c("NTU", "SMU")); s$place_km <- 5; s$mrt_dist <- 500
  txt <- paste(settings_summary(s, choices), collapse = " · ")
  expect_match(txt, "S\\$3,000 - S\\$5,000")
  expect_match(txt, "Near NTU, SMU")
  expect_match(txt, "Within 5 km of them")
  expect_match(txt, "Within 500 m of an MRT/LRT")
  expect_match(txt, "Any bedrooms")
  expect_match(txt, "Anywhere in Singapore")

  s$districts <- choices$districts
  expect_true("D01 Raffles Place, D10 Bukit Timah" %in% settings_summary(s, choices))
})

test_that("place icons are kept, and unknown icons become 'other'", {
  s <- default_settings(choices)
  s$places <- new_places(name = c("Lab", "Club"), kind = c("study", "castle"),
                         address = c("1 Research Link", ""), lat = c(1.30, 1.31),
                         lng = c(103.78, 103.80))
  back <- settings_from_json(settings_to_json(s), choices)
  expect_equal(back$places$kind, c("study", "other"))
  expect_equal(back$places$address, c("1 Research Link", ""))
  expect_equal(place_icon(back$places$kind), c("graduation-cap", "location-dot"))
})

test_that("place_address() drops the building name and ends with the postal code", {
  shop <- data.frame(address = "1 FUSIONOPOLIS WAY CHICKY FUN PLUS CONNEXIS",
                     building = "CHICKY FUN PLUS CONNEXIS", postal = "138632")
  expect_equal(place_address(shop), "1 Fusionopolis Way, Singapore 138632")
  blk <- data.frame(address = "201 TAMPINES STREET 21 SINGAPORE 520201", building = "NIL",
                    postal = "520201")
  expect_equal(place_address(blk), "201 Tampines Street 21, Singapore 520201")
  only_name <- data.frame(address = "CHANGI AIRPORT AIRCRAFT FLYOVER",
                          building = "CHANGI AIRPORT AIRCRAFT FLYOVER", postal = "NIL")
  expect_equal(place_address(only_name), "Changi Airport Aircraft Flyover")
})

test_that("default_place_name() uses the icon's label, numbered if taken", {
  expect_equal(default_place_name("work", character()), "Work")
  expect_equal(default_place_name("work", c("Work", "Work 2")), "Work 3")
  expect_equal(default_place_name("nope", character()), "Place")
})

test_that("'Other' places keep the kind of place the renter typed", {
  expect_equal(place_label(c("work", "other", "other"), c("ignored", "  church  ", "")),
               c("Work", "Church", "Other"))
  expect_equal(default_place_name("other", "Church", label = "Church"), "Church 2")

  s <- default_settings(choices)
  s$places <- new_places(name = c("Office", "St Andrew's"), kind = c("work", "other"),
                         label = c("Work", "Church"), address = "", lat = c(1.30, 1.29),
                         lng = c(103.80, 103.85))
  back <- settings_from_json(settings_to_json(s), choices)
  expect_equal(back$places$label, c("Work", "Church"))
})

test_that("chosen_place() reads a picked suggestion, or looks up typed text", {
  pick <- '{"name":"NUS","address":"21 LOWER KENT RIDGE ROAD SINGAPORE 119077","postal":"119077","lat":1.29,"lng":103.77}'
  p <- chosen_place(pick, search = function(q) stop("should not search"))
  expect_equal(p$name, "NUS")
  expect_equal(p$lat, 1.29)

  fake <- function(q) data.frame(address = "1 MAIN ROAD", building = "NIL", postal = "NIL",
                                 lat = 1.3, lng = 103.8)
  typed <- chosen_place("1 main road", search = fake)
  expect_equal(typed$name, "1 MAIN ROAD")
  expect_equal(nrow(chosen_place("", search = fake)), 0)
  expect_equal(nrow(chosen_place("nowhere", search = function(q) data.frame())), 0)
})

test_that("period_for() covers the last N months of data, at most all of it", {
  dates <- as.Date(c("2021-09-01", "2026-08-01"))
  expect_equal(period_for(12, dates), as.Date(c("2025-08-02", "2026-08-01")))
  expect_equal(period_for(60, dates), as.Date(c("2021-09-01", "2026-08-01")))
  # the same rentals as latest_months()
  d <- data.frame(lease_date = seq(dates[1], dates[2], by = "month"), rent = 1)
  p <- period_for(12, dates)
  expect_equal(latest_months(d, 12)$lease_date,
               d$lease_date[d$lease_date >= p[1] & d$lease_date <= p[2]])
})

test_that("title_case_place() keeps acronyms in capitals", {
  expect_equal(title_case_place("NUS (RIDGE VIEW RESIDENTIAL COLLEGE (RVRC))"),
               "NUS (Ridge View Residential College (RVRC))")
  expect_equal(title_case_place("10A JLN BT MERAH"), "10A Jln Bt Merah")
  expect_equal(title_case_place("ST ANDREW'S CATHEDRAL"), "St Andrew's Cathedral")
  expect_equal(title_case_place("BANK OF SINGAPORE"), "Bank of Singapore")
  expect_equal(title_case_place("KKH"), "KKH")
})

test_that("places saved with the old 'home' icon become 'Other', labelled Home", {
  json <- '{"places": [{"name": "Flat", "kind": "home", "lat": 1.30, "lng": 103.80}]}'
  s <- settings_from_json(json, choices)
  expect_equal(s$places$kind, "other")
  expect_equal(s$places$label, "Home")
  expect_false("home" %in% place_kinds$kind)
})

test_that("the floor area's upper limit is exact, and the largest home means no limit", {
  s <- default_settings(choices)
  expect_equal(s$area, c(0, 24650))
  expect_false(any(grepl("sq ft", settings_summary(s, choices))))
  s$area <- c(0, 6000)
  back <- settings_from_json(settings_to_json(s), choices)
  expect_equal(back$area, c(0, 6000))
  expect_true("0 - 6,000 sq ft (0 - 557 m²)" %in% settings_summary(back, choices))
  # saved before version 2, the slider's top (4,000) meant "and above"
  old <- settings_from_json('{"version": 1, "area": [500, 4000]}', choices)
  expect_equal(old$area, c(500, 24650))
  # from version 2 on, 4,000 is a real limit
  s$area <- c(500, 4000)
  expect_equal(settings_from_json(settings_to_json(s), choices)$area, c(500, 4000))
})
