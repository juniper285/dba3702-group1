choices <- list(districts = c("D01 Raffles Place, Marina", "D10 Bukit Timah, Holland, Tanglin"),
                types = c("Landed Properties", "Non-Landed Properties"),
                stations = c("Novena MRT", "Kent Ridge MRT"),
                dates = as.Date(c("2021-09-01", "2026-08-01")))

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
            "period": ["2020-01-01", "2030-01-01"],
            "places": [{"name": "London", "lat": 51.5, "lng": -0.1},
                       {"name": "Office", "lat": 1.29, "lng": 103.85}]}'
  s <- settings_from_json(json, choices)
  d <- default_settings(choices)
  expect_equal(s$budget, d$budget)          # min above max
  expect_equal(s$beds, "2")                 # unknown "9" dropped
  expect_equal(s$types, d$types)            # nothing valid left
  expect_equal(s$mrt_pick, "")
  expect_equal(s$mrt_dist, d$mrt_dist)
  expect_equal(s$recent, 12)
  expect_equal(s$period, choices$dates)     # clamped to the data
  expect_equal(s$places$name, "Office")     # outside Singapore dropped
})

test_that("settings_summary() describes the settings", {
  s <- default_settings(choices)
  s$places <- key_place(c("NTU", "SMU")); s$place_km <- 5; s$mrt_dist <- 500
  txt <- paste(settings_summary(s, choices), collapse = " · ")
  expect_match(txt, "S\\$3,000 - S\\$5,000")
  expect_match(txt, "near NTU, SMU")
  expect_match(txt, "within 5 km of them")
  expect_match(txt, "within 500 m of an MRT/LRT")
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
