test_that("haversine_km() gives straight-line distances", {
  expect_equal(haversine_km(1.3, 103.8, 1.3, 103.8), 0)
  # one degree of latitude is about 111 km
  expect_equal(haversine_km(1, 103.8, 2, 103.8), 111.2, tolerance = 0.01)
})

mrt <- data.frame(station = c("Near A", "Near B"), lat = c(1.301, 1.399), lng = c(103.80, 103.90))

test_that("nearest_mrt() picks the closest station, in metres", {
  out <- nearest_mrt(c(1.30, 1.40, NA), c(103.80, 103.90, NA), mrt)
  expect_equal(out$mrt_station, c("Near A", "Near B", NA))
  expect_equal(out$mrt_m, c(111, 111, NA))
  expect_equal(walk_minutes(out$mrt_m), c(1, 1, NA))
})

test_that("add_location_mrt() gives every contract its location's station", {
  d <- data.frame(loc_id = c(1L, 2L, 1L, NA), lat = c(1.30, 1.40, 1.30, NA),
                  lng = c(103.80, 103.90, 103.80, NA))
  out <- add_location_mrt(d, mrt)
  expect_equal(out$mrt_station, c("Near A", "Near B", "Near A", NA))
  expect_true(all(is.na(add_location_mrt(d, NULL)$mrt_m)))
})

test_that("add_distance() adds a distance per place and the walking time", {
  coords <- data.frame(project = "A", lat = 1.2966, lng = 103.7764,
                       mrt_station = "Kent Ridge MRT", mrt_m = 400)
  places <- key_place(c("NUS (Kent Ridge)", "NTU", "Not a place"))
  expect_equal(places$name, c("NUS (Kent Ridge)", "NTU"))
  s <- add_distance(data.frame(project = c("A", "Z")), places, coords)
  expect_equal(s[["To NUS (Kent Ridge) (km)"]], c(0, NA))
  expect_equal(s[["To NTU (km)"]][1], 11.2, tolerance = 0.1)
  expect_equal(s$walk_min, c(5, NA))
  expect_false(any(c("lat", "lng") %in% names(s)))
})

test_that("every key place has a group and sits in Singapore", {
  expect_equal(nrow(key_places), length(destinations))
  expect_true(all(unlist(destination_choices) %in% names(destinations)))
  expect_true(all(key_places$lat > 1.2 & key_places$lat < 1.5))
  expect_true(all(key_places$lng > 103.6 & key_places$lng < 104.1))
})
