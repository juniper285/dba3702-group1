test_that("haversine_km() gives straight-line distances", {
  expect_equal(haversine_km(1.3, 103.8, 1.3, 103.8), 0)
  # one degree of latitude is about 111 km
  expect_equal(haversine_km(1, 103.8, 2, 103.8), 111.2, tolerance = 0.01)
})

test_that("add_nearest_mrt() picks the closest station", {
  coords <- data.frame(project = c("A", "B"), lat = c(1.30, 1.40), lng = c(103.80, 103.90))
  mrt <- data.frame(station = c("Near A", "Near B"), lat = c(1.301, 1.399), lng = c(103.80, 103.90))
  out <- add_nearest_mrt(coords, mrt)
  expect_equal(out$mrt_station, c("Near A", "Near B"))
  expect_equal(out$mrt_km, c(0.1, 0.1))
})

test_that("add_distance() adds the distance to the destination", {
  coords <- data.frame(project = "A", lat = 1.2966, lng = 103.7764)
  s <- add_distance(data.frame(project = c("A", "Z")), "NUS (Kent Ridge)", coords)
  expect_equal(s$dest_km, c(0, NA))
  expect_false(any(c("lat", "lng") %in% names(s)))
})
