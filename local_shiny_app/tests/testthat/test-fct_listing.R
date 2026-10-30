past <- data.frame(
  project     = c("A", "A", "B", "C", "D"),
  district_no = c(1L, 1L, 1L, 2L, 22L),
  district    = c("D01", "D01", "D01", "D02", "D22"),
  rent        = c(3000, 4000, 5000, 6000, 2000),
  psf         = c(4, 5, 6, 7, 2),
  lat         = c(1.2800, 1.2800, 1.2830, 1.2700, 1.3400),
  lng         = c(103.8500, 103.8500, 103.8500, 103.8400, 103.7000),
  geo_match   = c("Exact", "Exact", "Exact", "Street only (project not found)", "Exact")
)

test_that("district_region() maps postal districts to regions", {
  expect_equal(district_region(c(1, 15, 22, 27, 19)),
               c("Central", "East", "West", "North", "North-East"))
})

test_that("comparable_sets() finds the project, the radius, the district and the region", {
  s <- comparable_sets(past, list(lat = 1.2800, lng = 103.8500, building = "a"), radius_m = 500)
  expect_equal(attr(s, "project"), "A")
  expect_equal(attr(s, "region"), "Central")
  expect_equal(nrow(s[["Same project"]]), 2)
  expect_equal(s[["Within 500 m"]]$project, c("A", "A", "B"))   # B is ~330 m away
  expect_equal(nrow(s[["Same district"]]), 3)
  expect_equal(nrow(s[["Same region"]]), 4)                     # D22 is West
})

test_that("comparable_sets() ignores street-level matches for distances", {
  s <- comparable_sets(past, list(lat = 1.2700, lng = 103.8400), radius_m = 100)
  expect_true(is.na(attr(s, "project")))
  expect_equal(nrow(s[["Within 100 m"]]), 0)
})

test_that("compare_levels() places the asking rent among past rents", {
  s <- comparable_sets(past, list(lat = 1.2800, lng = 103.8500, building = "A"))
  t <- compare_levels(s, asking = 4500, area_sqft = 1000)
  expect_equal(t$n, c(2, 3, 3, 4))
  expect_equal(t$median[1], 3500)
  expect_equal(t$pct_below[2], 100 * 2 / 3)
  expect_equal(t$pct_psf_below[1], 50)    # 4.5 psf vs 4 and 5
  expect_equal(t$diff_pct[1], 100 * (4500 / 3500 - 1))
  expect_equal(t$diff_pct[2], 12.5)        # 4,500 vs median 4,000
  expect_true(all(is.na(compare_levels(s)$pct_below)))
})

test_that("pct_below() counts ties as half", {
  expect_equal(pct_below(c(1, 2, 3, NA), 2), 50)
  expect_equal(pct_below(c(1, 2, 3), 10), 100)
})

test_that("signed_pct() shows the sign and the direction", {
  expect_equal(signed_pct(c(4.24, -3.1, 0.01, NA)),
               c("+4.2% above", "-3.1% below", "0.0% (at median)", "-"))
})

test_that("asking_verdict() reads as a sentence", {
  row <- data.frame(level = "Within 500 m", n = 143, pct_below = 72.2)
  expect_equal(asking_verdict(row, 4800, "2"),
               "S$4,800 is higher than 72% of 2-bedroom rentals within 500 m (143 rentals).")
  row <- data.frame(level = "Same district", n = 10, pct_below = 30)
  expect_equal(asking_verdict(row, 3000, "Any"),
               "S$3,000 is lower than 70% of rentals in the same district (10 rentals).")
})

test_that("no_rentals_message() points to a wider level", {
  expect_match(no_rentals_message("Same project"), "isn't in our data.*Try Nearby or Same district")
  expect_match(no_rentals_message("Nearby"), "^No past rentals nearby")
  expect_match(no_rentals_message("Same district"), "^No past rentals in the district")
})
