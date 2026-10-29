raw <- data.frame(
  district_no = c(1L, 10L, 10L),
  bedrooms    = c(1, 6, NA),
  lease_date  = as.Date(c("2024-02-10", "2024-05-01", "2024-05-20")),
  lat         = c(1.3, 1.3, NA),
  lng         = c(103.8, 103.8, NA),
  type        = "Non-Landed Properties",
  area_sqft   = c(450, NA, 1200)
)

test_that("prepare_rentals() adds labels, bedroom groups, quarters and locations", {
  d <- prepare_rentals(raw)
  expect_equal(d$district[2], "D10 Bukit Timah, Holland, Tanglin")
  expect_equal(d$bed_group, c("1", "5+", "Unknown"))
  expect_equal(d$quarter, as.Date(c("2024-01-01", "2024-04-01", "2024-04-01")))
  expect_equal(d$loc_id, c(1L, 1L, NA))
})

test_that("filter_market() applies the sidebar filters and drops missing floor areas", {
  d <- prepare_rentals(raw)
  all <- filter_market(d, NULL, bed_levels, d$type, c(0, area_max),
                       as.Date(c("2024-01-01", "2024-12-31")))
  expect_equal(nrow(all), 2)                    # row 2 has no floor area
  expect_false(anyNA(all$district))
  big <- filter_market(d, NULL, bed_levels, d$type, c(1000, area_max),
                       as.Date(c("2024-01-01", "2024-12-31")))
  expect_equal(big$area_sqft, 1200)
  expect_equal(nrow(filter_market(d, "D01 Raffles Place, Marina", bed_levels, d$type,
                                  c(0, area_max), as.Date(c("2024-01-01", "2024-12-31")))), 1)
})

test_that("filter_market() keeps buildings near an MRT/LRT station", {
  d <- prepare_rentals(raw)
  d$area_sqft <- 500
  d$mrt_m <- c(300, 300, NA)
  period <- as.Date(c("2024-01-01", "2024-12-31"))
  near_any <- filter_market(d, NULL, bed_levels, d$type, c(0, area_max), period, mrt_dist = 500)
  expect_equal(nrow(near_any), 2)
  expect_equal(nrow(filter_market(d, NULL, bed_levels, d$type, c(0, area_max), period,
                                  mrt_dist = 200)), 0)
  # 1.3, 103.8 is about 111 m south of this point
  station <- c(1.301, 103.8)
  expect_equal(nrow(filter_market(d, NULL, bed_levels, d$type, c(0, area_max), period,
                                  near = station, mrt_dist = 200)), 2)
  expect_equal(nrow(filter_market(d, NULL, bed_levels, d$type, c(0, area_max), period,
                                  near = station, mrt_dist = 100)), 0)
})

test_that("filter_market() keeps buildings near one of my places", {
  d <- prepare_rentals(raw)
  d$area_sqft <- 500
  period <- as.Date(c("2024-01-01", "2024-12-31"))
  places <- data.frame(name = c("Far", "Near"), lat = c(1.45, 1.305), lng = c(103.8, 103.8))
  near <- filter_market(d, NULL, bed_levels, d$type, c(0, area_max), period,
                        places = places, place_km = 1)
  expect_equal(nrow(near), 2)               # about 0.56 km from "Near"
  expect_equal(nrow(filter_market(d, NULL, bed_levels, d$type, c(0, area_max), period,
                                  places = places[1, ], place_km = 1)), 0)
})
