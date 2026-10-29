rents <- data.frame(
  project    = c("A", "A", "A", "B"),
  district   = "D01",
  bed_group  = c("1", "1", "2", "1"),
  area_band  = c("400 to 500", "400 to 500", "600 to 700", "400 to 500"),
  rent       = c(3000, 3400, 5000, 2000),
  psf        = c(6, NA, 7, 4),
  lease_date = as.Date(c("2024-01-01", "2024-06-01", "2024-06-01", "2023-01-01"))
)

test_that("latest_months() keeps the most recent months only", {
  expect_equal(nrow(latest_months(rents, 3)), 2)
  expect_equal(nrow(latest_months(rents, 6)), 3)
  expect_equal(nrow(latest_months(rents[0, ], 6)), 0)
})

test_that("summarise_rent() reports the share within budget and ignores missing psf", {
  s <- summarise_rent(rents, budget = c(2500, 4000))
  expect_equal(s$n, 4)
  expect_equal(s$within, 0.5)
  expect_equal(s$psf, 6)
})

test_that("summarise_options() gives one row per project and bedroom type", {
  s <- summarise_options(rents)
  expect_equal(nrow(s), 3)
  a1 <- s[s$project == "A" & s$bed_group == "1", ]
  expect_equal(a1$median, 3200)
  expect_equal(a1$size, "400 to 500")
  expect_equal(a1$last, as.Date("2024-06-01"))
})

test_that("budget_verdict() compares the budget with the typical range", {
  r <- c(3000, 3500, 4000, 4500, 5000)          # typical range 3,500 - 4,500
  expect_equal(budget_verdict(r, c(1000, 2000))$verdict, "Below typical")
  expect_equal(budget_verdict(r, c(6000, 7000))$verdict, "Above typical")
  v <- budget_verdict(r, c(3000, 4000))
  expect_equal(v$verdict, "Realistic")
  expect_equal(v$share, 60)
})

test_that("compare_two_areas() lines up the areas by bedrooms", {
  a <- data.frame(bed_group = c("1", "1", "2"), rent = c(3000, 3200, 4000), psf = 5)
  b <- data.frame(bed_group = c("1", "3"), rent = c(3300, 6000), psf = 4)
  s <- compare_two_areas(a, b)
  expect_equal(s$bed_group, c("1", "2", "3"))
  expect_equal(s$a_median, c(3100, 4000, NA))
  expect_equal(s$b_n, c(1, NA, 1))
  expect_equal(s$diff[1], 200 / 3100)
})
