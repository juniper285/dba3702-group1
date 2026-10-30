
test_that("rl_datatable() searches the Project column only, when there is one", {
  s <- data.frame(Project = c("A", "B"), District = "D10", Rent = 1:2)
  opts <- rl_datatable(s)$x$options
  expect_equal(opts$language$searchPlaceholder, "Search project")
  not_searched <- function(o) unlist(lapply(o$columnDefs, function(d) if (isFALSE(d$searchable)) d$targets))
  expect_equal(not_searched(opts), 1:2)
  rentals <- rl_datatable(s, search = "rental")$x$options
  expect_equal(rentals$language$searchPlaceholder, "Search rental")
  expect_null(not_searched(rentals))
  expect_equal(rl_datatable(data.frame(x = 1))$x$options$language$searchPlaceholder, "Search table")
})

test_that("short_colnames() shortens long headers, keeping the unit", {
  expect_equal(short_colnames(c("Project", "To NUS (Ridge View Residential College (RVRC)) (km)")),
               c("Project", "To NUS (Ridge View… (km)"))
})
