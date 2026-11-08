
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

test_that("rl_datatable() hides detail columns behind a Columns button", {
  s <- data.frame(Project = "A", District = "D19 Serangoon Garden, Hougang", `Median rent` = 1,
                  `25th percentile` = 1, `Recent rentals` = 3, check.names = FALSE)
  dt <- rl_datatable(s)
  opts <- dt$x$options
  hidden <- unlist(lapply(opts$columnDefs, function(d) if (isFALSE(d$visible)) d$targets))
  expect_equal(hidden, c(3, 4))
  expect_equal(opts$buttons[[1]]$extend, "colvis")
  expect_equal(opts$buttons[[1]]$columns, 1:4)          # Project can't be hidden
  expect_match(opts$dom, "B")
  expect_true(any(vapply(dt$dependencies, function(d) d$name == "dt-ext-buttons", logical(1))))
  # the District cell stays on one line, cut with an ellipsis
  district <- Filter(function(d) identical(d$className, "dt-district"), opts$columnDefs)[[1]]
  expect_equal(district$targets, 1)
  # nothing hidden: no button, same as before
  plain <- rl_datatable(s, hide = character())$x$options
  expect_null(plain$buttons)
  expect_false(grepl("B", plain$dom))
  expect_false(any(vapply(plain$columnDefs, function(d) isFALSE(d$visible), logical(1))))
  # every table can have its columns resized
  expect_match(as.character(plain$initComplete), "setupTable")
  # Project is never hidden, even when asked
  p <- rl_datatable(s, hide = "Project")$x$options
  expect_null(p$buttons)
})

test_that("short_colnames() shortens long headers, keeping the unit", {
  expect_equal(short_colnames(c("Project", "To NUS (Ridge View Residential College (RVRC)) (km)")),
               c("Project", "To NUS (Ridge View… (km)"))
})

test_that("rl_datatable(star =) adds a star cell that isn't a real column", {
  s <- data.frame(Project = "A", Bedrooms = "2", `Recent rentals` = 1, check.names = FALSE)
  dt <- rl_datatable(s, star = "within_budget-star")
  expect_equal(dt$x$data[[1]], shortlist_key("A", "2"))
  opts <- dt$x$options
  star <- Filter(function(d) identical(d$className, "rl-star-cell"), opts$columnDefs)[[1]]
  expect_equal(star$targets, 0)
  expect_false(star$orderable)
  expect_match(as.character(star$render), "within_budget-star")
  expect_equal(opts$buttons[[1]]$columns, 2:3)          # not the star, not Project
})
