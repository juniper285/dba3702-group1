test_that("money() formats Singapore dollars", {
  expect_equal(money(4500), "S$4,500")
  expect_equal(money(1234.6), "S$1,235")
})

test_that("quarter_label() names the quarter", {
  expect_equal(quarter_label(as.Date(c("2024-01-15", "2024-05-01", "2024-12-31"))),
               c("2024 Q1", "2024 Q2", "2024 Q4"))
})

test_that("prettify() renames known columns only", {
  s <- prettify(data.frame(project = 1, median = 2, other = 3))
  expect_equal(names(s), c("Project", "Median rent", "other"))
})

test_that("dt_bookmark_inputs() lists every DataTable input", {
  ids <- dt_bookmark_inputs(c("a", "b"))
  expect_true(all(c("a_rows_selected", "b_rows_selected", "a_state") %in% ids))
})

test_that("area_text() gives both square feet and square metres", {
  expect_equal(area_text(c(500, 1200)), "500 - 1,200 sq ft (46 - 111 m²)")
  expect_equal(area_text(c(500, 24650), top = 24650), "500+ sq ft (46+ m²)")
  expect_equal(area_text(c(500, 6000), top = 24650), "500 - 6,000 sq ft (46 - 557 m²)")
})
