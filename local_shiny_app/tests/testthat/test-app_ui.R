test_that("the whole page renders, with the settings script and controls", {
  r <- htmltools::renderTags(app_ui(NULL))
  html <- as.character(r$html)
  for (id in c("settings-open_nav", "settings-open", "filters-district", "filters-project")) {
    expect_match(html, id, fixed = TRUE)
  }
  expect_true("rentlens-settings" %in% vapply(r$dependencies, function(d) d$name, ""))
})
