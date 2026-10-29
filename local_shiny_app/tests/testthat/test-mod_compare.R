test_that("Compare areas summarises districts and draws the heatmap", {
  rentals <- rentals_data()
  filters <- list(market = reactive(rentals), recent = reactive(12),
                  budget = reactive(c(3000, 5000)), places = reactive(key_place("NUS (Kent Ridge)")))
  testServer(mod_compare_server, args = list(filters = filters), {
    session$setInputs(compare_by = "District", min_n = 5)
    expect_true(all(compare_data()$n >= 5))
    expect_no_error(output$heat)
    expect_true(all(mrt_projects()$n >= 5))
    expect_equal(mrt_projects()$walk_min, walk_minutes(mrt_projects()$mrt_m))
    expect_no_error(output$mrt_plot)
  })
})
