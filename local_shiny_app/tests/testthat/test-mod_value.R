test_that("Value for money keeps projects with enough rentals and a floor area", {
  rentals <- rentals_data()
  filters <- list(market = reactive(rentals[rentals$district_no == 15, ]), recent = reactive(12),
                  budget = reactive(c(3000, 5000)), places = reactive(key_place("NUS (Kent Ridge)")))
  testServer(mod_value_server, args = list(filters = filters), {
    session$setInputs(min_n = 5)
    expect_true(all(value_data()$n >= 5))
    expect_true(all(value_data()$fits %in% c("Below range", "Within range", "Above range")))
    expect_no_error(output$plot)
  })
})
