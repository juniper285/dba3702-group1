test_that("filters narrow the market and the selection", {
  testServer(mod_filters_server, {
    rentals <- rentals_data()
    session$setInputs(
      budget = c(3000, 5000), district = "D10 Bukit Timah, Holland, Tanglin", project = NULL,
      beds = bed_levels, type = unique(rentals$type), area = c(0, area_max),
      period = range(rentals$lease_date), recent = "12", destination = names(destinations)[1]
    )
    expect_gt(nrow(market()), 0)
    expect_true(all(market()$district == "D10 Bukit Timah, Holland, Tanglin"))

    one <- market()$project[1]
    session$setInputs(project = one)
    out <- session$getReturned()
    expect_true(all(out$selection()$project == one))
    expect_true(all(out$comparables()$lease_date >= max(out$selection()$lease_date) - 366))
  })
})
