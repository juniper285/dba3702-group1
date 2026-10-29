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

test_that("picking a station keeps rentals near it and starts from a 500 m walk", {
  testServer(mod_filters_server, {
    rentals <- rentals_data()
    session$setInputs(
      budget = c(3000, 5000), district = NULL, project = NULL,
      beds = bed_levels, type = unique(rentals$type), area = c(0, area_max),
      period = range(rentals$lease_date), recent = "12", destination = "NUS (Kent Ridge)",
      mrt_pick = "", mrt_dist = mrt_max
    )
    session$setInputs(mrt_pick = "Novena MRT", mrt_dist = 500)
    st <- station_location("Novena MRT")
    expect_gt(nrow(market()), 0)
    expect_true(all(haversine_km(market()$lat, market()$lng, st[1], st[2]) <= 0.5))
    expect_match(summary(), "within 500 m of Novena MRT")
  })
})
