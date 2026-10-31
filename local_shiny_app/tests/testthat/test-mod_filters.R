test_that("settings and quick filters narrow the market and the selection", {
  d10 <- "D10 Bukit Timah, Holland, Tanglin"
  settings <- fake_settings(districts = d10)
  testServer(mod_filters_server, args = list(settings = settings), {
    session$setInputs(district = d10, project = NULL)
    expect_gt(nrow(market()), 0)
    expect_true(all(market()$district == d10))

    one <- market()$project[1]
    session$setInputs(project = one)
    out <- session$getReturned()
    expect_true(all(out$selection()$project == one))
    expect_true(all(out$comparables()$lease_date >= max(out$selection()$lease_date) - 366))
  })
})

test_that("changing the quick District filter saves it to the settings", {
  settings <- fake_settings()
  testServer(mod_filters_server, args = list(settings = settings), {
    session$setInputs(district = NULL, project = NULL)
    session$setInputs(district = "D09 Orchard, River Valley")
    expect_equal(settings$settings()$districts, "D09 Orchard, River Valley")
    expect_true(all(market()$district == "D09 Orchard, River Valley"))
  })
})

test_that("a station in the settings keeps rentals near it", {
  settings <- fake_settings(mrt_pick = "Novena MRT", mrt_dist = 500)
  testServer(mod_filters_server, args = list(settings = settings), {
    session$setInputs(district = NULL, project = NULL)
    st <- station_location("Novena MRT")
    expect_gt(nrow(market()), 0)
    expect_true(all(haversine_km(market()$lat, market()$lng, st[1], st[2]) <= 0.5))
    expect_match(paste(summary(), collapse = " · "), "Within 500 m of Novena MRT")
  })
})
