test_that("a first visit has nothing saved; a saved profile is loaded", {
  testServer(mod_settings_server, {
    session$setInputs(stored = "none")
    expect_true(first_visit())
  })

  saved <- default_settings()
  saved$budget <- c(2000, 2800)
  testServer(mod_settings_server, {
    session$setInputs(stored = settings_to_json(saved))
    expect_false(first_visit())
    expect_equal(settings()$budget, c(2000, 2800))
  })
})

test_that("saving the dialog stores every section", {
  testServer(mod_settings_server, {
    session$setInputs(stored = "none")
    session$setInputs(open = 1)
    session$setInputs(budget = c(2500, 3500), key_places = c("NTU", "SMU"), custom_keep = NULL,
                      districts = "D22 Jurong, Boon Lay", period = default_settings()$period,
                      recent = "24", beds = c("1", "2"), types = "Non-Landed Properties",
                      area = c(400, 1200), mrt_dist = 600, mrt_pick = "", place_km = 10)
    session$setInputs(save = 1)
    s <- settings()
    expect_equal(s$budget, c(2500, 3500))
    expect_equal(s$places$name, c("NTU", "SMU"))
    expect_equal(s$districts, "D22 Jurong, Boon Lay")
    expect_equal(s$recent, 24)
    expect_equal(s$beds, c("1", "2"))
    expect_equal(s$mrt_dist, 600)
    expect_equal(s$place_km, 10)
    expect_false(first_visit())
  })
})

test_that("the first-visit wizard walks through every section and saves at the end", {
  testServer(mod_settings_server, {
    session$setInputs(stored = "none")
    expect_equal(step(), 1)

    session$setInputs(budget = c(2000, 3000), next_step = 1)
    expect_equal(step(), 2)
    session$setInputs(key_places = "SMU", custom_keep = NULL, next_step = 2)
    session$setInputs(districts = NULL, period = default_settings()$period, recent = "12",
                      next_step = 3)
    session$setInputs(beds = "1", types = "Non-Landed Properties", area = c(0, area_max),
                      next_step = 4)
    expect_equal(step(), 5)
    session$setInputs(mrt_dist = 500, mrt_pick = "", place_km = 5)
    expect_no_error(output$preview)

    session$setInputs(back = 1)
    expect_equal(step(), 4)
    session$setInputs(next_step = 5, finish = 1)
    s <- settings()
    expect_equal(s$budget, c(2000, 3000))
    expect_equal(s$places$name, "SMU")
    expect_equal(s$beds, "1")
    expect_equal(s$place_km, 5)
    expect_false(first_visit())
  })
})
