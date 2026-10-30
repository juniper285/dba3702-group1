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
    places(new_places(name = c("NTU", "SMU"), kind = c("study", "work"), address = "",
                      lat = c(1.3483, 1.2963), lng = c(103.6831, 103.8502)))
    session$setInputs(budget = c(2500, 3500),
                      districts = "D22 Jurong, Boon Lay", recent = "24", beds = c("1", "2"), types = "Non-Landed Properties",
                      area = c(400, 1200), mrt_dist = 600, mrt_pick = "", place_km = 10)
    session$setInputs(save = 1)
    s <- settings()
    expect_equal(s$budget, c(2500, 3500))
    expect_equal(s$places$name, c("NTU", "SMU"))
    expect_equal(s$places$kind, c("study", "work"))
    expect_equal(s$districts, "D22 Jurong, Boon Lay")
    expect_equal(s$recent, 24)
    expect_equal(s$period, period_for(24, settings_choices()$dates))
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
    places(new_places(name = "SMU", kind = "study", address = "81 Victoria Street",
                      lat = 1.2963, lng = 103.8502))
    session$setInputs(next_step = 2)
    session$setInputs(districts = NULL, recent = "12",
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

test_that("places are removed one at a time and empty addresses are refused", {
  testServer(mod_settings_server, {
    session$setInputs(stored = settings_to_json(default_settings()))
    session$setInputs(open = 1)
    places(new_places(name = c("Office", "Gym"), kind = c("work", "leisure"),
                      address = "", lat = c(1.30, 1.31), lng = c(103.8, 103.81)))
    session$setInputs(remove_place = "Gym")
    expect_equal(places()$name, "Office")

    session$setInputs(place_search = "  ", place_name = "", place_kind = "work", add_place = 1)
    expect_equal(nrow(places()), 1)          # nothing looked up, nothing added

    session$setInputs(save = 1)
    expect_equal(settings()$places$name, "Office")
  })
})

test_that("a picked suggestion is added without another lookup", {
  testServer(mod_settings_server, {
    session$setInputs(stored = settings_to_json(default_settings()))
    session$setInputs(open = 1)
    pick <- jsonlite::toJSON(list(name = "KENT RIDGE HILL RESIDENCES", building = "KENT RIDGE HILL RESIDENCES",
                                  address = "50 SOUTH BUONA VISTA ROAD KENT RIDGE HILL RESIDENCES SINGAPORE 118167",
                                  postal = "118167", lat = 1.2905, lng = 103.7856), auto_unbox = TRUE)
    session$setInputs(place_search = as.character(pick), place_name = "", place_kind = "family",
                      place_kind_label = "", add_place = 1)
    expect_equal(places()$name, "Kent Ridge Hill Residences")
    expect_equal(places()$address, "50 South Buona Vista Road, Singapore 118167")
    expect_equal(places()$kind, "family")
  })
})

test_that("switched-off optional distances mean any distance", {
  testServer(mod_settings_server, {
    session$setInputs(stored = settings_to_json(default_settings()))
    session$setInputs(open = 1)
    session$setInputs(mrt_limit = FALSE, mrt_dist = 700, station_on = FALSE, mrt_pick = "Novena MRT",
                      place_limit = TRUE, place_km = 8)
    session$setInputs(save = 1)
    expect_equal(settings()$mrt_dist, mrt_max)     # off = any
    expect_equal(settings()$mrt_pick, "")          # off = no station
    expect_equal(settings()$place_km, 8)

    session$setInputs(open = 2)
    session$setInputs(mrt_limit = TRUE, mrt_dist = 700, station_on = TRUE, place_limit = FALSE)
    session$setInputs(save = 2)
    expect_equal(settings()$mrt_dist, 700)
    expect_equal(settings()$mrt_pick, "Novena MRT")
    expect_equal(settings()$place_km, place_km_max)
  })
})

test_that("the preview works before any place is added", {
  testServer(mod_settings_server, {
    session$setInputs(stored = "none")
    step(5)
    session$setInputs(mrt_limit = TRUE, mrt_dist = 1000, station_on = FALSE, mrt_pick = "",
                      place_limit = FALSE, place_km = 5)
    expect_match(output$preview$html, "options in your budget")
  })
})
