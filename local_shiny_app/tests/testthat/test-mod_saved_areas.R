test_that("saved areas are compared side by side, with Singapore for reference", {
  rentals <- rentals_data()
  d <- rentals[!is.na(rentals$loc_id) & rentals$district_no %in% c(1, 9), ]
  areas <- new_saved_areas()
  filters <- list(settings = fake_settings()$settings, budget = reactive(c(3000, 5000)),
                  places = reactive(key_place("SMU")))
  testServer(mod_saved_areas_server, args = list(data = reactive(d), areas = areas,
                                                 filters = filters), {
    # fewer than two: a hint instead of the comparison
    areas(add_area(list(), unique(d$loc_id[d$district_no == 1]), "Marina"))
    session$flushReact()
    expect_match(as.character(output$body$html), "Save one more area")

    areas(add_area(areas(), unique(d$loc_id[d$district_no == 9]), "Orchard"))
    session$flushReact()
    expect_equal(names(sets()), c("Marina", "Orchard"))
    expect_true(all(sets()$Marina$district_no == 1))
    html <- as.character(output$table$html)
    for (txt in c("Marina", "Orchard", "Singapore (reference)", "Within my budget",
                  "Walk to nearest MRT/LRT", "To SMU")) expect_match(html, txt, fixed = TRUE)
    expect_s3_class(output$beds, "json")
    expect_s3_class(output$trend, "json")
  })
})

test_that("the saved list holds up to four areas and survives a round trip", {
  locs <- map_locations()
  a <- list()
  for (i in 1:4) a <- add_area(a, locs$loc_id[i], paste("Area", i))
  expect_equal(length(a), 4)
  expect_null(add_area(a, locs$loc_id[5], "Too many"))
  expect_equal(vapply(a, `[[`, "", "colour"), area_palette)
  # a removed area's colour goes to the next one saved
  a <- add_area(a[-2], locs$loc_id[5], "Fifth")
  expect_equal(a[[4]]$colour, area_palette[2])

  back <- areas_from_json(areas_to_json(a))
  expect_equal(vapply(back, `[[`, "", "name"), vapply(a, `[[`, "", "name"))
  expect_equal(lapply(back, `[[`, "locs"), lapply(a, `[[`, "locs"))
  expect_equal(areas_from_json("not json"), list())
  expect_equal(areas_from_json('[{"name": "Gone", "keys": ["0 0"]}]'), list())
})

test_that("name_area() names a building after its project, and a drawn area after a station", {
  d <- rentals_data()
  d <- d[!is.na(d$loc_id) & d$district_no == 11, ]
  one <- d[d$loc_id == d$loc_id[1], ]
  expect_equal(name_area(one), paste(head(unique(one$project), 2), collapse = " / "))
  expect_match(name_area(d), "^Around .+ · [0-9]+ buildings$")
})
