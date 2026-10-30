test_that("clicking a building selects it and its rows can be shortlisted", {
  rentals <- rentals_data()
  filters <- list(
    base     = reactive(rentals[rentals$district_no == 1, ]),
    projects = reactive(NULL),
    recent   = reactive(12),
    budget   = reactive(c(3000, 5000)),
    mrt_pick = reactive(NULL),
    places   = reactive(key_place("NTU")),
    mrt_dist = reactive(mrt_max)
  )
  shortlist <- reactiveVal(empty_shortlist())
  areas <- new_saved_areas()

  testServer(mod_map_server, args = list(filters = filters, shortlist = shortlist,
                                         areas = areas), {
    session$setInputs(metric = "psf", geo_match = unique(rentals$geo_match))
    expect_true(all(selected()$district_no == 1))

    loc <- loc_summary()$loc_id[1]
    session$setInputs(map_marker_click = list(id = as.character(loc)))
    expect_true(all(selected()$loc_id == loc))
    expect_equal(output$vb_proj, "1")
    expect_equal(nrow(contracts()), nrow(selected()))
    expect_true(all(contracts()$Project == selected()$project[1]))

    session$setInputs(table_rows_selected = 1, add_short = 1)
    expect_equal(nrow(shortlist()), 1)
    expect_equal(shortlist()$project, table_data()$project[1])

    session$setInputs(save_a = 1)
    expect_equal(areas$a_ids, loc)

    session$setInputs(clear = 1)
    expect_null(sel$ids)
    expect_null(areas$a_ids)
  })
})

test_that("an applied map selection narrows the filters' market", {
  map_sel <- new_map_selection()
  testServer(mod_filters_server, args = list(settings = fake_settings(), map_sel = map_sel), {
    session$setInputs(district = NULL, project = NULL)
    loc <- base()$loc_id[!is.na(base()$loc_id)][1]
    map_sel$ids <- loc; map_sel$label <- "One building"
    session$flushReact()
    expect_true(all(market()$loc_id == loc))
    expect_match(paste(summary(), collapse = " · "), "Map selection: One building")

    map_sel$apply <- FALSE
    session$flushReact()
    expect_equal(nrow(market()), nrow(base()))
  })
})
