test_that("dropping a pin compares the asking rent with past rentals", {
  filters <- list(recent = reactive(12))
  shortlist <- reactiveVal(empty_shortlist())

  testServer(mod_listing_check_server, args = list(filters = filters, shortlist = shortlist), {
    # The Sail @ Marina Bay
    session$setInputs(beds = "2", type = "Non-Landed Properties", radius = 500,
                      asking = 6000, area = NA, level = "Nearby",
                      map_click = list(lat = 1.28077, lng = 103.85266))
    expect_equal(attr(sets(), "project"), "THE SAIL @ MARINA BAY")
    expect_equal(attr(sets(), "region"), "Central")

    t <- levels_table()
    expect_equal(nrow(t), 4)
    expect_true(all(t$n[2:4] > 0))
    expect_true(all(diff(t$n[2:4]) >= 0))    # each level is wider than the last
    expect_true(all(t$pct_below >= 0 & t$pct_below <= 100))

    session$setInputs(add_short = 1)
    expect_equal(shortlist()$project, "THE SAIL @ MARINA BAY")
    expect_equal(shortlist()$bed_group, "2")
  })
})

test_that("the verdict explains a missing project and flags thin data", {
  testServer(mod_listing_check_server,
             args = list(filters = list(recent = reactive(12)), shortlist = reactiveVal(empty_shortlist())), {
    # Middle of the Central Catchment: no project here
    session$setInputs(beds = "Any", type = "Any", radius = 500, asking = 3000, area = NA,
                      level = "Same project", map_click = list(lat = 1.3600, lng = 103.8100))
    expect_match(output$verdict$html, "isn&#39;t in our data|isn't in our data")

    session$setInputs(level = "Same district")
    n <- level_row()$n
    expect_gt(n, 0)
    expect_equal(grepl("rough guide", output$verdict$html), n < few_rentals)
  })
})
