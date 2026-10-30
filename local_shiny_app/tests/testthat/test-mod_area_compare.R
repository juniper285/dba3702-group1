test_that("Area A vs B compares the two saved areas", {
  rentals <- rentals_data()
  d <- rentals[!is.na(rentals$loc_id) & rentals$district_no %in% c(1, 9), ]
  areas <- new_saved_areas()
  areas$a_ids <- unique(d$loc_id[d$district_no == 1]); areas$a_label <- "Marina"
  areas$b_ids <- unique(d$loc_id[d$district_no == 9]); areas$b_label <- "Orchard"

  filters <- list(settings = fake_settings()$settings)
  testServer(mod_area_compare_server, args = list(map_data = reactive(d), areas = areas,
                                                  filters = filters), {
    expect_gt(nrow(singapore()), nrow(both()))       # the Singapore reference
    expect_match(as.character(output$a_tiles$html), "Singapore median")
    expect_s3_class(output$beds, "json")
    expect_true(all(a_data()$district_no == 1))
    expect_true(all(b_data()$district_no == 9))
    expect_setequal(unique(both()$area), c("A: Marina", "B: Orchard"))
    expect_match(output$a_title, "Area A: Marina")
  })
})
