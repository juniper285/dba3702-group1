test_that("the shortlist summarises each option and draws its rent trend", {
  rentals <- rentals_data()
  one_bed <- rentals[rentals$bed_group == "1" & !grepl("HOUSING DEVELOPMENT", rentals$project), ]
  top <- names(sort(table(one_bed$project), decreasing = TRUE))[1:2]
  filters <- list(budget = reactive(c(3000, 5000)), period = reactive(range(rentals$lease_date)),
                  recent = reactive(12), places = reactive(key_place("NUS (Kent Ridge)")))
  shortlist <- reactiveVal(data.frame(project = top, bed_group = "1"))

  testServer(mod_shortlist_server, args = list(filters = filters, shortlist = shortlist), {
    expect_setequal(short_data()$project, top)
    expect_true(all(short_contracts()$bed_group == "1"))
    expect_no_error(output$trend)
  })
})

test_that("toggle_shortlist() stars and unstars one option", {
  sl <- reactiveVal(empty_shortlist())
  key <- shortlist_key("THE SAIL @ MARINA BAY", "2")
  session <- MockShinySession$new()
  withReactiveDomain(session, {
    expect_true(isolate(toggle_shortlist(sl, key)))
    expect_equal(isolate(sl())$project, "THE SAIL @ MARINA BAY")
    expect_equal(isolate(sl())$bed_group, "2")
    expect_false(isolate(toggle_shortlist(sl, key)))
    expect_equal(nrow(isolate(sl())), 0)
  })
})
