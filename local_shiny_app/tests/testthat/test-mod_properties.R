test_that("Compare properties fills from the shortlist and summarises each project", {
  rentals <- rentals_data()
  top <- names(sort(table(rentals$project), decreasing = TRUE))[1:2]
  filters <- list(period = reactive(range(rentals$lease_date)), budget = reactive(c(3000, 5000)),
                  destination = reactive("NUS (Kent Ridge)"))
  shortlist <- reactiveVal(data.frame(project = top, bed_group = "1"))

  testServer(mod_properties_server, args = list(filters = filters, shortlist = shortlist), {
    session$setInputs(a = top[1], b = top[2], c = "", bed = "All")
    expect_equal(summary_data()$project, top)
    expect_true(all(summary_data()$n > 0))
    expect_true(all(c("dest_km", "walk_min") %in% names(summary_data())))

    session$setInputs(bed = "2")
    expect_true(all(data()$bed_group == "2"))
  })
})
