test_that("the shortlist summarises each option and draws its rent trend", {
  rentals <- rentals_data()
  one_bed <- rentals[rentals$bed_group == "1" & !grepl("HOUSING DEVELOPMENT", rentals$project), ]
  top <- names(sort(table(one_bed$project), decreasing = TRUE))[1:2]
  filters <- list(budget = reactive(c(3000, 5000)), period = reactive(range(rentals$lease_date)),
                  recent = reactive(12), destination = reactive("NUS (Kent Ridge)"))
  shortlist <- reactiveVal(data.frame(project = top, bed_group = "1"))

  testServer(mod_shortlist_server, args = list(filters = filters, shortlist = shortlist), {
    expect_setequal(short_data()$project, top)
    expect_true(all(short_contracts()$bed_group == "1"))
    expect_no_error(output$trend)
  })
})
