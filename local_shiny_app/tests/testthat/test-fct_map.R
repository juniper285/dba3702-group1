test_that("drawn_to_sf() converts drawn polygons and circles", {
  square <- list(geometry = list(type = "Polygon", coordinates = list(list(
    list(103.8, 1.3), list(103.9, 1.3), list(103.9, 1.4), list(103.8, 1.4), list(103.8, 1.3)))))
  circle <- list(geometry = list(type = "Point", coordinates = list(103.85, 1.35)),
                 properties = list(radius = 1000))
  inside <- sf::st_sfc(sf::st_point(c(103.85, 1.35)), crs = 4326)
  expect_true(lengths(sf::st_intersects(inside, drawn_to_sf(square))) == 1)
  expect_true(lengths(sf::st_intersects(inside, drawn_to_sf(circle))) == 1)
})

test_that("base_map() has the key-place and MRT/LRT layers", {
  calls <- base_map()$x$calls
  methods <- vapply(calls, `[[`, "", "method")
  expect_true(all(c("addMarkers", "addAwesomeMarkers", "addCircleMarkers") %in% methods))
  overlays <- calls[[which(methods == "addLayersControl")]]$args[[2]]
  expect_true(all(c("Schools and work areas", "Airport, ports and hospitals",
                    "MRT/LRT stations") %in% overlays))
})

test_that("budget_fit() places rents below, within or above the budget", {
  f <- budget_fit(c(2000, 3000, 4000, 6000), c(3000, 5000))
  expect_equal(as.character(f), budget_levels[c(1, 2, 2, 3)])
  expect_equal(levels(f), budget_levels)
})

test_that("base_map() shows the schools layer when schools.csv is shipped", {
  skip_if(is.null(schools_data()))
  calls <- base_map()$x$calls
  methods <- vapply(calls, `[[`, "", "method")
  expect_true("Schools" %in% calls[[which(methods == "addLayersControl")]]$args[[2]])
})
