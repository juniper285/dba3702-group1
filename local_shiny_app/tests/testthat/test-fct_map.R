test_that("drawn_to_sf() converts drawn polygons and circles", {
  square <- list(geometry = list(type = "Polygon", coordinates = list(list(
    list(103.8, 1.3), list(103.9, 1.3), list(103.9, 1.4), list(103.8, 1.4), list(103.8, 1.3)))))
  circle <- list(geometry = list(type = "Point", coordinates = list(103.85, 1.35)),
                 properties = list(radius = 1000))
  inside <- sf::st_sfc(sf::st_point(c(103.85, 1.35)), crs = 4326)
  expect_true(lengths(sf::st_intersects(inside, drawn_to_sf(square))) == 1)
  expect_true(lengths(sf::st_intersects(inside, drawn_to_sf(circle))) == 1)
})
