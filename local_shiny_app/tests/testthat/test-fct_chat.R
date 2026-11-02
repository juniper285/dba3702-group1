# A small market: two districts, three projects, around two stations
chat_rents <- data.frame(
  project     = c("PALM COURT", "PALM COURT", "PALM COURT", "HILL VIEW", "HILL VIEW", "SEA HOUSE"),
  street      = c("PALM ROAD", "PALM ROAD", "PALM ROAD", "HILL STREET", "HILL STREET", "SEA LANE"),
  district_no = c(11L, 11L, 11L, 11L, 11L, 15L),
  district    = c(rep("D11 Novena, Newton, Thomson", 5), "D15 Katong, Joo Chiat, Marine Parade"),
  type        = c(rep("Non-Landed Properties", 5), "Terrace House"),
  bed_group   = c("1", "2", "2", "2", "3", "Unknown"),
  rent        = c(3000, 4000, 4400, 5000, 6000, 9000),
  psf         = c(6, 5, 5.5, 5, 4.5, 3),
  area_sqft   = c(500, 800, 800, 1000, 1300, 3000),
  lease_date  = as.Date(c("2026-08-01", "2026-07-01", "2026-06-01", "2026-05-01", "2024-01-01",
                          "2026-08-01")),
  mrt_station = c("Novena MRT", "Novena MRT", "Novena MRT", "Newton MRT", "Newton MRT", "Marine Parade MRT"),
  mrt_m       = c(300, 300, 300, 600, 600, 400)
)
chat_rents$quarter <- as.Date(cut(chat_rents$lease_date, "quarter"))

test_that("chat_where() finds districts, stations, projects and streets", {
  expect_equal(chat_where(chat_rents, "D15")$label, "D15 Katong, Joo Chiat, Marine Parade")
  expect_equal(chat_where(chat_rents, "11")$label, "D11 Novena, Newton, Thomson")
  expect_equal(nrow(chat_where(chat_rents, "Novena MRT")$d), 3)   # nearest station, not district
  expect_equal(nrow(chat_where(chat_rents, "novena")$d), 5)       # the district
  expect_equal(nrow(chat_where(chat_rents, "Newton")$d), 5)       # district names win over stations
  expect_equal(chat_where(chat_rents, "palm court")$label, "project PALM COURT")
  expect_equal(nrow(chat_where(chat_rents, "hill")$d), 2)         # part of a project name
  expect_equal(nrow(chat_where(chat_rents, "sea lane")$d), 1)     # a street
  expect_equal(nrow(chat_where(chat_rents, "")$d), 6)
  expect_null(chat_where(chat_rents, "Atlantis")$d)
})

test_that("chat_beds() and chat_types() read the model's words", {
  expect_equal(chat_beds("2"), "2")
  expect_equal(chat_beds("1, 2"), c("1", "2"))
  expect_equal(chat_beds("studio"), "1")
  expect_equal(chat_beds("6"), "5+")
  expect_equal(chat_beds(""), bed_levels)       # any, including unknown
  types <- c("Non-Landed Properties", "Terrace House", "Executive Condominium")
  expect_equal(chat_types("condo", types), c("Non-Landed Properties", "Executive Condominium"))
  expect_equal(chat_types("landed", types), "Terrace House")
  expect_equal(chat_types("", types), types)
})

test_that("chat_rent_stats() summarises the last N months for a place", {
  s <- chat_rent_stats(chat_rents, "Novena", "2", months = 12, budget = c(3500, 4500))
  expect_equal(s$rentals, 3)                   # the 2024 3-bedroom is too old anyway
  expect_equal(s$median_rent, 4400)
  expect_equal(s$share_within_my_budget, 0.67)
  expect_equal(s$period, "Sep 2025 to Aug 2026")
  expect_equal(chat_rent_stats(chat_rents, "Novena", "3")$rentals, 0)
  expect_equal(chat_rent_stats(chat_rents, "Novena", "3", months = 60)$rentals, 1)
  expect_match(chat_rent_stats(chat_rents, "Atlantis")$error, "nothing matched")
})

test_that("chat_rank_areas() and chat_find_projects() rank by median rent", {
  r <- chat_rank_areas(chat_rents, min_rentals = 1)
  expect_equal(r$areas$area[1], "D11 Novena, Newton, Thomson")
  expect_equal(chat_rank_areas(chat_rents, order = "dearest", min_rentals = 1)$areas$area[1],
               "D15 Katong, Joo Chiat, Marine Parade")
  expect_equal(chat_rank_areas(chat_rents, by = "station", min_rentals = 1)$areas$area[1], "Novena MRT")
  p <- chat_find_projects(chat_rents, "Novena", min_rentals = 1)
  expect_equal(p$projects$project, c("PALM COURT", "HILL VIEW"))
  expect_equal(chat_find_projects(chat_rents, "Novena", max_rent = 4500, min_rentals = 1)$projects_found, 1)
})

test_that("tool results go to the model as JSON", {
  j <- tool_json(chat_find_projects(chat_rents, "Novena", min_rentals = 1))
  back <- jsonlite::fromJSON(j)
  expect_equal(back$projects$project, c("PALM COURT", "HILL VIEW"))
})

test_that("the chatbot is set up with its tools, and hidden without a key", {
  ch <- new_rentlens_chat(list(base_url = "http://127.0.0.1:9", api_key = "test", model = "m"),
                          d = chat_rents)
  expect_setequal(names(ch$get_tools()),
                  c("rent_stats", "rank_areas", "find_projects", "rent_trend", "my_filters"))
  withr::local_envvar(SOCLAAS_API_KEY = "")
  expect_null(chat_config())
  expect_null(mod_chat_ui("chat"))
})
