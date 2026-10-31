# ---- Rent summaries used across the tabs -------------------------------

#' Keep only the most recent `months` months of a data set
#' @noRd
latest_months <- function(d, months) {
  if (nrow(d) == 0) return(d)
  cutoff <- seq(max(d$lease_date), by = paste0("-", months, " months"), length.out = 2)[2]
  d[d$lease_date > cutoff, ]
}

#' Standard rent summary (one row per group of `d`)
#'
#' @param budget c(min, max); `within` is the share of rentals inside it.
#' @noRd
summarise_rent <- function(d, budget = c(NA, NA)) {
  summarise(d,
    median  = median(rent),
    average = mean(rent),
    p25    = unname(quantile(rent, 0.25)),
    p75    = unname(quantile(rent, 0.75)),
    psf    = median(psf, na.rm = TRUE),
    n      = n(),
    within = mean(rent >= budget[1] & rent <= budget[2]),
    .groups = "drop"
  )
}

#' One row per project + bedroom type: the "options" a renter can pick from
#' @noRd
summarise_options <- function(d) {
  d |>
    group_by(project, district, bed_group) |>
    summarise(
      size    = most_common(area_band),
      median  = median(rent),
      average = mean(rent),
      p25    = unname(quantile(rent, 0.25)),
      p75    = unname(quantile(rent, 0.75)),
      psf    = median(psf, na.rm = TRUE),
      n      = n(),
      last   = max(lease_date),
      .groups = "drop"
    )
}

#' How a budget range compares with a set of rents
#'
#' @return A list: `verdict`, `colour` (a bootstrap theme), `advice` and
#'   `share` (% of rents inside the budget).
#' @noRd
budget_verdict <- function(rents, budget) {
  q <- quantile(rents, c(0.25, 0.75))
  share <- round(100 * mean(rents >= budget[1] & rents <= budget[2]))
  if (budget[2] < q[1]) {
    list(verdict = "Below typical", colour = "danger", share = share,
         advice = "Most comparable units rent for more. Try other districts, fewer bedrooms or a smaller unit.")
  } else if (budget[1] > q[2]) {
    list(verdict = "Above typical", colour = "info", share = share,
         advice = "Most comparable units rent for less than your minimum. You have room to negotiate.")
  } else {
    list(verdict = "Realistic", colour = "success", share = share,
         advice = "Your range overlaps the typical rents for these filters.")
  }
}

#' Empty shortlist (one row per project + bedroom type)
#' @noRd
empty_shortlist <- function() data.frame(project = character(), bed_group = character())

#' Saved areas side by side: one row per area
#'
#' @param sets Named list of contracts, one data frame per area (names are
#'   the area names).
#' @param places The renter's places (name, lat, lng): one "To <name> (km)"
#'   column each, measured from the area's centre.
#' @return `area`, `n`, `projects`, `median`, `q25`, `q75`, `within` (share of
#'   rentals inside `budget`), `psf`, `walk_min` (median walk to the nearest
#'   MRT/LRT), `district` (where most of its rentals are) and the place columns.
#' @noRd
compare_areas <- function(sets, budget, places = new_places()) {
  rows <- lapply(names(sets), function(nm) {
    d <- sets[[nm]]
    r <- data.frame(
      area = nm, n = nrow(d), projects = n_distinct(d$project),
      median = median(d$rent), q25 = unname(quantile(d$rent, 0.25)),
      q75 = unname(quantile(d$rent, 0.75)),
      within = mean(d$rent >= budget[1] & d$rent <= budget[2]),
      psf = median(d$psf, na.rm = TRUE),
      walk_min = walk_minutes(median(d$mrt_m, na.rm = TRUE)),
      district = if (nrow(d)) names(which.max(table(d$district))) else NA_character_
    )
    for (i in seq_len(nrow(places))) {
      r[[place_column(places$name[i])]] <-
        round(haversine_km(mean(d$lat, na.rm = TRUE), mean(d$lng, na.rm = TRUE),
                           places$lat[i], places$lng[i]), 1)
    }
    r
  })
  do.call(rbind, rows)
}

#' Saved areas side by side by bedrooms: one row per area and bedroom type
#'
#' @param sets Named list of contracts, one data frame per area.
#' @return `area` (a factor in the order of `sets`), `bed_group`, `n`, `median`.
#' @noRd
compare_areas_by_bed <- function(sets) {
  s <- do.call(rbind, lapply(names(sets), function(nm) {
    if (nrow(sets[[nm]]) == 0) return(NULL)
    sets[[nm]] |>
      group_by(bed_group) |>
      summarise(n = n(), median = median(rent), .groups = "drop") |>
      mutate(area = nm)
  }))
  s$area <- factor(s$area, levels = names(sets))
  s[order(s$area, match(s$bed_group, bed_levels)), c("area", "bed_group", "n", "median")]
}

#' Where a median rent sits against a budget range
#' @noRd
fits_budget <- function(median, budget) {
  case_when(is.na(median)     ~ "No rentals",
            median < budget[1] ~ "Below range",
            median > budget[2] ~ "Above range",
            TRUE               ~ "Within range")
}

#' One row per chosen project, in the order given, for "Compare properties"
#'
#' @param d Contracts of the chosen projects.
#' @param projects Project names; a project with no contracts gets a row with n = 0.
#' @noRd
summarise_projects <- function(d, projects, budget) {
  s <- d |>
    group_by(project) |>
    summarise(district = paste(sort(unique(district)), collapse = ", "),
              n = n(), median = median(rent), average = mean(rent),
              p25 = unname(quantile(rent, 0.25)), p75 = unname(quantile(rent, 0.75)),
              psf = median(psf, na.rm = TRUE), sqft = median(area_sqft, na.rm = TRUE),
              last = max(lease_date), .groups = "drop")
  s <- left_join(data.frame(project = projects), s, by = "project")
  s$n[is.na(s$n)] <- 0L
  s$fits <- fits_budget(s$median, budget)
  s
}
