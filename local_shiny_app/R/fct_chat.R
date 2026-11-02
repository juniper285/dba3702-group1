# ---- "Ask RentLens": the chatbot's model, prompt and data tools ----------
#
# The chatbot is an AI model (NUS SoC's LLM service, OpenAI-compatible) that
# answers with real figures by calling the tools below, which read the same
# rental data as the rest of the app. The tools are plain functions, so they
# are unit-tested without the model.

#' Connection settings for the chatbot, from environment variables
#'
#' Locally they come from local_shiny_app/.Renviron (git-ignored; app.R reads
#' it); on Posit Connect Cloud they are set as secret variables.
#'
#' @return A list (`base_url`, `api_key`, `model`), or NULL when no key is set,
#'   in which case the app hides the chatbot.
#' @noRd
chat_config <- function() {
  key <- Sys.getenv("SOCLAAS_API_KEY")
  if (!nzchar(key)) return(NULL)
  list(base_url = Sys.getenv("SOCLAAS_BASE_URL", "https://soclaas-api.comp.nus.edu.sg/v1"),
       api_key  = key,
       model    = Sys.getenv("SOCLAAS_MODEL", "default"))
}

#' Bedroom groups from the model's "2", "1,2", "studio" or "" (any)
#' @noRd
chat_beds <- function(bedrooms) {
  b <- trimws(strsplit(tolower(if (is.null(bedrooms)) "" else bedrooms), "[,;/ ]+|and")[[1]])
  b <- sub("^studio$", "1", b)
  b <- ifelse(suppressWarnings(as.numeric(sub("\\+$", "", b))) >= 5, "5+", b)
  b <- intersect(b, bed_choices)
  if (length(b)) b else bed_levels
}

#' Property types from the model's "condo", "landed" or "" (any)
#' @noRd
chat_types <- function(property_type, types) {
  pt <- tolower(if (is.null(property_type)) "" else property_type)
  if (grepl("landed|house|terrace|detached|bungalow", pt) && !grepl("non", pt)) {
    types[!types %in% c("Non-Landed Properties", "Executive Condominium")]
  } else if (grepl("condo|apartment|flat|non", pt)) {
    intersect(types, c("Non-Landed Properties", "Executive Condominium"))
  } else types
}

#' The rentals in a place the model names
#'
#' Tries, in order: a district code ("D10", "10"); a station when the text
#' says MRT/LRT ("Novena MRT"); an exact project name; a district name
#' ("Holland"); a station name ("Clementi"); part of a project name; part of a
#' street name. Empty means all of Singapore.
#'
#' @return list(d = matching rentals, label = what was matched), with `d`
#'   NULL when nothing matches.
#' @noRd
chat_where <- function(d, where) {
  w <- trimws(if (is.null(where)) "" else where)
  if (!nzchar(w)) return(list(d = d, label = "all of Singapore"))
  lw <- tolower(w)
  hit <- function(rows, label) list(d = d[rows, ], label = label)
  code <- regmatches(lw, regexec("^(d|district)? *0?([0-9]{1,2})$", lw))[[1]]
  if (length(code)) {
    rows <- d$district_no == as.integer(code[3])
    if (any(rows)) return(hit(rows, d$district[which(rows)[1]]))
  }
  stations <- unique(d$mrt_station)
  bare <- tolower(sub(" (MRT|LRT)$", "", stations))
  lw_bare <- sub(" *(mrt|lrt)( station)?$", "", lw)
  if (grepl("mrt|lrt", lw) && lw_bare %in% bare) {
    st <- stations[match(lw_bare, bare)]
    return(hit(d$mrt_station == st, paste("buildings whose nearest station is", st)))
  }
  if (lw %in% tolower(d$project)) {
    rows <- tolower(d$project) == lw
    return(hit(rows, paste("project", d$project[which(rows)[1]])))
  }
  dist <- unique(d$district)
  dmatch <- dist[grepl(lw, tolower(dist), fixed = TRUE)]
  if (length(dmatch)) return(hit(d$district %in% dmatch, paste(dmatch, collapse = "; ")))
  if (lw_bare %in% bare) {
    st <- stations[match(lw_bare, bare)]
    return(hit(d$mrt_station == st, paste("buildings whose nearest station is", st)))
  }
  rows <- grepl(lw, tolower(d$project), fixed = TRUE)
  if (any(rows)) {
    p <- unique(d$project[rows])
    return(hit(rows, paste0("projects named like '", w, "' (", length(p), ")")))
  }
  rows <- grepl(lw, tolower(d$street), fixed = TRUE)
  if (any(rows)) return(hit(rows, paste0("streets named like '", w, "'")))
  list(d = NULL, label = paste0("nothing matched '", w, "'"))
}

#' Rentals for a tool call: place, bedrooms, type and the last `months`
#' @noRd
chat_subset <- function(d, where = "", bedrooms = "", property_type = "", months = 12) {
  m <- chat_where(d, where)
  if (is.null(m$d)) return(m)
  x <- m$d
  x <- x[x$bed_group %in% chat_beds(bedrooms) &
           x$type %in% chat_types(property_type, unique(d$type)), ]
  months <- min(max(as.integer(if (is.null(months) || is.na(months)) 12 else months), 1), 60)
  # the window ends at the latest month in the whole data, not this subset
  start <- seq(max(d$lease_date), by = paste0("-", months, " months"), length.out = 2)[2]
  m$d <- x[x$lease_date > start, ]
  first_month <- seq(start, by = "month", length.out = 2)[2]   # contracts are dated by month
  m$period <- paste(format(first_month, "%b %Y"), "to", format(max(d$lease_date), "%b %Y"))
  m
}

chat_round <- function(x) round(x, 2)

#' A tool's result as JSON text for the model (tables as one object per row)
#' @noRd
tool_json <- function(x) {
  jsonlite::toJSON(x, auto_unbox = TRUE, dataframe = "rows", na = "null", digits = NA)
}

#' Tool: rent figures for a place
#' @noRd
chat_rent_stats <- function(d, where = "", bedrooms = "", property_type = "", months = 12,
                            budget = NULL) {
  m <- chat_subset(d, where, bedrooms, property_type, months)
  if (is.null(m$d)) return(list(error = m$label))
  x <- m$d
  if (nrow(x) == 0) return(list(matched = m$label, period = m$period, rentals = 0))
  out <- list(
    matched = m$label, period = m$period, rentals = nrow(x),
    projects = length(unique(x$project)),
    median_rent = median(x$rent),
    typical_range = unname(quantile(x$rent, c(0.25, 0.75))),
    median_psf = chat_round(median(x$psf, na.rm = TRUE)),
    median_floor_area_sqft = median(x$area_sqft),
    median_walk_to_mrt_min = walk_minutes(median(x$mrt_m, na.rm = TRUE)),
    by_bedrooms = x |> group_by(bedrooms = bed_group) |>
      summarise(rentals = n(), median_rent = median(rent), .groups = "drop") |>
      as.data.frame()
  )
  if (length(budget) == 2) {
    out$share_within_my_budget <- chat_round(mean(x$rent >= budget[1] & x$rent <= budget[2]))
  }
  out
}

#' Tool: districts or stations ranked by median rent
#' @noRd
chat_rank_areas <- function(d, by = "district", bedrooms = "", property_type = "",
                            months = 12, order = "cheapest", max_median_rent = NA,
                            top_n = 10, min_rentals = 20) {
  m <- chat_subset(d, "", bedrooms, property_type, months)
  x <- m$d
  x$area <- if (identical(by, "station")) x$mrt_station else x$district
  s <- x |> group_by(area) |>
    summarise(median_rent = median(rent), median_psf = chat_round(median(psf, na.rm = TRUE)),
              rentals = n(), .groups = "drop") |>
    filter(rentals >= min_rentals)
  if (!is.null(max_median_rent) && !is.na(max_median_rent)) {
    s <- s[s$median_rent <= max_median_rent, ]
  }
  s <- s[order(s$median_rent, decreasing = identical(order, "dearest")), ]
  list(period = m$period, ranked_by = paste(order, "median monthly rent"),
       areas = as.data.frame(utils::head(s, min(max(top_n, 1), 30))))
}

#' Tool: projects in a place, cheapest first, optionally under a rent
#' @noRd
chat_find_projects <- function(d, where = "", bedrooms = "", property_type = "",
                               months = 12, max_rent = NA, top_n = 10, min_rentals = 3) {
  m <- chat_subset(d, where, bedrooms, property_type, months)
  if (is.null(m$d)) return(list(error = m$label))
  s <- m$d |> group_by(project, district) |>
    summarise(median_rent = median(rent), rentals = n(),
              median_floor_area_sqft = median(area_sqft),
              nearest_mrt = first(mrt_station),
              walk_to_mrt_min = walk_minutes(first(mrt_m)), .groups = "drop") |>
    filter(rentals >= min_rentals)
  if (!is.null(max_rent) && !is.na(max_rent)) s <- s[s$median_rent <= max_rent, ]
  s <- s[order(s$median_rent), ]
  list(matched = m$label, period = m$period, projects_found = nrow(s),
       projects = as.data.frame(utils::head(s, min(max(top_n, 1), 30))))
}

#' Tool: median rent per quarter for a place
#' @noRd
chat_rent_trend <- function(d, where = "", bedrooms = "", property_type = "") {
  m <- chat_subset(d, where, bedrooms, property_type, months = 60)
  if (is.null(m$d)) return(list(error = m$label))
  s <- m$d |> group_by(quarter) |>
    summarise(median_rent = median(rent), rentals = n(), .groups = "drop") |>
    filter(rentals >= 3)
  s$quarter <- quarter_label(s$quarter)
  list(matched = m$label, period = m$period, quarters = as.data.frame(s))
}

#' What the chatbot is told before every conversation
#' @noRd
chat_system_prompt <- function(d = rentals_data()) {
  paste0(
    "You are 'Ask RentLens', the assistant inside RentLens, an app for people renting a ",
    "private home (condominium, apartment or landed house, not HDB) in Singapore. Many users ",
    "are international students and workers.\n\n",
    "The data: ", format(nrow(d), big.mark = ","), " private rental contracts from URA, ",
    format(min(d$lease_date), "%B %Y"), " to ", format(max(d$lease_date), "%B %Y"),
    ". Each is a signed contract (monthly rent, bedrooms, floor area, project, district, ",
    "nearest MRT/LRT), not an asking price. Bedroom count is unknown for landed houses.\n\n",
    "Rules:\n",
    "- For any rent figure, call a tool. Never guess or invent numbers.\n",
    "- Say which place was matched, the period and how many rentals a figure is based on; ",
    "under 10 rentals, say the figure is rough.\n",
    "- Money is in S$ per month. Use plain English, no jargon such as 'D10' alone: say the ",
    "district's name. Keep answers short; use a small table when comparing.\n",
    "- If the renter asks about 'my budget' or 'my filters', call my_filters first.\n",
    "- If a place isn't matched, suggest a district, MRT station or project name instead.\n",
    "- For questions outside renting in Singapore, say briefly that you only help with ",
    "renting, and with the rental data in this app.\n",
    "- The app's pages: Search (what my budget gets), Map, Check a listing (is an asking ",
    "rent fair?), Market (compare areas and properties), Shortlist and My filters. Point ",
    "to them when useful."
  )
}

#' A chat client for one browser session, with the data tools registered
#'
#' @param config From chat_config().
#' @param my_filters A function returning the renter's current filters as a
#'   list (see mod_chat_server()).
#' @param d The rentals the tools read.
#' @noRd
new_rentlens_chat <- function(config, my_filters = function() list(), d = rentals_data()) {
  chat <- ellmer::chat_openai_compatible(
    base_url = config$base_url, credentials = function() config$api_key, model = config$model,
    name = "NUS SoC LLM service", system_prompt = chat_system_prompt(d))

  where_arg <- ellmer::type_string(paste(
    "Where: a district name or code (e.g. 'Holland', 'D10'), an MRT/LRT station",
    "(e.g. 'Novena MRT'), a project (condo) name or a street. Empty for all of Singapore."),
    required = FALSE)
  beds_arg <- ellmer::type_string(
    "Bedrooms, e.g. '2' or '1,2' ('5' means 5 or more). Empty for any.", required = FALSE)
  type_arg <- ellmer::type_string(
    "'condo' (apartments and condos), 'landed' (houses) or empty for any.", required = FALSE)
  months_arg <- ellmer::type_integer(
    "How many recent months of contracts to use, 1 to 60 (default 12).", required = FALSE)
  budget <- function() { b <- my_filters()$budget; if (length(b) == 2) b else NULL }

  chat$register_tool(ellmer::tool(
    function(where = "", bedrooms = "", property_type = "", months = 12)
      tool_json(chat_rent_stats(d, where, bedrooms, property_type, months, budget())),
    name = "rent_stats",
    description = paste("Rent figures for a place: number of rentals, median and typical",
                        "(middle half) monthly rent, rent per sq ft, floor area, walk to the",
                        "MRT, a breakdown by bedrooms, and the share within the renter's budget."),
    arguments = list(where = where_arg, bedrooms = beds_arg, property_type = type_arg,
                     months = months_arg)))

  chat$register_tool(ellmer::tool(
    function(by = "district", bedrooms = "", property_type = "", months = 12,
             order = "cheapest", max_median_rent = NA, top_n = 10)
      tool_json(chat_rank_areas(d, by, bedrooms, property_type, months, order, max_median_rent, top_n)),
    name = "rank_areas",
    description = paste("Districts, or areas around each MRT/LRT station, ranked by median",
                        "monthly rent (areas with at least 20 rentals)."),
    arguments = list(
      by = ellmer::type_enum(c("district", "station"), "Rank districts or station areas.",
                             required = FALSE),
      bedrooms = beds_arg, property_type = type_arg, months = months_arg,
      order = ellmer::type_enum(c("cheapest", "dearest"), "Cheapest or dearest first.",
                                required = FALSE),
      max_median_rent = ellmer::type_number("Only areas with a median rent up to this (S$).",
                                            required = FALSE),
      top_n = ellmer::type_integer("How many to list (default 10, at most 30).",
                                   required = FALSE))))

  chat$register_tool(ellmer::tool(
    function(where = "", bedrooms = "", property_type = "", months = 12, max_rent = NA,
             top_n = 10)
      tool_json(chat_find_projects(d, where, bedrooms, property_type, months, max_rent, top_n)),
    name = "find_projects",
    description = paste("Projects (condos or landed estates) in a place, cheapest median",
                        "rent first, with floor area and nearest MRT/LRT. Projects with",
                        "fewer than 3 rentals are left out."),
    arguments = list(where = where_arg, bedrooms = beds_arg, property_type = type_arg,
                     months = months_arg,
                     max_rent = ellmer::type_number("Only projects with a median rent up to this (S$).",
                                                    required = FALSE),
                     top_n = ellmer::type_integer("How many to list (default 10, at most 30).",
                                                  required = FALSE))))

  chat$register_tool(ellmer::tool(
    function(where = "", bedrooms = "", property_type = "")
      tool_json(chat_rent_trend(d, where, bedrooms, property_type)),
    name = "rent_trend",
    description = "Median monthly rent per quarter for a place, over the whole data period.",
    arguments = list(where = where_arg, bedrooms = beds_arg, property_type = type_arg)))

  chat$register_tool(ellmer::tool(
    function() tool_json(my_filters()),
    name = "my_filters",
    description = paste("The renter's current filters in the app (budget, bedrooms, places",
                        "that matter, ...) and the median rent of the rentals that match them.")))
  chat
}
