#' "My settings": the renter's budget, places that matter, type of home and
#' commute, kept in the browser (see fct_settings.R)
#'
#' The settings are edited in a dialog; its inputs disappear when it
#' closes, so the values live in the `settings` reactiveVal.
#'
#' @return The server returns a list:
#'   - `settings`: reactiveVal with the current settings
#'   - `first_visit`: reactive, TRUE when nothing was saved in this browser
#'   - `save(s)`: apply settings `s` and save them in the browser
#' @noRd
mod_settings_summary_ui <- function(id) {
  ns <- NS(id)
  div(class = "settings-summary",
      div(class = "settings-icon", icon("user")),
      div(class = "flex-fill",
          div(class = "section-title mb-1", "My settings"),
          uiOutput(ns("summary"))),
      actionButton(ns("open"), "Edit my settings", icon = icon("pen"),
                   class = "btn-sm btn-outline-primary"))
}

#' Link in the navbar that opens the settings dialog
#' @noRd
mod_settings_nav_ui <- function(id) {
  nav_item(actionLink(NS(id, "open_nav"), span(icon("user"), "My settings")))
}

# The settings dialog's sections, in wizard order
settings_sections <- c(budget = "Budget", places = "Places that matter",
                       area = "Where to search", home = "Type of home",
                       commute = "Commute and MRT")

#' One section of the settings form
#'
#' @param ns The settings module's namespace function.
#' @param s The settings to show.
#' @param custom Addresses the renter added (name, lat, lng).
#' @noRd
settings_section_ui <- function(ns, section, s, choices = settings_choices(),
                                custom = custom_places(s)) {
  switch(
    section,
    budget = tagList(
      p(class = "text-muted", "How much can you spend on rent each month?"),
      sliderInput(ns("budget"), "Monthly budget", min = 1000, max = 15000,
                  value = s$budget, step = 100, pre = "S$", width = "100%"),
      uiOutput(ns("budget_hint"))
    ),
    places = tagList(
      p(class = "text-muted", "Where will you travel to most? Every list shows the ",
        "distance from each home to these places."),
      selectizeInput(ns("key_places"), "Universities, workplaces and other key places",
                     destination_choices, multiple = TRUE, width = "100%",
                     selected = intersect(s$places$name, key_places$name),
                     options = list(placeholder = "e.g. NUS, one-north, CBD",
                                    maxItems = max_places, plugins = list("remove_button"))),
      div(class = "d-flex gap-2 align-items-end",
          textInput(ns("address"), "Or add any address or postal code",
                    placeholder = "e.g. 1 Fusionopolis Way or 138632", width = "100%"),
          div(class = "mb-3", actionButton(ns("find"), "Add", icon = icon("plus"),
                                           class = "btn-outline-primary"))),
      div(class = "chips",
          checkboxGroupInput(ns("custom_keep"), if (nrow(custom)) "Your addresses (untick to remove)",
                             choices = custom$name, selected = custom$name, inline = TRUE))
    ),
    area = tagList(
      p(class = "text-muted", "Where should we look? Leave empty to search all of Singapore."),
      selectizeInput(ns("districts"), "Districts", choices$districts, multiple = TRUE,
                     selected = s$districts, width = "100%",
                     options = list(placeholder = "Anywhere in Singapore",
                                    plugins = list("remove_button"))),
      dateRangeInput(ns("period"), "Rentals that started between",
                     start = s$period[1], end = s$period[2],
                     min = choices$dates[1], max = choices$dates[2], format = "M yyyy"),
      selectInput(ns("recent"), "Compare my budget with rentals from the most recent",
                  choices = c("6 months" = 6, "12 months" = 12, "24 months" = 24),
                  selected = s$recent)
    ),
    home = tagList(
      p(class = "text-muted", "What kind of home are you after?"),
      div(class = "chips",
          checkboxGroupInput(ns("beds"), "Bedrooms", choices = bed_levels,
                             selected = s$beds, inline = TRUE),
          checkboxGroupInput(ns("types"), "Property type", choices = choices$types,
                             selected = s$types, inline = TRUE)),
      sliderInput(ns("area"), "Floor area (sqft)", min = 0, max = area_max,
                  value = s$area, step = 100, width = "100%"),
      helpText("Top of the slider = 4,000 sqft and above.")
    ),
    commute = tagList(
      p(class = "text-muted", "How far are you willing to travel?"),
      sliderInput(ns("mrt_dist"), "Distance to an MRT/LRT station (m)", min = 100, max = mrt_max,
                  value = s$mrt_dist, step = 100, width = "100%"),
      selectizeInput(ns("mrt_pick"), "Near a specific station (optional)",
                     choices = c("", choices$stations), selected = s$mrt_pick, width = "100%",
                     options = list(placeholder = "Any station")),
      helpText("No station chosen: distance to the nearest station; the top of the slider ",
               "means any distance. Station chosen: homes within that distance of it. ",
               "About 80 m is one minute's walk."),
      sliderInput(ns("place_km"), "Distance to one of my places (km)", min = 1,
                  max = place_km_max, value = s$place_km, step = 1, width = "100%"),
      helpText("The top of the slider means any distance. Straight-line distances.")
    )
  )
}

#' Addresses in the settings that are not key places
#' @noRd
custom_places <- function(s) s$places[!s$places$name %in% key_places$name, ]

#' Read the shown sections of the settings form back into settings
#'
#' @param input The settings module's input.
#' @param s The settings before editing (kept for sections not shown).
#' @param sections Sections whose inputs are on screen.
#' @param custom Addresses added so far (name, lat, lng).
#' @noRd
read_settings_form <- function(input, s, sections, custom) {
  if ("budget" %in% sections && !is.null(input$budget)) s$budget <- input$budget
  if ("places" %in% sections) {
    keep <- custom[custom$name %in% input$custom_keep, ]
    s$places <- utils::head(rbind(key_place(input$key_places), keep), max_places)
    rownames(s$places) <- NULL
  }
  if ("area" %in% sections) {
    s$districts <- if (is.null(input$districts)) character() else input$districts
    if (!is.null(input$period)) s$period <- as.Date(input$period)
    if (!is.null(input$recent)) s$recent <- as.numeric(input$recent)
  }
  if ("home" %in% sections) {
    if (length(input$beds)) s$beds <- input$beds
    if (length(input$types)) s$types <- input$types
    if (!is.null(input$area)) s$area <- input$area
  }
  if ("commute" %in% sections) {
    if (!is.null(input$mrt_dist)) s$mrt_dist <- input$mrt_dist
    s$mrt_pick <- if (is.null(input$mrt_pick)) "" else input$mrt_pick
    if (!is.null(input$place_km)) s$place_km <- input$place_km
  }
  s
}

#' @noRd
mod_settings_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    choices <- settings_choices()
    settings <- reactiveVal(default_settings(choices))
    first_visit <- reactiveVal(FALSE)
    restored <- FALSE       # settings came from a "Save search" link

    setBookmarkExclude(c("stored", "open", "open_nav", "save", "find", "address",
                         names(default_settings(choices)), "key_places", "custom_keep",
                         "types", "beds"))

    # What this browser saved last time ("none" on a first visit)
    observeEvent(input$stored, once = TRUE, {
      if (restored) return()
      s <- settings_from_json(input$stored, choices)
      if (is.null(s)) first_visit(TRUE) else settings(s)
    })

    save <- function(s) {
      settings(s)
      first_visit(FALSE)
      session$sendCustomMessage("rentlens-save-settings", settings_to_json(s))
    }

    # Addresses added in the form (kept while the form is open)
    custom <- reactiveVal(custom_places(default_settings(choices)))

    observeEvent(input$find, {
      hit <- onemap_search(input$address)
      if (nrow(hit) == 0) {
        showNotification("No address found. Try a postal code or a fuller address.", type = "warning")
        return()
      }
      name <- if (nzchar(hit$building[1]) && hit$building[1] != "NIL") hit$building[1] else hit$address[1]
      name <- tools::toTitleCase(tolower(name))
      custom(unique(rbind(custom(), data.frame(name = name, lat = hit$lat[1], lng = hit$lng[1]))))
      keep <- unique(c(input$custom_keep, name))
      updateCheckboxGroupInput(session, "custom_keep", label = "Your addresses (untick to remove)",
                               choices = custom()$name, selected = keep, inline = TRUE)
      updateTextInput(session, "address", value = "")
    })

    # Typical rents for the budget step, so the budget is realistic
    output$budget_hint <- renderUI({
      req(input$budget)
      r <- latest_months(rentals_data(), 12)$rent
      q <- quantile(r, c(0.25, 0.75))
      share <- round(100 * mean(r >= input$budget[1] & r <= input$budget[2]))
      p(class = "small text-muted", icon("circle-info"),
        sprintf(" Half of last year's rentals were between %s and %s; %s%% fall inside your range.",
                money(q[1]), money(q[2]), share))
    })

    # ---- The settings dialog ----
    open_dialog <- function() {
      s <- settings()
      custom(custom_places(s))
      tabs <- lapply(names(settings_sections), function(k) {
        nav_panel(settings_sections[[k]], div(class = "pt-3", settings_section_ui(ns, k, s, choices)))
      })
      showModal(modalDialog(
        title = span(icon("user"), " My settings"),
        do.call(navset_pill, c(list(id = ns("tab")), tabs)),
        footer = tagList(
          span(class = "small text-muted me-auto", "Saved in this browser only."),
          modalButton("Cancel"),
          actionButton(ns("save"), "Save", icon = icon("check"), class = "btn-primary")
        ),
        size = "l", easyClose = TRUE
      ))
    }
    observeEvent(input$open, open_dialog())
    observeEvent(input$open_nav, open_dialog())

    observeEvent(input$save, {
      save(read_settings_form(input, settings(), names(settings_sections), custom()))
      removeModal()
      showNotification("Settings saved.", type = "message", duration = 3)
    })

    output$summary <- renderUI({
      div(class = "settings-chips",
          lapply(settings_summary(settings(), choices), function(x) span(class = "settings-chip", x)))
    })

    # Settings travel inside "Save search" links; a link's settings apply to
    # this visit only and don't replace what this browser saved
    onBookmark(function(state) state$values$settings <- settings_to_json(settings()))
    onRestore(function(state) {
      s <- settings_from_json(state$values$settings, choices)
      if (!is.null(s)) {
        restored <<- TRUE
        settings(s)
      }
    })

    list(settings = settings, first_visit = reactive(first_visit()), save = save)
  })
}
