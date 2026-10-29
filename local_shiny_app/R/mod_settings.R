#' "My settings": the renter's budget, places that matter, type of home and
#' commute, kept in the browser (see fct_settings.R)
#'
#' On a first visit a setup wizard asks for them one section at a time,
#' with a preview of matching homes and their distances at the end; later
#' they are edited in a dialog. Dialog inputs disappear when it closes, so
#' the values live in the `settings` reactiveVal.
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
#' @param s The settings to show (the places list is drawn by the server,
#'   output `place_list`).
#' @noRd
settings_section_ui <- function(ns, section, s, choices = settings_choices()) {
  switch(
    section,
    budget = tagList(
      p(class = "text-muted", "How much can you spend on rent each month?"),
      sliderInput(ns("budget"), "Monthly budget", min = 1000, max = 15000,
                  value = s$budget, step = 100, pre = "S$", width = "100%"),
      uiOutput(ns("budget_hint"))
    ),
    places = tagList(
      p(class = "text-muted", "Add the places you travel to most, one at a time. Every list ",
        "shows the distance from each home to them."),
      div(class = "place-form",
          textInput(ns("place_address"), "Address or postal code", width = "100%",
                    placeholder = "e.g. 21 Lower Kent Ridge Road or 119077"),
          div(class = "chips place-kinds",
              radioButtons(ns("place_kind"), "Icon", inline = TRUE, selected = "work",
                           choiceNames = lapply(seq_len(nrow(place_kinds)), function(i) {
                             tagList(icon(place_kinds$icon[i]), place_kinds$label[i])
                           }),
                           choiceValues = place_kinds$kind),
              conditionalPanel(
                "input.place_kind == 'other'", ns = ns,
                textInput(ns("place_kind_label"), "What kind of place?", width = "100%",
                          placeholder = "e.g. Church, Clinic, Partner's office"))),
          div(class = "d-flex gap-2 align-items-end",
              textInput(ns("place_name"), "Name", width = "100%",
                        placeholder = "e.g. My office, NUS, Mum's place"),
              div(class = "mb-3", actionButton(ns("add_place"), "Add place", icon = icon("plus"),
                                               class = "btn-primary text-nowrap")))),
      uiOutput(ns("place_list"))
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

#' Read the shown sections of the settings form back into settings
#'
#' @param input The settings module's input.
#' @param s The settings before editing (kept for sections not shown).
#' @param sections Sections whose inputs are on screen.
#' @param places The places list being edited (see new_places()).
#' @noRd
read_settings_form <- function(input, s, sections, places) {
  if ("budget" %in% sections && !is.null(input$budget)) s$budget <- input$budget
  if ("places" %in% sections) s$places <- places
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

    setBookmarkExclude(c("stored", "open", "open_nav", "save", "next_step", "back", "skip",
                         "finish", "start_over", names(default_settings(choices)),
                         "place_address", "place_name", "place_kind", "place_kind_label",
                         "add_place",
                         "remove_place"))

    # What this browser saved last time ("none" on a first visit)
    observeEvent(input$stored, once = TRUE, {
      if (restored) return()
      s <- settings_from_json(input$stored, choices)
      if (is.null(s)) {
        first_visit(TRUE)
        open_wizard(default_settings(choices))
      } else {
        settings(s)
      }
    })

    save <- function(s) {
      settings(s)
      first_visit(FALSE)
      session$sendCustomMessage("rentlens-save-settings", settings_to_json(s))
    }

    # ---- Places that matter: added one at a time ----
    # The list being edited (kept while the dialog or wizard is open)
    places <- reactiveVal(new_places())

    observeEvent(input$add_place, {
      address <- trimws(input$place_address)
      if (!nzchar(address)) {
        showNotification("Type an address or postal code first.", type = "warning")
        return()
      }
      if (nrow(places()) >= max_places) {
        showNotification(paste("You can add up to", max_places, "places."), type = "warning")
        return()
      }
      hit <- onemap_search(address)
      if (nrow(hit) == 0) {
        showNotification("No address found. Try a postal code or a fuller address.", type = "warning")
        return()
      }
      name <- trimws(input$place_name)
      label <- place_label(input$place_kind, input$place_kind_label)
      if (!nzchar(name)) name <- default_place_name(input$place_kind, places()$name, label)
      if (name %in% places()$name) {
        showNotification(paste0("You already have a place called '", name, "'."), type = "warning")
        return()
      }
      places(rbind(places(), new_places(name = name, kind = input$place_kind, label = label,
                                        address = place_address(hit[1, ]),
                                        lat = hit$lat[1], lng = hit$lng[1])))
      updateTextInput(session, "place_address", value = "")
      updateTextInput(session, "place_name", value = "")
      updateTextInput(session, "place_kind_label", value = "")
    })

    observeEvent(input$remove_place, {
      places(places()[places()$name != input$remove_place, ])
    })

    output$place_list <- renderUI({
      pl <- places()
      if (nrow(pl) == 0) {
        return(div(class = "place-empty", icon("map-pin"), " No places yet. Add one above."))
      }
      div(class = "place-list",
          lapply(seq_len(nrow(pl)), function(i) {
            remove <- sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'}); return false;",
                              ns("remove_place"), jsonlite::toJSON(pl$name[i], auto_unbox = TRUE))
            div(class = "place-row",
                div(class = "place-icon", icon(place_icon(pl$kind[i]))),
                div(class = "flex-fill",
                    div(class = "place-name", pl$name[i],
                        # the kind of place, unless the name already says it ("Work")
                        if (!startsWith(pl$name[i], pl$label[i]))
                          span(class = "place-kind", pl$label[i])),
                    if (nzchar(pl$address[i])) div(class = "place-address", pl$address[i])),
                tags$a(href = "#", class = "place-remove", title = paste("Remove", pl$name[i]),
                       onclick = remove, icon("xmark")))
          }))
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
      places(s$places)
      tabs <- lapply(names(settings_sections), function(k) {
        nav_panel(settings_sections[[k]], div(class = "pt-3", settings_section_ui(ns, k, s, choices)))
      })
      showModal(modalDialog(
        title = span(icon("user"), " My settings"),
        do.call(navset_pill, c(list(id = ns("tab")), tabs)),
        footer = tagList(
          actionLink(ns("start_over"), "Start over", icon = icon("rotate-left"),
                     class = "me-auto small"),
          span(class = "small text-muted", "Saved in this browser only."),
          modalButton("Cancel"),
          actionButton(ns("save"), "Save", icon = icon("check"), class = "btn-primary")
        ),
        size = "l", easyClose = TRUE
      ))
    }
    observeEvent(input$open, open_dialog())
    observeEvent(input$open_nav, open_dialog())

    observeEvent(input$save, {
      save(read_settings_form(input, settings(), names(settings_sections), places()))
      removeModal()
      showNotification("Settings saved.", type = "message", duration = 3)
    })

    # ---- First-visit setup wizard: one section per step ----
    step  <- reactiveVal(1)
    draft <- reactiveVal(NULL)    # settings filled in so far
    n_steps <- length(settings_sections)

    show_step <- function() {
      k <- step()
      section <- names(settings_sections)[k]
      showModal(modalDialog(
        title = div(class = "wizard-title",
                    div(class = "small text-muted", sprintf("Set up your search · step %d of %d", k, n_steps)),
                    settings_sections[[section]]),
        div(class = "progress mb-3", style = "height: 4px;",
            div(class = "progress-bar", style = sprintf("width: %d%%;", round(100 * k / n_steps)))),
        settings_section_ui(ns, section, draft(), choices),
        if (section == "commute") uiOutput(ns("preview")),
        footer = tagList(
          actionLink(ns("skip"), "Skip setup", class = "me-auto small text-muted"),
          if (k > 1) actionButton(ns("back"), "Back", icon = icon("arrow-left"),
                                  class = "btn-outline-secondary"),
          if (k < n_steps) actionButton(ns("next_step"), "Next", class = "btn-primary")
          else actionButton(ns("finish"), "Show my options", icon = icon("magnifying-glass"),
                            class = "btn-primary")
        ),
        size = "l", easyClose = FALSE
      ))
    }
    open_wizard <- function(s) {
      draft(s)
      places(s$places)
      step(1)
      show_step()
    }
    # Keep what was entered on this step before moving on
    keep_step <- function() {
      draft(read_settings_form(input, draft(), names(settings_sections)[step()], places()))
    }
    observeEvent(input$next_step, { keep_step(); step(step() + 1); show_step() })
    observeEvent(input$back,      { keep_step(); step(step() - 1); show_step() })
    observeEvent(input$skip, {
      keep_step()
      save(draft())
      removeModal()
    })
    observeEvent(input$finish, {
      keep_step()
      save(draft())
      removeModal()
      # Steps 6 and 7: the list of options, and how to shortlist them
      nav_select("nav", "budget", session = session$rootScope())
      session$sendCustomMessage("rentlens-coach", list(
        selector = "#within_budget-add_short",
        title = "Your options",
        text = paste("The list below shows homes that fit your settings, with the distance",
                     "to each of your places. Tick the rows you like, then press",
                     "'Add to shortlist' to save them.")))
    })
    observeEvent(input$start_over, {
      session$sendCustomMessage("rentlens-clear-settings", TRUE)
      open_wizard(default_settings(choices))
    })

    # Last step: a preview of matching homes and how far they are
    output$preview <- renderUI({
      s <- read_settings_form(input, draft(), "commute", places())
      near <- if (nzchar(s$mrt_pick)) station_location(s$mrt_pick)
      d <- filter_market(rentals_data(), s$districts, s$beds, s$types, s$area, s$period,
                         near, s$mrt_dist, s$places, s$place_km)
      d <- latest_months(d, s$recent)
      opts <- if (nrow(d)) summarise_options(d) |> filter(median >= s$budget[1], median <= s$budget[2])
      if (is.null(opts) || nrow(opts) == 0) {
        return(div(class = "wizard-preview text-muted",
                   "No homes in your budget match these distances yet. Try a longer distance."))
      }
      opts <- add_distance(opts, s$places)
      dist_cols <- place_column(s$places$name)
      opts$nearest <- if (length(dist_cols)) do.call(pmin, c(unname(as.list(opts[dist_cols])), na.rm = TRUE)) else opts$walk_min
      top <- utils::head(opts[order(opts$nearest, opts$walk_min), ], 5)
      div(class = "wizard-preview",
          div(class = "section-title", sprintf("%s options in your budget · the closest:",
                                               format(nrow(opts), big.mark = ","))),
          tags$table(class = "table table-sm mb-0",
            tags$thead(tags$tr(tags$th("Home"), tags$th("Rent"), tags$th("Walk to MRT"),
                               lapply(s$places$name, tags$th))),
            tags$tbody(lapply(seq_len(nrow(top)), function(i) {
              x <- top[i, ]
              tags$tr(tags$td(paste0(x$project, " (", x$bed_group, " BR)")),
                      tags$td(money(x$median)),
                      tags$td(if (is.na(x$walk_min)) "-" else paste(x$walk_min, "min")),
                      lapply(dist_cols, function(col) tags$td(paste(x[[col]], "km"))))
            }))))
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
