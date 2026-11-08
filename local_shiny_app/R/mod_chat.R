#' "Ask RentLens": a chat button on every page that opens a chat window
#'
#' The window pops up over the darkened page (faint green, bottom right) and
#' can be dragged by its title bar; closing it keeps the conversation. The
#' model answers with real figures by calling the data tools in fct_chat.R.
#' Without a key (see chat_config()) nothing is shown.
#'
#' @param filters The list returned by mod_filters_server(): the my_filters
#'   tool reads the renter's current filters from it.
#' @noRd
mod_chat_ui <- function(id) {
  if (is.null(chat_config())) return(NULL)
  ns <- NS(id)
  tagList(
    htmltools::htmlDependency("rentlens-chat", asset_version("chat.js"), src = app_sys("app", "www"),
                              script = "chat.js"),
    tags$button(
      type = "button", class = "btn btn-primary chat-fab",
      `data-bs-toggle` = "modal", `data-bs-target` = paste0("#", ns("window")),
      icon("comments"), span(class = "chat-fab-label", "Ask RentLens")),
    # A Bootstrap modal (so the page behind it is darkened) styled as a
    # floating window that chat.js lets the renter drag by its title bar
    div(
      class = "modal fade chat-window", tabindex = "-1", id = ns("window"),
      `aria-labelledby` = ns("title"), `aria-hidden` = "true", `data-bs-backdrop` = "false",
      div(class = "modal-dialog chat-dialog",
        div(class = "modal-content",
          div(class = "modal-header", title = "Drag to move; double-click to put it back",
              span(class = "chat-grip", icon("grip-vertical")),
              h5(class = "modal-title me-auto", id = ns("title"), icon("comments"),
                 " Ask RentLens"),
              tags$button(type = "button", class = "btn-close", `data-bs-dismiss` = "modal",
                          `aria-label` = "Close")),
          div(class = "modal-body",
              div(class = "chat-note",
                  "Answers come from an AI model that looks up this app's rental data. It can ",
                  "make mistakes, so check important figures on the other pages, and don't ",
                  "share personal details."),
              shinychat::chat_ui(ns("chat"), placeholder = "Ask about rents, areas or condos...",
                                 width = "100%", allow_attachments = FALSE,
                                 show_history = FALSE)))))
  )
}

#' @noRd
mod_chat_server <- function(id, filters) {
  moduleServer(id, function(input, output, session) {
    config <- chat_config()
    if (is.null(config)) return(invisible(NULL))

    # The tools run outside Shiny's reactive context, so read with isolate()
    my_filters <- function() isolate({
      d <- filters$comparables()
      list(filters = filters$summary(), budget = filters$budget(),
           places = filters$places()$name,
           matching_rentals = nrow(d),
           median_rent = if (nrow(d)) median(d$rent) else NA,
           share_within_budget = if (nrow(d)) round(mean(d$rent >= filters$budget()[1] &
                                                           d$rent <= filters$budget()[2]), 2)
                                 else NA)
    })
    client <- new_rentlens_chat(config, my_filters)
    # history = FALSE: conversations aren't saved on the server
    shinychat::chat_server("chat", client, history = FALSE, greeting = paste(
      "Hi! Ask me about private rents in Singapore, for example:",
      "\n\n- *What does a 2-bedroom near Novena MRT cost?*",
      "\n- *Which districts are cheapest for a 1-bedroom?*",
      "\n- *Condos in Clementi under S$4,000 for 2 bedrooms*",
      "\n- *How have rents in Holland changed?*"))
  })
}
