# A stand-in for mod_settings_server()'s return value
fake_settings <- function(...) {
  s <- modifyList(default_settings(), list(...))
  rv <- reactiveVal(s)
  list(settings = rv, first_visit = reactive(FALSE), save = function(x) rv(x))
}
