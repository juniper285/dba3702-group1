# A stand-in for mod_settings_server()'s return value
fake_settings <- function(...) {
  s <- default_settings()
  changes <- list(...)
  s[names(changes)] <- changes      # not modifyList(): it would merge into `places`
  rv <- reactiveVal(s)
  list(settings = rv, first_visit = reactive(FALSE), save = function(x) rv(x))
}
