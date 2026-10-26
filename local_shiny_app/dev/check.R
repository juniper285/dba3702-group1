# Housekeeping to run before committing. Run from the package root.

roxygen2::roxygenise()   # rebuild NAMESPACE from the #' @import / @export tags
testthat::test_local()   # run tests/testthat
