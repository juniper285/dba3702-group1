#' Inputs a DataTable creates, which should not be saved in bookmark links
#'
#' @param table_ids Output ids of the DTOutput()s.
#' @noRd
dt_bookmark_inputs <- function(table_ids) {
  suffixes <- c("_rows_selected", "_rows_all", "_rows_current", "_row_last_clicked",
                "_cell_clicked", "_cells_selected", "_columns_selected", "_search",
                "_search_columns", "_state", "_cell_info")
  as.vector(outer(table_ids, suffixes, paste0))
}
