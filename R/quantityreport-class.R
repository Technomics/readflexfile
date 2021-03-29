
#' @keywords internal
new_quantityreport <- function(x) {
  structure(x, class = "quantityreport")
}

#' Check is object is of type quantityreport
#'
#' @export
is_quantityreport <- function(x) {
  inherits(x, "quantityreport")
}

#' Create object of type quantityreport
#'
#' @export
as_quantityreport <- function(x, .show_check = TRUE) {

  file_type = "Quantity"
  table_spec <- readflexfile::quantity_spec

  table_spec$fields <- table_spec$fields %>%
    dplyr::left_join(dplyr::select(table_spec$tables, table, snake_table), by = "table") %>%
    dplyr::mutate(table = snake_table,
                  field = snake_name)

  table_spec$tables <- table_spec$tables %>%
    dplyr::mutate(table = snake_table)

  check <- check_spec(x, table_spec, file_type, .silent = isFALSE(.show_check))

  new_quantityreport(x)
}

#' Check is object is a list of quantity reports
#'
#' @export
is_quantityreport_list <- function(x) {
  all(vapply(x, is_quantityreport, logical(1)))
}


