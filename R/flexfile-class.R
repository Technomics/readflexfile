
#' @keywords internal
new_flexfile <- function(x) {
  structure(x, class = "flexfile")
}

#' Check is object is of type flexfile
#'
#' @export
is_flexfile <- function(x) {
  inherits(x, "flexfile")
}

#' Create object of type flexfile
#'
#' @export
as_flexfile <- function(x, .show_check = TRUE) {

  file_type = "FlexFile"
  table_spec <- readflexfile::flexfile_spec

  table_spec$fields <- table_spec$fields %>%
    dplyr::left_join(dplyr::select(table_spec$tables, table, snake_table), by = "table") %>%
    dplyr::mutate(table = snake_table,
                  field = snake_name)

  table_spec$tables <- table_spec$tables %>%
    dplyr::mutate(table = snake_table)

  check <- check_spec(x, table_spec, file_type, .silent = isFALSE(.show_check))

  new_flexfile(x)
}

#' Check is object is a list of flexfiles
#'
#' @export
is_flexfile_list <- function(x) {
  all(vapply(x, is_flexfile, logical(1)))
}


