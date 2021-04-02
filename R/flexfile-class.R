#' flexfile class utilities
#'
#' Functions to help with with the 'flexfile' class type.
#'
#' @name flexfile_class
#'
#' @param x An object to test or coerce to type 'flexfile'.
#' @param .show_check Logical whether or not to show results from the check against
#' the file specification
#'
NULL

#' @keywords internal
new_flexfile <- function(x, allocated = FALSE, rolledup = FALSE) {
  structure(x, allocated = allocated, rolledup = rolledup, class = "flexfile")
}

#' is_flexfile
#'
#' \code{is_flexfile()} checks if object is of type 'flexfile'
#'
#' @rdname flexfile_class
#'
#' @export
is_flexfile <- function(x) {
  inherits(x, "flexfile")
}

#' as_flexfile
#'
#' \code{as_flexfile()} creates object of type 'flexfile' and checks it against
#' the file specification.
#'
#' @rdname flexfile_class
#'
#' @export
as_flexfile <- function(x, allocated = FALSE, rolledup = FALSE, .show_check = TRUE) {

  file_type = "FlexFile"
  table_spec <- readflexfile::flexfile_spec

  table_spec$fields <- table_spec$fields %>%
    dplyr::left_join(dplyr::select(table_spec$tables, .data$table, .data$snake_table), by = "table") %>%
    dplyr::mutate(table = .data$snake_table,
                  field = .data$snake_name)

  table_spec$tables <- table_spec$tables %>%
    dplyr::mutate(table = .data$snake_table)

  check <- check_spec(x, table_spec, file_type, .silent = isFALSE(.show_check))

  new_flexfile(x, allocated = allocated, rolledup = rolledup)
}

#' is_flexfile_list
#'
#' \code{is_flexfile_list()} check if the object is a list where all members are of class 'flexfile'.
#'
#' @rdname flexfile_class
#'
#' @export
is_flexfile_list <- function(x) {
  all(vapply(x, is_flexfile, logical(1)))
}


