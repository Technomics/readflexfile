#' techdatareport class utilities
#'
#' Functions to help with the 'techdatareport' class type.
#'
#' @details
#' The \code{techdatareport} class has the following attributes.
#' \describe{
#'   \item{allocated}{Logical. Have the allocations been applied to the \code{actualcosthourdata}
#'   table.}
#'   \item{rolledup}{Logical. Has the WBS roll up been applied to the \code{actualcosthourdata}
#'   and \code{forecastatcompletioncosthourdata} tables.}
#' }
#'
#' @name techdatareport_class
#'
#' @param x An object to test or coerce to type 'quantityreport'.
#'
NULL

#' @keywords internal
new_techdatareport <- function(x, fileinfo = NULL) {
  if (is.null(fileinfo))
    fileinfo <- fileinfo_proto()

  structure(x, fileinfo = fileinfo, class = "techdatareport")
}

#' is_techdatareport
#'
#' \code{is_techdatareport()} checks if object is of type 'techdatareport'
#'
#' @rdname techdatareport_class
#'
#' @export
is_techdatareport <- function(x) {
  inherits(x, "techdatareport")
}

# #' as_techdatareport
# #'
# #' \code{as_techdatareport()} creates object of type 'techdatareport' and checks it against
# #' the file specification.
# #'
# #' @rdname techdatareport_class
# #'
# #' @export
# as_techdatareport <- function(x, names_case = c("snake_case", "data_model"),
#                         allocated = FALSE, rolledup = FALSE, .drop_optional = TRUE, .show_check = TRUE) {
#
#   names_case <- names_case[1]
#
#   file_type = "techdatareport"
#   table_spec <- readflexfile::techdatareport_spec
#   table_spec_mod <- table_spec
#
#   if (names_case == "data_model") {
#     x <- x %>%
#       costmisc::coerce_to_spec(table_spec) %>%
#       data_model_to_snake(table_spec)
#   }
#
#   table_spec_mod$fields <- table_spec$fields %>%
#     dplyr::left_join(dplyr::select(table_spec$tables, .data$table, .data$snake_table), by = "table") %>%
#     dplyr::mutate(table = .data$snake_table,
#                   field = .data$snake_name)
#
#   table_spec_mod$tables <- table_spec$tables %>%
#     dplyr::mutate(table = .data$snake_table)
#
#   check <- costmisc::check_spec(x, table_spec_mod, file_type,
#                                 .silent = isFALSE(.show_check),
#                                 .include_table_type = "submission")
#
#   # add missing tables and columns and create techdatareport object
#   x <- x %>%
#     costmisc::add_missing_spec_tables(table_spec_mod, check) %>%
#     costmisc::add_missing_spec_cols(table_spec_mod, new_name = "field")
#
#   if (.drop_optional) x <- drop_na_optional_spec_tables(x, table_spec)
#
#   new_techdatareport(x, allocated = allocated, rolledup = rolledup)
# }

#' is_techdatareport_list
#'
#' \code{is_techdatareport_list()} check if the object is a list where all members are of class 'techdatareport'.
#'
#' @rdname techdatareport_class
#'
#' @export
is_techdatareport_list <- function(x) {
  all(vapply(x, is_techdatareport, logical(1)))
}
