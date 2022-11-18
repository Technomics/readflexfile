#' maintrepair class utilities
#'
#' Functions to help with with the 'maintrepair' class type.
#'
#' @details
#' The \code{maintrepair} class has the following attributes.
#' \describe{
#'   \item{allocated}{Logical. Have the allocations been applied to the \code{actualcosthourdata}
#'   table.}
#'   \item{rolledup}{Logical. Has the WBS roll up been applied to the \code{actualcosthourdata}
#'   and \code{forecastatcompletioncosthourdata} tables.}
#' }
#'
#' @name maintrepair_class
#'
#' @param x An object to test or coerce to type 'maintrepair'.
#' @param names_case Case of the object being passed in.
#' @param .show_check Logical whether or not to show results from the check against
#' the file specification.
#' @inheritParams read_maintrepair
#'
NULL

#' @keywords internal
new_maintrepair <- function(x, fileinfo = NULL) {
  if (is.null(fileinfo))
    fileinfo <- fileinfo_proto()

  structure(x, fileinfo = fileinfo, class = "maintrepair")
}

#' is_maintrepair
#'
#' \code{is_maintrepair()} checks if object is of type 'maintrepair'
#'
#' @rdname maintrepair_class
#'
#' @export
is_maintrepair <- function(x) {
  inherits(x, "maintrepair")
}

# #' as_maintrepair
# #'
# #' \code{as_maintrepair()} creates object of type 'maintrepair' and checks it against
# #' the file specification.
# #'
# #' @rdname maintrepair_class
# #'
# #' @export
# as_maintrepair <- function(x, names_case = c("snake_case", "data_model"),
#                         allocated = FALSE, rolledup = FALSE, .drop_optional = TRUE, .show_check = TRUE) {
#
#   names_case <- names_case[1]
#
#   file_type = "maintrepair"
#   table_spec <- readflexfile::maintrepair_spec
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
#   # add missing tables and columns and create maintrepair object
#   x <- x %>%
#     costmisc::add_missing_spec_tables(table_spec_mod, check) %>%
#     costmisc::add_missing_spec_cols(table_spec_mod, new_name = "field")
#
#   if (.drop_optional) x <- drop_na_optional_spec_tables(x, table_spec)
#
#   new_maintrepair(x, allocated = allocated, rolledup = rolledup)
# }

#' is_maintrepair_list
#'
#' \code{is_maintrepair_list()} check if the object is a list where all members are of class 'maintrepair'.
#'
#' @rdname maintrepair_class
#'
#' @export
is_maintrepair_list <- function(x) {
  all(vapply(x, is_maintrepair, logical(1)))
}

#' @keywords internal
fileinfo_proto <- function() {
  list(path = character(),
       name = character(),
       name_ext = character())
}
