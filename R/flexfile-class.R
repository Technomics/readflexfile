#' flexfile class utilities
#'
#' Functions to help with with the 'flexfile' class type.
#'
#' @details
#' The \code{flexfile} class has the following attributes.
#' \describe{
#'   \item{allocated}{Logical. Have the allocations been applied to the \code{ActualCostHourData}
#'   table.}
#'   \item{rolledup}{Logical. Has the WBS roll up been applied to the \code{ActualCostHourData}
#'   and \code{ForecastAtCompletionCostHourData} tables.}
#' }
#'
#' @name flexfile_class
#'
#' @param x An object to test or coerce to type 'flexfile'.
#' @param names_case Case of the object being passed in.
#' @param data_case Desired case of the returned object.
#' @param allocated Logical whether the flexfile has been allocated.
#' @param rolledup Logical whether the flexfile WBS has been rolled up.
#' @param .show_check Logical whether or not to show results from the check against
#' the file specification.
#' @inheritParams read_flexfile
#'
NULL

#' @keywords internal
new_flexfile <- function(x, fileinfo = NULL, allocated = FALSE, rolledup = FALSE, data_case = "snake") {
  if (is.null(fileinfo))
    fileinfo <- fileinfo_proto()

  structure(x,
            fileinfo = fileinfo, allocated = allocated, rolledup = rolledup, data_case = data_case,
            data_spec = readflexfile::flexfile_spec,
            class = "flexfile")
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
as_flexfile <- function(x, names_case = c("snake_case", "data_model"),
                        allocated = FALSE, rolledup = FALSE, .drop_optional = TRUE, .show_check = TRUE) {

  names_case <- names_case[1]

  file_type = "FlexFile"
  table_spec <- readflexfile::flexfile_spec
  table_spec_mod <- table_spec

  if (names_case == "data_model") {
    x <- x %>%
      costmisc::coerce_to_spec(table_spec) %>%
      data_model_to_snake(table_spec)
  }

  table_spec_mod$fields <- table_spec$fields %>%
    dplyr::left_join(dplyr::select(table_spec$tables, .data$table, .data$snake_table), by = "table") %>%
    dplyr::mutate(table = .data$snake_table,
                  field = .data$snake_name)

  table_spec_mod$tables <- table_spec$tables %>%
    dplyr::mutate(table = .data$snake_table)

  check <- costmisc::check_spec(x, table_spec_mod, file_type,
                                .silent = isFALSE(.show_check),
                                .include_table_type = "submission")

  # add missing tables and columns and create flexfile object
  x <- x %>%
    costmisc::add_missing_spec_tables(table_spec_mod, check) %>%
    costmisc::add_missing_spec_cols(table_spec_mod, new_name = "field")

  if (.drop_optional) x <- drop_na_optional_spec_tables(x, table_spec)

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

#' @keywords internal
fileinfo_proto <- function() {
  list(path = character(),
       name = character(),
       name_ext = character())
}
