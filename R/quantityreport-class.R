#' quantityreport class utilities
#'
#' Functions to help with with the 'quantityreport' class type.
#'
#' @name quantityreport_class
#'
#' @param x An object to test or coerce to type 'quantityreport'.
#' @inheritParams flexfile_class
#'
NULL

#' @keywords internal
new_quantityreport <- function(x, fileinfo = NULL) {
  if (is.null(fileinfo))
    fileinfo <- fileinfo_proto()

  structure(x, fileinfo = fileinfo, class = "quantityreport")
}

#' is_quantityreport
#'
#' \code{is_quantityreport()} checks if object is of type 'quantityreport'.
#'
#' @rdname quantityreport_class
#'
#' @export
is_quantityreport <- function(x) {
  inherits(x, "quantityreport")
}

#' as_quantityreport
#'
#' \code{as_quantityreport()} creates object of type 'quantityreport' and checks it against
#' the file specification.
#'
#' @rdname quantityreport_class
#'
#' @export
as_quantityreport <- function(x, .show_check = TRUE) {

  file_type = "Quantity"
  table_spec <- readflexfile::quantity_spec

  table_spec$fields <- table_spec$fields %>%
    dplyr::left_join(dplyr::select(table_spec$tables, .data$table, .data$snake_table), by = "table") %>%
    dplyr::mutate(table = .data$snake_table,
                  field = .data$snake_name)

  table_spec$tables <- table_spec$tables %>%
    dplyr::mutate(table = .data$snake_table)

  check <- check_spec(x, table_spec, file_type, .silent = isFALSE(.show_check))

  new_quantityreport(x)
}

#' is_quantityreport_list
#'
#' \code{is_quantityreport_list()} check if the object is a list where all members are of class 'quantityreport'.
#'
#' @rdname quantityreport_class
#'
#' @export
is_quantityreport_list <- function(x) {
  all(vapply(x, is_quantityreport, logical(1)))
}


