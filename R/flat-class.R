#' *_flat class utilities
#'
#' Functions to help with with the 'flexfile_flat' and 'quantityreport_flat' class type.
#'
#' @name flat_class
#'
#' @param x An object to check for type '*_flat'.
#'
NULL

#' @keywords internal
new_flexfile_flat <- function(x) {
  structure(x, class = c("flexfile_flat", class(x)))
}

#' is_flexfile_flat
#'
#' \code{is_flexfile_flat()} checks if object is of type 'flexfile_flat'
#'
#' @rdname flat_class
#'
#' @export
is_flexfile_flat <- function(x) {
  inherits(x, "flexfile_flat")
}

#' @keywords internal
new_quantityreport_flat <- function(x) {
  structure(x, class = c("quantityreport_flat", class(x)))
}

#' is_quantityreport_flat
#'
#' \code{is_quantityreport_flat()} checks if object is of type 'quantityreport_flat'
#'
#' @rdname flat_class
#'
#' @export
is_quantityreport_flat <- function(x) {
  inherits(x, "quantityreport_flat")
}
