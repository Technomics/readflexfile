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
#' @param x An object to test or coerce to type 'techdatareport'.
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
#' \code{is_techdatareport()} checks whether an object is of type 'techdatareport'.
#'
#' @rdname techdatareport_class
#'
#' @export
is_techdatareport <- function(x) {
  inherits(x, "techdatareport")
}

#' is_techdatareport_list
#'
#' \code{is_techdatareport_list()} checks whether the object is a list where all members are of class 'techdatareport'.
#'
#' @rdname techdatareport_class
#'
#' @export
is_techdatareport_list <- function(x) {
  all(vapply(x, is_techdatareport, logical(1)))
}

