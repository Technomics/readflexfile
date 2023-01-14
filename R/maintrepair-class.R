#' maintrepair class utilities
#'
#' Functions to help with with the 'maintrepair' class type.
#'
#' @name maintrepair_class
#'
#' @param x An object to test or coerce to type 'maintrepair'.
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
