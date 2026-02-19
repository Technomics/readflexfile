
#' Apply a function over flexfile objects
#'
#' \code{apply_flexfile()} applies a function to each element of a list of
#' objects of type 'flexfile'. If the input is a single 'flexfile' (that is,
#' not a list), the function is applied to that object only.
#'
#' Use \code{apply_flexfile()} when you want one interface that accepts either
#' a single flexfile or a list of flexfiles.
#'
#' @param flexfile A single flexfile or a list of flexfiles imported with
#' \code{read_flexfile()}.
#' @param fun Function to apply.
#' @param ... Additional arguments passed to \code{fun}.
#'
#' @return The original structure with the function applied.
apply_flexfile <- function(flexfile, fun, ...) {

  if (is_flexfile(flexfile)) {
    fun(flexfile, ...)
  } else if (is_flexfile_list(flexfile)) {
    purrr::modify(flexfile, fun, ...)
  } else {
    stop("One or more elements of 'flexfile' is not of class 'flexfile'")
  }

}
