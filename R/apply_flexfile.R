
#' Apply a function over a list of flexfiles
#'
#' \code{apply_flexfile()} is used to apply a function over each element of a list of
#' objects of type 'flexfile'. If the input is a single 'flexfile' is (i.e., not a list),
#' then the function will be applied only to that item.\cr
#' \cr
#' Therefore, use \code{apply_flexfile} when you wish to apply a function and are unsure
#' if the input is a list of flexfiles or a single flexfile.
#'
#' @param flexfile A single flexfile or a list of flexfiles imported through the \code{read_flexfile} function.
#' @param fun  The function to be applied.
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
