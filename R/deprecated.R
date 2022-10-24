
#' Read FlexFile or Quantity report
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' \code{read_ff()} was renamed to \code{\link{read_flexfile}()} to be less ambiguous.
#'
#' @keywords internal
#' @export
read_ff <- function(file, .show_check = FALSE, .coerce_spec = TRUE, .warn_utf8_bom = TRUE) {
  lifecycle::deprecate_warn(when = "0.3.0", what = "read_ff()", with = "read_flexfile()")

  read_flexfile(file, .show_check = FALSE, .coerce_spec = TRUE, .warn_utf8_bom = TRUE)
}

## ===== Stack FlexFiles =====

#' Stack list of FlexFiles or Quantity Reports into one list
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' This function is depreciated because stacking individual JSON tables is no longer
#' recommended. See \code{vignette("importing-flexfile")} for the preferred workflow.
#' If the functionality is still required, use \code{\link[costmisc]{unnest_df}()} instead.
#'
#' \code{stack_ff()} reads in a list of lists of FlexFile or Quantity Report tibbles and returns a
#' single list of stacked tibbles. The \code{data} list should not mix report type (i.e.,
#' it should contain all FlexFiles or all Quantity Reports).
#'
#' This is a thin wrapper around \code{\link[costmisc]{unnest_df}()}.
#'
#' @export
#'
#' @param data A list of FlexFile or Quantity Report tibbles converted from JSON format.
#'
#' @return A list of stacked tibbles.
#'
#' @examples
#' \dontrun{
#' folder <- system.file("extdata", package = "flexample")
#'
#' flexfiles <- read_folder(folder, read_flexfile) %>%
#'   listindex_to_col() %>%
#'   stack_ff()
#'}
stack_ff <- function(data) {
  lifecycle::deprecate_warn(when = "0.3.0", what = "stack_ff()",
                            details = c("Stacking individual JSON tables is no longer recommended.\n  See `vignette(\"importing-flexfile\")` for the preferred workflow",
                                        "If the functionality is still required, use `costmisc::unnest_df()` instead."))

  costmisc::unnest_df(data)
}
