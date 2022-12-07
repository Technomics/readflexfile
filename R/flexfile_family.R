
#' is_flexfilefamily
#'
#' \code{is_flexfile_family()} checks if object is of a type within the FlexFile family. This means
#' is of type \code{flexfile}, \code{quantityreport}, \code{maintrepair}, or \code{techdatareport}.
#'
#' @rdname flexfile_class
#'
#' @export
is_flexfile_family <- function(x) {

  flexfile_fam <- c("flexfile", "quantityreport", "maintrepair", "techdatareport")
  any(purrr::map_lgl(flexfile_fam, ~ inherits(x, .x)))
}

#' Check if Reports have Identical Metadata
#'
#' @param ... Two or more reports from the FlexFile Family to check.
#' @param report_list Alternatively a list of reports to check. If specified, then
#' \code{...} will be ignored.
#'
#' @return Logical whether all of the reports are from the FlexFile family and with identical
#' Metadata tables.
#' @export
#'
identical_report_metadata <- function(..., report_list = NULL) {

  if (is.null(report_list)) report_list <- rlang::list2(...)

  is_fam <- purrr::map_lgl(report_list, is_flexfile_family)

  if (!all(is_fam)) return(FALSE)
  if (length(report_list) < 2) return(TRUE)

  report_metadata <- purrr::map(report_list, "metadata")

  all(purrr::map_lgl(report_metadata[-1], identical, report_metadata[[1]]))

}
