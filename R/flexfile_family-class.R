
#' @keywords internal
new_flexfile_family <- function(flexfile = NULL, quantityreport = NULL, maintrepair = NULL, techdatareport = NULL) {

  report_list <- list(flexfile = flexfile,
                      quantityreport = quantityreport,
                      maintrepair = maintrepair,
                      techdatareport = techdatareport)

  report_list <- purrr::compact(report_list)

  check_reports <- identical_report_metadata(report_list = report_list)
  if (!check_reports) stop("Each report must be from the same FlexFile family.")

  structure(report_list, class = "flexfile_family")
}

#' is_flexfile_family
#'
#' \code{is_flexfile_family()} checks if object is of type 'flexfile_family'
#'
#' @rdname flexfile_class
#'
#' @export
is_flexfile_family <- function(x) {
  inherits(x, "flexfile_family")
}
