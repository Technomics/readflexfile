
#' @keywords internal
new_flexfile_family <- function(flexfile = NULL, quantityreport = NULL, maintrepair = NULL, techdatareport = NULL) {

  report_list <- list(flexfile = flexfile,
                      quantityreport = quantityreport,
                      maintrepair = maintrepair,
                      techdatareport = techdatareport)

  check_reports <- identical_report_metadata(report_list = report_list)
  if (!check_reports) stop("Each report must be of the same FlexFile family.")

  structure(report_list, class = "flexfile_family")
}
