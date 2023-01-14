
#' One of FlexFile Family
#'
#' \code{one_of_flexfile_family()} checks if object is of a type within the FlexFile family. This means
#' is of type \code{flexfile}, \code{quantityreport}, \code{maintrepair}, or \code{techdatareport}.
#'
#' @rdname flexfile_class
#'
#' @export
one_of_flexfile_family <- function(x) {

  flexfile_fam_types <- c("flexfile", "quantityreport", "maintrepair", "techdatareport")
  any(purrr::map_lgl(flexfile_fam_types, ~ inherits(x, .x)))
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

  is_fam <- purrr::map_lgl(report_list, one_of_flexfile_family)

  if (!all(is_fam)) return(FALSE)
  if (length(report_list) < 2) return(TRUE)

  # remove fields that are okay to be different
  report_metadata <- report_list %>%
    purrr::map("ReportMetadata") %>%
    purrr::map(dplyr::select(-tidyselect::starts_with("PointOfContact")))

  all(purrr::map_lgl(report_metadata[-1], identical, report_metadata[[1]]))

}

create_flexfile_family <- function(..., report_list = NULL) {

  if (is.null(reports)) reports <- rlang::list2(...)

  # check to make sure there is:
  # 1 and only 1 flexfile
  # 0 or 1 quantityreport, maintrepair, techdatareport

  flexfile_fam_types <- c("flexfile", "quantityreport", "maintrepair", "techdatareport")

  purrr::map_lgl(reports, purrr::map_lgl(flexfile_fam_types, ~ inherits(x, .x)))

  n_flexfile <- sum(purrr::map_lgl(reports, is_flexfile))
  if (!(n_flexfile == 0)) stop("FlexFile Family must include one and only one FlexFile.")

  n_reports <- c("quantityreport" = sum(purrr::map_lgl(reports, is_quantityreport)),
                 "maintrepair" = sum(purrr::map_lgl(reports, is_maintrepair)),
                 "techdatareport" = sum(purrr::map_lgl(reports, is_techdatareport)))



}
