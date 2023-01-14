
#' @keywords internal
.flexfile_fam_types <- c("flexfile", "quantityreport", "maintrepair", "techdatareport")

#' One of FlexFile Family
#'
#' \code{one_of_flexfile_family()} checks if object is of a type within the FlexFile family. This means
#' is of type \code{flexfile}, \code{quantityreport}, \code{maintrepair}, or \code{techdatareport}.
#'
#' @rdname flexfile_class
#'
#' @export
one_of_flexfile_family <- function(x) {

  #any(purrr::map_lgl(.flexfile_fam_types, ~ inherits(x, .x)))
  inherits(x, .flexfile_fam_types)

}

#' Which of FlexFile Family
#'
#' \code{which_flexfile_family()} returns which type within the family the object is. Results are
#' \code{flexfile}, \code{quantityreport}, \code{maintrepair}, or \code{techdatareport}.
#'
#' @rdname flexfile_class
#'
#' @export
which_flexfile_family <- function(x) {

  if (!one_of_flexfile_family(x)) return(NA_character_)
  .flexfile_fam_types[purrr::map_lgl(.flexfile_fam_types, ~ inherits(x, .x))]

}


#' Check if Reports have Identical Metadata
#'
#' \code{identical_report_metadata()} checks a collection reports to see if they have identical metadata.
#' This check will exclude the "PointOfContact" fields.
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
    purrr::map(~ dplyr::select(.x,
                               -tidyselect::starts_with("PointOfContact"),
                               -tidyselect::any_of("ReportingPeriodID")))

  all(purrr::map_lgl(report_metadata[-1], identical, report_metadata[[1]]))

}

#' Create a FlexFile Family
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' \code{create_flexfile_family()} creates a list of related FlexFile Family reports. For example,
#' it is used to store the matching FlexFile and Quantity Data Report together.
#'
#' @param ... One or more reports in the FlexFile family of types.
#' @param report_list Alternatively, a list of reports. If provided then the
#' contents of \code{...} will be ignored.
#'
#' @return A list of reports of type \code{flexfile_family}.
#' @export
#'
create_flexfile_family <- function(..., report_list = NULL) {

  if (is.null(report_list)) report_list <- rlang::list2(...)

  # check to make sure there is:
  # 1 and only 1 flexfile
  # 0 or 1 quantityreport, maintrepair, techdatareport
  report_types <- purrr::map_chr(report_list, which_flexfile_family)

  if (sum(is.na(report_types)) > 0)
    stop(paste("Flexfile Family must only include reports of type",
               "'flexfile', 'quantityreport', 'maintrepair', or 'techdatareport'."))

  n_reports <- .flexfile_fam_types %>%
    rlang::set_names() %>%
    purrr::map_int(~ sum(report_types == .x))

  if (n_reports["flexfile"] != 1) stop("FlexFile Family must include one and only one report of type 'flexfile'.")

  if (any(n_reports > 1))
    stop("FlexFile Family must not include more than one report of type 'quantityreport', 'maintrepair', or 'techdatareport'.")

  report_list_typed <- rlang::set_names(report_list, report_types)

  new_flexfile_family(flexfile = report_list_typed$flexfile,
                      quantityreport = report_list_typed$quantityreport,
                      maintrepair = report_list_typed$maintrepair,
                      techdatareport = report_list_typed$techdatareport)

}
