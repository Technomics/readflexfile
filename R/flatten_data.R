
#' Flatten list of data
#'
#' \code{flatten_data.list()} will check if each item in the list has the same class. If
#' so, iterate over each item and let its method dispatch.
#'
#' @inheritParams costmisc::flatten_data
#'
#' @export
flatten_data.list <- function(x, ...) {
  all_class_equal <- length(unique.default(lapply(x, class))) == 1L

  if (all_class_equal) lapply(x, flatten_data) else x
}

## ===== Flatten FlexFile ----

#' Create a cost and hour dataframe from a FlexFile
#'
#' \code{flatten_data.flexfile()} creates a single flat file for working with FlexFile
#' data. Input should be a list of one or more FlexFiles imported through the \code{read_flexfile} function.\cr
#' \cr
#' All fields retain their original names from the data model unless they are added in the flattening process. These
#' new fields are in 'snake_case' rather than ProperCase.
#'
#' @export
#'
#' @inheritParams costmisc::flatten_data
#' @param .allocate Logical whether or not to apply the allocations before flattening. In almost all
#' cases this should be left as \code{TRUE}.
#'
#' @examples
#' \dontrun{
#' # Flatten one FlexFile
#' file <- system.file("extdata", "Sample_FlexFile_A.zip", package = "flexample")
#'
#' flat_flex_file <- read_flexfile(file) %>%
#'   flatten_data()
#'
#' # Flatten multiple FlexFiles
#' library(dplyr)
#'
#' files <- system.file("extdata", package = "flexample")
#'
#' flat_flexfiles <- read_folder(files, read_flexfile) %>%
#'   flatten_data()
#'   bind_rows(.id = "doc_id")
#'}
flatten_data.flexfile <- function(x, .allocate = TRUE, ...) {

  x <- costmisc::assert_case(x, target_case = "native")

  # if it says to allocate and it hasn't been already
  if (.allocate & !(attr(x, "allocated")))
    x <- allocate_flexfile(x)

  x_allocated <- attr(x, "allocated")
  x_rolledup <- attr(x, "rolledup")

  # join in the sfc mappings
  flexfile_sfc <- normalize_functional_categories(x, direct_or_oh_mapping = TRUE)

  actuals <- flatten_actuals(flexfile_sfc)
  forecasts <- flatten_forecasts(flexfile_sfc)

  flatfile <- dplyr::bind_rows(actuals, forecasts) %>%
    flexfile_order_columns(.all = (x_allocated & !x_rolledup))

  new_flexfile_flat(flatfile)
}

#' Key columns for the flexfile
#'
#' \code{flexfile_key_columns()} returns a vector of column names that uniquely define
#' a FlexFile and Quantity Data Report.
#'
#' @export
flexfile_key_columns <- function() {
  c("ApprovedPlanNumber", "ApprovedPlanRevisionNumber",
    "SubmissionEvent_Number", "ResubmissionNumber")
}

# find_duplicates <- function(x) {
#
#   dups <- duplicated_report(x)
#
#   stacked_metadata <- x %>%
#     costmisc::listindex_to_col("list_index") %>%
#     purrr::map_dfr("ReportMetadata", .id = "doc_id") %>%
#     dplyr::filter(dups) %>%
#     dplyr::select(.data$list_index, .data$doc_id, tidyselect::all_of(flexfile_key_columns()),
#                   .data$ProgramName, .data$ReportingOrganization_OrganizationName,
#                   .data$report_as_of, .data$date_prepared)
#
#   message(paste(flexfile_key_columns(), collapse = ", "))
#
# }

#' Check for duplication in the metadata
#'
#' @param x A flexfile or quantityreport
#'
#' @keywords internal
duplicated_report <- function(x) {

  stacked_metadata <- x %>%
    purrr::map_dfr("ReportMetadata")

  stacked_metadata %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(flexfile_key_columns()))) %>%
    dplyr::mutate(is_unique = (dplyr::n() > 1)) %>%
    dplyr::pull(.data$is_unique)

}

## ===== Internal FlexFile Helpers =====

#' @keywords internal
flatten_metadata <- function(x) {

  # single row metadata
  x$ReportMetadata %>%
    dplyr::select(.data$ProgramName, .data$ApprovedPlanNumber, .data$ApprovedPlanRevisionNumber,
                  .data$SubmissionEvent_Number, .data$ResubmissionNumber, .data$ReportingOrganization_OrganizationName)
}

#' @keywords internal
flatten_actuals <- function(x)  {

  # single row metadata
  meta <- flatten_metadata(x)

  # join in all of the information
  # note: the order matters! for example UnitsOrSublots must come before EndItems and OrderOrLots
  x$ActualCostHourData %>%
    tibble::add_column(!!!meta, .before = 1) %>%
    dplyr::left_join(dplyr::select(x$Accounts,
                                   .data$ID, .data$Name),
                     by = c(AccountID = "ID"),
                     suffix = c("", ".accounts")) %>%
    dplyr::left_join(x$UnitsOrSublots,
                     by = c(UnitOrSublotID = "ID"),
                     suffix = c("", ".unitsorsublots")) %>%
    dplyr::mutate(OrderOrLotID = dplyr::coalesce(.data$OrderOrLotID, .data$OrderOrLotID.unitsorsublots),
                  EndItemID = dplyr::coalesce(.data$EndItemID, .data$EndItemID.unitsorsublots)) %>%
    dplyr::left_join(dplyr::select(x$EndItems,
                                   .data$ID, .data$Name),
                     by = c(EndItemID = "ID"),
                     suffix = c("", ".enditems")) %>%
    dplyr::left_join(dplyr::select(x$OrdersOrLots,
                                   .data$ID, .data$Name),
                     by = c(OrderOrLotID = "ID"),
                     suffix = c("", ".ordersorlots")) %>%
    dplyr::left_join(dplyr::select(x$CLINs,
                                   .data$ID, .data$Name),
                     by = c(CLIN_ID = "ID"),
                     suffix = c("", ".clins")) %>%
    dplyr::left_join(dplyr::select(x$WBS,
                                   .data$Level, .data$ID, .data$Name, .data$ParentID),
                     by = c(WBSElementID = "ID"),
                     suffix = c("", ".wbs")) %>%
    dplyr::left_join(dplyr::select(x$FunctionalCategories,
                                   .data$ID, .data$Name),
                     by = c(FunctionalCategoryID = "ID"),
                     suffix = c("", ".functionalcategories")) %>%
    dplyr::left_join(dplyr::select(x$FunctionalOverheadCategories,
                                   .data$ID, .data$Name),
                     by = c(FunctionalOverheadCategoryID = "ID"),
                     suffix = c("", ".overheadcategories")) %>%
    dplyr::left_join(dplyr::select(x$ReportingCalendar,
                                   .data$ID, .data$StartDate, .data$EndDate),
                     by = c(ReportingPeriodID = "ID"),
                     suffix = c("", ".reportingcalendar")) %>%
    dplyr::mutate(StartDate = lubridate::ymd(.data$StartDate),
                  EndDate = lubridate::ymd(.data$EndDate),
                  atd_or_fac = "ATD") %>%
    dplyr::rename(AccountName = .data$Name,
                  CLIN_Name = .data$Name.clins,
                  WBSParentID = .data$ParentID,
                  WBSName = .data$Name.wbs,
                  WBSLevel = .data$Level,
                  EndItemName = .data$Name.enditems,
                  OrderOrLotName = .data$Name.ordersorlots,
                  FunctionalCategoryName = .data$Name.functionalcategories,
                  FunctionalOverheadCategoryName = .data$Name.overheadcategories) %>%
    costmisc::add_missing_column(FirstUnitNumber = NA_integer_, LastUnitNumber = NA_integer_) %>%
    dplyr::select(!tidyselect::contains("."))

}

#' @keywords internal
flatten_forecasts <- function(x) {

  if (nrow(x$ForecastAtCompletionCostHourData) > 0) {

    # single row metadata
    meta <- flatten_metadata(x)

    x$ForecastAtCompletionCostHourData %>%
      tibble::add_column(!!!meta, .before = 1) %>%
      dplyr::left_join(dplyr::select(x$OrdersOrLots,
                                     OrderOrLotID = .data$ID,
                                     OrderOrLotName = .data$Name),
                       by = "OrderOrLotID") %>%
      dplyr::left_join(dplyr::select(x$WBS,
                                     WBSElementID = .data$ID,
                                     WBSLevel = .data$Level,
                                     WBSName = .data$Name,
                                     WBSParentID = .data$ParentID),
                       by = "WBSElementID") %>%
      tibble::add_column(atd_or_fac = "FAC")

  } else {
    NULL
  }

}

#' @keywords internal
flexfile_order_columns <- function(x, .all = TRUE) {

  select_fn <- ifelse(.all, tidyselect::all_of, tidyselect::any_of)

  select_cols <- c("ProgramName",
                   flexfile_key_columns(),
                   "ReportingOrganization_OrganizationName",
                   "OrderOrLotID", "OrderOrLotName",
                   "UnitOrSublotID", "FirstUnitNumber", "LastUnitNumber",
                   "CLIN_ID", "CLIN_Name",
                   "EndItemID", "EndItemName",
                   "WBSElementID", "WBSName", "WBSParentID", "WBSLevel",
                   "AccountID", "AccountName",
                   "NonrecurringOrRecurringID",
                   "FunctionalCategoryID", "FunctionalCategoryName",
                   "FunctionalOverheadCategoryID", "FunctionalOverheadCategoryName",
                   "StandardCategoryID",
                   "DetailedStandardCategoryID",
                   "ReportingPeriodID", "StartDate", "EndDate",
                   "AllocationMethodID",
                   "atd_or_fac",
                   "PercentValue", "Value_Dollars", "Value_Hours")

  x %>%
    dplyr::select(select_fn(select_cols),
                  tidyselect::starts_with("Tag"),
                  tidyselect::everything()) #everything else isn't required by the data model

}



## ===== Flatten Quantity Data Report ----

#' Create a cost and hour dataframe from a Quantity Data Report
#'
#' \code{flatten_data.quantityreport()} creates a single flat file for working with
#' the Quantity Data Report. Input should be a list of one or more Quantity Data Reports
#' imported through the \code{read_flexfile} function.
#'
#' @export
#'
#' @inheritParams costmisc::flatten_data
#'
#' @examples
#' \dontrun{
#' # Flatten one Quantity Report
#' file <- system.file("extdata", "Sample_Quantity_A.zip", package = "flexample")
#'
#' flat_flex_file <- read_flexfile(file) %>%
#'   flatten_data()
#'}
flatten_data.quantityreport <- function(x, ...) {

  # single row metadata
  meta <- x$ReportMetadata %>%
    dplyr::select(.data$ProgramName, .data$ApprovedPlanNumber, .data$ApprovedPlanRevisionNumber,
                  .data$SubmissionEvent_Number, .data$ResubmissionNumber, .data$ReportingOrganization_OrganizationName)

  join_common_fields <- function(start_table) {
    start_table %>%
      dplyr::left_join(dplyr::select(x$OrdersOrLots,
                                     OrderOrLotID = .data$ID,
                                     OrderOrLotName = .data$Name),
                       by = "OrderOrLotID") %>%
      dplyr::left_join(dplyr::select(x$WBS,
                                     WBSElementID = .data$ID,
                                     WBSLevel = .data$Level,
                                     WBSName = .data$Name,
                                     WBSParentID = .data$ParentID),
                       by = "WBSElementID")
  }

  quant_to_date <- join_common_fields(x$QuantitiesToDate) %>%
    tibble::add_column(atd_or_fac = "ATD")

  quant_completion <- join_common_fields(x$QuantitiesAtCompletion) %>%
    dplyr::left_join(dplyr::select(x$EndItems,
                                   EndItemID = .data$ID,
                                   EndItemName = .data$Name),
                     by = "EndItemID") %>%
    dplyr::left_join(x$ProductionSequence,
                     by = c("EndItemID", "OrderOrLotID")) %>%
    tibble::add_column(atd_or_fac = "FAC")

  flatfile <- dplyr::bind_rows(quant_to_date, quant_completion) %>%
    tibble::add_column(!!!meta, .before = 1) %>%
    quantityreport_order_columns()

  new_quantityreport_flat(flatfile)
}

#' @keywords internal
quantityreport_order_columns <- function(x, .all = TRUE) {

  select_fn <- ifelse(.all, tidyselect::all_of, tidyselect::any_of)

  select_cols <- c("ProgramName",
                   flexfile_key_columns(),
                   "ReportingOrganization_OrganizationName",
                   "OrderOrLotID", "OrderOrLotName",
                   "EndItemID", "EndItemName",
                   "WBSElementID", "WBSName", "WBSParentID", "WBSLevel",
                   "atd_or_fac",
                   "CompletedQuantityToDate", "InProcessQuantity",
                   "DeliveredQuantityAtCompletion", "InternalQuantityAtCompletion",
                   "CoproductionOrConcurrentQuantityAtCompletion", "GFEQuantityAtCompletion",
                   "FirstUnitNumber", "LastUnitNumber", "IsInternal")

  x %>%
    dplyr::select(select_fn(select_cols),
                  tidyselect::everything()) #everything else isn't required by the data model

}

## ===== Depreciated =====

#' Flatten a FlexFile report
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' \code{flatten_ff()} was removed in favor of \code{\link{flatten_data}()}. See
#' `vignette("importing-flexfile")` for the preferred workflow.
#'
#' @keywords internal
#' @export
flatten_ff <- function(file, .show_check = FALSE, .coerce_spec = TRUE, .warn_utf8_bom = TRUE) {
  lifecycle::deprecate_stop(when = "0.3.0", what = "flatten_ff()", with = "flatten_data()")

}
