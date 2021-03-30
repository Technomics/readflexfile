#' Create a flat file from multiple data frames
#'
#' A generic function that is used to create a single flat file from a list of data
#' frames. This list is usually created by reading in a data format
#' with data spanning multiple tables.
#'
#' @param x A list of one or more collections of data frames to be flattened.
#' @param ... Arguments passed on to methods.
#'
#' @return A data frame with the flat file representation of the input data.
#'
#' @export
#'
flatten_data <- function(x, ...) {
  UseMethod("flatten_data")
}

#' Flatten list of data
#'
#' \code{flatten_data.list()} will check if each item in the list has the same class. If
#' so, iterate over each item and let its method dispatch.
#'
#' @export
#'
#' @name flatten_data
#'
flatten_data.list <- function(x, ...) {
  all_class_equal <- length(unique.default(lapply(x, class))) == 1L

  if (all_class_equal) lapply(x, flatten_data) else x
}

## ===== Flatten FlexFile ----

#' Create a cost and hour dataframe from a FlexFile
#'
#' \code{flatten_data.flexfile()} creates a single flat file for working with FlexFile
#' data. Input should be a list of one or more FlexFiles imported through the \code{read_ff} function.
#'
#' @export
#'
#' @name flatten_data
#' @param .allocate Logical whether or not to apply the allocations before flattening. In almost all
#' cases this should be left as \code{TRUE}.
#'
#' @examples
#' \dontrun{
#' # Flatten one FlexFile
#' file <- system.file("extdata", "Sample_FlexFile_A.zip", package = "flexample")
#'
#' flat_flex_file <- read_ff(file) %>%
#'   flatten_data()
#'
#' # Flatten multiple FlexFiles
#' library(dplyr)
#'
#' files <- system.file("extdata", package = "flexample")
#'
#' flat_flexfiles <- read_folder(files, read_ff) %>%
#'   flatten_data()
#'   bind_rows(.id = "doc_id")
#'}
flatten_data.flexfile <- function(x, .allocate = TRUE, ...) {

  if (.allocate)
    x <- allocate_ff(x)

  # selects all, but provides a quick safety net in case of changes
  cats <- readflexfile::sfc_mapping %>%
    dplyr::distinct(.data$standard_category_id, .data$detailed_standard_category_id, .data$direct_or_overhead) %>%
    dplyr::mutate(detailed_standard_category_id = as.character(.data$detailed_standard_category_id))

  dir_oh <- readflexfile::sfc_mapping %>%
    dplyr::distinct(.data$standard_category_id, .data$direct_or_overhead)

  # function to join in the sfc category
  join_sfc <- function(the_table) {
    the_table %>%
      dplyr::mutate(detailed_standard_category_id = as.character(.data$detailed_standard_category_id)) %>%
      dplyr::left_join(cats, by = "detailed_standard_category_id", suffix = c("", "_sfc")) %>%
      dplyr::mutate(standard_category_id = dplyr::coalesce(.data$standard_category_id,
                                                           .data$standard_category_id_sfc)) %>%
      dplyr::select(-.data$direct_or_overhead, -.data$standard_category_id_sfc) %>%
      dplyr::left_join(dir_oh, by = "standard_category_id")
  }

  join_sfc_tables <- c("actualcosthourdata", "forecastatcompletioncosthourdata")

  # for the tables where relevant, join in the sfc mappings
  flexfile_sfc <- purrr::modify_at(x, join_sfc_tables, join_sfc)

  actuals <- flatten_actuals(flexfile_sfc)
  forecasts <- flatten_forecasts(flexfile_sfc)

  dplyr::bind_rows(actuals, forecasts) %>%
    flexfile_order_columns()
}

## ===== Internal FlexFile Helpers =====

#' @keywords internal
flatten_actuals <- function(flexfile)  {

  flexfile$actualcosthourdata %>%
    dplyr::mutate(program_name = flexfile$reportmetadata$program_name[1],
                  reporting_organization_organization_name = flexfile$reporting_organization_organization_name[1]) %>%
    dplyr::left_join(dplyr::select(flexfile$accounts,
                                   .data$id, .data$name, 1),
                     by = c(account_id = "id"),
                     suffix = c("", ".accounts")) %>%
    dplyr::left_join(dplyr::select(flexfile$enditems,
                                   .data$id, .data$name, 1),
                     by = c(end_item_id = "id"),
                     suffix = c("", ".enditems")) %>%
    dplyr::left_join(dplyr::select(flexfile$ordersorlots,
                                   .data$id, .data$name, 1),
                     by = c(order_or_lot_id = "id"),
                     suffix = c("", ".ordersorlots")) %>%
    dplyr::left_join(dplyr::select(flexfile$clins,
                                   .data$id, .data$name, 1),
                     by = c(clin_id = "id"),
                     suffix = c("", ".clins")) %>%
    dplyr::left_join(dplyr::select(flexfile$wbs,
                                   .data$level, .data$id, .data$name, .data$parent_id, 1),
                     by = c(wbs_element_id = "id"),
                     suffix = c("", ".wbs")) %>%
    dplyr::left_join(dplyr::select(flexfile$functionalcategories,
                                   .data$id, .data$name, 1),
                     by = c(functional_category_id = "id"),
                     suffix = c("", ".functionalcategories")) %>%
    dplyr::left_join(dplyr::select(flexfile$functionaloverheadcategories,
                                   .data$id, .data$name, 1),
                     by = c(functional_overhead_category_id = "id"),
                     suffix = c("", ".overheadcategories")) %>%
    dplyr::left_join(dplyr::select(flexfile$reportingcalendar,
                                   .data$id, .data$start_date, .data$end_date, 1),
                     by = c(reporting_period_id = "id"),
                     suffix = c("", ".reportingcalendar")) %>%
    dplyr::left_join(flexfile$unitsorsublots,
                     by = c(unit_or_sublot_id = "id"),
                     suffix = c("", ".unitsorsublots")) %>%
    dplyr::mutate(start_date = lubridate::ymd(.data$start_date),
                  end_date = lubridate::ymd(.data$end_date),
                  atd_or_fac = "ATD",
                  order_or_lot_id = dplyr::coalesce(.data$order_or_lot_id, .data$order_or_lot_id.unitsorsublots),
                  end_item_id = dplyr::coalesce(.data$end_item_id, .data$end_item_id.unitsorsublots)) %>%
    dplyr::rename(account_name = .data$name,
                  clin_name = .data$name.clins,
                  wbs_parent = .data$parent_id,
                  wbs_name = .data$name.wbs,
                  end_item_name = .data$name.enditems,
                  order_or_lot_name = .data$name.ordersorlots,
                  wbs_level = .data$level,
                  functional_category_name = .data$name.functionalcategories,
                  functional_overhead_category_name = .data$name.overheadcategories) %>%
    costmisc::add_missing_column(first_unit_number = NA_integer_, last_unit_number = NA_integer_) %>%
    dplyr::select(!tidyselect::contains("."))

}

#' @keywords internal
flatten_forecasts <- function(flexfile) {

  if (nrow(flexfile$forecastatcompletioncosthourdata) > 0) {

    flexfile$forecastatcompletioncosthourdata %>%
      dplyr::left_join(dplyr::select(flexfile$wbs,
                                     .data$level, .data$id, .data$name, .data$parent_id, 1),
                       by = stats::setNames(c("id"), c("wbs_element_id"))) %>%
      dplyr::left_join(dplyr::select(flexfile$ordersorlots,
                                     .data$id, .data$name, 1),
                       by = stats::setNames(c("id"), c("order_or_lot_id"))) %>%
      dplyr::rename(wbs_parent = .data$parent_id,
                    wbs_name = .data$name.x,
                    wbs_level = .data$level,
                    order_or_lot_name = .data$name.y) %>%
      dplyr::mutate(atd_or_fac = "FAC",
                    order_or_lot_id = .data$order_or_lot_id)

  } else {
    NULL
  }

}


#' @keywords internal
flexfile_order_columns <- function(flexfile, .all = TRUE) {

  select_fn <- ifelse(.all, tidyselect::all_of, tidyselect::any_of)

  select_cols <- c("program_name",
                   "order_or_lot_id", "order_or_lot_name",
                   "unit_or_sublot_id", "first_unit_number", "last_unit_number",
                   "clin_id", "clin_name",
                   "end_item_id", "end_item_name",
                   "wbs_element_id", "wbs_name", "wbs_parent", "wbs_level",
                   "account_id", "account_name",
                   "nonrecurring_or_recurring_id",
                   "functional_category_id", "functional_category_name",
                   "functional_overhead_category_id", "functional_overhead_category_name",
                   "standard_category_id",
                   "detailed_standard_category_id",
                   "reporting_period_id", "start_date", "end_date",
                   "allocation_method_id",
                   "atd_or_fac",
                   "percent_value", "value_dollars", "value_hours")

  flexfile %>%
    dplyr::select(select_fn(select_cols),
                  tidyselect::starts_with("tag"),
                  tidyselect::everything()) #everything else isn't required by the data model

}

## ===== Flatten Quantity Data Report ----

#' Create a cost and hour dataframe from a Quantity Data Report
#'
#' @description
#'
#' \code{flatten_data.quantityreport()} creates a single flat file for working with
#' the Quantity Data Report. Input should be a list of one or more Quantity Data Reports
#' imported through the \code{read_ff} function.
#'
#' @export
#'
#' @name flatten_data
#'
#' @examples
#' \dontrun{
#' # Flatten one Quantity Report
#' file <- system.file("extdata", "Sample_Quantity_A.zip", package = "flexample")
#'
#' flat_flex_file <- read_ff(file) %>%
#'   flatten_data()
#'}
flatten_data.quantityreport <- function(x, ...) {

  quant_to_date <- x$quantitiestodate %>%
    dplyr::left_join(x$ordersorlots,
                     by = c(order_or_lot_id = "id")) %>%
    dplyr::left_join(x$wbs,
                     by = c(wbs_element_id = "id")) %>%
    dplyr::left_join(dplyr::select(x$wbselementremarks, -.data$order_or_lot_id),
                     by = "wbs_element_id") %>%
    dplyr::rename("dictionary_definition" = .data$text,
                  "order_or_lot_name" = .data$name.x,
                  "wbs_element_name" = .data$name.y) %>%
    dplyr::mutate(atd_or_fac = "ATD")

  quant_completion <- x$quantitiesatcompletion %>%
    dplyr::left_join(x$ordersorlots,
                     by = c(order_or_lot_id = "id")) %>%
    dplyr::left_join(x$wbs,
                     by = c(wbs_element_id = "id")) %>%
    dplyr::left_join(dplyr::select(x$wbselementremarks, -.data$order_or_lot_id),
                     by = "wbs_element_id") %>%
    dplyr::left_join(x$productionsequence,
                     by = c("end_item_id", "order_or_lot_id")) %>%
    dplyr::rename("dictionary_definition" = .data$text,
                  "order_or_lot_name" = .data$name.x,
                  "wbs_element_name" = .data$name.y) %>%
    dplyr::mutate(atd_or_fac = "FAC")

  dplyr::bind_rows(quant_to_date, quant_completion)
}
