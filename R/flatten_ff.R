#' Flatten FlexFile and Quantity Data Report
#'
#' @description Create separate data frames for the FlexFile and the Quantity Data Report.
#' These functions merge together many tables into a single one more useful for analysis.
#' This generally involves joining together numerous ID fields. Actuals and forecasts are also
#' stacked into a single table.
#'
#' @param flexfile A flexfile created from the \code{read_ff} function.
#' @param quantity_data A quantity data report created from the \code{read_ff} function.
#' @param .id The name of the ID column which uniquely identifies a flexfile.
#'
#' @name flatten_lists
#'
#' @return A data frame with the flattened flexfile or quantity data.
#'
NULL

## ===== Flatten FlexFile ----

#' Create a cost and hour dataframe from a FlexFile
#'
#' @description
#'
#' \code{flatten_ff()} creates a master dataframe for working with FlexFile data.
#'
#' @export
#'
#' @name flatten_lists
#'
#' @examples
#' \dontrun{
#' # Flatten one FlexFile
#' file <- system.file("extdata", "Sample_FlexFile_A.zip", package = "readflexfile")
#'
#' flat_flex_file <- read_ff(file) %>%
#' add_id_col(var = "doc_id") %>%
#' flatten_ff()
#'
#' #Flatten mutliple FlexFiles
#'
#' files <- system.file("extdata", package = "readflexfile")
#'
#' flat_flexfiles <- read_folder(files, read_ff) %>%
#' listindex_to_col(var = "doc_id") %>%
#' stack_ff() %>%
#' flatten_ff()
#'}
flatten_ff <- function(flexfile, .id = "doc_id") {
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
  flexfile_sfc <- purrr::modify_at(flexfile, join_sfc_tables, join_sfc)

  actuals <- flatten_actuals(flexfile_sfc, .id)
  forecasts <- flatten_forecasts(flexfile_sfc, .id)

  dplyr::bind_rows(actuals, forecasts) %>%
    flexfile_order_columns()
}

## ===== Internal FlexFile Helpers =====

#' @keywords internal
flatten_actuals <- function(flexfile, .id)  {

  flexfile$actualcosthourdata %>%
    dplyr::left_join(dplyr::select(flexfile$reportmetadata,
                                   .data$program_name,
                                   .data$reporting_organization_organization_name,
                                   1),
                     by = .id) %>%
    dplyr::left_join(dplyr::select(flexfile$accounts,
                                   .data$id, .data$name, 1),
                     by = stats::setNames(c("id", .id), c("account_id", .id)),
                     suffix = c("", ".accounts")) %>%
    dplyr::left_join(dplyr::select(flexfile$enditems,
                                   .data$id, .data$name, 1),
                     by = stats::setNames(c("id", .id), c("end_item_id", .id)),
                     suffix = c("", ".enditems")) %>%
    dplyr::left_join(dplyr::select(flexfile$ordersorlots,
                                   .data$id, .data$name, 1),
                     by = stats::setNames(c("id", .id), c("order_or_lot_id", .id)),
                     suffix = c("", ".ordersorlots")) %>%
    dplyr::left_join(dplyr::select(flexfile$clins,
                                   .data$id, .data$name, 1),
                     by = stats::setNames(c("id", .id), c("clin_id", .id)),
                     suffix = c("", ".clins")) %>%
    dplyr::left_join(dplyr::select(flexfile$wbs,
                                   .data$level, .data$id, .data$name, .data$parent_id, 1),
                     by = stats::setNames(c("id", .id), c("wbs_element_id", .id)),
                     suffix = c("", ".wbs")) %>%
    dplyr::left_join(dplyr::select(flexfile$functionalcategories,
                                   .data$id, .data$name, 1),
                     by = stats::setNames(c("id", .id), c("functional_category_id", .id)),
                     suffix = c("", ".functionalcategories")) %>%
    dplyr::left_join(dplyr::select(flexfile$functionaloverheadcategories,
                                   .data$id, .data$name, 1),
                     by = stats::setNames(c("id", .id), c("functional_overhead_category_id", .id)),
                     suffix = c("", ".overheadcategories")) %>%
    dplyr::left_join(dplyr::select(flexfile$reportingcalendar,
                                   .data$id, .data$start_date, .data$end_date, 1),
                     by = stats::setNames(c("id", .id), c("reporting_period_id", .id)),
                     suffix = c("", ".reportingcalendar")) %>%
    dplyr::mutate(start_date = lubridate::ymd(.data$start_date),
                  end_date = lubridate::ymd(.data$end_date),
                  atd_or_fac = "ATD") %>%
    dplyr::rename(account_name = .data$name,
                  clin_name = .data$name.clins,
                  wbs_parent = .data$parent_id,
                  wbs_name = .data$name.wbs,
                  end_item_name = .data$name.enditems,
                  order_or_lot_name = .data$name.ordersorlots,
                  wbs_level = .data$level,
                  functional_category_name = .data$name.functionalcategories,
                  functional_overhead_category_name = .data$name.overheadcategories)

}


#' @keywords internal
flatten_forecasts <- function(flexfile, .id) {

  has_fac <- function(flexfile) {
    "forecastatcompletioncosthourdata" %in% names(flexfile)
  }

  if (has_fac(flexfile)) {

    flexfile$forecastatcompletioncosthourdata %>%
      dplyr::left_join(dplyr::select(flexfile$wbs,
                                     .data$level, .data$id, .data$name, .data$parent_id, 1),
                       by = stats::setNames(c("id", .id), c("wbs_element_id", .id))) %>%
      dplyr::left_join(dplyr::select(flexfile$ordersorlots,
                                     .data$id, .data$name, 1),
                       by = stats::setNames(c("id", .id), c("order_or_lot_id", .id))) %>%
      dplyr::rename(wbs_parent = .data$parent_id,
                    wbs_name = .data$name.x,
                    wbs_level = .data$level,
                    order_or_lot_name = .data$name.y) %>%
      dplyr::mutate(atd_or_fac = "FAC",
                    order_or_lot_id = NULL)

  }

  else {

    NULL

  }

}


#' @keywords internal
flexfile_order_columns <- function(flexfile) {

  flexfile %>%
    dplyr::select(1,
                  .data$order_or_lot_id, .data$order_or_lot_name,
                  .data$clin_id, .data$clin_name,
                  .data$end_item_id, .data$end_item_name,
                  .data$wbs_element_id, .data$wbs_name, .data$wbs_parent, .data$wbs_level,
                  .data$account_id, .data$account_name,
                  .data$nonrecurring_or_recurring_id,
                  .data$functional_category_id, .data$functional_category_name,
                  .data$functional_overhead_category_id, .data$functional_overhead_category_name,
                  .data$standard_category_id,
                  .data$detailed_standard_category_id,
                  .data$reporting_period_id, .data$start_date, .data$end_date,
                  .data$allocation_method_id,
                  .data$unit_or_sublot_id,
                  .data$value_dollars,
                  .data$value_hours,
                  dplyr::everything()) #everything else isn't required by the data model

}

## ===== Flatten Quantity Data Report ----

#' Create a cost and hour dataframe from a Quantity Data Report
#'
#' @description
#'
#' \code{flatten_qdr()} creates a master dataframe for working with FlexFile quantity data.
#'
#' @export
#'
#' @name flatten_lists
#'
#' @examples
#' \dontrun{
#' # read a sample quantity report
#' file <- system.file("extdata", "Sample_Quantity_A.zip", package = "readflexfile")
#'
#' flat_flex_file <- read_ff(file) %>%
#'   add_id_col() %>%
#'   flatten_qdr()
#'}
flatten_qdr <- function(quantity_data, .id = "doc_id") {

  quant_to_date <- quantity_data$quantitiestodate %>%
    dplyr::left_join(quantity_data$ordersorlots,
                     by = stats::setNames(c("id", .id), c("order_or_lot_id", .id))) %>%
    dplyr::left_join(quantity_data$wbs,
                     by = stats::setNames(c("id", .id), c("wbs_element_id", .id))) %>%
    dplyr::left_join(dplyr::select(quantity_data$wbselementremarks, -.data$order_or_lot_id),
                     by = c("wbs_element_id", .id)) %>%
    dplyr::rename("dictionary_definition" = .data$text,
                  "order_or_lot_name" = .data$name.x,
                  "wbs_element_name" = .data$name.y) %>%
    dplyr::mutate(atd_or_fac = "ATD")

  quant_completion <- quantity_data$quantitiesatcompletion %>%
    dplyr::left_join(quantity_data$ordersorlots,
                     by = stats::setNames(c("id", .id), c("order_or_lot_id", .id))) %>%
    dplyr::left_join(quantity_data$wbs,
                     by = stats::setNames(c("id", .id), c("wbs_element_id", .id))) %>%
    dplyr::left_join(dplyr::select(quantity_data$wbselementremarks, -.data$order_or_lot_id),
                     by = c("wbs_element_id", .id)) %>%
    dplyr::left_join(quantity_data$productionsequence,
                     by = c("end_item_id", .id, "order_or_lot_id")) %>%
    dplyr::rename("dictionary_definition" = .data$text,
                  "order_or_lot_name" = .data$name.x,
                  "wbs_element_name" = .data$name.y) %>%
    dplyr::mutate(atd_or_fac = "FAC")

  dplyr::bind_rows(quant_to_date, quant_completion)
}
