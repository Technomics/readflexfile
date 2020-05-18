#' Flatten FlexFile and Quantity Data Report
#'
#' @description Create separate data frames for the FlexFile and the Quantity Data Report.
#' These functions merge together many tables into a single one more useful for analysis.
#' This generally involves joining together numerous ID fields. Actuals and forecasts are also
#' stacked into a single table.
#'
#' @param flexfile A flexfile created from the \code{read_ff} function
#' @param quantity_data A quantity data report created from the \code{read_ff} function
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
#' @param flexfile A flexfile created from the \code{\link{read_ff}} function
#'
#' @examples
#' # Flatten one FlexFile
#' file <- system.file("extdata", "Sample File_FF.zip", package = "readflexfile")
#'
#' flat_flex_file <- read_ff(file) %>%
#' add_id_col(var = "doc_id") %>%
#' flatten_ff()
#'
#' #Flatten mutliple FlexFiles
#'
#' files <- system.file("extdata/multiple-flexfiles", package = "readflexfile")
#'
#' flat_flexfiles <- read_folder(files, read_ff) %>%
#' listindex_to_col(var = "doc_id") %>%
#' stack_ff() %>%
#' flatten_ff()
#'
flatten_ff <- function(flexfile, .id = "doc_id") {
  # selects all, but provides a quick safety net in case of changes
  cats <- readflexfile::sfc_mapping %>%
    dplyr::distinct(standard_category_id, standard_category, detailed_standard_category_id, direct_or_overhead)

  # function to join in the sfc category
  join_sfc <- function(the_table) {
    is_detailed <- isFALSE(all(is.na(the_table$detailed_standard_category_id)))

    if (is_detailed) {
      # remove the standard_category_id since it will get joined back in
      the_table %>%
        dplyr::select(-standard_category_id) %>%
        dplyr::left_join(cats, by = "detailed_standard_category_id")
    } else {
      the_table %>%
        dplyr::left_join(dplyr::distinct_at(cats, dplyr::vars(-detailed_standard_category_id)),
                         by = "standard_category_id")
    }
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
                                   c(program_name,
                                     contract_number,
                                     approved_plan_number,
                                     reporting_organization_organization_name,
                                     1)),
                     by = .id) %>%
    dplyr::left_join(dplyr::select(flexfile$accounts,
                                   c(id, name, 1)),
                     by = setNames(c("id", .id), c("account_id", .id)),
                     suffix = c("", ".accounts")) %>%
    dplyr::left_join(dplyr::select(flexfile$enditems,
                                   c(id, name, 1)),
                     by = setNames(c("id", .id), c("end_item_id", .id)),
                     suffix = c("", ".enditems")) %>%
    dplyr::left_join(dplyr::select(flexfile$ordersorlots,
                                   c(id, name, 1)),
                     by = setNames(c("id", .id), c("order_or_lot_id", .id)),
                     suffix = c("", ".ordersorlots")) %>%
    dplyr::left_join(dplyr::select(flexfile$clins,
                                   c(id, name, 1)),
                     by = setNames(c("id", .id), c("clin_id", .id)),
                     suffix = c("", ".clins")) %>%
    dplyr::left_join(dplyr::select(flexfile$wbs,
                                   c(level, id, name, parent_id, 1)),
                     by = setNames(c("id", .id), c("wbs_element_id", .id)),
                     suffix = c("", ".wbs")) %>%
    dplyr::left_join(dplyr::select(flexfile$functionalcategories,
                                   c(id, name, 1)),
                     by = setNames(c("id", .id), c("functional_category_id", .id)),
                     suffix = c("", ".functionalcategories")) %>%
    dplyr::left_join(dplyr::select(flexfile$functionaloverheadcategories,
                                   c(id, name, 1)),
                     by = setNames(c("id", .id), c("functional_overhead_category_id", .id)),
                     suffix = c("", ".overheadcategories")) %>%
    dplyr::left_join(dplyr::select(flexfile$reportingcalendar,
                                   c(id, start_date, end_date, 1)),
                     by = setNames(c("id", .id), c("reporting_period_id", .id)),
                     suffix = c("", ".reportingcalendar")) %>%
    dplyr::mutate(start_date = lubridate::ymd(start_date),
                  end_date = lubridate::ymd(end_date),
                  atd_or_fac = "ATD") %>%
    dplyr::rename(account_name = name,
                  clin_name = name.clins,
                  wbs_parent = parent_id,
                  wbs_name = name.wbs,
                  end_item_name = name.enditems,
                  order_or_lot_name = name..ordersorlots,
                  wbs_level = level,
                  functional_category_name = name.functionalcategories,
                  functional_overhead_category_name = name.overheadcategories) %>%
    dplyr::select(-account_id,
                  -clin_id,
                  -functional_category_id,
                  -functional_overhead_category_id,
                  -reporting_period_id,
                  -end_item_id,
                  -order_or_lot_id,
                  -account_id,
                  -order_or_lot_id)

}

#' @keywords internal
flatten_forecasts <- function(flexfile, .id) {

  has_fac <- function(flexfile) {
    "forecastatcompletioncosthourdata" %in% names(flexfile)
  }

  if (has_fac(flexfile)) {

    flexfile$forecastatcompletioncosthourdata %>%
      dplyr::left_join(dplyr::select(flexfile$wbs,
                                     c(level, id, name, parent_id, 1)),
                       by = setNames(c("id", .id), c("wbs_element_id", .id))) %>%
      dplyr::left_join(dplyr::select(flexfile$ordersorlots,
                                     c(id, name, 1)),
                       by = setNames(c("id", .id), c("order_or_lot_id", .id))) %>%
      dplyr::rename(wbs_parent = parent_id,
                    wbs_name = name.x,
                    wbs_level = level,
                    order_or_lot_name = name.y) %>%
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
                  order_or_lot_name,
                  clin_name,
                  end_item_name,
                  wbs_element_id, wbs_name, wbs_parent, wbs_level,
                  account_name,
                  nonrecurring_or_recurring_id,
                  functional_category_name,
                  functional_overhead_category_name,
                  standard_category,
                  start_date,
                  end_date,
                  value_dollars,
                  value_hours,
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
#' # read a sample quantity report
#' file <- system.file("extdata", "Sample File_Q.zip", package = "readflexfile")
#'
#' flat_flex_file <- read_ff(file) %>%
#'   add_id_col() %>%
#'   flatten_qdr()
#'
flatten_qdr <- function(quantity_data, .id = "doc_id") {

  quant_to_date <- quantity_data$quantitiestodate %>%
    dplyr::left_join(quantity_data$ordersorlots,
                     by = setNames(c("id", .id), c("order_or_lot_id", .id))) %>%
    dplyr::left_join(quantity_data$wbs,
                     by = setNames(c("id", .id), c("wbs_element_id", .id))) %>%
    dplyr::left_join(dplyr::select(quantity_data$wbselementremarks, -order_or_lot_id),
                     by = c("wbs_element_id", .id)) %>%
    dplyr::rename("dictionary_definition" = text,
                  "order_or_lot_name" = name.x,
                  "wbs_element_name" = name.y) %>%
    dplyr::mutate(atd_or_fac = "ATD")

  quant_completion <- quantity_data$quantitiesatcompletion %>%
    dplyr::left_join(quantity_data$ordersorlots,
                     by = setNames(c("id", .id), c("order_or_lot_id", .id))) %>%
    dplyr::left_join(quantity_data$wbs,
                     by = setNames(c("id", .id), c("wbs_element_id", .id))) %>%
    dplyr::left_join(dplyr::select(quantity_data$wbselementremarks, -order_or_lot_id),
                     by = c("wbs_element_id", .id)) %>%
    dplyr::left_join(quantity_data$productionsequence,
                     by = c("end_item_id", .id, "order_or_lot_id")) %>%
    dplyr::rename("dictionary_definition" = text,
                  "order_or_lot_name" = name.x,
                  "wbs_element_name" = name.y) %>%
    dplyr::mutate(atd_or_fac = "FAC")

  dplyr::bind_rows(quant_to_date, quant_completion)
}
