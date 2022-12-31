
#' Join in standard functional categories
#'
#' \code{normalize_functional_categories()} joins in the standard functional categories
#' from the detailed function categories when not provided in the original data. Input
#' should be one or more FlexFiles imported through the \code{read_flexfile} function.
#'
#' @inheritParams apply_flexfile
#' @param direct_or_oh_mapping Logical whether not not to join in the direct or overhead.
#' attributes.
#'
#' @export
normalize_functional_categories <- function(flexfile, direct_or_oh_mapping = FALSE) {

  apply_flexfile(flexfile, normalize_functional_categories_single, direct_or_oh_mapping = direct_or_oh_mapping)

}

#' @keywords internal
normalize_functional_categories_single <- function(flexfile, direct_or_oh_mapping = FALSE) {

  # selects all, but provides a quick safety net in case of changes
  cats <- readflexfile::sfc_mapping %>%
    dplyr::distinct(.data$StandardCategoryID, .data$DetailedStandardCategoryID, .data$direct_or_overhead) %>%
    dplyr::mutate(DetailedStandardCategoryID = as.character(.data$DetailedStandardCategoryID))

  dir_oh <- readflexfile::sfc_mapping %>%
    dplyr::distinct(.data$StandardCategoryID, .data$direct_or_overhead)

  # function to join in the sfc category
  join_sfc <- function(the_table) {
    the_table_dfc <- the_table %>%
      dplyr::mutate(DetailedStandardCategoryID = as.character(.data$DetailedStandardCategoryID)) %>%
      dplyr::left_join(cats, by = "DetailedStandardCategoryID", suffix = c("", "_sfc")) %>%
      dplyr::mutate(StandardCategoryID = dplyr::coalesce(.data$StandardCategoryID,
                                                           .data$StandardCategoryID_sfc)) %>%
      dplyr::select(-.data$direct_or_overhead, -.data$StandardCategoryID_sfc)

    if (direct_or_oh_mapping) {
      the_table_dfc%>%
        dplyr::left_join(dir_oh, by = "StandardCategoryID")
    } else {
      the_table_dfc
    }
  }

  join_sfc_tables <- c("ActualCostHourData", "ForecastAtCompletionCostHourData")

  # for the tables where relevant, join in the sfc mappings
  purrr::modify_at(flexfile, join_sfc_tables, join_sfc)

}
