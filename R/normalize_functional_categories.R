
#' Join in standard functional categories
#'
#' \code{normalize_functional_categories()} joins in the standard functional categories
#' from the detailed function categories when not provided in the original data. Input
#' should a single FlexFile imported through the \code{read_flexfile} function.
#'
#' @param x A flexfile imported through the \code{read_flexfile} function.
#' @param direct_or_oh_mapping Logical whether not not to join in the direct or overhead
#' attributes.
#'
#' @export
normalize_functional_categories <- function(x, direct_or_oh_mapping = FALSE) {

  # selects all, but provides a quick safety net in case of changes
  cats <- readflexfile::sfc_mapping %>%
    dplyr::distinct(.data$standard_category_id, .data$detailed_standard_category_id, .data$direct_or_overhead) %>%
    dplyr::mutate(detailed_standard_category_id = as.character(.data$detailed_standard_category_id))

  dir_oh <- readflexfile::sfc_mapping %>%
    dplyr::distinct(.data$standard_category_id, .data$direct_or_overhead)

  # function to join in the sfc category
  join_sfc <- function(the_table) {
    the_table_dfc <- the_table %>%
      dplyr::mutate(detailed_standard_category_id = as.character(.data$detailed_standard_category_id)) %>%
      dplyr::left_join(cats, by = "detailed_standard_category_id", suffix = c("", "_sfc")) %>%
      dplyr::mutate(standard_category_id = dplyr::coalesce(.data$standard_category_id,
                                                           .data$standard_category_id_sfc)) %>%
      dplyr::select(-.data$direct_or_overhead, -.data$standard_category_id_sfc)

    if (direct_or_oh_mapping) {
      the_table_dfc%>%
        dplyr::left_join(dir_oh, by = "standard_category_id")
    } else {
      the_table_dfc
    }
  }

  join_sfc_tables <- c("actualcosthourdata", "forecastatcompletioncosthourdata")

  # for the tables where relevant, join in the sfc mappings
  purrr::modify_at(x, join_sfc_tables, join_sfc)

}
