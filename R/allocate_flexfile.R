## ===== Apply Allocations =====

#' Apply allocation methodologies provided
#'
#' \code{allocate_flexfile()} applies the allocations provided in the Allocation Methodology table to
#' the Actual Cost Hour Data table. Returns a list of tibbles from a zip folder submission of the FlexFiles.
#' Each tibble corresponds to its respective JSON table. \cr
#' \cr
#' Currently this is implemented for \code{allocation_method_type_id == "PERCENT"}.
#'
#' @export
#'
#' @param flexfile A list of one or more FlexFiles imported through the \code{read_flexfile} function.
#'
#' @return A list of tibbles for the \code{file}.
#'
allocate_flexfile <- function(flexfile) {

  # set all percents to be 1 if no allocations
  if (nrow(flexfile$allocationcomponents) == 0) {
    flexfile$actualcosthourdata <- flexfile$actualcosthourdata %>%
      dplyr::mutate(percent_value = 1)

    attr(flexfile, "allocated") <- TRUE

    return(flexfile)
  }

  # check methods
  valid_methods <- c("PERCENT")
  allocation_methods <- unique(flexfile$allocationmethods$allocation_method_type_id)

  if (!(all(allocation_methods %in% valid_methods))) {
    # then some allocation method is used that we do not recognize
    warning(paste("unknown allocation method(s): "),
            paste(allocation_methods[!(allocation_methods %in% valid_methods)], collapse = ", "))
  }

  allocation_fields <- c("order_or_lot_id", "end_item_id", "wbs_element_id", "unit_or_sublot_id")

  coalesce_field <- function(df, field, suffix) {
    field_list <- rlang::syms(list(paste0(field, suffix), field))

    df %>%
      dplyr::mutate(!!field := dplyr::coalesce(!!!field_list))
  }

  new_actualcosthourdata <- flexfile$actualcosthourdata %>%
    dplyr::left_join(flexfile$allocationcomponents,
                     by = c("allocation_method_id"),
                     suffix = c("", "_allocations")) %>%
    dplyr::left_join(dplyr::select(flexfile$allocationmethods, .data$id, .data$allocation_method_type_id),
                     by = c(allocation_method_id = "id"))

  # iterate over the function to apply it across all allocation fields
  # reduce will take the output from iteration i and use it as input to i + 1
  flexfile$actualcosthourdata <- purrr::reduce(allocation_fields, coalesce_field, suffix = "_allocations", .init = new_actualcosthourdata) %>%
    tidyr::replace_na(list(percent_value = 1)) %>%
    dplyr::mutate_at(dplyr::vars(tidyselect::starts_with("value_")), ~ . * .data$percent_value) %>% # need to handle other methods
    dplyr::select(-(tidyselect::ends_with("_allocations")), -.data$allocation_method_type_id)

  # join in the remaining 'order_or_lot_id' to fill in 'end_item_id' and 'order_or_lot_id'
  sublot_fields <- c("order_or_lot_id", "end_item_id")

  new_actualcosthourdata2 <- flexfile$actualcosthourdata %>%
    dplyr::left_join(dplyr::select(flexfile$unitsorsublots,
                                   unit_or_sublot_id = .data$id, .data$end_item_id, .data$order_or_lot_id),
                     by = c("unit_or_sublot_id"),
                     suffix = c("", "_unitorsublot"))

  flexfile$actualcosthourdata <- purrr::reduce(sublot_fields, coalesce_field, suffix = "_unitorsublot", .init = new_actualcosthourdata2) %>%
    dplyr::select(-(tidyselect::ends_with("_unitorsublot")))

  attr(flexfile, "allocated") <- TRUE

  flexfile
}
