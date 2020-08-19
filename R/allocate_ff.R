## ===== Apply Allocations =====

#' Apply allocation methodologies provided
#'
#' @description
#'
#' \code{allocate_ff()} applies the allocations provided in the Allocation Methodology table to
#' the Actual Cost Hour Data table. Returns a list of tibbles from a zip folder submission of the FlexFiles.
#' Each tibble corresponds to its respective JSON table. \cr
#' \cr
#' Currently this is implemented for \code{allocation_method_type_id == "PERCENT"}.
#'
#' @export
#'
#' @param flexfile A list of one or multiple FlexFiles imported through the \code{read_ff()} and \code{read_folder()} functions.
#' @param .silent Logical whether to print information to the console about the allocation or not.
#' @inheritParams flatten_lists
#' @return A list of tibbles for the \code{file}.
#'
#' @examples
#'
#'#Work in progress
allocate_ff <- function(flexfile, .id = "doc_id", .silent = FALSE) {

  # return the input if the allocation table is not found
  if (is.null(flexfile$allocationcomponents)) {
    if (!(.silent) & interactive()) message("no allocation table available, returning input")
    return(flexfile)
  }

  # check methods
  valid_methods <- c("PERCENT")
  allocation_methods <- flexfile$allocationmethods %>%
    dplyr::distinct(.data$allocation_method_type_id) %>%
    dplyr::pull()

  allocation_methods[!(allocation_methods %in% valid_methods)]

  if (!(all(allocation_methods %in% valid_methods))) {
    # then some allocation method is used that we do not recognize
    if (!(.silent)) warning(paste("unknown allocation method(s): "),
                            paste(allocation_methods[!(allocation_methods %in% valid_methods)], collapse = ", "))
  }

  allocation_fields <- c("order_or_lot_id", "end_item_id", "wbs_element_id", "unit_or_sublot_id")

  coalesce_field <- function(df, field) {
    field_list <- rlang::syms(list(paste0(field, "_allocations"), field))

    df %>%
      dplyr::mutate(!!field := dplyr::coalesce(!!!field_list))
  }

  new_actualcosthourdata <- flexfile$actualcosthourdata %>%
    dplyr::left_join(flexfile$allocationcomponents,
                     by = c("allocation_method_id", .id),
                     suffix = c("", "_allocations")) %>%
    dplyr::left_join(dplyr::select(flexfile$allocationmethods, !!.id, .data$id, .data$allocation_method_type_id),
                     by = c(allocation_method_id = "id", .id))

  # iterate over the function to apply it across all allocation fields
  # reduce will take the output from iteration i and use it as input to i + 1
  flexfile$actualcosthourdata <- purrr::reduce(allocation_fields, coalesce_field, .init = new_actualcosthourdata) %>%
    tidyr::replace_na(list(percent_value = 1)) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("value_")), ~ . * .data$percent_value) %>% # need to handle other methods
    dplyr::select(-(dplyr::ends_with("_allocations")), -.data$allocation_method_type_id)

  flexfile
}
