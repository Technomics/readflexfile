## ===== Apply Allocations =====

#' Apply allocation methodologies provided
#'
#' @description
#'
#' \code{allocate_ff()} applies the allocations provided in the Allocation Methodology table to
#' the Actual Coust Hour Data table. Returns a list of tibbles from a zip folder submission of the FlexFiles.
#' Each tibble corresponds to its respective JSON table.
#'
#'
#' @export
#'
#' @param flexfile A list of one or multiple FlexFiles imported through the \code{read_ff()} and \code{read_folder()} functions.
#' @return A list of tibbles for the \code{file}.
#'
#' @examples
#'
#'#Work in progress

allocate_ff <- function(flexfile) {

  flexfile$actualcosthourdata <- left_join(flexfile$actualcosthourdata, flexfile$allocationcomponents,
                                           by = c("allocation_method_id",
                                                  names(flexfile$actualcosthourdata)[1]),
                                           suffix = c("", ".allocations")) %>%
    try(mutate(order_or_lot_id =
                 case_when(
                   is.na(order_or_lot_id) ~ order_or_lot_id.allocations,
                   TRUE ~ order_or_lot_id))) %>%
    try(mutate(end_item_id =
                 case_when(
                   is.na(end_item_id) ~ end_item_id.allocations,
                   TRUE ~ end_item_id))) %>%
    try(mutate(wbs_element_id =
                 case_when(
                   is.na(wbs_element_id) ~ wbs_element_id.allocations,
                   TRUE ~ wbs_element_id))) %>%
    try(mutate(wbs_element_id =
                 case_when(
                   is.na(unit_or_sublot_id) ~ unit_or_sublot_id.allocations,
                   TRUE ~ unit_or_sublot_id))) %>%
    mutate(value_dollars =
             case_when(!is.na(allocation_method_id) ~ value_dollars*percent_value,
                       TRUE ~ value_dollars),
           value_hours =
             case_when(!is.na(allocation_method_id) ~ value_hours*percent_value,
                       TRUE ~ value_hours))

  return(flexfile)

}
