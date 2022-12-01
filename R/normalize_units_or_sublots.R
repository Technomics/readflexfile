
#' Join in end_item and order_or_lot id for actuals data from the unitsorsublots table
#'
#' \code{normalize_units_or_sublots()} joins in the end_item and order_or_lot_id from
#' the unitsorsublots table when not provided in the original data. Input should
#' be one or more FlexFiles imported through the \code{read_flexfile} function.
#'
#' @inheritParams apply_flexfile
#'
#' @export
normalize_units_or_sublots <- function(flexfile) {

  apply_flexfile(flexfile, normalize_units_or_sublots_single)

}

#' @keywords internal
normalize_units_or_sublots_single <- function(flexfile) {

  flexfile %>%
    purrr::map_at("actualcosthourdata", ~dplyr::left_join(.x, flexfile$unitsorsublots,
                   by = c(unit_or_sublot_id = "id"),
                   suffix = c("", ".unitsorsublots"))) %>%
    purrr::map_at("actualcosthourdata", ~
                    dplyr::mutate(.x, order_or_lot_id = dplyr::coalesce(.data$order_or_lot_id,
                                                                       .data$order_or_lot_id.unitsorsublots),
                                  end_item_id = dplyr::coalesce(.data$end_item_id,
                                                                .data$end_item_id.unitsorsublots))) %>%
    purrr::map_at("actualcosthourdata", ~
                    dplyr::select(.x, -.data$first_unit_number, -.data$last_unit_number)) %>%
    purrr::map_at("actualcosthourdata", ~dplyr::select(.x, !tidyselect::contains(".")))

}




