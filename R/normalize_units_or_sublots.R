
#' Join in EndItemID and OrderOrLotID for actuals data from the UnitsOrSublots table
#'
#' \code{normalize_units_or_sublots()} joins in the EndItemID and OrderOrLotID from
#' the UnitsOrSublots table when not provided in the original data. Input should
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
    purrr::map_at("ActualCostHourData", ~dplyr::left_join(.x, flexfile$UnitsOrSublots,
                                                          by = c(UnitOrSublotID = "ID"),
                                                          suffix = c("", ".unitsorsublots"))) %>%
    purrr::map_at("ActualCostHourData", ~
                    dplyr::mutate(.x, OrderOrLotID = dplyr::coalesce(.data$OrderOrLotID,
                                                                     .data$OrderOrLotID.unitsorsublots),
                                  EndItemID = dplyr::coalesce(.data$EndItemID,
                                                              .data$EndItemID.unitsorsublots))) %>%
    purrr::map_at("ActualCostHourData", ~
                    dplyr::select(.x, -.data$FirstUnitNumber, -.data$LastUnitNumber)) %>%
    purrr::map_at("ActualCostHourData", ~dplyr::select(.x, !tidyselect::contains(".")))

}




