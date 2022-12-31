## ===== Apply Allocations =====

#' Apply allocation methodologies provided
#'
#' \code{allocate_flexfile()} applies the allocations provided in the Allocation Methodology table to
#' the Actual Cost Hour Data table. Returns a list of tibbles from a zip folder submission of the FlexFiles.
#' Each tibble corresponds to its respective JSON table. \cr
#' \cr
#' Currently this is implemented for \code{AllocationMethodTypeID == "PERCENT"}.
#'
#' @inheritParams apply_flexfile
#'
#' @export
allocate_flexfile <- function(flexfile) {

  apply_flexfile(flexfile, allocate_flexfile_single)

}

#' @keywords internal
allocate_flexfile_single <- function(flexfile) {

  flexfile <- costmisc::assert_case(flexfile, target_case = "native")

  # set all percents to be 1 if no allocations
  if (nrow(flexfile$AllocationComponents) == 0) {
    flexfile$ActualCostHourData <- flexfile$ActualCostHourData %>%
      dplyr::mutate(PercentValue = 1)

    attr(flexfile, "allocated") <- TRUE

    return(flexfile)
  }

  # check methods
  valid_methods <- c("PERCENT")
  allocation_methods <- unique(flexfile$AllocationMethods$AllocationMethodTypeID)

  if (!(all(allocation_methods %in% valid_methods))) {
    # then some allocation method is used that we do not recognize
    warning(paste("unknown allocation method(s): "),
            paste(allocation_methods[!(allocation_methods %in% valid_methods)], collapse = ", "))
  }

  allocation_fields <- c("OrderOrLotID", "EndItemID", "WBSElementID", "UnitOrSublotID")

  coalesce_field <- function(df, field, suffix) {
    field_list <- rlang::syms(list(paste0(field, suffix), field))

    df %>%
      dplyr::mutate(!!field := dplyr::coalesce(!!!field_list))
  }

  new_actualcosthourdata <- flexfile$ActualCostHourData %>%
    dplyr::left_join(flexfile$AllocationComponents,
                     by = c("AllocationMethodID"),
                     suffix = c("", "_allocations")) %>%
    dplyr::left_join(dplyr::select(flexfile$AllocationMethods, .data$ID, .data$AllocationMethodTypeID),
                     by = c(AllocationMethodID = "ID"))

  # iterate over the function to apply it across all allocation fields
  # reduce will take the output from iteration i and use it as input to i + 1
  flexfile$ActualCostHourData <- purrr::reduce(allocation_fields, coalesce_field, suffix = "_allocations", .init = new_actualcosthourdata) %>%
    tidyr::replace_na(list(PercentValue = 1)) %>%
    #dplyr::mutate_at(dplyr::vars(tidyselect::starts_with("Value_")), ~ . * .data$PercentValue) %>% # need to handle other methods
    dplyr::mutate(Value_Dollars = Value_Dollars * .data$PercentValue,
                  Value_Hours = Value_Hours * .data$PercentValue) %>%
    dplyr::select(-(tidyselect::ends_with("_allocations")), -.data$AllocationMethodTypeID)

  # join in the remaining 'UnitOrSublotID' to fill in 'EndItemID' and 'OrderOrLotID'
  sublot_fields <- c("OrderOrLotID", "EndItemID")

  new_actualcosthourdata2 <- flexfile$ActualCostHourData %>%
    dplyr::left_join(dplyr::select(flexfile$UnitsOrSublots,
                                   UnitOrSublotID = .data$ID, .data$EndItemID, .data$OrderOrLotID),
                     by = c("UnitOrSublotID"),
                     suffix = c("", "_unitorsublot"))

  flexfile$ActualCostHourData <- purrr::reduce(sublot_fields, coalesce_field, suffix = "_unitorsublot", .init = new_actualcosthourdata2) %>%
    dplyr::select(-(tidyselect::ends_with("_unitorsublot")))

  attr(flexfile, "allocated") <- TRUE

  flexfile
}
