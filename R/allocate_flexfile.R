## ===== Apply Allocations =====

#' Apply allocation methodologies provided
#'
#' \code{allocate_flexfile()} applies the allocations provided in the Allocation Methodology table to
#' the Actual Cost Hour Data table. Returns a list of tibbles from a zip folder submission of the FlexFiles.
#' Each tibble corresponds to its respective JSON table. \cr
#' \cr
#' Currently this is implemented for \code{AllocationMethodTypeID == "PERCENT"} and \code{AllocationMethodTypeID == "PRORATE"}.
#'
#' @inheritParams apply_flexfile
#'
#' @export
allocate_flexfile <- function(flexfile) {

  apply_flexfile(flexfile, allocate_flexfile_single)

}

#' @keywords internal
allocate_flexfile_single <- function(flexfile) {

  .flexfile <- costmisc::assert_case(flexfile, target_case = "native")

  # set all percents to be 1 if no allocations
  if (nrow(.flexfile$AllocationComponents) == 0) {
    .flexfile$ActualCostHourData <- .flexfile$ActualCostHourData %>%
      dplyr::mutate(PercentValue = 1)

    attr(.flexfile, "allocated") <- TRUE

    return(.flexfile)
  }

  # join in EndItemID and OrderOrLotID to the Actual Cost-Hour Data table from the UnitsorSublots table
  .flexfile <- .flexfile %>%
    normalize_units_or_sublots()

  # combine the two allocation tables from the FlexFile into a single flat table
  combined_allocation_table <- .flexfile$AllocationComponents %>%
    dplyr::left_join(
      .flexfile$AllocationMethods %>%
        dplyr::select("ID", "AllocationMethodTypeID"),
      by = c("AllocationMethodID" = "ID")
    )

  ## General algorithm
  # Allocations are applied with the same algorithm regardless of Methodology
  # The hard part is to determine the percentage values to split the records
  # This percentage is derived from one of three ways:
  #
  #   1) PERCENT Method: The allocation is provided to you
  #   2) PRORATE Method: Aggregate over ActualsToDate and calculate historical percentages
  #   3) PRORATE Method: No historical data exists, apply a uniform distribution

  ## PERCENT METHOD
  # Derivation method #1
  # isolate percent allocation components
  method_percent_allocation_components_1 <- combined_allocation_table %>%
    dplyr::filter(AllocationMethodTypeID == "PERCENT") %>%
    dplyr::mutate(
      DollarPercentValue = PercentValue,
      HourPercentValue = PercentValue,
      ProrateBucket = "PERCENT",
      ReportingPeriodID = NULL
    ) %>%
    dplyr::select(-"AllocationMethodTypeID", -"PercentValue") %>%
    tidyr::crossing(ReportingPeriodID = unique(.flexfile$ActualCostHourData$ReportingPeriodID))

  ## PRORATE METHOD
  # create prorate bucket mapping table from the standard category mapping table to calculate percent allocations by labor category
  prorate_bucket_mapping <- sfc_mapping %>%
    dplyr::mutate(
      ProrateBucket = dplyr::case_when(
        functional_category %in% c("Engineering","Maintenance","Manufacturing") ~ "Labor",
        functional_category == "Materials" ~ "Material",
        functional_category == "FCCM" ~ "FCCM",
        functional_category == "GA" ~ "GA",
        functional_category == "Other" ~ "Other",
        TRUE ~ NA_character_)
    ) %>%
    dplyr::select(
      "join_category" = "DetailedStandardCategoryID",
      "ProrateBucket"
    ) %>%
    dplyr::bind_rows(
      tibble::tibble(
        join_category = c("OTHER_DIRECT_COSTS","DIRECT_MATERIALS"),
        ProrateBucket = c("Other","Material")
      )
    )

  # isolate prorated allocation components
  prorate_allocation_components <- combined_allocation_table %>%
    dplyr::filter(AllocationMethodTypeID == "PRORATE") %>%
    # bring in OrderOrLotID and EndItemID where there are UnitsofSublots
    dplyr::left_join(
      dplyr::select(
        .flexfile$UnitsOrSublots,
        "ID", "OrderOrLotID", "EndItemID"
      ),
      by = c("UnitOrSublotID" = "ID"),
      suffix = c("", ".unitsorsublots")
    ) %>%
    dplyr::mutate(
      OrderOrLotID = dplyr::coalesce(OrderOrLotID, OrderOrLotID.unitsorsublots),
      EndItemID = dplyr::coalesce(EndItemID, EndItemID.unitsorsublots)
    ) %>%
    dplyr::select("AllocationMethodID", "OrderOrLotID", "EndItemID", "WBSElementID", "UnitOrSublotID")

  # create Actual Cost-Hour Data table with prorate bucket column to reference later
  atd_table_with_proratebucket <- .flexfile$ActualCostHourData %>%
    dplyr::mutate(
      join_category = dplyr::coalesce(.data$DetailedStandardCategoryID, .data$StandardCategoryID)
    ) %>%
    dplyr::left_join(prorate_bucket_mapping, by = "join_category") %>%
    dplyr::select(-"join_category")

  # create lookup dataframe with prorated allocation rows grouped
  prorate_allocation_lookup_rows <- atd_table_with_proratebucket %>%
    dplyr::inner_join(
      prorate_allocation_components,
      by = c("AllocationMethodID"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      OrderOrLotID = dplyr::coalesce(OrderOrLotID.x, OrderOrLotID.y),
      EndItemID = dplyr::coalesce(EndItemID.x, EndItemID.y),
      WBSElementID = dplyr::coalesce(WBSElementID.x, WBSElementID.y),
      UnitOrSublotID = dplyr::coalesce(UnitOrSublotID.x, UnitOrSublotID.y)
    ) %>%
    dplyr::select(
      -"OrderOrLotID.x", -"OrderOrLotID.y", -"EndItemID.x", -"EndItemID.y",
      -"WBSElementID.x", -"WBSElementID.y", -"UnitOrSublotID.x", -"UnitOrSublotID.y"
    ) %>%
    dplyr::distinct(OrderOrLotID, EndItemID, WBSElementID, UnitOrSublotID, ReportingPeriodID, ProrateBucket, AllocationMethodID) %>%
    dplyr::mutate(
      allocation_group = dplyr::dense_rank(
        paste(AllocationMethodID, ReportingPeriodID, ProrateBucket, sep = "_")
      )
    )

  # Derivation method #2
  # calculate even split percentage allocations for rows that must be prorated but do not yet exist in the data
  method_prorate_allocation_components_3 <- prorate_allocation_lookup_rows %>%
    dplyr::anti_join(
      atd_table_with_proratebucket,
      by = c("OrderOrLotID", "EndItemID", "WBSElementID", "UnitOrSublotID", "ReportingPeriodID", "ProrateBucket")
    ) %>%
    dplyr::group_by(allocation_group) %>%
    dplyr::mutate(
      DollarPercentValue = 1 / dplyr::n(),
      HourPercentValue   = DollarPercentValue
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"allocation_group")

  # Derivation method #3
  # calculate allocation percentages for prorate records down to the reporting period level by labor category
  method_prorate_allocation_components_2 <- atd_table_with_proratebucket %>%
    dplyr::semi_join(
      prorate_allocation_components,
      by = c("OrderOrLotID", "EndItemID", "WBSElementID")
    ) %>%
    dplyr::group_by(OrderOrLotID, EndItemID, WBSElementID, UnitOrSublotID, ReportingPeriodID, ProrateBucket) %>%
    dplyr::summarise(
      TotalValueDollars = sum(Value_Dollars, na.rm = TRUE),
      TotalValueHours = sum(Value_Hours, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::group_by(UnitOrSublotID, ReportingPeriodID, ProrateBucket) %>%
    dplyr::mutate(
      DenomDollars = sum(TotalValueDollars, na.rm = TRUE),
      DenomHours = sum(TotalValueHours, na.rm = TRUE),
      DollarPercentValue = dplyr::if_else(DenomDollars == 0, 0, TotalValueDollars / DenomDollars),
      HourPercentValue = dplyr::if_else(DenomHours == 0, 0, TotalValueHours / DenomHours)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"TotalValueDollars", -"TotalValueHours", -"DenomDollars", -"DenomHours") %>%
    dplyr::left_join(
      prorate_allocation_components,
      by = c("OrderOrLotID", "EndItemID", "WBSElementID", "UnitOrSublotID"),
      relationship = "many-to-many"
    )

  ## Combine derived percentages into a single set
  combined_allocation_components <- dplyr::bind_rows(
    method_percent_allocation_components_1,   # method 1
    method_prorate_allocation_components_2,   # method 2
    method_prorate_allocation_components_3    # method 3
  )

  # join the allocation component percentages and allocation method id to the actual cost hour data table
  new_actualcosthourdata <- atd_table_with_proratebucket %>%
    dplyr::left_join(
      dplyr::select(
        .flexfile$AllocationMethods,
        "ID", "AllocationMethodTypeID",
      ),
      by = c("AllocationMethodID" = "ID")
    ) %>%
    dplyr::mutate(
      ProrateBucket = dplyr::if_else(
        AllocationMethodTypeID == "PERCENT",
        "PERCENT",
        ProrateBucket)
    ) %>%
    dplyr::left_join(
      combined_allocation_components,
      by = c("AllocationMethodID", "ProrateBucket", "ReportingPeriodID"),
      suffix = c("", "_allocations")
    )

  # perform the allocations
  .flexfile$ActualCostHourData <- new_actualcosthourdata %>%
    dplyr::mutate(
      OrderOrLotID = dplyr::coalesce(OrderOrLotID_allocations, OrderOrLotID),
      EndItemID = dplyr::coalesce(EndItemID_allocations, EndItemID),
      WBSElementID = dplyr::coalesce(WBSElementID_allocations, WBSElementID),
      UnitOrSublotID = dplyr::coalesce(UnitOrSublotID_allocations, UnitOrSublotID)
    ) %>%
    tidyr::replace_na(list(PercentValue = 1)) %>%
    dplyr::mutate(
      DollarPercentValue = dplyr::if_else(is.na(DollarPercentValue), 1, DollarPercentValue),
      HourPercentValue = dplyr::if_else(is.na(HourPercentValue), 1, HourPercentValue),
      Value_Dollars = Value_Dollars * DollarPercentValue,
      Value_Hours = Value_Hours * HourPercentValue) %>%
    dplyr::select(
      -tidyselect::ends_with("_allocations"),
      -"AllocationMethodTypeID",
      -"ProrateBucket",
      -"DollarPercentValue",
      -"HourPercentValue")

  attr(.flexfile, "allocated") <- TRUE

  .flexfile
}
