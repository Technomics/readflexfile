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

  flexfile <- costmisc::assert_case(flexfile, target_case = "native")

  # set all percents to be 1 if no allocations
  if (nrow(flexfile$AllocationComponents) == 0) {
    flexfile$ActualCostHourData <- flexfile$ActualCostHourData %>%
      dplyr::mutate(PercentValue = 1)

    attr(flexfile, "allocated") <- TRUE

    return(flexfile)
  }

  allocation_fields <- c("OrderOrLotID", "EndItemID", "WBSElementID", "UnitOrSublotID")

  coalesce_field <- function(df, field, suffix) {
    field_list <- rlang::syms(list(paste0(field, suffix), field))

    df %>%
      dplyr::mutate(!!field := dplyr::coalesce(!!!field_list))
  }

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
    dplyr::select("join_category" = "DetailedStandardCategoryID","ProrateBucket") %>%
    dplyr::bind_rows(
      tibble::tibble(
        join_category = c("OTHER_DIRECT_COSTS","DIRECT_MATERIALS"),
        ProrateBucket = c("Other","Material")
      )
    )

  # combine allocation tables
  combined_allocation_table <- flexfile$AllocationComponents %>%
    dplyr::left_join(
      flexfile$AllocationMethods %>%
        dplyr::select("ID", "AllocationMethodTypeID"),
      by = c("AllocationMethodID" = "ID")
    )

  ## PERCENT METHOD
  # isolate percent allocation components
  percent_allocation_components <- combined_allocation_table %>%
    dplyr::filter(AllocationMethodTypeID == "PERCENT") %>%
    dplyr::mutate(
      DollarPercentValue = PercentValue,
      HourPercentValue = PercentValue,
      ProrateBucket = "PERCENT",
      ReportingPeriodID = NULL
    ) %>%
    dplyr::select(-"AllocationMethodTypeID", -"PercentValue") %>%
    tidyr::crossing(ReportingPeriodID = unique(flexfile$ActualCostHourData$ReportingPeriodID))

  ## PRORATE METHOD
  # isolate prorated allocation components
  prorate_allocation_components <- combined_allocation_table %>%
    dplyr::filter(AllocationMethodTypeID == "PRORATE") %>%
    dplyr::left_join(
      flexfile$UnitsOrSublots %>%
        dplyr::select("ID", "OrderOrLotID", "EndItemID"),
      by = c("UnitOrSublotID" = "ID")
    ) %>%
    dplyr::mutate(
      OrderOrLotID = dplyr::coalesce(OrderOrLotID.x, OrderOrLotID.y),
      EndItemID = dplyr::coalesce(EndItemID.x, EndItemID.y)
    ) %>%
    dplyr::select("AllocationMethodID", "OrderOrLotID", "EndItemID", "WBSElementID", "UnitOrSublotID", "PercentValue")

  # join in EndItemID and OrderOrLotID to the Actual Cost-Hour Data table from the Units or Sublots table
  flexfile$ActualCostHourData <- flexfile$ActualCostHourData %>%
    normalize_units_or_sublots()

  # create Actual Cost-Hour Data table with prorate bucket column to reference later
  atd_table_with_proratebucket <- flexfile$ActualCostHourData %>%
    dplyr::mutate(
      join_category = dplyr::coalesce(.data$DetailedStandardCategoryID, .data$StandardCategoryID)
    ) %>%
    dplyr::left_join(prorate_bucket_mapping, by = "join_category") %>%
    dplyr::select(-"join_category")

  # create lookup dataframe with prorated allocation rows grouped
  prorate_allocation_lookup_rows <- atd_table_with_proratebucket %>%
    dplyr::filter(.data$AllocationMethodID %in% prorate_allocation_components$AllocationMethodID) %>%
    dplyr::select(
      "OrderOrLotID", "EndItemID", "WBSElementID", "UnitOrSublotID",
      "ReportingPeriodID", "AllocationMethodID", "ProrateBucket"
    ) %>%
    dplyr::left_join(prorate_allocation_components, by = "AllocationMethodID") %>%
    dplyr::mutate(
      OrderOrLotID = dplyr::coalesce(OrderOrLotID.x, OrderOrLotID.y),
      EndItemID = dplyr::coalesce(EndItemID.x, EndItemID.y),
      WBSElementID = dplyr::coalesce(WBSElementID.x, WBSElementID.y),
      UnitOrSublotID = dplyr::coalesce(UnitOrSublotID.x, UnitOrSublotID.y)
    ) %>%
    dplyr::select(
      -"OrderOrLotID.x", -"OrderOrLotID.y", -"EndItemID.x", -"EndItemID.y",
      -"WBSElementID.x", -"WBSElementID.y", -"UnitOrSublotID.x", -"UnitOrSublotID.y", -"PercentValue"
    ) %>%
    dplyr::distinct(OrderOrLotID,EndItemID,WBSElementID,UnitOrSublotID,ReportingPeriodID,ProrateBucket, AllocationMethodID) %>%
    dplyr::mutate(
      allocation_group = dplyr::dense_rank(paste(AllocationMethodID, ReportingPeriodID, ProrateBucket, sep = "_"))
    )

 # calculate even split percentage allocations for rows that must be prorated but do not already exist in the data
  prorate_split_percentage_components <- prorate_allocation_lookup_rows %>%
    dplyr::group_by(allocation_group) %>%
    dplyr::mutate(n_group = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::anti_join(
      atd_table_with_proratebucket %>%
        dplyr::select("OrderOrLotID","EndItemID","WBSElementID","UnitOrSublotID","ReportingPeriodID","ProrateBucket"),
      by = c("OrderOrLotID","EndItemID","WBSElementID","UnitOrSublotID","ReportingPeriodID","ProrateBucket")
    ) %>%
    dplyr::group_by(allocation_group) %>%
    dplyr::filter(dplyr::n() == n_group) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      DollarPercentValue = 1 / n_group,
      HourPercentValue   = 1 / n_group
    ) %>%
    dplyr::select(-"n_group", -"AllocationMethodID", -"allocation_group")

  # calculate allocation percentages for prorate records down to the reporting period level by labor category
  prorate_allocation_percentages <- atd_table_with_proratebucket %>%
    dplyr::semi_join(
      prorate_allocation_components %>%
        dplyr::select("OrderOrLotID", "EndItemID", "WBSElementID"),
      by = c("OrderOrLotID", "EndItemID", "WBSElementID")) %>%
    dplyr::group_by(OrderOrLotID, EndItemID, WBSElementID, UnitOrSublotID, ReportingPeriodID, ProrateBucket) %>%
    dplyr::summarise(
      TotalValueDollars = sum(Value_Dollars, na.rm = TRUE),
      TotalValueHours = sum(Value_Hours, na.rm = TRUE),
      .groups = "drop") %>%
    dplyr::group_by(UnitOrSublotID, ReportingPeriodID, ProrateBucket) %>%
    dplyr::mutate(
      DenomDollars = sum(TotalValueDollars, na.rm = TRUE),
      DenomHours = sum(TotalValueHours, na.rm = TRUE),
      DollarPercentValue = dplyr::if_else(DenomDollars == 0, 0, TotalValueDollars / DenomDollars),
      HourPercentValue = dplyr::if_else(DenomHours == 0, 0, TotalValueHours / DenomHours)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"TotalValueDollars", -"TotalValueHours", -"DenomDollars", -"DenomHours") %>%
    dplyr::bind_rows(prorate_split_percentage_components)

  ## Combine back to single component set
  # stack prorate and percentage allocation components
  combined_allocation_components <- prorate_allocation_percentages %>%
    dplyr::left_join(
      prorate_allocation_components %>%
        dplyr::select(-"PercentValue"),
      by = c("OrderOrLotID", "EndItemID", "WBSElementID", "UnitOrSublotID")) %>%
    dplyr::bind_rows(percent_allocation_components) #%>%
    # dplyr::mutate(
    #   ProrateBucket = dplyr::if_else(is.na(ProrateBucket), "PERCENT", ProrateBucket)
    # )

  # # temporarily split out prorate allocations
  # temporary_prorate_only <- combined_allocation_components %>%
  #   dplyr::filter(ProrateBucket == "PRORATE")

  # expand percent components to all reporting periods since percent components are reporting id agnostic
  # temporary_percent_expanded <- combined_allocation_components %>%
  #   dplyr::filter(ProrateBucket == "PERCENT") %>%
  #   dplyr::mutate(ReportingPeriodID = NULL) %>%
  #   tidyr::crossing(ReportingPeriodID = unique(flexfile$ActualCostHourData$ReportingPeriodID))

  # stack final result to be able to join on reporting period ID for both allocation methods
  # combined_allocation_components_expanded <- dplyr::bind_rows(temporary_prorate_only, temporary_percent_expanded)

  # join the allocation component percentages and allocation method id to the actual cost hour data table
  new_actualcosthourdata <- atd_table_with_proratebucket %>%
    dplyr::mutate(
      ProrateBucket = dplyr::if_else(
        AllocationMethodID %in% percent_allocation_components$AllocationMethodID,
        "PERCENT",
        ProrateBucket)
    ) %>%
    dplyr::left_join(
      combined_allocation_components,
      by = c("AllocationMethodID", "ProrateBucket", "ReportingPeriodID"),
      suffix = c("", "_allocations")
    ) %>%
    dplyr::left_join(
      flexfile$AllocationMethods %>%
        dplyr::filter(AllocationMethodTypeID == "PERCENT") %>%
        dplyr::select("ID", "AllocationMethodTypeID"),
      by = c("AllocationMethodID" = "ID")
    )

  # perform the allocations
  # iterate over the function to apply it across all allocation fields
  # reduce will take the output from iteration i and use it as input to i + 1
  flexfile$ActualCostHourData <- purrr::reduce(
      allocation_fields,
      coalesce_field,
      suffix = "_allocations",
      .init = new_actualcosthourdata
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

  attr(flexfile, "allocated") <- TRUE

  flexfile
}
