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

  #load in labor bucket mapping table as a dataframe
  prorate_bucket_lookup <- tibble::tribble(
    ~join_category, ~ProrateBucket,
    "DIRECT_ENGINEERING_LABOR", "Labor",
    "ENGINEERING_LABOR_OVERHEAD", "Labor",
    "DIRECT_MANUFACTURING_TOUCH_LABOR", "Labor",
    "DIRECT_MANUFACTURING_OTHER_LABOR", "Labor",
    "DIRECT_MANUFACTURING_TOOLING_LABOR", "Labor",
    "DIRECT_MANUFACTURING_SUPPORT_LABOR", "Labor",
    "MANUFACTURING_OPERATIONS_LABOR_OVERHEAD", "Labor",
    "DIRECT_MAINTENANCE_OTHER_LABOR", "Labor",
    "DIRECT_MAINTENANCE_SUPPORT_LABOR", "Labor",
    "DIRECT_MAINTENANCE_TOUCH_LABOR", "Labor",
    "MAINTENANCE_OPERATIONS_LABOR_OVERHEAD", "Labor",
    "OTHER_DIRECT_COSTS", "Other",
    "DIRECT_PROGRAM_MANAGEMENT_LABOR", "Other",
    "DIRECT_OTHER_LABOR", "Other",
    "DIRECT_SERVICES", "Other",
    "OTHER_DIRECT_NON_LABOR", "Other",
    "OTHER_OVERHEAD", "Other",
    "DIRECT_MATERIALS", "Material",
    "DIRECT_REPORTING_SUBCONTRACTOR", "Material",
    "INTERCOMPANY_WORK_ORDERS", "Material",
    "PURCHASED_PARTS", "Material",
    "PURCHASED_EQUIPMENT", "Material",
    "RAW_MATERIALS", "Material",
    "DIRECT_TOOLING_AND_EQUIPMENT", "Material",
    "OTHER_MATERIAL", "Material",
    "MATERIAL_OVERHEAD", "Material",
    "GENERAL_AND_ADMINISTRATIVE", "GA",
    "FACILITIES_CAPITAL_COST_OF_MONEY", "FCCM")


  #isolate percent allocation components
  percent_allocation_components <- flexfile$AllocationComponents %>%
    dplyr::left_join(flexfile$AllocationMethods %>% dplyr::select("ID", "AllocationMethodTypeID"),
      by = c("AllocationMethodID" = "ID")) %>%
    dplyr::filter(AllocationMethodTypeID == "PERCENT") %>%
    dplyr::mutate(DollarPercentValue = PercentValue, HourPercentValue = PercentValue) %>%
    dplyr::select(-"AllocationMethodTypeID", -"PercentValue")

  #isolate prorated allocation components
  prorate_allocation_components <- flexfile$AllocationComponents %>%
    dplyr::left_join(flexfile$AllocationMethods %>% dplyr::select("ID", "AllocationMethodTypeID"), by = c("AllocationMethodID" = "ID")) %>%
    dplyr::filter(AllocationMethodTypeID == "PRORATE") %>%
    dplyr::left_join(flexfile$UnitsOrSublots %>% dplyr::select("ID", "OrderOrLotID", "EndItemID"), by = c("UnitOrSublotID" = "ID")) %>%
    dplyr::mutate(OrderOrLotID = dplyr::coalesce(OrderOrLotID.x, OrderOrLotID.y), EndItemID = dplyr::coalesce(EndItemID.x, EndItemID.y)) %>%
    dplyr::select("AllocationMethodID", "OrderOrLotID", "EndItemID", "WBSElementID", "UnitOrSublotID", "PercentValue")

  #Join in EndItemID and OrderOrLotID for actuals data from the UnitsOrSublots table

  flexfile$ActualCostHourData <- flexfile$ActualCostHourData %>%
    dplyr::left_join(flexfile$UnitsOrSublots %>% dplyr::select("ID", "OrderOrLotID", "EndItemID"), by = c("UnitOrSublotID" = "ID")) %>%
    dplyr::mutate(
      OrderOrLotID = dplyr::coalesce(OrderOrLotID.x, OrderOrLotID.y),
      EndItemID = dplyr::coalesce(EndItemID.x, EndItemID.y)
    ) %>%
    dplyr::select(-"OrderOrLotID.x", -"OrderOrLotID.y", -"EndItemID.x", -"EndItemID.y")

  #calculate allocation percentages for prorate records down to the reporting period level by labor category
  prorate_allocation_percentages <- flexfile$ActualCostHourData %>%
    dplyr::semi_join(
      prorate_allocation_components %>% dplyr::select("OrderOrLotID", "EndItemID", "WBSElementID"),
      by = c("OrderOrLotID", "EndItemID", "WBSElementID")) %>%
    dplyr::mutate(join_category = dplyr::coalesce(.data$DetailedStandardCategoryID, .data$StandardCategoryID)) %>%
    dplyr::left_join(prorate_bucket_lookup,by = "join_category") %>%
    dplyr::select(-"join_category") %>%
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
    dplyr::select(-"TotalValueDollars", -"TotalValueHours", -"DenomDollars", -"DenomHours")


  #stack prorate and percentage allocation components
  combined_allocation_components <- prorate_allocation_percentages %>%
    dplyr::left_join(
      prorate_allocation_components %>% dplyr::select(-"PercentValue"),
      by = c("OrderOrLotID", "EndItemID", "WBSElementID", "UnitOrSublotID")) %>%
    dplyr::bind_rows(percent_allocation_components) %>%
    dplyr::mutate( ProrateBucket = dplyr::if_else(is.na(ProrateBucket), "PERCENT", ProrateBucket)
    )

  prorate_only <- combined_allocation_components %>%
    dplyr::filter(ProrateBucket != "PERCENT")

  #expand percent components to all reporting periods
  percent_expanded <- combined_allocation_components %>%
    dplyr::filter(ProrateBucket == "PERCENT") %>%
    dplyr::mutate(ReportingPeriodID = NULL) %>%
    tidyr::crossing(ReportingPeriodID = unique(flexfile$ActualCostHourData$ReportingPeriodID))

  #stack final result to be able to join on reporting period ID for both allocation methods
  combined_allocation_components_expanded <- dplyr::bind_rows(prorate_only, percent_expanded)

  #join the allocation component percentages to the actual cost hour data table
  new_actualcosthourdata <- flexfile$ActualCostHourData %>%
    dplyr::mutate(join_category = dplyr::coalesce(.data$DetailedStandardCategoryID, .data$StandardCategoryID)) %>%
    dplyr::left_join(prorate_bucket_lookup,by = "join_category") %>%
    dplyr::select(-"join_category") %>%
    dplyr::mutate(
      ProrateBucket = dplyr::if_else(AllocationMethodID %in% percent_allocation_components$AllocationMethodID,
                                     "PERCENT", ProrateBucket)) %>%
    dplyr::left_join(
      combined_allocation_components_expanded,
      by = c("AllocationMethodID", "ProrateBucket", "ReportingPeriodID"),
      suffix = c("", "_allocations")) %>%
    dplyr::left_join(
      flexfile$AllocationMethods %>%
        dplyr::filter(AllocationMethodTypeID == "PERCENT") %>%
        dplyr::select("ID", "AllocationMethodTypeID"),
      by = c("AllocationMethodID" = "ID"))

  # iterate over the function to apply it across all allocation fields
  # reduce will take the output from iteration i and use it as input to i + 1
  flexfile$ActualCostHourData <- purrr::reduce(
    allocation_fields,
    coalesce_field,
    suffix = "_allocations",
    .init = new_actualcosthourdata) %>%
    tidyr::replace_na(list(PercentValue = 1)) %>%
    dplyr::mutate(
      DollarPercentValue = dplyr::if_else(is.na(DollarPercentValue), 1, DollarPercentValue),
      HourPercentValue = dplyr::if_else(is.na(HourPercentValue), 1, HourPercentValue),
      Value_Dollars = Value_Dollars * DollarPercentValue,
      Value_Hours = Value_Hours * HourPercentValue) %>%
    dplyr::select(
      -(tidyselect::ends_with("_allocations")),
      -"AllocationMethodTypeID",
      -"ProrateBucket",
      -"DollarPercentValue",
      -"HourPercentValue")


  attr(flexfile, "allocated") <- TRUE

  flexfile
}
