## ===== Apply Allocations =====

## ===== Apply Allocations =====

#' Apply allocation methodologies
#'
#' \code{allocate_flexfile()} applies the allocation methods defined in the
#' AllocationMethods and AllocationComponents tables to ActualCostHourData.
#'
#' Allocation expands an ActualCostHourData record into one or more destination
#' records and scales the record's dollars and hours by the percentage assigned
#' to each destination.
#'
#' Two allocation methodologies are currently supported:
#'
#' \itemize{
#'   \item \code{PERCENT}: Use the submitted allocation percentages directly.
#'   \item \code{PRORATE}: Derive percentages from the distribution of reported
#'         actual dollars and hours. When no historical basis exists, split the
#'         record evenly among the eligible destinations.
#' }
#'
#' @inheritParams apply_flexfile
#'
#' @export
allocate_flexfile <- function(flexfile) {

  apply_flexfile(flexfile, allocate_flexfile_single)

}

#' @keywords internal
allocate_flexfile_single <- function(flexfile) {

  # Normalize table and column names to the FlexFile's native casing before
  # performing joins.
  .flexfile <- costmisc::assert_case(flexfile, target_case = "native")

  # A FlexFile without AllocationComponents has no records that need to be
  # divided among multiple destinations. Assign each actual record a weight of
  # 1 so downstream processing can treat allocated and unallocated submissions
  # consistently.
  if (nrow(.flexfile$AllocationComponents) == 0) {
    .flexfile$ActualCostHourData <- .flexfile$ActualCostHourData %>%
      dplyr::mutate(PercentValue = 1)

    attr(.flexfile, "allocated") <- TRUE

    return(.flexfile)
  }

  # Columns in ActualCostHour which are allocated over
  allocation_cols <- c("OrderOrLotID", "EndItemID", "WBSElementID", "UnitOrSublotID")

  # ActualCostHourData may identify a UnitOrSublotID without directly carrying
  # the unit's associated EndItemID and OrderOrLotID. Add those parent
  # identifiers before deriving or applying allocations.
  .flexfile <- .flexfile %>%
    normalize_units_or_sublots()

  # AllocationMethods identifies the type of an allocation method, while
  # AllocationComponents identifies the destinations and, for PERCENT methods,
  # their submitted percentages. Flatten those tables so every component also
  # carries its AllocationMethodTypeID.
  combined_allocation_table <- .flexfile$AllocationComponents %>%
    dplyr::left_join(
      .flexfile$AllocationMethods %>%
        dplyr::select("ID", "AllocationMethodTypeID"),
      by = c("AllocationMethodID" = "ID")
    )

  ## -------------------------------------------------------------------------
  ## Allocation approach
  ## -------------------------------------------------------------------------
  #
  # Regardless of methodology, the final operation is the same:
  #
  #   1. Match an ActualCostHourData record to its allocation destinations.
  #   2. Create one output row for each destination.
  #   3. Multiply dollars and hours by the destination percentages.
  #
  # The methodology determines only how those percentages are obtained:
  #
  #   Method 1 — Submitted percentage
  #     PERCENT allocations use PercentValue from AllocationComponents.
  #
  #   Method 2 — Historical prorate
  #     PRORATE allocations derive separate dollar and hour percentages from
  #     the distribution of reported ActualCostHourData within the same
  #     reporting period and prorate bucket.
  #
  #   Method 3 — Equal-share prorate fallback
  #     When a PRORATE destination has no corresponding reported actuals from
  #     which to derive a percentage, the eligible destinations receive equal
  #     shares.
  #
  # PRORATE percentages are calculated separately for broad cost categories
  # called prorate buckets, such as Labor, Material, FCCM, G&A, and Other.
  # This prevents, for example, material dollars from determining the
  # allocation of labor hours.

  ## -------------------------------------------------------------------------
  ## Method 1: submitted PERCENT allocations
  ## -------------------------------------------------------------------------

  # PERCENT methods already contain the allocation weight in PercentValue.
  # Use the same submitted percentage for both dollars and hours.
  #
  # PERCENT percentages are not reporting-period-specific in the submitted
  # allocation tables, so cross them with every reporting period present in
  # ActualCostHourData. This produces the reporting-period grain needed by the
  # common allocation join later in the function.
  method_percent_allocation_components_1 <- combined_allocation_table %>%
    dplyr::filter(.data$AllocationMethodTypeID == "PERCENT") %>%
    dplyr::mutate(
      DollarPercentValue = .data$PercentValue,
      HourPercentValue = .data$PercentValue,

      # PERCENT is used as a synthetic bucket so PERCENT and PRORATE records can
      # be joined using the same keys later.
      ProrateBucket = "PERCENT",

      # Remove any existing reporting-period value before expanding the rows
      # across all reporting periods below.
      ReportingPeriodID = NULL
    ) %>%
    dplyr::select(-"AllocationMethodTypeID", -"PercentValue") %>%
    tidyr::crossing(ReportingPeriodID = unique(.flexfile$ActualCostHourData$ReportingPeriodID))

  ## -------------------------------------------------------------------------
  ## Prepare PRORATE allocation destinations
  ## -------------------------------------------------------------------------

  # Reduce PRORATE AllocationComponents to the identifiers that describe each
  # possible allocation destination.
  #
  # A component may identify its destination directly through OrderOrLotID,
  # EndItemID, or WBSElementID, or indirectly through UnitOrSublotID. For the
  # latter case, retrieve the unit's parent OrderOrLotID and EndItemID from
  # UnitsOrSublots.
  prorate_allocation_components <- combined_allocation_table %>%
    dplyr::filter(.data$AllocationMethodTypeID == "PRORATE") %>%
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
      OrderOrLotID = dplyr::coalesce(.data$OrderOrLotID, .data$OrderOrLotID.unitsorsublots),
      EndItemID = dplyr::coalesce(.data$EndItemID, .data$EndItemID.unitsorsublots)
    ) %>%
    dplyr::select("AllocationMethodID", tidyselect::all_of(allocation_cols))

  # Assign every ActualCostHourData row to a prorate bucket.
  #
  # DetailedStandardCategoryID is preferred because it provides the most
  # specific category. StandardCategoryID is used when no detailed category is
  # available.
  #
  # The resulting ProrateBucket is later used to calculate independent
  # distributions for Labor, Material, FCCM, G&A, and Other.
  atd_table_with_proratebucket <- .flexfile$ActualCostHourData %>%
    dplyr::mutate(
      join_category = dplyr::coalesce(.data$DetailedStandardCategoryID, .data$StandardCategoryID)
    ) %>%
    dplyr::left_join(.create_prorate_bucket_mappings(), by = "join_category") %>%
    dplyr::select(-"join_category")

  ## -------------------------------------------------------------------------
  ## Enumerate the expected PRORATE destination rows
  ## -------------------------------------------------------------------------

  # Build the complete set of candidate destination rows for each PRORATE
  # method, reporting period, and prorate bucket.
  #
  # The join is intentionally many-to-many:
  #
  #   - ActualCostHourData supplies the reporting periods and prorate buckets
  #     in which an allocation method appears.
  #   - AllocationComponents supplies all destinations belonging to that
  #     allocation method.
  #
  # Coalescing the identifier pairs fills a destination identifier from the
  # allocation component when the source actual record does not already carry
  # it.
  prorate_allocation_lookup_rows <- atd_table_with_proratebucket %>%
    # Reduce to distinct rows to avoid having to join duplicate values
    dplyr::distinct(
      .data$AllocationMethodID, .data$ReportingPeriodID, .data$ProrateBucket,
      dplyr::across(tidyselect::all_of(allocation_cols))
    ) %>%
    dplyr::inner_join(
      prorate_allocation_components,
      by = c("AllocationMethodID"),
      relationship = "many-to-many"
    ) %>%
    dplyr::mutate(
      OrderOrLotID = dplyr::coalesce(.data$OrderOrLotID.x, .data$OrderOrLotID.y),
      EndItemID = dplyr::coalesce(.data$EndItemID.x, .data$EndItemID.y),
      WBSElementID = dplyr::coalesce(.data$WBSElementID.x, .data$WBSElementID.y),
      UnitOrSublotID = dplyr::coalesce(.data$UnitOrSublotID.x, .data$UnitOrSublotID.y)
    ) %>%
    # Multiple source actual rows can produce the same candidate destination.
    # Retain one row per destination, period, bucket, and allocation method.
    dplyr::distinct(
      dplyr::across(tidyselect::all_of(allocation_cols)),
      .data$ReportingPeriodID, .data$ProrateBucket, .data$AllocationMethodID
    )

  ## -------------------------------------------------------------------------
  ## Method 2: historical PRORATE percentages
  ## -------------------------------------------------------------------------

  # Derive PRORATE percentages from actual dollars and hours already reported
  # for the allocation's destination structure.
  #
  # Step 1: Keep actual rows whose Order/Lot, End Item, and WBS combination is
  # represented by a PRORATE allocation component.
  #
  # UnitOrSublotID is intentionally not part of this semi-join. The surrounding
  # Order/Lot, End Item, and WBS identify the relevant population, while the
  # unit or sublot is the destination across which percentages are derived.
  method_prorate_allocation_components_2 <- atd_table_with_proratebucket %>%
    dplyr::semi_join(
      prorate_allocation_components,
      by = c("OrderOrLotID", "EndItemID", "WBSElementID")
    ) %>%

    # Step 2: Aggregate actual dollars and hours at the destination grain.
    #
    # Dollars and hours are retained separately because their historical
    # distributions can differ. A destination can therefore receive one
    # percentage for Value_Dollars and another for Value_Hours.
    dplyr::group_by(
      dplyr::across(tidyselect::all_of(allocation_cols)),
      .data$ReportingPeriodID, .data$ProrateBucket
    ) %>%
    dplyr::summarise(
      TotalValueDollars = sum(.data$Value_Dollars, na.rm = TRUE),
      TotalValueHours = sum(.data$Value_Hours, na.rm = TRUE),
      .groups = "drop"
    ) %>%

    # Step 3: Within each unit/sublot, reporting period, and cost bucket,
    # calculate each destination's share of the corresponding total.
    #
    # Conceptually:
    #
    #   dollar percentage =
    #     destination dollars / total dollars in the allocation group
    #
    #   hour percentage =
    #     destination hours / total hours in the allocation group
    #
    # A zero denominator produces a zero percentage rather than NaN or Inf.
    # Destinations with no historical row at all are handled separately by the
    # equal-share fallback above.
    dplyr::group_by(.data$UnitOrSublotID, .data$ReportingPeriodID, .data$ProrateBucket) %>%
    dplyr::mutate(
      DenomDollars = sum(.data$TotalValueDollars, na.rm = TRUE),
      DenomHours = sum(.data$TotalValueHours, na.rm = TRUE),
      DollarPercentValue = dplyr::if_else(.data$DenomDollars == 0, 0, .data$TotalValueDollars / .data$DenomDollars),
      HourPercentValue = dplyr::if_else(.data$DenomHours == 0, 0, .data$TotalValueHours / .data$DenomHours)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"TotalValueDollars", -"TotalValueHours", -"DenomDollars", -"DenomHours") %>%

    # Step 4: Restore AllocationMethodID and the submitted allocation-component
    # structure so the derived percentages can be matched back to source actual
    # records.
    dplyr::left_join(
      prorate_allocation_components,
      by = allocation_cols,
      relationship = "many-to-many"
    )

  ## -------------------------------------------------------------------------
  ## Method 3: equal-share fallback where no historical basis exists
  ## -------------------------------------------------------------------------

  # Identify candidate destination rows that do not have a matching
  # ActualCostHourData record at the same dimensional grain.
  #
  # These rows have no reported dollars or hours that could contribute to a
  # historical prorate calculation. Divide the allocation group evenly across
  # those destinations instead.
  #
  # Dollar and hour percentages are identical in the fallback because neither
  # measure provides a historical basis for a different distribution.
  method_prorate_allocation_components_3 <- prorate_allocation_lookup_rows %>%
    dplyr::anti_join(
      atd_table_with_proratebucket,
      by = c(allocation_cols, "ReportingPeriodID", "ProrateBucket")
    ) %>%
    dplyr::group_by(.data$AllocationMethodID, .data$ReportingPeriodID, .data$ProrateBucket) %>%
    dplyr::mutate(
      DollarPercentValue = 1 / dplyr::n(),
      HourPercentValue   = .data$DollarPercentValue
    ) %>%
    dplyr::ungroup()

  ## -------------------------------------------------------------------------
  ## Combine all percentage sources
  ## -------------------------------------------------------------------------

  # Place submitted PERCENT values, historically derived PRORATE values, and
  # equal-share PRORATE fallbacks into one table with a common structure.
  combined_allocation_components <- dplyr::bind_rows(
    method_percent_allocation_components_1,   # method 1
    method_prorate_allocation_components_2,   # method 2
    method_prorate_allocation_components_3    # method 3
  )

  ## -------------------------------------------------------------------------
  ## Match percentages to source actual records
  ## -------------------------------------------------------------------------

  # Add the method type to each actual record so PERCENT methods can be assigned
  # the synthetic "PERCENT" bucket used when their rows were prepared.
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
        .data$AllocationMethodTypeID == "PERCENT",
        "PERCENT",
        .data$ProrateBucket)
    ) %>%

    # Joining by allocation method, cost bucket, and reporting period attaches
    # one or more destination rows to the source actual record. When a method
    # has multiple components, this join expands the source record into one row
    # per allocation destination.
    dplyr::left_join(
      combined_allocation_components,
      by = c("AllocationMethodID", "ProrateBucket", "ReportingPeriodID"),
      suffix = c("", "_allocations")
    )

  ## -------------------------------------------------------------------------
  ## Apply destinations and scale values
  ## -------------------------------------------------------------------------

  .flexfile$ActualCostHourData <- new_actualcosthourdata %>%
    dplyr::mutate(
      # Replace the source dimensional identifiers with the allocation
      # destination identifiers where a destination value was supplied.
      # Otherwise, retain the source record's original identifier.
      OrderOrLotID = dplyr::coalesce(.data$OrderOrLotID_allocations, .data$OrderOrLotID),
      EndItemID = dplyr::coalesce(.data$EndItemID_allocations, .data$EndItemID),
      WBSElementID = dplyr::coalesce(.data$WBSElementID_allocations, .data$WBSElementID),
      UnitOrSublotID = dplyr::coalesce(.data$UnitOrSublotID_allocations, .data$UnitOrSublotID)
    ) %>%

    # Preserve the package's general PercentValue field for records without an
    # explicitly submitted value. DollarPercentValue and HourPercentValue are
    # the fields actually used to scale the measures below.
    tidyr::replace_na(
      list(
        PercentValue = 1,
        DollarPercentValue = 1,
        HourPercentValue = 1
      )
    ) %>%
    dplyr::mutate(
      # Apply dollar and hour percentages independently.
      Value_Dollars = .data$Value_Dollars * .data$DollarPercentValue,
      Value_Hours = .data$Value_Hours * .data$HourPercentValue) %>%
    dplyr::select(
      # Remove temporary destination columns and calculation fields, returning
      # ActualCostHourData to its expected schema.
      -tidyselect::ends_with("_allocations"),
      -"AllocationMethodTypeID",
      -"ProrateBucket",
      -"DollarPercentValue",
      -"HourPercentValue")

  # Mark the FlexFile so downstream functions can determine that allocation has
  # already been applied.
  attr(.flexfile, "allocated") <- TRUE

  .flexfile
}

#' @keywords internal
.create_prorate_bucket_mappings <- function() {
  # Map standard cost categories into broader buckets used as independent
  # historical bases for PRORATE calculations.
  #
  # Engineering, maintenance, and manufacturing are combined into Labor.
  # Material, FCCM, G&A, and Other remain separate because their cost
  # distributions may differ materially.
  sfc_mapping %>%
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

    # These categories require explicit mappings because they are not supplied
    # by the detailed-category rows selected from sfc_mapping above.
    dplyr::bind_rows(
      tibble::tibble(
        join_category = c("OTHER_DIRECT_COSTS","DIRECT_MATERIALS"),
        ProrateBucket = c("Other","Material")
      )
    )
}
