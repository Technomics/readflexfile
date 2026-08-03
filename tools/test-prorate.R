

rel_dir <- r"(Consolidated ACDB\_Year 3\5.2 Recommend ACDB Improvements\Prorated FlexFile Allocations\Normal Allocations)"
dir <- normalizePath(file.path(Sys.getenv("ONEDRIVE"), rel_dir), winslash = "/")

flexfile <- read_flexfile(file.path(dir, "prorated_allocations_test Import.zip"), .data_case = "native")

flexfile_allo <- flexfile %>%
  allocate_flexfile()

flexfile_allo$ActualCostHourData %>%
  dplyr::summarize(
    v = sum(Value_Dollars)
  )

nrow(flexfile_allo$ActualCostHourData)

flexfile_allo$ActualCostHourData %>%
  dplyr::count(AllocationMethodID)


flexfile_allo$ActualCostHourData %>%
  dplyr::filter(
    ReportingPeriodID == 12,
    AllocationMethodID == "Test_Prorate_Unit1"
  ) %>%
  View()
