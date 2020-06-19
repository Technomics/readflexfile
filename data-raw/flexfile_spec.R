# FlexFile Data Spec for building database

library(magrittr)

flexfile_tables <- readxl::read_excel("data-raw/flexfile-tables.xlsx",
                                      col_types = c(rep("text", 4), "logical")) %>%
  dplyr::mutate(safe_name = dplyr::coalesce(safe_name, field))

flexfile_constraints <- list(actualcosthourdata_fk01 = list(type = "constraint",
                                                            table = "actualcosthourdata",
                                                            ref_table = "ordersorlots",
                                                            by = c("doc_id" = "doc_id",
                                                                   "order_or_lot_id" = "id")),

                             actualcosthourdata_fk02 = list(type = "constraint",
                                                            table = "actualcosthourdata",
                                                            ref_table = "enditems",
                                                            by = c("doc_id" = "doc_id",
                                                                   "end_item_id" = "id")),

                             actualcosthourdata_fk03 = list(type = "constraint",
                                                            table = "actualcosthourdata",
                                                            ref_table = "wbs",
                                                            by = c("doc_id" = "doc_id",
                                                                   "wbs_element_id" = "id")),

                             actualcosthourdata_fk04 = list(type = "constraint",
                                                            table = "actualcosthourdata",
                                                            ref_table = "accounts",
                                                            by = c("doc_id" = "doc_id",
                                                                   "account_id" = "id")),

                             actualcosthourdata_fk05 = list(type = "constraint",
                                                            table = "actualcosthourdata",
                                                            ref_table = "functionalcategories",
                                                            by = c("doc_id" = "doc_id",
                                                                   "functional_category_id" = "id")),

                             actualcosthourdata_fk06 = list(type = "constraint",
                                                            table = "actualcosthourdata",
                                                            ref_table = "functionaloverheadcategories",
                                                            by = c("doc_id" = "doc_id", "functional_overhead_category_id" = "id")),

                             actualcosthourdata_fk07 = list(type = "constraint",
                                                            table = "actualcosthourdata",
                                                            ref_table = "unitsorsublots",
                                                            by = c("doc_id" = "doc_id", "unit_or_sublot_id" = "id")),

                             actualcosthourdata_fk08 = list(type = "constraint",
                                                            table = "actualcosthourdata",
                                                            ref_table = "allocationmethods",
                                                            by = c("doc_id" = "doc_id", "allocation_method_id" = "id")),

                             actualcosthourdata_fk09 = list(type = "constraint",
                                                            table = "actualcosthourdata",
                                                            ref_table = "reportingcalendar",
                                                            by = c("doc_id" = "doc_id", "reporting_period_id" = "id")),

                             actualcosthourdata_fk10 = list(type = "constraint",
                                                            table = "actualcosthourdata",
                                                            ref_table = "standardcategoryenum",
                                                            by = c("standard_category_id" = "id")),

                             actualcosthourdata_fk11 = list(type = "constraint",
                                                            table = "actualcosthourdata",
                                                            ref_table = "detailedstandardcategoryenum",
                                                            by = c("detailed_standard_category_id" = "id")),

                             actualcosthourdata_fk12 = list(type = "constraint",
                                                            table = "actualcosthourdata",
                                                            ref_table = "clins",
                                                            by = c("doc_id" = "doc_id", "clin_id" = "id")),

                             actualcosthourdata_fk13 = list(type = "constraint",
                                                            table = "actualcosthourdata",
                                                            ref_table = "nonrecurringorrecurringenum",
                                                            by = c("nonrecurring_or_recurring_id" = "id")),

                             allocationcomponents_fk01 = list(type = "constraint",
                                                              table = "allocationcomponents",
                                                              ref_table = "allocationmethods",
                                                              by = c("doc_id" = "doc_id",
                                                                     "allocation_method_id" = "id")),

                             allocationcomponents_fk02 = list(type = "constraint",
                                                              table = "allocationcomponents",
                                                              ref_table = "ordersorlots",
                                                              by = c("doc_id" = "doc_id",
                                                                     "order_or_lot_id" = "id")),

                             allocationcomponents_fk03 = list(type = "constraint",
                                                              table = "allocationcomponents",
                                                              ref_table = "enditems",
                                                              by = c("doc_id" = "doc_id",
                                                                     "end_item_id" = "id")),

                             allocationcomponents_fk04 = list(type = "constraint",
                                                              table = "allocationcomponents",
                                                              ref_table = "wbs",
                                                              by = c("doc_id" = "doc_id",
                                                                     "wbs_element_id" = "id")),

                             allocationcomponents_fk05 = list(type = "constraint",
                                                              table = "allocationcomponents",
                                                              ref_table = "unitsorsublots",
                                                              by = c("doc_id" = "doc_id",
                                                                     "unit_or_sublot_id" = "id")),

                             allocationmethods_fk01 = list(type = "constraint",
                                                           table = "allocationmethods",
                                                           ref_table = "allocationmethodtypeenum",
                                                           by = c("allocation_method_type_id" = "id")),

                             clins_fk01 = list(type = "constraint",
                                               table = "clins",
                                               ref_table = "contracttypeenum",
                                               by = c("contract_type_id" = "id")),

                             costhourtagdefinitions_fk01 = list(type = "constraint",
                                                                    table = "costhourtagdefinitions",
                                                                    ref_table = "costhourtagenum",
                                                                    by = c("cost_hour_tag_id" = "id")),

                             forecastatcompletioncosthourdata_fk01 = list(type = "constraint",
                                                                          table = "forecastatcompletioncosthourdata",
                                                                          ref_table = "ordersorlots",
                                                                          by = c("doc_id" = "doc_id", "order_or_lot_id" = "id")),

                             forecastatcompletioncosthourdata_fk02 = list(type = "constraint",
                                                                          table = "forecastatcompletioncosthourdata",
                                                                          ref_table = "wbs",
                                                                          by = c("doc_id" = "doc_id", "wbs_element_id" = "id")),

                             forecastatcompletioncosthourdata_fk03 = list(type = "constraint",
                                                                          table = "forecastatcompletioncosthourdata",
                                                                          ref_table = "standardcategoryenum",
                                                                          by = c("standard_category_id" = "id")),

                             forecastatcompletioncosthourdata_fk04 = list(type = "constraint",
                                                                          table = "forecastatcompletioncosthourdata",
                                                                          ref_table = "detailedstandardcategoryenum",
                                                                          by = c("detailed_standard_category_id" = "id")),

                             forecastatcompletioncosthourdata_fk05 = list(type = "constraint",
                                                                          table = "forecastatcompletioncosthourdata",
                                                                          ref_table = "nonrecurringorrecurringenum",
                                                                          by = c("nonrecurring_or_recurring_id" = "id")),

                             ordersorlots_fk01 = list(type = "constraint",
                                                      table = "ordersorlots",
                                                      ref_table = "phaseormilestoneenum",
                                                      by = c("phase_or_milestone_id" = "id")),

                             ordersorlots_fk02 = list(type = "constraint",
                                                      table = "ordersorlots",
                                                      ref_table = "contracttypeenum",
                                                      by = c("contract_type_id" = "id")),

                             ordersorlots_fk03 = list(type = "constraint",
                                                      table = "ordersorlots",
                                                      ref_table = "appropriationtypeenum",
                                                      by = c("appropriation_type_id" = "id")),

                             reportmetadata_fk01 = list(type = "constraint",
                                                        table = "reportmetadata",
                                                        ref_table = "phaseormilestoneenum",
                                                        by = c("phase_or_milestone_id" = "id")),

                             reportmetadata_fk02 = list(type = "constraint",
                                                        table = "reportmetadata",
                                                        ref_table = "contracttypeenum",
                                                        by = c("contract_type_id" = "id")),

                             reportmetadata_fk03 = list(type = "constraint",
                                                        table = "reportmetadata",
                                                        ref_table = "reportcycleenum",
                                                        by = c("report_cycle_id" = "id")),

                             reportmetadata_fk04 = list(type = "constraint",
                                                        table = "reportmetadata",
                                                        ref_table = "reportingcalendar",
                                                        by = c("doc_id" = "doc_id",
                                                               "reporting_period_id" = "id")),

                             summarycostdata_fk01 = list(type = "constraint",
                                                         table = "summarycostdata",
                                                         ref_table = "ordersorlots",
                                                         by = c("doc_id" = "doc_id",
                                                                "order_or_lot_id" = "id")),

                             summaryremarks_fk01 = list(type = "constraint",
                                                        table = "summaryremarks",
                                                        ref_table = "ordersorlots",
                                                        by = c("doc_id" = "doc_id", "order_or_lot_id" = "id")),

                             unitsorsublots_fk01 = list(type = "constraint",
                                                        table = "unitsorsublots",
                                                        ref_table = "enditems",
                                                        by = c("doc_id" = "doc_id",
                                                               "end_item_id" = "id")),

                             unitsorsublots_fk02 = list(type = "constraint",
                                                        table = "unitsorsublots",
                                                        ref_table = "ordersorlots",
                                                        by = c("doc_id" = "doc_id",
                                                               "order_or_lot_id" = "id")),

                             wbs_fk01 = list(type = "contraint",
                                             table = "wbs",
                                             ref_table = "wbs",
                                             by = c("parent_id" = "id")),

                             wbsdictionarydefintions_fk01 = list(type = "constraint",
                                                                 table = "wbsdictionarydefinitions",
                                                                 ref_table = "wbs",
                                                                 by = c("doc_id" = "doc_id",
                                                                        "wbs_element_id" = "id")),

                             wbselementremarks_fk01 = list(type = "constraint",
                                                           table = "wbselementremarks",
                                                           ref_table = "ordersorlots",
                                                           by = c("doc_id" = "doc_id",
                                                                  "order_or_lot_id" = "id")),

                             wbselementremarks_fk02 = list(type = "constraint",
                                                           table = "wbselementremarks",
                                                           ref_table = "wbs",
                                                           by = c("doc_id" = "doc_id",
                                                                  "wbs_element_id" = "id")))

flexfile_constraints <- purrr::modify(flexfile_constraints, ~ purrr::modify_at(.x, .at = "by", costmisc::clean_by))

usethis::use_data(flexfile_tables, overwrite = TRUE)
usethis::use_data(flexfile_constraints, overwrite = TRUE)
