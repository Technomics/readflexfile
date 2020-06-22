# FlexFile Data Spec for building database

library(magrittr)

flexfile_tables <- readxl::read_excel("data-raw/flexfile-tables.xlsx",
                                      sheet = "tables",
                                      col_types = rep("text", 4))

flexfile_fields <- readxl::read_excel("data-raw/flexfile-tables.xlsx",
                                      sheet = "fields",
                                      col_types = c(rep("text", 5), "logical", rep("text", 2))) %>%
  dplyr::mutate(safe_name = dplyr::coalesce(safe_name, snake_name))

flexfile_spec <- list(flexfile_tables = flexfile_tables,
                      flexfile_fields = flexfile_fields)

usethis::use_data(flexfile_spec, overwrite = TRUE)
