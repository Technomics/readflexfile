# FlexFile Data Spec for building database

library(magrittr)

flexfile_tables <- readxl::read_excel("data-raw/flexfile-tables.xlsx",
                                      sheet = "tables",
                                      col_types = c(rep("text", 4), "logical", rep("text", 2)))

flexfile_fields <- readxl::read_excel("data-raw/flexfile-tables.xlsx",
                                      sheet = "fields",
                                      col_types = c(rep("text", 5), "logical", rep("text", 2), "logical", "text")) %>%
  dplyr::mutate(safe_name = dplyr::coalesce(safe_name, snake_name))

flexfile_spec <- list(tables = flexfile_tables,
                      fields = flexfile_fields)


usethis::use_data(flexfile_spec, overwrite = TRUE)

flexfile_enum_tables <- readxl::read_excel("data-raw/flexfile_enum-tables.xlsx",
                                      sheet = "tables",
                                      col_types = c(rep("text", 4), "logical"))

flexfile_enum_fields <- readxl::read_excel("data-raw/flexfile_enum-tables.xlsx",
                                      sheet = "fields",
                                      col_types = c(rep("text", 5), "logical", rep("text", 2), "logical")) %>%
  dplyr::mutate(safe_name = dplyr::coalesce(safe_name, snake_name))

flexfile_enum_spec <- list(tables = flexfile_enum_tables,
                           fields = flexfile_enum_fields)

usethis::use_data(flexfile_enum_spec, overwrite = TRUE)
