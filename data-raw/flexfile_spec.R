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
