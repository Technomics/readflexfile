# FlexFile Data Spec for building database

library(magrittr)

maintrepair_tables <- readxl::read_excel("data-raw/maintrepair-tables.xlsx",
                                         sheet = "tables",
                                         col_types = c(rep("text", 4), "logical"))

maintrepair_fields <- readxl::read_excel("data-raw/maintrepair-tables.xlsx",
                                         sheet = "fields",
                                         col_types = c(rep("text", 5), "logical", rep("text", 2), "logical")) %>%
  dplyr::mutate(safe_name = dplyr::coalesce(safe_name, snake_name))

maintrepair_spec <- list(tables = maintrepair_tables,
                         fields = maintrepair_fields)


usethis::use_data(maintrepair_spec, overwrite = TRUE)
