# FlexFile Data Spec for building database

library(magrittr)

techdatareport_tables <- readxl::read_excel("data-raw/techdatareport-tables.xlsx",
                                         sheet = "tables",
                                         col_types = c(rep("text", 4), "logical"))

techdatareport_fields <- readxl::read_excel("data-raw/techdatareport-tables.xlsx",
                                         sheet = "fields",
                                         col_types = c(rep("text", 5), "logical", rep("text", 2), "logical")) %>%
  dplyr::mutate(safe_name = dplyr::coalesce(safe_name, snake_name))

techdatareport_spec <- list(tables = techdatareport_tables,
                         fields = techdatareport_fields)


usethis::use_data(techdatareport_spec, overwrite = TRUE)
