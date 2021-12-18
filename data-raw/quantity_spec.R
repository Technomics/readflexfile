# Quantity Report Data Spec for building database

library(magrittr)

quantity_tables <- readxl::read_excel("data-raw/quantity-tables.xlsx",
                                      sheet = "tables",
                                      col_types = rep("text", 4))

quantity_fields <- readxl::read_excel("data-raw/quantity-tables.xlsx",
                                      sheet = "fields",
                                      col_types = c(rep("text", 5), "logical", rep("text", 2), "logical")) %>%
  dplyr::mutate(safe_name = dplyr::coalesce(safe_name, snake_name))

quantity_spec <- list(tables = quantity_tables,
                      fields = quantity_fields)


usethis::use_data(quantity_spec, overwrite = TRUE)
