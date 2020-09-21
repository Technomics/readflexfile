
sfc_mapping <- readxl::read_excel("data-raw/standard-category-mapping.xlsx")

usethis::use_data(sfc_mapping, overwrite = TRUE)
