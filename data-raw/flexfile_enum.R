# FlexFile enumerations from file specs

enum_file <- "data-raw/flexfile-enumerations.xlsx"

all_sheets <- purrr::set_names(readxl::excel_sheets(enum_file))
flexfile_enum <- purrr::map(all_sheets, ~readxl::read_excel(enum_file, sheet = .x))

usethis::use_data(flexfile_enum, overwrite = TRUE)
