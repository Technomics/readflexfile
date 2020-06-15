# FlexFile enumerations from file specs

flexfile_enum <- list(phaseormilestoneenum = readxl::read_excel("data-raw/flexfile-enumerations.xlsx", sheet = 1),
                      contracttypeenum = readxl::read_excel("data-raw/flexfile-enumerations.xlsx", sheet = 1),
                      appropriationtypeenum = readxl::read_excel("data-raw/flexfile-enumerations.xlsx", sheet = 1),
                      reportcycleenum = readxl::read_excel("data-raw/flexfile-enumerations.xlsx", sheet = 1),
                      nonrecurringorrecurringenum = readxl::read_excel("data-raw/flexfile-enumerations.xlsx", sheet = 1),
                      standardcategoryenum = readxl::read_excel("data-raw/flexfile-enumerations.xlsx", sheet = 1),
                      detailedstandardcategoryenum = readxl::read_excel("data-raw/flexfile-enumerations.xlsx", sheet = 1),
                      allocationmethodtypeenum = readxl::read_excel("data-raw/flexfile-enumerations.xlsx", sheet = 1),
                      costhourtagenum = readxl::read_excel("data-raw/flexfile-enumerations.xlsx", sheet = 1))

usethis::use_data(flexfile_enum, overwrite = TRUE)
