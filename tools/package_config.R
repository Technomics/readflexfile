
## ===== Project Setup ====

# Set name
options(usethis.full_name = "Adam H. James")

# Ignore folders on build
usethis::use_build_ignore("tools")

# Import badges for use in documentation
usethis::use_lifecycle()

# License
usethis::use_gpl3_license("Technomics, Inc.")

# Data folders
usethis::use_data_raw(name = "sfc_mapping")
usethis::use_data_raw(name = "sql_to_r_types")

# Package site
usethis::use_pkgdown()

# Readme setup with RMarkdown
rnomics::use_readme()
usethis::use_news_md()

# Allow markdown rendering in documentation
desc::desc_set("Roxygen", "list(markdown = TRUE)")

# Vignettes
usethis::use_vignette("csdrtool-vignette")
usethis::use_vignette("importing-flexfile")

# Tests
usethis::use_test("read")

## ===== DESCRIPTION =====

# Description list
desc::desc_add_author("Adam H.", "James", "ajames@technomics.net", "aut")
desc::desc_add_author("Ben", "Berkman", "bberkman@technomics.net", "aut")
desc::desc_add_author("Justin", "Cooper", "jcooper@technomics.net", "aut")

desc::desc_add_author(given = "Technomics, Inc", role = "cph")

desc::desc_set(Description = "Read the FlexFile data from the JSON specification into R. This includes simply reading the FlexFile into a list, and joining the individual tables together into a single flat file. These functions can also
               handle nuances of the FlexFile, such as allocations and optional fields.")

# Package dependencies
usethis::use_pipe()
usethis::use_package("dplyr", min_version = "0.8.3")
usethis::use_package("rio")
usethis::use_package("jsonlite")
usethis::use_package("tidyr", min_version = "1.0.0")
usethis::use_package("tibble", min_version = "2.0.0")
usethis::use_package("purrr", min_version = "0.3.3")
usethis::use_package("rlang", min_version = "0.4.2")
usethis::use_package("costmisc", min_version = "0.2.1")
usethis::use_package("stringr", min_version = "1.4.0")
usethis::use_package("glue", min_version = "1.4.1")
usethis::use_package("cli", min_version = "2.0.2")
usethis::use_package("lifecycle")
usethis::use_package("magrittr")
usethis::use_package("lubridate")
usethis::use_package("janitor")
usethis::use_package("readr")
usethis::use_package("stats")

# These are only in the vignettes
usethis::use_package("ezflexfile", min_version = "0.0.9", type = "Suggests")
usethis::use_package("kableExtra", min_version = "1.1.0", type = "Suggests")
usethis::use_package("scales", min_version = "1.1.0", type = "Suggests")

## ===== README & NEWS =====

rnomics::use_badge_costverse()
usethis::use_lifecycle_badge("Stable")
rnomics::use_badge_passing()
rnomics::use_badge_gpl3()

## ===== Developmental Tools =====

cvg <- devtools::test_coverage()
rnomics::use_badge_coverage(cvg)

pkgdown::build_reference()

devtools::build_site()
devtools::document()

devtools::spell_check()
devtools::check()

usethis::use_version()
rnomics::use_badge_version()
usethis::use_citation()

devtools::load_all()

devtools::build(binary = TRUE)
devtools::build()

detach("package:readflexfile", unload = TRUE)

## ===== Scratch Work =====

test_data <- readflexfile::read_folder("I:/Tools/costverse/data/readflexfile", read_ff)
standard <- read_ff("I:/Tools/costverse/data/readflexfile/1. standard_category_id.zip", TRUE)
detailed <- read_ff("I:/Tools/costverse/data/readflexfile/2. detailed_category_id.zip")

test_df <- test_data %>%
  listindex_to_col(var = "doc_id") %>%
  stack_ff() %>%
  flatten_ff()

test_df %>% dplyr::distinct(standard_category_id, detailed_standard_category_id)

standard_df <- standard %>%
  add_id_col(var = "doc_id") %>%
  flatten_ff()

standard_df %>% dplyr::distinct(standard_category_id, detailed_standard_category_id)

detailed_df <- detailed %>%
  add_id_col(var = "doc_id") %>%
  allocate_ff() %>%
  flatten_ff()

detailed_df %>% dplyr::distinct(standard_category_id, detailed_standard_category_id)

