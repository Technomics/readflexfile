
## ===== Project Setup ====

# Data folders
usethis::use_data_raw(name = "sfc_mapping")
usethis::use_data_raw(name = "sql_to_r_types")

# Allow markdown rendering in documentation
desc::desc_set("Roxygen", "list(markdown = TRUE)")

# Vignettes
usethis::use_vignette("importing-flexfile")

# Tests
usethis::use_test("read")

# Citation
usethis::use_citation()

# GitHub
usethis::use_github_actions() # this includes the standard R-CMD-check
usethis::use_github_action("pkgdown")

## ===== DESCRIPTION =====

# Description list
desc::desc_set(Description = "Read the FlexFile data from the JSON specification into R. This includes simply reading the FlexFile into a list, and joining the individual tables together into a single flat file. These functions can also
               handle nuances of the FlexFile, such as allocations and optional fields.")

# Package dependencies
usethis::use_pipe()
usethis::use_package("dplyr", min_version = "0.8.3")
usethis::use_package("rio")
usethis::use_package("jsonlite")
usethis::use_package("tidyselect", min_version = "1.1.0")
usethis::use_package("tidyr", min_version = "1.0.0")
usethis::use_package("tibble", min_version = "2.0.0")
usethis::use_package("purrr", min_version = "0.3.3")
usethis::use_package("rlang", min_version = "0.4.2")
usethis::use_package("costmisc", min_version = "0.5.1")
usethis::use_package("stringr", min_version = "1.4.0")
usethis::use_package("glue", min_version = "1.4.1")
usethis::use_package("cli", min_version = "2.0.2")
usethis::use_package("lifecycle")
usethis::use_package("magrittr")
usethis::use_package("lubridate")
usethis::use_package("janitor")
usethis::use_package("readr")
usethis::use_package("stats")

# Set GitHub remote
desc::desc_set_remotes("technomics/costmisc")

# These are only in the vignettes
usethis::use_package("kableExtra", min_version = "1.1.0", type = "Suggests")
usethis::use_package("scales", min_version = "1.1.0", type = "Suggests")
usethis::use_package("flexample", min_version = "1.0.0", type = "Suggests")

## ===== README & NEWS =====

usethis::use_lifecycle_badge("Stable")

## ===== Developmental Tools =====

cvg <- devtools::test_coverage()
rnomics::use_badge_coverage(cvg)

pkgdown::build_reference()

devtools::build_site()
devtools::document()

devtools::spell_check()
devtools::check(vignettes = FALSE)

usethis::use_version()
rnomics::use_badge_version()

devtools::load_all()

devtools::install(build_vignettes = TRUE)

devtools::build(binary = TRUE)
devtools::build()

detach("package:readflexfile", unload = TRUE)

## ===== Scratch Work =====


