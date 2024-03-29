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
usethis::use_package("tidyselect", min_version = "1.1.0")
usethis::use_package("tidyr", min_version = "1.0.0")
usethis::use_package("tibble", min_version = "2.0.0")
usethis::use_package("stringr", min_version = "1.0.0")
usethis::use_package("purrr", min_version = "0.3.3")
usethis::use_package("rlang", min_version = "0.4.2")
usethis::use_package("lifecycle", min_version = "1.0.0")
usethis::use_package("magrittr")
usethis::use_package("lubridate")
usethis::use_package("janitor", min_version = "2.1.0")
usethis::use_package("zip", min_version = "2.1.1")
usethis::use_package("jsonlite", min_version = "1.7.2")
usethis::use_package("readr")

usethis::use_package("costmisc", min_version = "0.7.3")

# Set GitHub remote
desc::desc_set_remotes(c("technomics/costmisc",
                         "technomics/flexample"))

# These are only in the vignettes
usethis::use_package("kableExtra", min_version = "1.1.0", type = "Suggests")
usethis::use_package("markdown", min_version = "1.1", type = "Suggests")
usethis::use_package("scales", min_version = "1.1.0", type = "Suggests")
usethis::use_package("flexample", min_version = "1.1.1", type = "Suggests")
usethis::use_package("readxl", min_version = "1.4.0", type = "Suggests")

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

detach("package:readflexfile", unload = TRUE)

## ===== Build =====

build_path_root <- file.path(setupr::get_dirs()$git_local, "costverse", "_builds")
build_path <- list(bin = file.path(build_path_root, "bin", rnomics::r_version()),
                   src = file.path(build_path_root, "src"))

fs::dir_create(unlist(build_path))

src_build_file <- devtools::build(path = build_path$src)
bin_build_file <- devtools::build(src_build_file, binary = TRUE, path = build_path$bin)

drat_repo <- file.path(setupr::get_dirs()$git_local, "costverse", "repo")
rnomics::add_to_drat(c(bin_build_file, src_build_file), drat_repo)

## ===== Scratch Work =====

vignette("importing-flexfile", package = "readflexfile")

# single file
file_ff <- system.file("extdata", "cerberus", "Annual-Submission-2016_flexfile.zip", package = "reviewcsdr")
file_qdr <- system.file("extdata", "cerberus", "Annual-Submission-2016_quantity.zip", package = "reviewcsdr")

flexfile <- read_flexfile(file_ff, .data_case = "native")
quantity <- read_flexfile(file_qdr, .data_case = "native")

ff_fam <- create_flexfile_family(flexfile, quantity)
ff_fam2 <- force_flexfile_family(flexfile, quantity)
