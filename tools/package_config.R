
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

## ===== DESCRIPTION =====

# Description list
description <- list(Description = "Tools for Cost and Software Data Reporting (CSDR) data.",
                    Title = "Tools for CSDR data",
                    `Authors@R` = list(person(given = "Justin", family = "Cooper",
                                              email = "jcooper@technomics.net",
                                              role = c("cre", "aut"))))

desc::desc_add_author("Adam H.", "James", "ajames@technomics.net", "aut")
desc::desc_add_author("Ben", "Berkman", "bberkman@technomics.net", "aut")

# Run this to set description. It will replace whatever is there! Keep in mind the version before doing this.
#usethis::use_description(fields = description)

desc::desc_add_author(given = "Technomics, Inc", role = "cph")

# Package dependencies
usethis::use_pipe()
usethis::use_package("dplyr", min_version = "0.8.3")
usethis::use_package("rio")
usethis::use_package("jsonlite")
usethis::use_package("janitor")
usethis::use_package("tidyr", min_version = "1.0.0")
usethis::use_package("tibble", min_version = "2.0.0")
usethis::use_package("purrr", min_version = "0.3.3")
usethis::use_package("rlang", min_version = "0.4.2")
usethis::use_package("costmisc", min_version = "0.2.1")
usethis::use_package("stringr", min_version = "1.4.0")
usethis::use_package("lifecycle")
usethis::use_package("magrittr")
usethis::use_package("lubridate")

## ===== README & NEWS =====

usethis::use_lifecycle_badge("Maturing")
rnomics::use_badge_passing()

## ===== Developmental Tools =====

pkgdown::build_reference()

devtools::build_site()
devtools::document()

devtools::spell_check()
devtools::check()

usethis::use_version()

devtools::load_all()

devtools::build(binary = TRUE)
devtools::build()

detach("package:readflexfile", unload = TRUE)

## ===== Scratch Work =====

test_data <- read_folder("I:/Tools/costverse/data/readflexfile", read_ff)

df <- test_data %>%
  listindex_to_col(var = "doc_id") %>%
  stack_ff()

flexfile <- test_data[[3]] %>%
  add_id_col()

df <- df %>%
  flatten_ff()

View(df)


