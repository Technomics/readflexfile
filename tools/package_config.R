
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

# allow markdown rendering in documentation
desc::desc_set("Roxygen", "list(markdown = TRUE)")

## ===== DESCRIPTION =====

# Description list
description <- list(Description = "Read the FlexFile native data into R.",
                    Title = "Read FlexFile data",
                    `Authors@R` = list(person(given = "Adam H.", family = "James",
                                              email = "ajames@technomics.net", role = c("cre","aut"))))

# Run this to set description. It will replace whatever is there! Keep in mind the version before doing this.
#usethis::use_description(fields = description)

desc::desc_add_author(given = "Technomics, Inc", role = "cph")

# Package dependencies


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

setupr::check_r()
