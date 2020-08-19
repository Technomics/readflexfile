
## code to prepare `sql_to_r_types` dataset goes here

sql_to_r_types <- ff2db::sql_to_r_types

usethis::use_data(sql_to_r_types, internal = TRUE, overwrite = TRUE)
