
#' @keywords internal
drop_na_optional_spec_tables <- function(table_list, table_spec) {

  na_fields <- purrr::map(table_list, ~ purrr::map_lgl(.x, ~ all(is.na(.x))))

  spec_fields <- table_spec$fields %>%
    dplyr::left_join(dplyr::select(table_spec$tables, .data$table, .data$snake_table), by = "table") %>%
    dplyr::select(.data$snake_table, .data$snake_name, .data$optional)

  remove_fields <- na_fields %>%
    tibble::enframe("snake_table", "fields") %>%
    dplyr::mutate(fields = purrr::map(.data$fields, tibble::enframe, name = "snake_name", value = "is_na")) %>%
    tidyr::unnest("fields") %>%
    dplyr::left_join(spec_fields, by = c("snake_table", "snake_name")) %>%
    dplyr::filter(.data$is_na, .data$optional)

  remove_list <- split(remove_fields, remove_fields$snake_table)

  # remove the listed fields from each table
  for (i_table in names(remove_list)) {
    table_list[[i_table]] <- table_list[[i_table]] %>%
      dplyr::select(-tidyselect::all_of(remove_list[[i_table]]$snake_name))
  }

  table_list

}
