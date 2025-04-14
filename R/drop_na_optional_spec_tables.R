
#' @keywords internal
drop_na_optional_spec_tables <- function(table_list, table_spec, .data_case = "snake") {
  if(.data_case == "snake"){
    na_fields <- purrr::map(table_list, ~ purrr::map_lgl(.x, ~ all(is.na(.x))))

    spec_fields <- table_spec$fields %>%
      dplyr::left_join(dplyr::select(table_spec$tables, table, snake_table), by = "table") %>%
      dplyr::select(snake_table, snake_name, optional)

    remove_fields<- na_fields %>%
      tibble::enframe("snake_table", "fields") %>%
      dplyr::mutate(fields = purrr::map(fields, tibble::enframe, name = "snake_name", value = "is_na")) %>%
      tidyr::unnest("fields")  %>%
      dplyr::left_join(spec_fields, by = c("snake_table", "snake_name")) %>%
      dplyr::filter(is_na == TRUE | optional == TRUE)

    remove_list <- split(remove_fields, remove_fields$snake_table)

    # remove the listed fields from each table
    for (i_table in names(remove_list)) {
      table_list[[i_table]] <- table_list[[i_table]] %>%
        dplyr::select(-tidyselect::all_of(remove_list[[i_table]]$snake_name))
    }
  } else {
    na_fields <- purrr::map(table_list, ~ purrr::map_lgl(.x, ~ all(is.na(.x))))

    spec_fields <- table_spec$fields %>%
      dplyr::left_join(dplyr::select(table_spec$tables, table), by = "table") %>%
      dplyr::select(table, field, optional)

    remove_fields<- na_fields %>%
      tibble::enframe("table", "fields") %>%
      dplyr::mutate(fields = purrr::map(fields, tibble::enframe, name = "field", value = "is_na")) %>%
      tidyr::unnest("fields")  %>%
      dplyr::left_join(spec_fields, by = c("table", "field")) %>%
      dplyr::filter(is_na == TRUE | optional == TRUE)

    remove_list <- split(remove_fields, remove_fields$table)

    for (i_table in names(remove_list)) {
      table_list[[i_table]] <- table_list[[i_table]] %>%
        dplyr::select(-tidyselect::all_of(remove_list[[i_table]]$field))
    }
  }

  table_list

}



