
#' @keywords internal
drop_na_optional_spec_tables <- function(table_list, table_spec, .data_case = NULL) {
  # .data_case is required when the table_list argument does not have a data_case attribute
  if (!is.null(.data_case)) attr(table_list, "data_case") <- .data_case

  # force into native case if it wasn't already
  .table_list <- costmisc::assert_case(table_list, target_case = "native", .table_spec = table_spec)

  na_fields <- purrr::map(.table_list, ~ purrr::map_lgl(.x, ~ all(is.na(.x))))

  spec_fields <- table_spec$fields %>%
    dplyr::left_join(
      dplyr::select(tidyselect::all_of(table_spec$tables), "table"),
      by = "table"
    ) %>%
    dplyr::select("table", "field", "optional")

  remove_fields<- na_fields %>%
    tibble::enframe("table", "fields") %>%
    dplyr::mutate(fields = purrr::map(.data$fields, tibble::enframe, name = "field", value = "is_na")) %>%
    tidyr::unnest("fields")  %>%
    dplyr::left_join(spec_fields, by = c("table", "field")) %>%
    dplyr::filter(.data$is_na == TRUE | .data$optional == TRUE)

  remove_list <- split(remove_fields, remove_fields$table)

  for (i_table in names(remove_list)) {
    .table_list[[i_table]] <- .table_list[[i_table]] %>%
      dplyr::select(-tidyselect::all_of(remove_list[[i_table]]$field))
  }

  .table_list

}



