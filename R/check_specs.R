
#' @keywords internal
check_spec <- function(table_list, table_spec, type_label = "Import File", .silent = TRUE) {

  table_names <- names(table_list)
  check_table <- table_spec$tables %>%
    dplyr::filter(.data$type == "submission") %>%
    dplyr::pull(.data$table)

  # check table names
  unknown_tables <- table_names[!(table_names %in% check_table)]
  missing_tables <- check_table[!(check_table %in% table_names)]

  if (!.silent) {
    cli::cli_h1(paste(type_label, "Format Check"))
    cli::cat_line(c(paste("The following shows status of the", type_label, "against the File Format"),
                    "Specification. Some fields and tables are optional, so being missing",
                    "does not necessarily indicate a problem."))

    if (length(unknown_tables) > 0) {
      cli::cli_h2("Unknown Tables")
      cli::cat_bullet(unknown_tables)
      cli::cat_line("")
    }

    if (length(missing_tables) > 0) {
      cli::cli_h2("Missing Tables")
      cli::cat_bullet(missing_tables)
      cli::cat_line("")
    }

    cli::cli_h2(glue::glue("Individual Tables"))
  }

  # check field names
  check_field_names <- function(table, table_name) {

    field_names <- names(table)
    check_field <- table_spec$fields %>%
      dplyr::filter(table == table_name) %>%
      dplyr::pull(.data$field)

    unknown_fields <- field_names[!(field_names %in% check_field)]
    missing_fields <- check_field[!(check_field %in% field_names)]

    if (!.silent) {
      if ((length(unknown_fields) == 0) && (length(missing_fields) == 0)) {
        cli::cli_alert_success(cli::style_bold(table_name))
      } else {
        cli::cli_alert_warning(cli::style_bold(table_name))
      }

      if (length(unknown_fields) > 0) {
        cli::cat_bullet(glue::glue("Unknown: {unknown_fields}"))
        cli::cat_line("")
      }
      if (length(missing_fields) > 0) {
        cli::cat_bullet(glue::glue("Missing: {missing_fields}"))
        cli::cat_line("")
      }
    }

    list(unknown = unknown_fields,
         missing = missing_fields)
  }

  field_check <- purrr::imap(table_list, check_field_names)

  list(tables = list(unknown = unknown_tables,
                     missing = missing_tables),
       fields = field_check)
}

#' @keywords internal
coerce_to_spec <- function(table_list, table_spec) {

  # function to apply for a given SQL type
  r_to_sql_fns <- list(VARCHAR = as.character,
                       LONG = as.integer,
                       DOUBLE = as.numeric,
                       BIT = as.logical,
                       DATETIME = as.Date)

  # function to alter a single table in the list
  coerce_table <- function(the_df, table_name) {

    # subset the field types
    table_fields <- table_spec$fields %>%
      dplyr::filter(table == table_name) %>%
      dplyr::select(.data$field, .data$type)

    # function to coerce all columns of a given type in the data frame
    coerce_to <- function(new_type, the_df) {
      the_cols <- dplyr::filter(table_fields, .data$type == new_type)$field
      if (length(the_cols) == 0) return(the_df)

      dplyr::mutate(the_df, dplyr::across(dplyr::any_of(the_cols), r_to_sql_fns[new_type], .names = "{.col}"))
    }

    # loop over each type - this accumulates so using a for loop (can use purrr::accumulate)
    for (current_type in names(r_to_sql_fns)) {
      the_df <- coerce_to(current_type, the_df)
    }

    the_df

  }

  purrr::imap(table_list, coerce_table)

}

#' @keywords internal
add_missing_spec_cols <- function(table_list, table_spec, new_name = "snake_name") {

  # add any missing columns back in and rename
  add_missing_columns <- function(table, table_name) {
    field_spec <- table_spec$fields %>%
      dplyr::filter(table == table_name)

    # build a prototype list
    all_cols <- rlang::set_names(unclass(sql_to_r_types[field_spec$type]),
                                 field_spec$field)

    new_names <- rlang::set_names(field_spec$field, field_spec[[new_name]])

    tibble::add_column(table, !!!all_cols[setdiff(names(all_cols), names(table))]) %>%
      dplyr::select(tidyselect::all_of(names(all_cols))) %>%
      dplyr::rename(tidyselect::all_of(new_names))
  }

  purrr::imodify(table_list, add_missing_columns)
}

#' @keywords internal
add_missing_spec_tables <- function(table_list, table_spec, checked_spec) {

  missing_tables <- purrr::set_names(checked_spec$tables$missing)

  # create the missing tables
  create_missing_table <- function(table_name) {

    col_names <- table_spec$fields %>%
      dplyr::filter(.data$table == table_name) %>%
      dplyr::pull(.data$field)

    tibble::as_tibble(sapply(col_names, function(x) logical()))
  }

  # set the data types
  new_tables <- coerce_to_spec(lapply(missing_tables, create_missing_table), table_spec)

  # append and return
  c(table_list, new_tables)
}

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
