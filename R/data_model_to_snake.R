
#' Convert table names
#'
#' \code{data_model_to_snake()} renames from the data model notation to snake_case. This
#' function will also add any missing columns from the spec to the model. \cr
#' \cr
#' Note that this function set may move to another location in a future version.
#'
#' @param table_list A list of tables to rename.
#' @param table_spec Data model specification object.
#'
#' @export
data_model_to_snake <- function(table_list, table_spec) {

  # convert to snake_case (and adds in any missing)
  table_list <- add_missing_spec_cols(table_list, table_spec, new_name = "snake_name")

  # clean up table names
  clean_table_names <- rlang::set_names(table_spec$tables$snake_table,
                                        table_spec$tables$table)
  names(table_list) <- clean_table_names[names(table_list)]

  table_list

}

#' Convert table names
#'
#' \code{snake_to_data_model()} renames from snake_case to data model notation.
#'
#' @rdname data_model_to_snake
#'
#' @export
snake_to_data_model <- function(table_list, table_spec) {

  # rename
  rename_columns <- function(table, table_name) {
    field_spec <- table_spec$fields %>%
      dplyr::filter(table == table_name)

    # vector of new names
    new_names <- rlang::set_names(field_spec$snake_name, field_spec$field)

    table %>%
      dplyr::rename(tidyselect::all_of(new_names))
  }

  # clean up table names
  clean_table_names <- rlang::set_names(table_spec$tables$table,
                                        table_spec$tables$snake_table)
  names(table_list) <- clean_table_names[names(table_list)]

  purrr::imodify(table_list, rename_columns)

}
