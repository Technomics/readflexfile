#' Read Maintenance & Repair Parts or Technical Data Report template
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' \code{read_excel_template()} is generally called by higher-level functions,
#' such as \code{\link{read_techdatareport}()} or
#' \code{\link{read_maintrepair}()}. It returns a list of tibbles from an Excel
#' template structure, with one tibble per sheet.
#'
#' This function is currently experimental because Excel templates may not be
#' stable.
#'
#' @export
#'
#' @param file Path to the Excel template file to read.
#' @param table_spec List containing data specifications for both tables and
#' fields.
#' @param file_type Type of Excel template used. This is used only for status
#' messages.
#' @inheritParams read_flexfile
#'
#' @return A list of tibbles for \code{file}. Result will be of class
#' \code{maintrepair} or \code{techdatareport}.
#'
#' @seealso [maintrepair_class], [techdatareport_class]
#'
read_excel_template <- function(file, table_spec, file_type, .show_check = FALSE, .coerce_spec = TRUE){

  table_spec = table_spec
  file_type <- file_type

  tables_to_read <- table_spec$tables$table
  scalar_tables <- table_spec$tables %>%
    dplyr::filter(.data$is_scalar) %>%
    dplyr::pull(.data$table)

  table_list <- tables_to_read %>%
    rlang::set_names() %>%
    # skip 1 for 2 row headings
    purrr::map(~ readxl::read_xlsx(file, sheet = .x, trim_ws = TRUE, col_names = TRUE, skip = 1,
                                   col_types = "text")) %>%
    purrr::map_at(scalar_tables, ~ tibble::as_tibble(t(tibble::deframe(.x)))) %>%
    purrr::map(~ .remove_space(.x))

  # cleanup tables by checking against the file spec
  fn_date <- function(x) janitor::excel_numeric_to_date(as.numeric(x))

  table_list <- spec_cleanup(table_list = table_list, table_spec = table_spec, file_type = file_type, .show_check = .show_check, .coerce_spec = .coerce_spec,
                             .drop_optional = FALSE, .data_case = "pascal", .fn_date = fn_date)

  table_list



}

#' @keywords internal
.remove_space <- function(df){
  df %>%
    dplyr::rename_with(~ stringr::str_replace_all(.x, "[\\r\\n]", " ") %>%
                         stringr::str_replace_all(" ", ""))
}
