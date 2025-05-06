#' Read Maintenance & Repair Parts report
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' \code{read_excel_template()} returns a list of tibbles from an Excel template. Templates currently supported include
#' the Maintenance and Repair Parts Report (MNR), and the Technical Data Report (TDR). Each tibble corresponds to one Excel sheet.\cr
#' \cr
#' This function is currently experimental because it reads from the Excel template, which
#' may not be stable.
#'
#' @export
#'
#' @param file Path to a Maintenance & Repair Part Report or Technical Data Report Excel file.
#' @param table_spec List containing data specifications for both tables, and fields within the tables
#' @param file_type Type of excel template used (e.g. "M&R Report", "TDR Report"). Used to inform check_spec function.
#' @param constructor Constructor to create object of the class pertaining to the file type (e.g. new_maintrepair, new_techdatareport)
#' @param .show_check Logical whether to print information about the file check to the console or not.
#' @param .coerce_spec Logical whether to coerce all column data types to those from the data models.
#' If \code{FALSE}, the types will be as detected upon read by the Excel reader.
#'
#' @return A list of tibbles for the \code{file}. Result will be of class \code{maintrepair} or \code{techdatareport}.
#'
#' @seealso [maintrepair_class]
#' @seealso [techdatareport_class]
#'

read_excel_template <- function(file, table_spec, file_type, constructor, .show_check = FALSE, .coerce_spec = TRUE){

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


  fileinfo <- list(path = normalizePath(dirname(file), winslash = "/"),
                   name = sub(".xlsx$", "", basename(file)),
                   name_ext = basename(file))

  constructor(table_list, fileinfo = fileinfo)

}

#' @keywords internal
.remove_space <- function(df){
  df %>%
    dplyr::rename_with(~ stringr::str_replace_all(.x, "[\\r\\n]", " ") %>%
                         stringr::str_replace_all(" ", ""))
}
