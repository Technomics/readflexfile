
#' Read Technical Data report
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' \code{read_techdatareport()} returns a list of tibbles from an Excel submission of the Technical
#' Data Report. Each tibble corresponds to its Excel sheet.\cr
#' \cr
#' This function is currently experimental because it reads from the Excel template, which is
#' may not be stable.
#'
#' @export
#'
#' @param file Path to a Technical Data report Excel file.
#' @param .show_check Logical whether to print information about the file check to the console or not.
#' @param .coerce_spec Logical whether to coerce all column data types to those from the data models.
#' If \code{FALSE}, the types will be as detected upon read by the Excel reader.
#'
#' @return A list of tibbles for the \code{file}. Result will be either of class \code{techdatareport}.
#'
#' @seealso [techdatareport_class]
#'
read_techdatareport <- function(file, .show_check = FALSE, .coerce_spec = TRUE){

  table_spec <- readflexfile::techdatareport_spec
  file_type <- "TDR Report"

  tables_to_read <- table_spec$tables$table
  scalar_tables <- table_spec$tables %>%
    dplyr::filter(.data$is_scalar) %>%
    dplyr::pull(.data$table)

  table_list <- tables_to_read %>%
    rlang::set_names() %>%
    # skip 8 for header metadata from CADE
    purrr::map(~ readxl::read_xlsx(file, sheet = .x, trim_ws = TRUE, col_names = TRUE, skip = 1,
                                   col_types = "text")) %>%
    purrr::map_at(scalar_tables, ~ tibble::as_tibble(t(tibble::deframe(.x)))) %>%
    purrr::map(~ .remove_space(.x))

  # cleanup tables by checking against the file spec
  fn_date <- function(x) janitor::excel_numeric_to_date(as.numeric(x))

  table_list <- spec_cleanup(table_list, table_spec, file_type, .show_check, .coerce_spec, .drop_optional = FALSE,
                             fn_date)

  fileinfo <- list(path = normalizePath(dirname(file), winslash = "/"),
                   name = sub(".xlsx$", "", basename(file)),
                   name_ext = basename(file))

  new_techdatareport(table_list, fileinfo = fileinfo)

}

#' @keywords internal
.remove_space <- function(df){
  df %>%
    dplyr::rename_with(~ stringr::str_replace_all(.x, "[\\r\\n]", " ") %>%
                         stringr::str_replace_all(" ", ""))

}
