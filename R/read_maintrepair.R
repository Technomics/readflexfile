
#' Read Maintenance & Repair Parts report
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' \code{read_maintrepair()} returns a list of tibbles from an Excel submission of the Maintenance
#' and Repair Parts report. Each tibble corresponds to its Excel sheet.\cr
#' \cr
#' This function is currently experimental because it reads from the Excel template, which is
#' may not be stable.
#'
#' @export
#'
#' @param file Path to a Maintenance & Repair Part report Excel file.
#' @param .show_check Logical whether to print information about the file check to the console or not.
#' @param .coerce_spec Logical whether to coerce all column data types to those from the data models.
#' If \code{FALSE}, the types will be as detected upon read by the Excel reader.
#'
#' @return A list of tibbles for the \code{file}. Result will be of class \code{maintrepair}.
#'
#' @seealso [maintrepair_class]
#'
read_maintrepair <- function(file, .show_check = FALSE, .coerce_spec = TRUE, .drop_optional = FALSE){

  ##################### MODIFIED -- temporarily using local spec variable for debugging
  #table_spec <- readflexfile::maintrepair_spec
  table_spec = maintrepair_spec
  file_type <- "M&R Report"

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

  #################################################### MODIFIED
  table_list <- spec_cleanup(table_list = table_list, table_spec = table_spec, file_type = file_type, .show_check = .show_check, .coerce_spec = .coerce_spec,
                             .drop_optional = .drop_optional, .data_case = "pascal", .fn_date = fn_date)

  ## NOTE: if data case is set to 'snake', the costmisc::change_case_from_spec function in spec_cleanup automatically handles the following two steps
  ## 1. remove whitespace from table names, 2. remove special characters from field names
  ## This is because tentatively, the "snake_table" column in spec/tables is set to the cleaned table name,
  ## and likewise the "snake_name" in spec/fields is the cleaned up name for the data fields
  ## Previously, I had some regular expressions here to handle the cleanup, but I thought it was elegant to leverage the spec file.
  ## If this costmisc function is updated for spec fields named "clean_table" etc, then maybe we can just use it directly.

  # table_list <- costmisc::change_case_from_spec(table_list, table_spec,
  #                                               from_case = NULL, to_case = "clean",
  #                                               add_missing = FALSE)

  fileinfo <- list(path = normalizePath(dirname(file), winslash = "/"),
                   name = sub(".xlsx$", "", basename(file)),
                   name_ext = basename(file))

  new_maintrepair(table_list, fileinfo = fileinfo)

}

#################################################### MODIFIED

#' #' @keywords internal
#' .remove_space <- function(df){
#'   df %>%
#'     dplyr::rename_with(~ stringr::str_replace_all(.x, "[\\r\\n]", " ") %>%
#'                          stringr::str_replace_all(" ", ""))
#'
#' }

