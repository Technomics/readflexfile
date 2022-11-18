
# file_dir <- "C:/Users/ajames/OneDrive - Technomics/Stryker/Task 0003 - Stryker CSDR and Contracts CY21-25/Technical/TDR M&R Dev/Read Files/M&R"
# file <- file.path(file_dir, "Cerberus_M&R_Report_(Contract Complete).xlsx")


#' Read Maintenance & Repair Parts report
#'
#' \code{read_maintrepair()} returns a list of tibbles from an Excel submission of the Maintenance
#' and Repair Parts report. Each tibble corresponds to its Excel sheet.
#'
#' Can be used with \code{\link[costmisc:read_folder]{read_folder}} as in example.
#'
#' @export
#'
#' @param file Path to a Maintenance & Repair Part report Excel file.
#' @param .show_check Logical whether to print information about the file check to the console or not.
#' @param .coerce_spec Logical whether to coerce all column data types to those from the data models.
#' If \code{FALSE}, the types will be as detected upon read by the Excel reader.
#'
#' @return A list of tibbles for the \code{file}. Result will be either of class \code{maintrepair}.
#'
#' @seealso [maintrepair_class]
#'
read_maintrepair <- function(file, .show_check = FALSE, .coerce_spec = TRUE){

  table_spec <- readflexfile::maintrepair_spec
  file_type <- "M&R Report"

  tables_to_read <- table_spec$tables$table
  scalar_tables <- table_spec$tables %>%
    dplyr::filter(is_scalar) %>%
    dplyr::pull(table)

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

  new_maintrepair(table_list, fileinfo = fileinfo)

}

#' @keywords internal
.remove_space <- function(df){
  df %>%
    dplyr::rename_with(~ stringr::str_replace_all(.x, "[\\r\\n]", " ") %>%
                         stringr::str_replace_all(" ", ""))

}
