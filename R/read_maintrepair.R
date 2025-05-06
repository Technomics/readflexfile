
#' Read Maintenance & Repair Parts report
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' \code{read_maintrepair()} returns a list of tibbles from an Excel submission of the Maintenance
#' and Repair Parts report. Each tibble corresponds to its Excel sheet.\cr
#' \cr
#' This function is currently experimental because it reads from the Excel template, which
#' may not be stable.
#'
#' @export
#'
#' @inheritParams read_excel_template
#'
#' @return A list of tibbles for the \code{file}. Result will be of class \code{maintrepair}.
#'
#' @seealso [maintrepair_class]
#' @seealso [read_excel_template()]

read_maintrepair <- function(file, .show_check = FALSE, .coerce_spec = TRUE){
  read_excel_template(file = file, table_spec = maintrepair_spec, file_type = "M&R Report", constructor = new_maintrepair,
                      .show_check = .show_check, .coerce_spec = .coerce_spec)
}
