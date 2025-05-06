
#' Read Technical Data report
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' \code{read_techdatareport()} returns a list of tibbles from an Excel submission of the Technical
#' Data Report. Each tibble corresponds to its Excel sheet.\cr
#' \cr
#' This function is currently experimental because it reads from the Excel template, which
#' may not be stable.
#'
#' @export
#'
#' @inheritParams read_excel_template
#'
#' @return A list of tibbles for the \code{file}. Result will be of class \code{techdatareport}.
#'
#' @seealso [techdatareport_class]
#' @seealso [read_excel_template()]

read_techdatareport <- function(file, .show_check = FALSE, .coerce_spec = TRUE){
  read_excel_template(file = file, table_spec = techdatareport_spec, file_type = "TDR Report", constructor = new_techdatareport,
                      .show_check = .show_check, .coerce_spec = .coerce_spec)
}
