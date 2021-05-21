## ===== Read FlexFiles =====

#' Read FlexFile or Quantity report
#'
#' \code{read_flexfile()} returns a list of tibbles from a zip folder submission of the FlexFiles.
#' Each tibble corresponds to its respective JSON table. This function can read both a FlexFile
#' and a Quantity report.
#'
#' Can be used with \code{\link[costmisc:read_folder]{read_folder}} as in example.
#'
#' @export
#'
#' @param file Path to a FlexFile or Quantity Report .zip archive.
#' @param .show_check Logical whether to print information to the console about the
#' file check to the console or not.
#' @param .coerce_spec Logical whether to coerce all column data types to those from the data models.
#' If \code{FALSE}, the types will be as detected upon read by the JSON parser.
#' @inheritParams costmisc::read_json_zip
#'
#' @return A list of tibbles for the \code{file}. Result will be either of class \code{flexfile} or
#' of class \code{quantityreport}.
#'
#' @seealso [flexfile_class], [quantityreport_class]
#'
#' @examples
#' \dontrun{
#' # Read in one FlexFile
#' file <- system.file("extdata", "Sample_FlexFile_A.zip", package = "flexample")
#'
#' flexfile <- read_flexfile(file)
#'
#' # Read in multiple FlexFiles by using read_folder
#' folder <- system.file("extdata", package = "flexample")
#'
#' flexfiles <- read_folder(folder, read_flexfile)
#'}
read_flexfile <- function(file, .show_check = FALSE, .coerce_spec = TRUE, .warn_utf8_bom = TRUE) {

  # check the file type
  file_type <- check_filetype(file)

  # assign a file specification based on the type
  if (file_type == "FlexFile") {
    table_spec <- readflexfile::flexfile_spec
    set_class_function <- new_flexfile
  } else if (file_type == "Quantity") {
    table_spec <- readflexfile::quantity_spec
    set_class_function <- new_quantityreport
  }

  # read into a list of tables, dropping the FileType.txt input
  table_list <- costmisc::read_json_zip(file, .warn_utf8_bom)

  # check file against the spec
  check <- check_spec(table_list, table_spec, file_type, .silent = isFALSE(.show_check))

  table_list <- add_missing_spec_tables(table_list, table_spec, check)
  table_list <- add_missing_spec_cols(table_list, table_spec, new_name = "field")

  # coerce to the data model data types if desired
  if (.coerce_spec) table_list <- coerce_to_spec(table_list, table_spec)

  # convert to snake_case (done after the coercing)
  table_list <- add_missing_spec_cols(table_list, table_spec, new_name = "snake_name")

  # clean up table names
  clean_table_names <- rlang::set_names(table_spec$tables$snake_table,
                                        table_spec$tables$table)
  names(table_list) <- clean_table_names[names(table_list)]

  fileinfo <- list(path = normalizePath(dirname(file), winslash = "/"),
                   name = sub(".zip$", "", basename(file)),
                   name_ext = basename(file))

  set_class_function(table_list, fileinfo = fileinfo)
}

#' Read FlexFile or Quantity report
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' \code{read_ff()} was renamed to \code{\link{read_flexfile}()} to be less ambiguous.
#'
#' @keywords internal
#' @export
read_ff <- function(file, .show_check = FALSE, .coerce_spec = TRUE, .warn_utf8_bom = TRUE) {
  lifecycle::deprecate_warn(when = "0.3.0", what = "read_ff()", with = "read_flexfile()")

  read_flexfile(file, .show_check = FALSE, .coerce_spec = TRUE, .warn_utf8_bom = TRUE)
}

#' @keywords internal
check_filetype <- function(file) {
  # read the FileType.txt without loading the entire file
  file_type <- readr::read_file(unz(file, "FileType.txt"))

  valid_files <- c(FlexFile = "CSDR_COST_HOUR_REPORT/1.0",
                   Quantity = "CSDR_QUANTITY_REPORT/1.0")

  the_type <- valid_files[file_type == valid_files]

  if (length(the_type) == 0) stop("this is not a flexfile or quantity report")

  names(the_type)
}

## ===== Stack FlexFiles =====

#' Stack list of FlexFiles or Quantity Reports into one list
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' This function is depreciated because stacking individual JSON tables is no longer
#' recommended. See \code{vignette("importing-flexfile")} for the preferred workflow.
#' If the functionality is still required, use \code{\link[costmisc]{unnest_df}()} instead.
#'
#' \code{stack_ff()} reads in a list of lists of FlexFile or Quantity Report tibbles and returns a
#' single list of stacked tibbles. The \code{data} list should not mix report type (i.e.,
#' it should contain all FlexFiles or all Quantity Reports).
#'
#' This is a thin wrapper around \code{\link[costmisc]{unnest_df}()}.
#'
#' @export
#'
#' @param data A list of FlexFile or Quantity Report tibbles converted from JSON format.
#'
#' @return A list of stacked tibbles.
#'
#' @examples
#' \dontrun{
#' folder <- system.file("extdata", package = "flexample")
#'
#' flexfiles <- read_folder(folder, read_flexfile) %>%
#'   listindex_to_col() %>%
#'   stack_ff()
#'}
stack_ff <- function(data) {
  lifecycle::deprecate_warn(when = "0.3.0", what = "stack_ff()",
                            details = c("Stacking individual JSON tables is no longer recommended.\n  See `vignette(\"importing-flexfile\")` for the preferred workflow",
                                        "If the functionality is still required, use `costmisc::unnest_df()` instead."))

  costmisc::unnest_df(data)
}
