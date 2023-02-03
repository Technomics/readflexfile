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
#' @param file Path to a FlexFile or Quantity Report .zip archive. For the 3-Part Template,
#' a character vector of the three Excel files to read.
#' @param .show_check Logical whether to print information about the file check to the console or not.
#' @param .coerce_spec Logical whether to coerce all column data types to those from the data models.
#' If \code{FALSE}, the types will be as detected upon read by the JSON parser.
#' @param .drop_optional Logical whether to drop optional columns or not.
#' @param .data_case Either 'native' or 'snake'. Controls if the names of the tables and columns
#' reflect the native data model or the transformed snake_case. The default option was changed from
#' snake to native in readflexfile v0.5.0 to simplify usage of readflexfile.
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
read_flexfile <- function(file,
                          .show_check = FALSE,
                          .coerce_spec = TRUE,
                          .drop_optional = TRUE,
                          .warn_utf8_bom = FALSE,
                          .data_case = c("native", "snake")) {

  lifecycle::deprecate_warn(
    when = "0.5.0",
    what = I('The default of returning tables and fields using "snake_case"'),
    with = I('the new names in any new code and we strongly advise refactoring any existing code.
    Note that new names align with those used in the native report.\n\nIf you must use the old naming
    in the interim, please use the argument `.data_case = "snake"` or use `costmisc::native_to_snake_case()`')
  )

  # check the file type
  file_type <- check_filetype(file)

  .data_case <- .data_case[1]

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

  # cleanup tables by checking against the file spec
  table_list <- spec_cleanup(table_list, table_spec, file_type, .show_check, .coerce_spec, .drop_optional, .data_case,
                             as.Date)

  fileinfo <- list(path = normalizePath(dirname(file), winslash = "/"),
                   name = sub(".zip$", "", basename(file)),
                   name_ext = basename(file))

  set_class_function(table_list, fileinfo = fileinfo, data_case = .data_case)
}

#' Read FlexFile 3-Part Template
#'
#' \code{read_flexfile_3part()} reads the Excel 3-Part Template into the same structure
#' as from the JSON.
#'
#' @rdname read_flexfile
#'
#' @export
read_flexfile_3part <- function(file, .show_check = FALSE, .coerce_spec = TRUE, .drop_optional = TRUE) {

  costmisc::check_pkg_suggests("readxl")

  # file should be a list of three files for the three part template
  if (length(file) != 3) stop("Expecting 'file' to be a vector of three filenames that make up the 3-Part Template.")

  file_heads <- purrr::map(file, readxl::read_xlsx,
                           sheet = "Template Info",
                           range = "A1:A3",
                           col_types = "text",
                           col_names = "Header")

  file_types <- file_heads %>%
    purrr::map_dfr(dplyr::slice, 1) %>%
    dplyr::pull(.data$Header)

  req_file_type <- "CSDR Cost and Hour Report (Flex File) Multi-Part Template"

  if (any(file_types != req_file_type)) stop("One or more 'file' is not from the FlexFile 3-Part Template.")

  # check if the files are parts 1, 2, and 3. each only occuring once
  file_parts <- file_heads %>%
    purrr::map_dfr(dplyr::slice, 2) %>%
    dplyr::pull(.data$Header)

  req_file_parts <- c("Part 1 - Metadata and Structures", "Part 2 - Actual Cost-Hour Data", "Part 3 - Supplemental Data")
  right_parts <- setequal(rowSums(outer(req_file_parts, file_parts, `==`)), c(1, 1, 1))

  if (!right_parts) stop("Expecting 'file' to contain each of section of the 3-Part Template.")

  table_spec <- readflexfile::flexfile_spec
  set_class_function <- new_flexfile
  file_type <- "FlexFile 3-Part Template"

  file_ordered <- rlang::set_names(file[order(file_parts)],
                                   c("part_1", "part_2", "part_3"))

  tables_to_read <- table_spec$tables %>%
    dplyr::filter(!is.na(.data$excel_3part)) %>%
    dplyr::select(.data$table, .data$excel_3part, .data$excel_3part_table) %>%
    dplyr::group_split(.data$excel_3part) %>%
    purrr::map(~ rlang::set_names(.x$excel_3part_table))

  read_template_single <- function(file, sheets) purrr::map(sheets,
                                                            purrr::possibly(
                                                              ~ readxl::read_xlsx(file,
                                                                                  sheet = .x,
                                                                                  skip = 1,
                                                                                  col_types = "text"),
                                                              NULL))

  table_list <- purrr::map2(file_ordered, tables_to_read, read_template_single)

  # merge into a single list, regardless of part
  table_list <- table_list %>%
    purrr::flatten() %>%
    purrr::compact()

  # convert the scalar tables from long to wide
  table_list <- convert_scalar_tables(table_list, table_spec)

  # convert back into standard JSON model case
  table_list <- costmisc::change_case_from_spec(table_list, table_spec, "excel_3part")

  # cleanup tables by checking against the file spec
  table_list <- spec_cleanup(table_list, table_spec, file_type, .show_check, .coerce_spec, .drop_optional,
                             .fn_date = purrr::compose(janitor::excel_numeric_to_date, as.numeric))


  fileinfo <- list(path = normalizePath(dirname(file), winslash = "/"),
                   name = sub(".xlsx$", "", basename(file)),
                   name_ext = basename(file))

  set_class_function(table_list, fileinfo = fileinfo)

}

#' @keywords internal
spec_cleanup <- function(table_list, table_spec, file_type,
                         .show_check, .coerce_spec, .drop_optional, .data_case,
                         .fn_date) {

  # check file against the spec
  check <- costmisc::check_spec(table_list, table_spec, file_type,
                                .silent = isFALSE(.show_check),
                                .include_table_type = "submission")

  table_list <- costmisc::add_missing_spec_tables(table_list, table_spec, check)
  table_list <- costmisc::add_missing_spec_cols(table_list, table_spec, new_name = "field")

  # coerce to the data model data types if desired
  if (.coerce_spec) table_list <- costmisc::coerce_to_spec(table_list, table_spec, .fn_date)

  # convert tables and fields to snake_case (done after the coercing)
  if (.data_case !=  "native") {
    # table_list <- data_model_to_snake(table_list, table_spec)
    table_list <- costmisc::change_case_from_spec(table_list, table_spec,
                                                  from_case = NULL, to_case = .data_case,
                                                  add_missing = FALSE)
  }


  # remove optional fields
  if (.drop_optional) table_list <- drop_na_optional_spec_tables(table_list, table_spec)

  table_list

}

#' @keywords internal
convert_scalar_tables <- function(table_list, table_spec) {

  scalar_tables <- table_spec$tables %>%
    dplyr::filter(.data$is_scalar) %>%
    dplyr::pull(.data$excel_3part_table)

  # fix any duplicated field names, if it occurs
  repair_dup_names <- function(table) {

    dup_names <- table %>%
      dplyr::count(.data$Field) %>%
      dplyr::filter(.data$n > 1) %>%
      dplyr::pull(.data$Field)

    dup_nums <- which(table$Field %in% dup_names)

    table$Field[dup_nums] <- paste0(table$Field[dup_nums], "..", dup_nums)

    table
  }

  # fix names
  table_list <- purrr::map_at(table_list, scalar_tables, repair_dup_names)

  # pivot wide
  table_list <- purrr::map_at(table_list, scalar_tables,
                              ~ tidyr::pivot_wider(.x,
                                                   names_from = "Field",
                                                   values_from = "Value"))

  table_list

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


