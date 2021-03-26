## ===== Read FlexFiles =====

#' Read FlexFile or Quantity report
#'
#' \code{read_ff()} returns a list of tibbles from a zip folder submission of the FlexFiles.
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
#' @return A list of tibbles for the \code{file}.
#'
#' @examples
#' \dontrun{
#' # Read in one FlexFile
#' file <- system.file("extdata", "Sample_FlexFile_A.zip", package = "flexample")
#'
#' flexfile <- read_ff(file) %>%
#'   add_id_col(var = "doc_id")
#'
#' # Read in multiple FlexFiles by using read_folder
#' files <- system.file("extdata", package = "flexample")
#'
#' flexfiles <- read_folder(files, read_ff) %>%
#'   listindex_to_col(var = "doc_id") %>%
#'   stack_ff() %>%
#'   flatten_ff()
#'}
read_ff <- function(file, .show_check = FALSE, .coerce_spec = TRUE, .warn_utf8_bom = TRUE) {
  # check the file type
  file_type <- check_filetype(file)

  # assign a file specification based on the type
  if (file_type == "FlexFile") {
    table_spec <- readflexfile::flexfile_spec
    set_class_function <- new_flexfile
  } else if (file_type == "Quantity") {
    table_spec <- readflexfile::quantity_spec
    #set_class_function <- new_quantityreport
  }

  # read into a list of tables, dropping the FileType.txt input
  table_list <- costmisc::read_json_zip(file, .warn_utf8_bom)

  # check file against the spec
  check <- check_spec(table_list, table_spec, file_type, .silent = isFALSE(.show_check))

  # add any missing columns back in and rename
  add_missing_columns <- function(table, table_name) {
    field_spec <- table_spec$fields %>%
      dplyr::filter(table == table_name)

    # build a prototype list
    all_cols <- rlang::set_names(unclass(sql_to_r_types[field_spec$type]),
                                 field_spec$field)

    new_names <- rlang::set_names(field_spec$field, field_spec$snake_name)

    tibble::add_column(table, !!!all_cols[setdiff(names(all_cols), names(table))]) %>%
      dplyr::select(tidyselect::all_of(names(all_cols))) %>%
      dplyr::rename(tidyselect::all_of(new_names))
  }

  table_list <- purrr::imodify(table_list, add_missing_columns)

  # coerce to the data model data types if desired
  if (.coerce_spec) table_list <- coerce_to_spec(table_list, table_spec)

  # clean up table names
  clean_table_names <- rlang::set_names(table_spec$tables$snake_table,
                                        table_spec$tables$table)
  names(table_list) <- clean_table_names[names(table_list)]

  set_class_function(table_list)
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

#' @keywords internal
check_spec <- function(table_list, table_spec, type_label = "Import File", .silent = TRUE) {

  table_names <- names(table_list)
  check_table <- table_spec$tables %>%
    dplyr::filter(.data$type == "submission") %>%
    dplyr::pull(.data$table)

  # check table names
  unknown_tables <- table_names[!(table_names %in% check_table)]
  missing_tables <- check_table[!(check_table %in% table_names)]

  if (!.silent) {
    cli::cli_h1(paste(type_label, "Format Check"))
    cli::cat_line(c(paste("The following shows status of the", type_label, "against the File Format"),
                    "Specification. Some fields and tables are optional, so being missing",
                    "does not necessarily indicate a problem."))

    if (length(unknown_tables) > 0) {
      cli::cli_h2("Unknown Tables")
      cli::cat_bullet(unknown_tables)
      cli::cat_line("")
    }

    if (length(missing_tables) > 0) {
      cli::cli_h2("Missing Tables")
      cli::cat_bullet(missing_tables)
      cli::cat_line("")
    }

    cli::cli_h2(glue::glue("Individual Tables"))
  }

  # check field names
  check_field_names <- function(table, table_name) {

    field_names <- names(table)
    check_field <- table_spec$fields %>%
      dplyr::filter(table == table_name) %>%
      dplyr::pull(.data$field)

    unknown_fields <- field_names[!(field_names %in% check_field)]
    missing_fields <- check_field[!(check_field %in% field_names)]

    if (!.silent) {
      if ((length(unknown_fields) == 0) && (length(missing_fields) == 0)) {
        cli::cli_alert_success(cli::style_bold(table_name))
      } else {
        cli::cli_alert_warning(cli::style_bold(table_name))
      }

      if (length(unknown_fields) > 0) {
        cli::cat_bullet(glue::glue("Unknown: {unknown_fields}"))
        cli::cat_line("")
      }
      if (length(missing_fields) > 0) {
        cli::cat_bullet(glue::glue("Missing: {missing_fields}"))
        cli::cat_line("")
      }
    }

    list(unknown = unknown_fields,
         missing = missing_fields)
  }

  field_check <- purrr::imap(table_list, check_field_names)

  list(tables = list(unknown = unknown_tables,
                     missing = missing_tables),
       fields = field_check)
}

#' @keywords internal
coerce_to_spec <- function(table_list, table_spec) {

  # function to apply for a given SQL type
  r_to_sql_fns <- list(VARCHAR = as.character,
                       LONG = as.integer,
                       DOUBLE = as.numeric,
                       BIT = as.logical,
                       DATETIME = as.Date)

  # function to alter a single table in the list
  coerce_table <- function(the_df, table_name) {

    # subset the field types
    table_fields <- table_spec$fields %>%
      dplyr::filter(table == table_name) %>%
      dplyr::select(.data$field, .data$type)

    # function to coerce all columns of a given type in the data frame
    coerce_to <- function(new_type, the_df) {
      the_cols <- dplyr::filter(table_fields, .data$type == new_type)$field
      if (length(the_cols) == 0) return(the_df)

      dplyr::mutate(the_df, dplyr::across(dplyr::any_of(the_cols), r_to_sql_fns[new_type], .names = "{.col}"))
    }

    # loop over each type - this accumulates so using a for loop (can use purrr::accumulate)
    for (current_type in names(r_to_sql_fns)) {
      the_df <- coerce_to(current_type, the_df)
    }

    the_df

  }

  purrr::imodify(table_list, coerce_table)

}

## ===== Stack FlexFiles =====

#' Stack list of FlexFiles or Quantity Reports into one list
#'
#' \code{stack_ff()} reads in a list of lists of FlexFile or Quantity Report tibbles and returns a
#' single list of stacked tibbles. The \code{data} list should not mix report type (i.e.,
#' it should contain all FlexFiles or all Quantity Reports).\cr
#' \cr
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
#' files <- system.file("extdata", package = "flexample")
#'
#' flexfiles <- read_folder(files, read_ff) %>%
#'   listindex_to_col() %>%
#'   stack_ff()
#'}
stack_ff <- function(data) {
  costmisc::unnest_df(data)
}
