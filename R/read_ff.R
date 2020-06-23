## ===== Read FlexFiles =====

#' Read one FlexFile into a list of tibbles
#'
#' @description
#'
#' \code{read_ff()} returns a list of tibbles from a zip folder submission of the FlexFiles.
#' Each tibble corresponds to its respective JSON table.
#'
#' Can be used with \code{\link[costmisc:read_folder]{read_folder}} as in example.
#'
#' @export
#'
#' @param file Path to a FlexFile .zip archive.
#' @param .show_check Logical whether to print information about the file check to the
#' console or not.
#'
#' @return A list of tibbles for the \code{file}.
#'
#' @examples
#' # Read in one FlexFile
#' file <- system.file("extdata", "Sample File_FF.zip", package = "readflexfile")
#'
#' flexfile <- read_ff(file) %>%
#'   add_id_col(var = "doc_id")
#'
#' # Read in multiple FlexFiles by using read_folder
#' files <- system.file("extdata/multiple-flexfiles", package = "readflexfile")
#'
#' flexfiles <- read_folder(files, read_ff) %>%
#'   listindex_to_col(var = "doc_id") %>%
#'   stack_ff() %>%
#'   flatten_ff()
#'
read_ff <- function(file, .show_check = FALSE) {
  ff <- rio::import_list(file, setclass = "tibble")

  # check file type
  check <- check_ff(ff, .silent = TRUE)

  ff[["FileType"]] <- NULL

  # add any missing columns back in and rename
  add_missing_columns <- function(table, table_name) {
    field_spec <- flexfile_spec$flexfile_fields %>%
      dplyr::filter(table == table_name)

    # build a prototype list
    all_cols <- rlang::set_names(unclass(ff2db::sql_to_r_types[field_spec$type]),
                                 field_spec$field)

    new_names <- rlang::set_names(field_spec$field, field_spec$snake_name)

    tibble::add_column(table, !!!all_cols[setdiff(names(all_cols), names(table))]) %>%
      dplyr::select(dplyr::all_of(names(all_cols))) %>%
      dplyr::rename(new_names)
  }

  ff <- purrr::imodify(ff, add_missing_columns)

  # clean up table names
  clean_table_names <- rlang::set_names(flexfile_spec$flexfile_tables$snake_table,
                                        flexfile_spec$flexfile_tables$table)
  names(ff) <- clean_table_names[names(ff)]

  ff
}

#' @keywords internal
check_ff <- function(ff, .silent = TRUE) {
  # check file type
  if (names(ff[["FileType"]]) != "CSDR_COST_HOUR_REPORT/1.0")
    stop("this is not a flexfile file type")

  ff["FileType"] <- NULL

  table_names <- names(ff)
  check_table <- flexfile_spec$flexfile_tables %>%
    dplyr::filter(type == "submission") %>%
    dplyr::pull(table)

  # check table names
  unknown_tables <- table_names[!(table_names %in% check_table)]
  missing_tables <- check_table[!(check_table %in% table_names)]

  if (!.silent) {
    cli::cli_h1("FlexFile Format Check")
    cli::cat_line(c("The following shows status of the FlexFile against the File Format",
                    "Specification. Some fields and tables are optional, so being missing",
                    "does not necessarily indicate a problem."))

    if (length(unknown_fields) > 0) {
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
    check_field <- flexfile_spec$flexfile_fields %>%
      dplyr::filter(table == table_name) %>%
      dplyr::pull(field)

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

  field_check <- purrr::imap(ff, check_field_names)

  list(tables = list(unknown = unknown_tables,
                     missing = missing_tables),
       fields = field_check)
}

## ===== Stack FlexFiles =====

#' Stack list of multiple FlexFile submissions into one list of tibbles
#'
#' @description
#'
#' \code{stack_ff()} reads in a list of lists of FlexFile submissions returns one list of stacked tibbles.
#' Thin wrapper around \code{\link{unnest_df}()}.
#'
#' @export
#'
#'
#' @param .data A list of FlexFile submissions' tibbles converted from JSON format.
#'
#' @return A list of stacked tibbles of multiple dataframes
#'
#' @examples
#' files <- system.file("extdata/multiple-flexfiles", package = "readflexfile")
#'
#' flexfiles <- read_folder(files, read_ff) %>%
#' listindex_to_col() %>%
#' stack_ff()
#'

stack_ff <- function(.data) {
  costmisc::unnest_df(.data)
}
