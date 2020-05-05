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
#' @param .clean_names Logical whether to clean the names using \code{\link[janitor]{clean_names}()}
#' or not.
#' @return A list of tibbles for the \code{file}.
#'
#' @examples
#' file <- system.file("extdata", "Sample File_FF.zip", package = "readflexfile")
#'
#' flexfile <- read_ff(file)
#'
#' files <- system.file("extdata/multiple-flexfiles", package = "readflexfile")
#'
#' flexfiles <- costmisc::read_folder(files, read_ff) %>%
#' costmisc::listindex_to_col() %>%
#' stack_ff()
#'

read_ff <- function(file, .clean_names = TRUE) {
  ff <- rio::import_list(file, setclass = "tibble")

  if (.clean_names) {
    ff <- purrr::map(ff, janitor::clean_names)
    names(ff) <- janitor::make_clean_names(tolower(names(ff)))
  }

  # add the missing sfc id column in for each of the two tables
  fix_sfc_tables <- c("actualcosthourdata", "forecastatcompletioncosthourdata")
  sfc_cols <- c(detailed_standard_category_id = NA_character_,
                standard_category_id = NA_character_)

  purrr::modify_at(ff, fix_sfc_tables,
                   ~ tibble::add_column(.x, !!!sfc_cols[setdiff(names(sfc_cols), names(.x))]))

}


# #' @export
# check_ff <- function(.data) {
#   cat("not yet implemented\n")
# }

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
#' flexfiles <- costmisc::read_folder(files, read_ff) %>%
#' costmisc::listindex_to_col() %>%
#' stack_ff()
#'

stack_ff <- function(.data) {
  costmisc::unnest_df(.data)
}
