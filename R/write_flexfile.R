
#' Read FlexFile or Quantity report
#'
#' \code{write_flexfile()} writes a list of tibbles into a zip folder of JSON files.
#' Each tibble corresponds to its respective JSON table. This function can write both a FlexFile
#' and a Quantity report.
#'
#' @param x An object of class \link{flexfile_class} or \link{quantityreport_class}.
#' @inheritParams read_flexfile
#'
#' @export
write_flexfile <- function(x, file) {

  if (is_flexfile(x)) {
    file_type <- "CSDR_COST_HOUR_REPORT/1.0"
    table_spec <- readflexfile::flexfile_spec
  } else if (is_quantityreport(x)) {
    file_type <- "CSDR_QUANTITY_REPORT/1.0"
    table_spec <- readflexfile::quantitydata_spec
  } else {
    stop("'x' must be an object of class 'flexfile' or 'quantityreport'.")
  }

  data_model <- snake_to_data_model(x, table_spec)

  # remove columns which are all NA
  data_model_pruned <- purrr::map(data_model, ~ purrr::discard(.x, ~ all(is.na(.x))))

  # write the json package
  ff <- costmisc::write_json_zip(data_model_pruned, file, na = "null")

  # write the file type
  ff <- append_textfile_zip(ff, "FileType.txt", file_type)

  invisible(ff)
}

#' @keywords internal
append_textfile_zip <- function(zipfile, file_name, file_text) {

  # create a temporary directory
  tf <- tempfile(pattern = "costmisc", tmpdir = tempdir(check = TRUE))

  if (dir.exists(tf)) unlink(tf, recursive = TRUE)
  dir.create(tf)

  temp_path <- normalizePath(tf, winslash = "/")
  file_path <- file.path(temp_path, file_name)

  # write file to the zip archive
  if (file.exists(file_path)) unlink(file_path)
  writeLines(file_text, con = file_path, sep = "")

  zip::zipr_append(zipfile, file_path)

  # delete the temp directory
  if (dir.exists(tf)) unlink(tf, recursive = TRUE)

  invisible(tools::file_path_as_absolute(zipfile))

}
