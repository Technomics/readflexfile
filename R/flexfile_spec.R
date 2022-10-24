#' File specification for the FlexFile
#'
#' A list containing two data frames: \code{fields} and \code{tables}.
#'
#' @section tables:
#' \code{tables} is a data frame with 20 rows and 5 variables:
#' \describe{
#'   \item{table}{The table name in the specification}
#'   \item{entity}{The entity name in the specification}
#'   \item{snake_table}{A snake_case version of table}
#'   \item{type}{Either submission or enum}
#'   \item{is_scalar}{Boolean whether the table is a list of scalars or a data table}
#'   \item{excel_3part}{The "part" in the 3-Part Template}
#'   \item{excel_3part_table}{The table name in the 3-Part Template}
#' }
#'
#' @section fields:
#' \code{fields} is a data frame with 153 rows and 10 variables:
#' \describe{
#'   \item{table}{The table name in the specification}
#'   \item{field}{The field name in the specification}
#'   \item{snake_name}{A snake_case version of field}
#'   \item{safe_name}{A database-safe version of field}
#'   \item{type}{SQL data type for the field}
#'   \item{pk}{Boolean if the field is a primary key}
#'   \item{fk_table}{Foreign key constraint table, if applicable}
#'   \item{fk_field}{Foreign key constraint field, if applicable}
#'   \item{optional}{Boolean if the field is optional or not}
#'   \item{excel_3part_name}{The field name in the 3-Part Template}
#' }
#'
#' @family Data Specs
#'
"flexfile_spec"
