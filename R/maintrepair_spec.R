#' File specification for the M&R Report
#'
#' A list containing two data frames: \code{fields} and \code{tables}.
#'
#' @section tables:
#' \code{tables} is a data frame with 6 rows and 5 variables:
#' \describe{
#'   \item{table}{The table name in the specification}
#'   \item{entity}{The entity name in the specification}
#'   \item{snake_table}{A snake_case version of table}
#'   \item{type}{Either submission or enum}
#'   \item{is_scalar}{Boolean whether the table is a list of scalars or a data table}
#' }
#'
#' @section fields:
#' \code{fields} is a data frame with 72 rows and 9 variables:
#' \describe{
#'   \item{table}{The table name in the specification}
#'   \item{field}{The field name in the specification}
#'   \item{snake_name}{A snake_case version of field}
#'   \item{safe_name}{A database-safe version of field}
#'   \item{type}{SQL data type for the field}
#'   \item{pk}{Boolean if the field is a primary key}
#'   \item{fk_table}{Foreign key constraint table, if applicable}
#'   \item{fk_field}{Foreign key constraint field, if applicable}
#'   \item{optional}{Boolean if field is optional or required}
#' }
#'
#' @family Data Specs
#'
"maintrepair_spec"
