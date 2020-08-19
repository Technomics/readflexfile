#' File specification for the FlexFile
#'
#' A list containing two data frames: \code{fields} and \code{tables}.
#'
#' @section tables:
#' \code{tables} is a data frame with 29 rows and 4 variables:
#' \describe{
#'   \item{table}{The table name in the specification}
#'   \item{entity}{The entity name in the specification}
#'   \item{snake_table}{A snake_case version of table}
#'   \item{type}{Either submission or enumeration}
#' }
#'
#' @section fields:
#' \code{fields} is a data frame with 171 rows and 8 variables:
#' \describe{
#'   \item{table}{The table name in the specification}
#'   \item{field}{The field name in the specification}
#'   \item{snake_name}{A snake_case version of field}
#'   \item{safe_name}{A database-safe version of field}
#'   \item{type}{SQL data type for the field}
#'   \item{pk}{Boolean if the field is a primary key}
#'   \item{fk_table}{Foreign key contraint table, if applicable}
#'   \item{fk_field}{Foreign key contraint field, if applicable}
#' }
#'
"flexfile_spec"
