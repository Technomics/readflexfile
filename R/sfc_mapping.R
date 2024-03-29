#' Standard functional category mapping table
#'
#' A data frame containing the mapping between standard and detailed categories.
#'
#' @format \code{sfc_mapping} is a data frame with 26 rows and 5 variables:
#' \describe{
#'   \item{StandardCategoryID}{The standard category identifier.}
#'   \item{DetailedStandardCategoryID}{The detailed category identifier.}
#'   \item{functional_category}{The overarching functional category grouping.}
#'   \item{direct_or_overhead}{Either "direct" or "overhead".}
#'   \item{direct_or_overhead}{Either "labor", "material", or "other".}
#' }
#'
"sfc_mapping"
