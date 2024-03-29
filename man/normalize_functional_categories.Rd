% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize_functional_categories.R
\name{normalize_functional_categories}
\alias{normalize_functional_categories}
\title{Join in standard functional categories}
\usage{
normalize_functional_categories(flexfile, direct_or_oh_mapping = FALSE)
}
\arguments{
\item{flexfile}{A single flexfile or a list of flexfiles imported through the \code{read_flexfile} function.}

\item{direct_or_oh_mapping}{Logical whether not not to join in the direct or overhead.
attributes.}
}
\description{
\code{normalize_functional_categories()} joins in the standard functional categories
from the detailed function categories when not provided in the original data. Input
should be one or more FlexFiles imported through the \code{read_flexfile} function.
}
