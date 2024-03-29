% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allocate_flexfile.R
\name{allocate_flexfile}
\alias{allocate_flexfile}
\title{Apply allocation methodologies provided}
\usage{
allocate_flexfile(flexfile)
}
\arguments{
\item{flexfile}{A single flexfile or a list of flexfiles imported through the \code{read_flexfile} function.}
}
\description{
\code{allocate_flexfile()} applies the allocations provided in the Allocation Methodology table to
the Actual Cost Hour Data table. Returns a list of tibbles from a zip folder submission of the FlexFiles.
Each tibble corresponds to its respective JSON table. \cr
\cr
Currently this is implemented for \code{AllocationMethodTypeID == "PERCENT"}.
}
