% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flatten_ff.R
\name{flatten_lists}
\alias{flatten_lists}
\alias{flatten_ff}
\alias{flatten_qdr}
\title{Flatten FlexFile and Quantity Data Report}
\usage{
flatten_ff(flexfile, .id = "doc_id")

flatten_qdr(quantity_data, .id = "doc_id")
}
\arguments{
\item{flexfile}{A list of one or more FlexFiles imported through the \code{read_ff} function.}

\item{.id}{The name of the ID column which uniquely identifies a flexfile.}

\item{quantity_data}{A list of one or more Quantity Data Reports created from the \code{read_ff} function.}
}
\value{
A data frame with the flattened flexfile or quantity data.
}
\description{
Create separate data frames for the FlexFile and the Quantity Data Report.
These functions merge together many tables into a single one more useful for analysis.
This generally involves joining together numerous ID fields. Actuals and forecasts are also
stacked into a single table.

\code{flatten_ff()} creates a single flat file for working with FlexFile data.

\code{flatten_qdr()} creates a single flat file for working with FlexFile quantity data.
}
\examples{
\dontrun{
# Flatten one FlexFile
file <- system.file("extdata", "Sample_FlexFile_A.zip", package = "flexample")

flat_flex_file <- read_ff(file) \%>\%
  add_id_col(var = "doc_id") \%>\%
  flatten_ff()

#Flatten mutliple FlexFiles

files <- system.file("extdata", package = "flexample")

flat_flexfiles <- read_folder(files, read_ff) \%>\%
  listindex_to_col(var = "doc_id") \%>\%
  stack_ff() \%>\%
  flatten_ff()
}
\dontrun{
# read a sample quantity report
file <- system.file("extdata", "Sample_Quantity_A.zip", package = "flexample")

flat_flex_file <- read_ff(file) \%>\%
  add_id_col() \%>\%
  flatten_qdr()
}
}
