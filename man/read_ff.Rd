% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_ff.R
\name{read_ff}
\alias{read_ff}
\title{Read FlexFile or Quantity report}
\usage{
read_ff(file, .show_check = FALSE, .coerce_spec = TRUE, .warn_utf8_bom = TRUE)
}
\arguments{
\item{file}{Path to a FlexFile or Quantity Report .zip archive.}

\item{.show_check}{Logical whether to print information to the console about the
file check to the console or not.}

\item{.coerce_spec}{Logical whether to coerce all column data types to those from the data models.
If \code{FALSE}, the types will be as detected upon read by the JSON parser.}

\item{.warn_utf8_bom}{Logical whether to report UTF8 byte-order-mark errors.}
}
\value{
A list of tibbles for the \code{file}.
}
\description{
\code{read_ff()} returns a list of tibbles from a zip folder submission of the FlexFiles.
Each tibble corresponds to its respective JSON table. This function can read both a FlexFile
and a Quantity report.
}
\details{
Can be used with \code{\link[costmisc:read_folder]{read_folder}} as in example.
}
\examples{
\dontrun{
# Read in one FlexFile
file <- system.file("extdata", "Sample_FlexFile_A.zip", package = "flexample")

flexfile <- read_ff(file) \%>\%
  add_id_col(var = "doc_id")

# Read in multiple FlexFiles by using read_folder
files <- system.file("extdata", package = "flexample")

flexfiles <- read_folder(files, read_ff) \%>\%
  listindex_to_col(var = "doc_id") \%>\%
  stack_ff() \%>\%
  flatten_ff()
}
}
