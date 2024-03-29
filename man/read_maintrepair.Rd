% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_maintrepair.R
\name{read_maintrepair}
\alias{read_maintrepair}
\title{Read Maintenance & Repair Parts report}
\usage{
read_maintrepair(file, .show_check = FALSE, .coerce_spec = TRUE)
}
\arguments{
\item{file}{Path to a Maintenance & Repair Part report Excel file.}

\item{.show_check}{Logical whether to print information about the file check to the console or not.}

\item{.coerce_spec}{Logical whether to coerce all column data types to those from the data models.
If \code{FALSE}, the types will be as detected upon read by the Excel reader.}
}
\value{
A list of tibbles for the \code{file}. Result will be either of class \code{maintrepair}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

\code{read_maintrepair()} returns a list of tibbles from an Excel submission of the Maintenance
and Repair Parts report. Each tibble corresponds to its Excel sheet.\cr
\cr
This function is currently experimental because it reads from the Excel template, which is
may not be stable.
}
\seealso{
\link{maintrepair_class}
}
