% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flexfile-class.R, R/flexfile_family-class.R,
%   R/flexfile_family.R
\name{flexfile_class}
\alias{flexfile_class}
\alias{is_flexfile}
\alias{as_flexfile}
\alias{is_flexfile_list}
\alias{is_flexfile_family}
\alias{one_of_flexfile_family}
\alias{which_flexfile_family}
\title{flexfile class utilities}
\usage{
is_flexfile(x)

as_flexfile(
  x,
  names_case = c("snake_case", "data_model"),
  allocated = FALSE,
  rolledup = FALSE,
  .drop_optional = TRUE,
  .show_check = TRUE
)

is_flexfile_list(x)

is_flexfile_family(x)

one_of_flexfile_family(x)

which_flexfile_family(x)
}
\arguments{
\item{x}{An object to test or coerce to type 'flexfile'.}

\item{names_case}{Case of the object being passed in.}

\item{allocated}{Logical whether the flexfile has been allocated.}

\item{rolledup}{Logical whether the flexfile WBS has been rolled up.}

\item{.drop_optional}{Logical whether to drop optional columns or not.}

\item{.show_check}{Logical whether or not to show results from the check against
the file specification.}
}
\description{
Functions to help with with the 'flexfile' class type.

\code{is_flexfile()} checks if object is of type 'flexfile'

\code{as_flexfile()} creates object of type 'flexfile' and checks it against
the file specification.

\code{is_flexfile_list()} check if the object is a list where all members are of class 'flexfile'.

\code{is_flexfile_family()} checks if object is of type 'flexfile_family'

\code{one_of_flexfile_family()} checks if object is of a type within the FlexFile family. This means
is of type \code{flexfile}, \code{quantityreport}, \code{maintrepair}, or \code{techdatareport}.

\code{which_flexfile_family()} returns which type within the family the object is. Results are
\code{flexfile}, \code{quantityreport}, \code{maintrepair}, or \code{techdatareport}.
}
\details{
The \code{flexfile} class has the following attributes.
\describe{
\item{allocated}{Logical. Have the allocations been applied to the \code{ActualCostHourData}
table.}
\item{rolledup}{Logical. Has the WBS roll up been applied to the \code{ActualCostHourData}
and \code{ForecastAtCompletionCostHourData} tables.}
}
}
