% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/techdatareport-class.R
\name{techdatareport_class}
\alias{techdatareport_class}
\alias{is_techdatareport}
\alias{is_techdatareport_list}
\title{techdatareport class utilities}
\usage{
is_techdatareport(x)

is_techdatareport_list(x)
}
\arguments{
\item{x}{An object to test or coerce to type 'quantityreport'.}
}
\description{
Functions to help with the 'techdatareport' class type.

\code{is_techdatareport()} checks if object is of type 'techdatareport'

\code{is_techdatareport_list()} check if the object is a list where all members are of class 'techdatareport'.
}
\details{
The \code{techdatareport} class has the following attributes.
\describe{
\item{allocated}{Logical. Have the allocations been applied to the \code{actualcosthourdata}
table.}
\item{rolledup}{Logical. Has the WBS roll up been applied to the \code{actualcosthourdata}
and \code{forecastatcompletioncosthourdata} tables.}
}
}
