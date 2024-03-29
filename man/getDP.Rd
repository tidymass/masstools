% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spectra_functions.R
\name{getDP}
\alias{getDP}
\title{Compute Dot Product Between Two Intensity Vectors}
\usage{
getDP(exp.int, lib.int)
}
\arguments{
\item{exp.int}{A numeric vector representing the intensity values of the experimental spectrum.}

\item{lib.int}{A numeric vector representing the intensity values of the library spectrum.}
}
\value{
A numeric value representing the dot product between
  \code{exp.int} and \code{lib.int}.
}
\description{
This function computes the weighted dot product between two intensity vectors
from experimental and library spectra, considering weights for each intensity.
}
\details{
The function computes weights for each intensity value in both vectors based
on their relative contribution to the total intensity. The dot product is then
computed using these weighted intensity values.
}
\examples{
getDP(exp.int = 1:10, lib.int = 1:10)
}
