% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/99_check_functions.R
\name{check_chemical_element}
\alias{check_chemical_element}
\title{Validate if a given chemical element symbol is valid}
\usage{
check_chemical_element(element)
}
\arguments{
\item{element}{A character string representing the chemical element symbol to be validated.}
}
\value{
A logical value: TRUE if the element symbol is valid, FALSE otherwise.
}
\description{
This function checks whether a given symbol is a valid chemical element.
The function uses a list of known chemical elements up to 2022.
It returns TRUE if the symbol is valid, otherwise returns FALSE.
}
\examples{
check_chemical_element("Fe") # TRUE
check_chemical_element("Fz") # FALSE

}
