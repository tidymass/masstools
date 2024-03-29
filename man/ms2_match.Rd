% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spectra_functions.R
\name{ms2_match}
\alias{ms2_match}
\title{Match Fragments in Experimental and Library Spectra}
\usage{
ms2_match(exp.spectrum, lib.spectrum, ppm.tol = 30, mz.ppm.thr = 400)
}
\arguments{
\item{exp.spectrum}{A data frame representing the experimental spectrum with columns for m/z and intensity.}

\item{lib.spectrum}{A data frame representing the library spectrum with columns for m/z and intensity.}

\item{ppm.tol}{A numeric value specifying the ppm tolerance for matching fragments. Default is 30.}

\item{mz.ppm.thr}{A numeric value for the m/z threshold. Default is 400.}
}
\value{
A data frame with columns for library index, experimental index, library m/z, library intensity, experimental m/z, and experimental intensity.
}
\description{
This function matches fragments in experimental and library spectra based on their m/z values
within a given ppm tolerance. Noisy fragments are removed before matching.
}
\details{
For each fragment in the library spectrum, the function searches for a matching fragment in the experimental spectrum based on the specified ppm tolerance. 
Prior to matching, noisy fragments are removed from both spectra using the `remove_noise` function.
The resulting data frame provides matched fragments as well as unmatched fragments from both spectra.
}
\examples{
exp.spectrum <- data.frame(mz = 1:10, intensity = 1:10)
lib.spectrum <- data.frame(mz = 1:10, intensity = 1:10)
ms2_match(exp.spectrum, lib.spectrum)
}
