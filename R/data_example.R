#' Example Mass Spectrometry Data Files
#'
#' @description Example mzXML and MGF files distributed with the package for
#'   demonstrations, tests, and reproducible examples.
#'
#' @docType data
#' @name example_data
#' @aliases example.mzXML example.mgf
#' @keywords datasets
#'
#' @format 
#' The package contains two example files located in the `inst/extdata` directory:
#' \describe{
#'   \item{example.mzXML}{An mzXML file containing LC-MS data (approx. 100 scans).}
#'   \item{example.mgf}{An MGF file containing 10 representative MS2 spectra.}
#' }
#'
#' @details
#' The package includes two example data files located in `inst/extdata`:
#'
#' ## example.mzXML
#' A down-sampled LC-MS file containing approximately 100 scans from a
#' representative 1 to 5 minute retention time window. The file can be read
#' with `read_mzxml()`.
#'
#' ## example.mgf
#' An MGF file containing 10 representative MS2 spectra selected from
#' metabolite annotation workflows. The spectra can be used with matching and
#' scoring functions such as `ms2_match()` and
#' `calculate_spectra_match_score()`.
#'
#' @source
#' The data files are derived from the publication:
#'
#' Shen, X., Li, X., Liang, L. et al. A plasma lipidomics strategy reveals
#' perturbed lipid metabolic pathways and potential lipid biomarkers of human
#' colorectal cancer. J. Chromatogr. B 1068-1069 (2017) 41-48.
#'
#' Original publication: \doi{10.1016/j.cell.2020.05.002}
#'
#' The files were down-sampled and subset for package distribution to maintain
#' a reasonable package size while providing realistic example data.
#'
#' @section Data Processing:
#' - **example.mzXML**: Extracted from full LC-MS run, reduced to representative
#'   time window (1-5 minutes) with ~100 scans
#' - **example.mgf**: Selected 10 representative MS2 spectra from full dataset,
#'   no modifications to peak intensities or m/z values
#'
#' For detailed provenance information, see `inst/scripts/data_sources.txt`.
#'
#' @examples
#' # Get path to example mzXML file
#' mzxml_file <- system.file("extdata", "example.mzXML", package = "masstools")
#'
#' # Get path to example MGF file
#' mgf_file <- system.file("extdata", "example.mgf", package = "masstools")
#'
NULL
