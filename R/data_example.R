#' Example Mass Spectrometry Data Files
#'
#' @description
#' Example mass spectrometry data files included in the package for demonstration
#' and testing purposes. These files are derived from a lipidomics study published
#' in Cell (2020).
#'
#' @details
#' The package includes two example data files located in `inst/extdata`:
#'
#' ## example.mzXML
#' An mzXML file containing LC-MS data. This file is a down-sampled subset of the
#' original data, containing approximately 100 scans from a 1-5 minute time window.
#' The file demonstrates typical LC-MS data structure and can be read using the
#' `read_mzxml()` function.
#'
#' ## example.mgf
#' An MGF (Mascot Generic Format) file containing MS2 spectra. This file includes
#' 10 representative MS2 spectra selected from metabolite annotation workflows.
#' The spectra represent common metabolite classes and can be used with MS2 matching
#' functions such as `ms2Match()` and `getSpectraMatchScore()`.
#'
#' @section Data Source:
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
#' @section Usage:
#' To access these files in your R session:
#' ```
#' # Get path to example mzXML file
#' mzxml_file <- system.file("extdata", "example.mzXML", package = "masstools")
#'
#' # Get path to example MGF file
#' mgf_file <- system.file("extdata", "example.mgf", package = "masstools")
#' ```
#'
#' @name example_data
#' @aliases example.mzXML example.mgf
#' @docType data
#' @keywords datasets
NULL
