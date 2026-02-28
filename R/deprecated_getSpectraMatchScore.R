#' Compute a Similarity Score Between Two Mass Spectra
#'
#' `getSpectraMatchScore()` is deprecated. Please use
#' `calculate_spectra_match_score()` instead.
#'
#' @inheritParams calculate_spectra_match_score
#' @return A numeric value representing the similarity score between
#'   `exp.spectrum` and `lib.spectrum`.
#' @export
#' @examples
#' exp.spectrum <- data.frame(
#'   mz = c(100.00, 124.04, 150.06, 180.08, 205.10),
#'   intensity = c(10, 35, 100, 45, 20)
#' )
#' lib.spectrum <- data.frame(
#'   mz = c(100.01, 124.05, 150.06, 180.09, 220.12),
#'   intensity = c(12, 30, 95, 50, 10)
#' )
#'
#' getSpectraMatchScore(exp.spectrum, lib.spectrum, ppm.tol = 50)
getSpectraMatchScore <- function(exp.spectrum,
                                 lib.spectrum,
                                 ppm.tol = 30,
                                 mz.ppm.thr = 400,
                                 fraction.weight = 0.2,
                                 dp.forward.weight = 0.7,
                                 dp.reverse.weight = 0.1,
                                 remove_fragment_intensity_cutoff = 0) {
  lifecycle::deprecate_soft(
    when = "0.99.9",
    what = "getSpectraMatchScore()",
    with = "calculate_spectra_match_score()"
  )

  calculate_spectra_match_score(
    exp.spectrum = exp.spectrum,
    lib.spectrum = lib.spectrum,
    ppm.tol = ppm.tol,
    mz.ppm.thr = mz.ppm.thr,
    fraction.weight = fraction.weight,
    dp.forward.weight = dp.forward.weight,
    dp.reverse.weight = dp.reverse.weight,
    remove_fragment_intensity_cutoff = remove_fragment_intensity_cutoff
  )
}
