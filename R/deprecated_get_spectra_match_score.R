#' Compute a Similarity Score Between Two Mass Spectra
#'
#' `get_spectra_match_score()` is deprecated. Please use
#' `calculate_spectra_match_score()` instead.
#'
#' @inheritParams calculate_spectra_match_score
#' @return A numeric similarity score.
#' @export
#' @examples
#' exp.spectrum <- data.frame(mz = c(100, 150), intensity = c(10, 100))
#' lib.spectrum <- data.frame(mz = c(100.01, 150), intensity = c(12, 95))
#' get_spectra_match_score(exp.spectrum, lib.spectrum, ppm.tol = 50)
get_spectra_match_score <-
  function(exp.spectrum,
           lib.spectrum,
           ppm.tol = 30,
           mz.ppm.thr = 400,
           fraction.weight = 0.2,
           dp.forward.weight = 0.7,
           dp.reverse.weight = 0.1,
           remove_fragment_intensity_cutoff = 0) {
    lifecycle::deprecate_soft(
      when = "0.99.1",
      what = "get_spectra_match_score()",
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
