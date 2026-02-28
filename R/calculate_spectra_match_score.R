#' Compute a Similarity Score Between Two Mass Spectra
#'
#' Compute a similarity score between two mass spectra, with optional filtering
#' of low-intensity fragments.
#'
#' @param exp.spectrum The experimental spectrum. Can be:
#'   \itemize{
#'     \item A data frame with 'mz' and 'intensity' columns
#'     \item A Spectra object from the Spectra package
#'   }
#' @param lib.spectrum The library/reference spectrum. Can be:
#'   \itemize{
#'     \item A data frame with 'mz' and 'intensity' columns
#'     \item A Spectra object from the Spectra package
#'   }
#' @param ppm.tol A numeric value indicating the ppm tolerance.
#' @param mz.ppm.thr A numeric value for m/z threshold for ppm calculation.
#' @param fraction.weight A numeric value for weight of fraction component in score.
#' @param dp.forward.weight A numeric value for weight of forward dot product component in score.
#' @param dp.reverse.weight A numeric value for weight of reverse dot product component in score.
#' @param remove_fragment_intensity_cutoff A numeric value specifying the intensity cutoff for filtering out fragments in the spectra. Default is 0, which means no fragments are filtered out based on intensity.
#'
#' @return A numeric value representing the similarity score between
#'   \code{exp.spectrum} and \code{lib.spectrum}.
#'
#' @details
#' Intensities are scaled to the range 0 to 1, peaks are matched, and the
#' final score combines the matched-peak fraction with forward and reverse dot
#' products. Fragments below the specified intensity cutoff are removed before
#' matching.
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
#' calculate_spectra_match_score(
#'   exp.spectrum = exp.spectrum,
#'   lib.spectrum = lib.spectrum,
#'   ppm.tol = 50,
#'   remove_fragment_intensity_cutoff = 0.05
#' )
calculate_spectra_match_score <-
  function(exp.spectrum,
           lib.spectrum,
           ppm.tol = 30,
           mz.ppm.thr = 400,
           fraction.weight = 0.2,
           dp.forward.weight = 0.7,
           dp.reverse.weight = 0.1,
           remove_fragment_intensity_cutoff = 0) {
    exp.spectrum <- as_spectrum_df(exp.spectrum)
    lib.spectrum <- as_spectrum_df(lib.spectrum)

    exp.spectrum$intensity <-
      exp.spectrum$intensity / max(exp.spectrum$intensity)
    lib.spectrum$intensity <-
      lib.spectrum$intensity / max(lib.spectrum$intensity)

    exp.spectrum <-
      exp.spectrum %>%
      dplyr::filter(intensity > remove_fragment_intensity_cutoff)

    lib.spectrum <-
      lib.spectrum %>%
      dplyr::filter(intensity > remove_fragment_intensity_cutoff)

    match.matrix <- ms2_match(
      exp.spectrum = exp.spectrum,
      lib.spectrum = lib.spectrum,
      ppm.tol = ppm.tol,
      mz.ppm.thr = mz.ppm.thr
    )

    fraction <-
      sum(!is.na(match.matrix$Lib.index) &
            !is.na(match.matrix$Exp.index)) / nrow(match.matrix)

    dp.forward <- calculate_dot_product(
      exp.int = match.matrix$Exp.intensity,
      lib.int = match.matrix$Lib.intensity
    )
    dp.reverse <-
      calculate_dot_product(
        exp.int = match.matrix$Exp.intensity[which(match.matrix$Lib.intensity > 0)],
        lib.int = match.matrix$Lib.intensity[which(match.matrix$Lib.intensity > 0)]
      )
    dp.forward[is.na(dp.forward)] <- 0
    dp.reverse[is.na(dp.reverse)] <- 0
    score <-
      dp.forward * dp.forward.weight + dp.reverse * dp.reverse.weight +
      fraction * fraction.weight
    return(score)
  }
