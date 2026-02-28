#' Match Fragments Between Two Spectra
#'
#' `ms2Match()` is deprecated. Please use `ms2_match()` instead.
#'
#' @inheritParams ms2_match
#' @return A data frame with columns for library index, experimental index,
#'   library m/z, library intensity, experimental m/z, and experimental
#'   intensity.
#' @export
#' @examples
#' exp.spectrum <- data.frame(
#'   mz = c(100.0000, 124.0400, 150.0600, 180.0800, 205.1000),
#'   intensity = c(10, 35, 100, 45, 20)
#' )
#' lib.spectrum <- data.frame(
#'   mz = c(100.0015, 124.0395, 150.0610, 180.0820, 220.1200),
#'   intensity = c(12, 30, 95, 50, 10)
#' )
#'
#' ms2Match(exp.spectrum, lib.spectrum, ppm.tol = 50)
ms2Match <- function(exp.spectrum,
                     lib.spectrum,
                     ppm.tol = 30,
                     mz.ppm.thr = 400) {
  lifecycle::deprecate_soft(
    when = "0.99.9",
    what = "ms2Match()",
    with = "ms2_match()"
  )

  ms2_match(
    exp.spectrum = exp.spectrum,
    lib.spectrum = lib.spectrum,
    ppm.tol = ppm.tol,
    mz.ppm.thr = mz.ppm.thr
  )
}
