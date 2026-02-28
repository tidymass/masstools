#' Remove Noisy Fragments from a Spectrum
#'
#' `removeNoise()` is deprecated. Please use `remove_noise()` instead.
#'
#' @inheritParams remove_noise
#' @return A spectrum in the same format as input.
#' @export
#' @examples
#' spectrum <- data.frame(
#'   mz = c(100.0000, 100.0010, 124.0400, 150.0600, 150.0610),
#'   intensity = c(10, 5, 35, 100, 20)
#' )
#'
#' removeNoise(spectrum, ppm.ms2match = 30)
removeNoise <- function(spec,
                        ppm.ms2match = 30,
                        mz.ppm.thr = 400) {
  lifecycle::deprecate_soft(
    when = "0.99.9",
    what = "removeNoise()",
    with = "remove_noise()"
  )

  remove_noise(
    spec = spec,
    ppm.ms2match = ppm.ms2match,
    mz.ppm.thr = mz.ppm.thr
  )
}
