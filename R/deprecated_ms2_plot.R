#' Plot MS2 Spectra Comparisons
#'
#' `ms2_plot()` is deprecated. Please use `plot_ms2()` instead.
#'
#' @inheritParams plot_ms2
#' @return A plot visualizing the comparison between the provided MS2 spectra.
#' @export
#' @examples
#' spectrum1 <- data.frame(
#'   mz = c(100.00, 124.04, 150.06, 180.08, 205.10),
#'   intensity = c(10, 35, 100, 45, 20)
#' )
#' spectrum2 <- data.frame(
#'   mz = c(100.01, 124.05, 150.06, 180.09, 220.12),
#'   intensity = c(12, 30, 95, 50, 10)
#' )
#' ms2_plot(spectrum1, spectrum2)
ms2_plot <-
  function(spectrum1,
           spectrum2,
           spectrum1_name = "spectrum1",
           spectrum2_name = "spectrum2",
           range.mz,
           ppm.tol = 30,
           mz.ppm.thr = 400,
           xlab = "Mass to charge ratio (m/z)",
           ylab = "Relative intensity",
           col1 = "red",
           col2 = "black",
           title.size = 15,
           lab.size = 15,
           axis.text.size = 15,
           legend.title.size = 15,
           legend.text.size = 15,
           interactive_plot = FALSE) {
    UseMethod("ms2_plot")
  }

#' @export
ms2_plot.data.frame <- function(spectrum1,
                                spectrum2,
                                spectrum1_name = "spectrum1",
                                spectrum2_name = "spectrum2",
                                range.mz,
                                ppm.tol = 30,
                                mz.ppm.thr = 400,
                                xlab = "Mass to charge ratio (m/z)",
                                ylab = "Relative intensity",
                                col1 = "red",
                                col2 = "black",
                                title.size = 15,
                                lab.size = 15,
                                axis.text.size = 15,
                                legend.title.size = 15,
                                legend.text.size = 15,
                                interactive_plot = FALSE) {
  lifecycle::deprecate_soft(
    when = "0.99.1",
    what = "ms2_plot()",
    with = "plot_ms2()"
  )
  plot_ms2.data.frame(
    spectrum1 = spectrum1,
    spectrum2 = spectrum2,
    spectrum1_name = spectrum1_name,
    spectrum2_name = spectrum2_name,
    range.mz = range.mz,
    ppm.tol = ppm.tol,
    mz.ppm.thr = mz.ppm.thr,
    xlab = xlab,
    ylab = ylab,
    col1 = col1,
    col2 = col2,
    title.size = title.size,
    lab.size = lab.size,
    axis.text.size = axis.text.size,
    legend.title.size = legend.title.size,
    legend.text.size = legend.text.size,
    interactive_plot = interactive_plot
  )
}

#' @export
ms2_plot.matrix <- function(spectrum1,
                            spectrum2,
                            spectrum1_name = "spectrum1",
                            spectrum2_name = "spectrum2",
                            range.mz,
                            ppm.tol = 30,
                            mz.ppm.thr = 400,
                            xlab = "Mass to charge ratio (m/z)",
                            ylab = "Relative intensity",
                            col1 = "red",
                            col2 = "black",
                            title.size = 15,
                            lab.size = 15,
                            axis.text.size = 15,
                            legend.title.size = 15,
                            legend.text.size = 15,
                            interactive_plot = FALSE) {
  lifecycle::deprecate_soft(
    when = "0.99.1",
    what = "ms2_plot()",
    with = "plot_ms2()"
  )
  plot_ms2.matrix(
    spectrum1 = spectrum1,
    spectrum2 = spectrum2,
    spectrum1_name = spectrum1_name,
    spectrum2_name = spectrum2_name,
    range.mz = range.mz,
    ppm.tol = ppm.tol,
    mz.ppm.thr = mz.ppm.thr,
    xlab = xlab,
    ylab = ylab,
    col1 = col1,
    col2 = col2,
    title.size = title.size,
    lab.size = lab.size,
    axis.text.size = axis.text.size,
    legend.title.size = legend.title.size,
    legend.text.size = legend.text.size,
    interactive_plot = interactive_plot
  )
}
