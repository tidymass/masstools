#' Plot MS2 Spectra Comparisons
#'
#' This function provides a method to visualize and compare two MS2 spectra side by side.
#' The method to be used for the comparison depends on the class of the `spectrum1` and `spectrum2`
#' objects provided.
#'
#' @param spectrum1 A spectrum object representing the first MS2 spectrum.
#' @param spectrum2 A spectrum object representing the second MS2 spectrum.
#' @param spectrum1_name A character string specifying the name/label for the first spectrum. Default is "spectrum1".
#' @param spectrum2_name A character string specifying the name/label for the second spectrum. Default is "spectrum2".
#' @param range.mz Numeric vector of length 2 specifying the range of m/z values to display in the plot.
#' @param ppm.tol Numeric value specifying the ppm tolerance for matching peaks between the spectra. Default is 30.
#' @param mz.ppm.thr Numeric value specifying the threshold for m/z ppm difference. Default is 400.
#' @param xlab Character string specifying the x-axis label. Default is "Mass to charge ratio (m/z)".
#' @param ylab Character string specifying the y-axis label. Default is "Relative intensity".
#' @param col1 Character string specifying the color for the first spectrum. Default is "red".
#' @param col2 Character string specifying the color for the second spectrum. Default is "black".
#' @param title.size Numeric value specifying the font size for the plot title. Default is 15.
#' @param lab.size Numeric value specifying the font size for x and y labels. Default is 15.
#' @param axis.text.size Numeric value specifying the font size for axis text. Default is 15.
#' @param legend.title.size Numeric value specifying the font size for the legend title. Default is 15.
#' @param legend.text.size Numeric value specifying the font size for the legend text. Default is 15.
#' @param interactive_plot Logical indicating whether the plot should be interactive (e.g., using plotly). Default is FALSE.
#'
#' @return A plot visualizing the comparison between the two provided MS2 spectra.
#'
#' @seealso The specific plotting methods associated with different spectrum classes that implement this generic function.
#'
#' @examples
#' # Assuming `spec1` and `spec2` are spectrum objects
#' spec1 <- data.frame(
#'     mz = c(
#'         87.50874,
#'         94.85532,
#'         97.17808,
#'         97.25629,
#'         103.36186,
#'         106.96647,
#'         107.21461,
#'         111.00887,
#'         113.79269,
#'         118.70564
#'     ),
#'     intensity =
#'         c(
#'             8356.306,
#'             7654.128,
#'             9456.207,
#'             8837.188,
#'             8560.228,
#'             8746.359,
#'             8379.361,
#'             169741.797,
#'             7953.080,
#'             8378.066
#'         )
#' )
#' spec2 <- spec1
#' ms2_plot(spec1, spec2)
#'
#' @author Xiaotao Shen <xiaotao.shen@outlook.com>
#' @export
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


#' @method ms2_plot data.frame
#' @rdname ms2_plot

#' @author Xiaotao Shen <xiaotao.shen@outlook.com>
#' @export
#' @examples
#' spectrum1 <- data.frame(
#'     mz = c(
#'         87.50874,
#'         94.85532,
#'         97.17808,
#'         97.25629,
#'         103.36186,
#'         106.96647,
#'         107.21461,
#'         111.00887,
#'         113.79269,
#'         118.70564
#'     ),
#'     intensity =
#'         c(
#'             8356.306,
#'             7654.128,
#'             9456.207,
#'             8837.188,
#'             8560.228,
#'             8746.359,
#'             8379.361,
#'             169741.797,
#'             7953.080,
#'             8378.066
#'         )
#' )
#' spectrum2 <- spectrum1
#' ms2_plot(spectrum1, spectrum2)
#' ms2_plot(spectrum1, spectrum2, interactive_plot = TRUE)
#' ms2_plot(spectrum1)
#' ms2_plot(spectrum1, interactive_plot = TRUE)

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
  if (missing(spectrum1) && missing(spectrum2)) {
    stop("No spectrum1 and spectrum2")
  }
  if (!missing(spectrum1)) {
    spectrum1 <-
      spectrum1 %>%
      as.data.frame() %>%
      purrr::map(function(x) {
        as.numeric(x)
      }) %>%
      dplyr::bind_cols() %>%
      as.data.frame()

    spectrum1[, 2] <- spectrum1[, 2] / max(spectrum1[, 2])
  }

  if (!missing(spectrum2)) {
    spectrum2 <-
      spectrum2 %>%
      as.data.frame() %>%
      purrr::map(function(x) {
        as.numeric(x)
      }) %>%
      dplyr::bind_cols() %>%
      as.data.frame()
    spectrum2[, 2] <- spectrum2[, 2] / max(spectrum2[, 2])
  }

  ## two spectrum
  if (!missing(spectrum1) && !missing(spectrum2)) {
    if (missing(range.mz)) {
      range.mz <- c(min(spectrum1[, 1], spectrum2[, 1]),
                    max(spectrum1[, 1], spectrum2[, 1]))
    }

    matched.spec <- ms2_match(spectrum1,
                              spectrum2,
                              ppm.tol = ppm.tol,
                              mz.ppm.thr = mz.ppm.thr)

    matched.idx <- which(matched.spec[, "Lib.intensity"] > 0 &
                           matched.spec[, "Exp.intensity"] > 0)

    plot <- ggplot2::ggplot(matched.spec) +
      ggplot2::geom_segment(
        mapping = ggplot2::aes(
          x = Exp.mz,
          y = Exp.intensity - Exp.intensity,
          xend = Exp.mz,
          yend = Exp.intensity
        ),
        colour = col2
      ) +
      ggplot2::geom_point(
        data = matched.spec[matched.idx, , drop = FALSE],
        mapping = ggplot2::aes(x = Exp.mz, y = Exp.intensity),
        colour = col2
      ) +
      ggplot2::xlim(range.mz[1], range.mz[2]) +
      ggplot2::labs(x = xlab, y = ylab) +
      ggplot2::scale_y_continuous(
        limits = c(-1, 1),
        breaks = c(-1, -0.5, 0, 0.5, 1),
        labels = c("1", "0.5", "0", "0.5", "1")
      ) +
      theme_bw() +
      theme(
        # axis.line = element_line(arrow = arrow()),
        plot.title = element_text(
          color = "black",
          size = title.size,
          face = "plain",
          hjust = 0.5
        ),
        axis.title = element_text(
          color = "black",
          size = lab.size,
          face = "plain"
        ),
        axis.text = element_text(
          color = "black",
          size = axis.text.size,
          face = "plain"
        ),
        legend.title = element_text(
          color = "black",
          size = legend.title.size,
          face = "plain"
        ),
        legend.text = element_text(
          color = "black",
          size = legend.text.size,
          face = "plain"
        )
      )

    plot <- plot +
      annotate(
        geom = "text",
        x = Inf,
        y = Inf,
        label = spectrum1_name,
        color = col2,
        hjust = 1,
        vjust = 1
      ) +
      annotate(
        geom = "text",
        x = Inf,
        y = -Inf,
        label = spectrum2_name,
        color = col1,
        hjust = 1,
        vjust = -1
      )

    plot <- plot +
      geom_segment(
        data = matched.spec,
        mapping = aes(
          x = Lib.mz,
          y = Lib.intensity - Lib.intensity,
          xend = Lib.mz,
          yend = -Lib.intensity
        ),
        colour = col1
      ) +
      geom_point(
        data = matched.spec[matched.idx, , drop = FALSE],
        mapping = aes(x = Lib.mz, y = -Lib.intensity),
        colour = col1
      )
    if (interactive_plot) {
      if (requireNamespace("plotly", quietly = TRUE)) {
        plot <- plotly::ggplotly(plot)
      }
    }
    return(plot)
  }

  if (!missing(spectrum1) && missing(spectrum2)) {
    spectrum <- spectrum1
  }

  if (!missing(spectrum2) && missing(spectrum1)) {
    spectrum <- spectrum2
  }

  if (missing(range.mz)) {
    range.mz <- c(min(spectrum[, 1]), max(spectrum[, 1]))
  }

  plot <- ggplot(spectrum) +
    geom_segment(mapping = aes(
      x = mz,
      y = 0,
      xend = mz,
      yend = intensity
    ),
    colour = col1) +
    xlim(range.mz[1], range.mz[2]) +
    labs(x = xlab, y = ylab) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    theme_bw() +
    theme(
      # axis.line = element_line(arrow = arrow()),
      plot.title = element_text(
        color = "black",
        size = title.size,
        face = "plain",
        hjust = 0.5
      ),
      axis.title = element_text(
        color = "black",
        size = lab.size,
        face = "plain"
      ),
      axis.text = element_text(
        color = "black",
        size = axis.text.size,
        face = "plain"
      ),
      legend.title = element_text(
        color = "black",
        size = legend.title.size,
        face = "plain"
      ),
      legend.text = element_text(
        color = "black",
        size = legend.text.size,
        face = "plain"
      )
    )

  if (interactive_plot) {
    if (requireNamespace("plotly", quietly = TRUE)) {
      plot <- plotly::ggplotly(plot)
    }
  }
  plot
}



#' @examples
#' # Assuming `spec1_mat` and `spec2_mat` are matrices with MS2 spectra data
#' spec1_mat <- data.frame(
#'     mz = c(
#'         87.50874,
#'         94.85532,
#'         97.17808,
#'         97.25629,
#'         103.36186,
#'         106.96647,
#'         107.21461,
#'         111.00887,
#'         113.79269,
#'         118.70564
#'     ),
#'     intensity =
#'         c(
#'             8356.306,
#'             7654.128,
#'             9456.207,
#'             8837.188,
#'             8560.228,
#'             8746.359,
#'             8379.361,
#'             169741.797,
#'             7953.080,
#'             8378.066
#'         )
#' )
#' spec2_mat <- spec1_mat
#' ms2_plot(spec1_mat, spec2_mat)
#'
#' @author Xiaotao Shen <xiaotao.shen@outlook.com>
#' @method ms2_plot matrix
#' @rdname ms2_plot
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
  if (missing(spectrum1) && missing(spectrum2)) {
    stop("No spectrum1 and spectrum2")
  }
  if (!missing(spectrum1)) {
    spectrum1 <-
      spectrum1 %>%
      as.data.frame() %>%
      purrr::map(function(x) {
        as.numeric(x)
      }) %>%
      dplyr::bind_cols() %>%
      as.data.frame()

    spectrum1[, 2] <- spectrum1[, 2] / max(spectrum1[, 2])
  }

  if (!missing(spectrum2)) {
    spectrum2 <-
      spectrum2 %>%
      as.data.frame() %>%
      purrr::map(function(x) {
        as.numeric(x)
      }) %>%
      dplyr::bind_cols() %>%
      as.data.frame()
    spectrum2[, 2] <- spectrum2[, 2] / max(spectrum2[, 2])
  }

  ms2_plot.data.frame(
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
