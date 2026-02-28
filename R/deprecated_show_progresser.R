#' @title Build a Simple Progress Lookup Table
#' @description `show_progresser()` is deprecated. Please use
#'   `build_progress_table()` instead.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param index Integer vector giving the sequence of iteration indices.
#' @param progresser Numeric vector of percentage labels to display.
#' @return A data frame with columns `idx` and `progresser`.
#' @export
#' @examples
#' show_progresser()
show_progresser <-
  function(index = seq_len(1000),
           progresser = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) {
    lifecycle::deprecate_soft(
      when = "0.99.1",
      what = "show_progresser()",
      with = "build_progress_table()"
    )
    build_progress_table(index = index, progresser = progresser)
  }
