#' @title Build a Simple Progress Lookup Table
#' @description Create a small data frame that maps iteration indices to
#'   percentage labels for progress reporting.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param index Integer vector giving the sequence of iteration indices.
#' @param progresser Numeric vector of percentage labels to display.
#' @return A data frame with columns `idx` and `progresser`.
#' @export
#' @examples
#' build_progress_table()
build_progress_table <-
  function(index = seq_len(1000),
           progresser = c(1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) {
    idx <-
      seq(
        from = 1,
        to = max(index),
        length.out = length(progresser)
      ) %>%
      round()

    data.frame(idx = idx, progresser = paste0(progresser, "%"))
  }
