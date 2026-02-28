#' @title Keep the Best Match per Feature
#' @description Reduce duplicated matches returned by `match_mz_rt()` to a
#'   single row per feature.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @param result A result object returned by `match_mz_rt()`.
#' @param according.to Character. Keep the row with the smallest `mz error` or
#'   `rt error`.
#' @return A filtered match table with at most one row per `Index1`.
#' @export
#' @examples
#' data1 <- data.frame(
#'   mz = c(100.0000, 150.0600),
#'   rt = c(120, 240)
#' )
#' data2 <- data.frame(
#'   mz = c(100.0008, 100.0005, 150.0610),
#'   rt = c(121, 119, 238)
#' )
#' result <- match_mz_rt(data1, data2, mz.tol = 10, rt.tol = 2, rt.error.type = "abs")
#'
#' keep_best_match(result)
keep_best_match <- function(result,
                            according.to = c("mz.error", "rt.error")) {
  according.to <- match.arg(according.to)
  if (is.null(result)) {
    return(result)
  }

  if (!is.matrix(result) && !is.data.frame(result)) {
    stop("result must be matrix or data.frame.")
  }

  if (ncol(result) != 8) {
    stop("result must come from match_mz_rt().")
  }
  if (paste(colnames(result), collapse = ";") !=
      "Index1;Index2;mz1;mz2;mz error;rt1;rt2;rt error") {
    stop("result must come from match_mz_rt().")
  }

  duplicated.idx <-
    unique(result$Index1[duplicated(result$Index1)])
  if (length(duplicated.idx) == 0) {
    return(result)
  }

  for (i in seq_along(duplicated.idx)) {
    temp.idx <- which(result$Index1 == duplicated.idx[i])
    temp.result <- result[temp.idx, ]
    if (according.to == "mz.error") {
      temp.idx1 <- temp.idx[which.min(temp.result[["mz error"]])]
      temp.idx2 <- setdiff(temp.idx, temp.idx1)
      result <- result[-temp.idx2, ]
    }

    if (according.to == "rt.error") {
      temp.idx1 <- temp.idx[which.min(temp.result[["rt error"]])]
      temp.idx2 <- setdiff(temp.idx, temp.idx1)
      result <- result[-temp.idx2, ]
    }
  }
  result
}
