#' @title Keep the Best Match per Feature
#' @description `keep_one()` is deprecated. Please use
#'   `keep_best_match()` instead.
#' @author Xiaotao Shen
#' \email{shenxt1990@@outlook.com}
#' @inheritParams keep_best_match
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
#' keep_one(result)
keep_one <- function(result,
                     according.to = c("mz.error", "rt.error")) {
  lifecycle::deprecate_soft(
    when = "0.99.1",
    what = "keep_one()",
    with = "keep_best_match()"
  )
  keep_best_match(result = result, according.to = according.to)
}
