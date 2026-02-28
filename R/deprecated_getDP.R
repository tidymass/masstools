#' Compute a Dot Product Between Two Intensity Vectors
#'
#' `getDP()` is deprecated. Please use `calculate_dot_product()` instead.
#'
#' @inheritParams calculate_dot_product
#' @return A numeric value representing the weighted dot product between
#'   `exp.int` and `lib.int`.
#' @export
#' @examples
#' exp.int <- c(0.10, 0.35, 1.00, 0.45)
#' lib.int <- c(0.12, 0.30, 0.95, 0.50)
#'
#' getDP(exp.int = exp.int, lib.int = lib.int)
getDP <- function(exp.int, lib.int) {
  lifecycle::deprecate_soft(
    when = "0.99.9",
    what = "getDP()",
    with = "calculate_dot_product()"
  )

  calculate_dot_product(exp.int = exp.int, lib.int = lib.int)
}
