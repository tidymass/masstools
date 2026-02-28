#' Compute a Weighted Dot Product Between Two Intensity Vectors
#'
#' `get_dp()` is deprecated. Please use `calculate_dot_product()` instead.
#'
#' @inheritParams calculate_dot_product
#' @return A numeric value representing the weighted dot product.
#' @export
#' @examples
#' exp.int <- c(0.10, 0.35, 1.00, 0.45)
#' lib.int <- c(0.12, 0.30, 0.95, 0.50)
#' get_dp(exp.int = exp.int, lib.int = lib.int)
get_dp <- function(exp.int, lib.int) {
  lifecycle::deprecate_soft(
    when = "0.99.1",
    what = "get_dp()",
    with = "calculate_dot_product()"
  )
  calculate_dot_product(exp.int = exp.int, lib.int = lib.int)
}
