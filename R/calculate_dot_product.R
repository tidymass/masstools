#' Compute a Weighted Dot Product Between Two Intensity Vectors
#'
#' Compute a weighted dot product between two intensity vectors.
#'
#' @param exp.int A numeric vector representing the intensity values of the experimental spectrum.
#' @param lib.int A numeric vector representing the intensity values of the library spectrum.
#'
#' @return A numeric value representing the weighted dot product between
#'   \code{exp.int} and \code{lib.int}.
#'
#' @details
#' Weights are derived from the relative contribution of each intensity to the
#' total signal before the dot product is calculated.
#'
#' @export
#' @examples
#' exp.int <- c(0.10, 0.35, 1.00, 0.45)
#' lib.int <- c(0.12, 0.30, 0.95, 0.50)
#'
#' calculate_dot_product(exp.int = exp.int, lib.int = lib.int)
calculate_dot_product <-
  function(exp.int, lib.int) {
    exp.weight <- lapply(exp.int, function(x) {
      1 / (1 + x / (sum(exp.int) - 0.5))
    }) %>%
      unlist()

    lib.weight <- lapply(lib.int, function(x) {
      1 / (1 + x / (sum(lib.int) - 0.5))
    }) %>%
      unlist()

    x <- exp.weight * exp.int
    y <- lib.weight * lib.int
    return(sum(x * y) ^ 2 / (sum(x ^ 2) * sum(y ^ 2)))
  }
