#' Parse a Molecular Formula
#'
#' `split_formula()` is deprecated. Please use `parse_chemical_formula()`
#' instead.
#'
#' @param formula Character string containing a molecular formula.
#'
#' @return A data frame with columns `element` and `number`.
#' @author Xiaotao Shen <xiaotao.shen@outlook.com>
#' @export
#' @examples
#' split_formula(formula = "C9H11NO2")
#' split_formula(formula = "CH3")
#' split_formula(formula = "CH3C")
split_formula <- function(formula = "C9H11NO2") {
  lifecycle::deprecate_soft(
    when = "0.99.9",
    what = "split_formula()",
    with = "parse_chemical_formula()"
  )

  formula_df <- parse_chemical_formula(formula = formula)
  formula_df <- stats::aggregate(
    Count ~ Element,
    data = formula_df,
    FUN = sum
  )
  formula_df <- formula_df[order(formula_df$Element), , drop = FALSE]
  colnames(formula_df) <- c("element", "number")
  rownames(formula_df) <- NULL
  formula_df
}
