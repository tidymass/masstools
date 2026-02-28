#' Sum a Molecular Formula with an Adduct
#'
#' `sum_formula()` is deprecated. Please use `combine_formula_adduct()`
#' instead.
#'
#' @inheritParams combine_formula_adduct
#' @return A character string containing the summed formula, or `NA` if the
#'   result is invalid.
#' @author Xiaotao Shen <xiaotao.shen@outlook.com>
#' @export
#' @examples
#' sum_formula(formula = "C9H11NO2", adduct = "M+H")
sum_formula <-
  function(formula = "C9H11NO2", adduct = "M-H2O+H") {
    lifecycle::deprecate_soft(
      when = "0.99.1",
      what = "sum_formula()",
      with = "combine_formula_adduct()"
    )
    combine_formula_adduct(formula = formula, adduct = adduct)
  }
