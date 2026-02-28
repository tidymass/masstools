test_that("combine_formula_adduct returns expected formulas", {
  expect_equal(combine_formula_adduct(formula = "C9H11NO2", adduct = "M+H"), "C9H12NO2")
  expect_equal(combine_formula_adduct(formula = "C9H11NO2", adduct = "M+"), "C9H11NO2")
})
