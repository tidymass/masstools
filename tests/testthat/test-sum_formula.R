test_that("sum_formula is right", {
  expect_equal(sum_formula(formula = "C9H11NO2", adduct = "M+H"), "C9H12NO2")
  expect_equal(sum_formula(formula = "C9H11NO2", adduct = "M+"), "C9H11NO2")
})