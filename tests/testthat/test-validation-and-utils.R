test_that("chemical element metadata is available through helper", {
  info <- getFromNamespace("chemical_elements_information", "masstools")()

  expect_s3_class(info, "data.frame")
  expect_true(all(c("element", "name", "accurate_mass", "average_mass") %in% colnames(info)))
  expect_true("C" %in% info$element)
})

test_that("check_chemical_element and check_chemical_formula handle edge cases", {
  expect_true(check_chemical_element("NaCl"))
  expect_false(check_chemical_element("Xx"))

  expect_true(check_chemical_formula("NaCl"))
  expect_false(check_chemical_formula("H0O"))
  expect_false(check_chemical_formula("2H"))
  expect_false(check_chemical_formula("H-2O"))
})

test_that("calculate_mass and adduct helpers handle supported paths", {
  element_info <- getFromNamespace("chemical_elements_information", "masstools")()
  expected_average_mass <-
    sum(element_info$average_mass[element_info$element %in% c("H", "O")] * c(2, 1))

  expect_equal(
    unname(calculate_mass("H2O", which = "average_mass")),
    expected_average_mass,
    tolerance = 1e-6
  )

  expect_equal(
    combine_formula_adduct("C6H12O6", "M+H"),
    "C6H13O6"
  )

  expect_true(is.na(convert_precursor_mz2accurate_mass(NA_real_, "(M-H)-")))
  expect_error(
    convert_precursor_mz2accurate_mass(100, "bad"),
    "Invalid adduct formula"
  )
})

test_that("package helper tables and version helper are available", {
  adducts <- getFromNamespace("adduct_table", "masstools")()
  expect_s3_class(adducts, "data.frame")
  expect_true(all(c("adduct", "mz") %in% colnames(adducts)))
  expect_true("(M+H)+" %in% adducts$adduct)

  chemspider_codes <- getFromNamespace("chemspider_code_name_table", "masstools")()
  expect_s3_class(chemspider_codes, "data.frame")
  expect_true(all(c("code", "name") %in% colnames(chemspider_codes)))

  expect_equal(
    getFromNamespace("masstools_version", "masstools")(),
    as.character(utils::packageVersion("masstools"))
  )
})

test_that("masstools_logo prints version information", {
  expect_match(
    paste(testthat::capture_messages(masstools::masstools_logo()), collapse = "\n"),
    paste0("Version ", getFromNamespace("masstools_version", "masstools")())
  )

  expect_match(
    paste(capture.output(masstools::masstools_logo()), collapse = "\n"),
    "_______"
  )
})
