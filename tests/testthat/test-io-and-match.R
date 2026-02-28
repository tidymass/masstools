example_file <- function(name) {
  installed <- system.file("extdata", name, package = "masstools")
  if (nzchar(installed)) {
    return(installed)
  }

  testthat::test_path("..", "..", "inst", "extdata", name)
}

test_that("keep_best_match resolves duplicated matches", {
  result <- data.frame(
    Index1 = c(1, 1),
    Index2 = c(1, 2),
    mz1 = c(100, 100),
    mz2 = c(100, 100.00005),
    `mz error` = c(0, 0.5),
    rt1 = c(10, 10),
    rt2 = c(10.4, 10.1),
    `rt error` = c(0.4, 0.1),
    check.names = FALSE
  )

  mz_kept <- masstools::keep_best_match(result, according.to = "mz.error")
  expect_equal(nrow(mz_kept), 1)
  expect_equal(mz_kept$Index2, 1)

  rt_kept <- masstools::keep_best_match(result, according.to = "rt.error")
  expect_equal(nrow(rt_kept), 1)
  expect_equal(rt_kept$Index2, 2)
})

test_that("read_mgf reads bundled example data", {
  file_path <- example_file("example.mgf")
  expect_true(file.exists(file_path))

  result <- suppressWarnings(masstools::read_mgf(file_path))
  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_true(all(c("info", "spec") %in% names(result[[1]])))
})

test_that("read_mzxml reads bundled example data", {
  testthat::skip_if_not_installed("MSnbase")

  file_path <- example_file("example.mzXML")
  expect_true(file.exists(file_path))

  result <- masstools::read_mzxml(file_path, mode = "inMemory")
  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_true(all(c("info", "spec") %in% names(result[[1]])))
})
