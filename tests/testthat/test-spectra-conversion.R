test_that("df_to_spectra and spectra_to_df round-trip spectral data", {
  spectrum_df <- data.frame(
    mz = c(100, 150, 200),
    intensity = c(10, 50, 20)
  )

  spec <- masstools::df_to_spectra(
    spectrum_df,
    precursor_mz = 250.1,
    precursor_rt = 12.3,
    spectrum_id = "spec-1"
  )

  expect_s4_class(spec, "Spectra")

  out <- masstools::spectra_to_df(spec)
  expect_equal(out, spectrum_df)
})

test_that("df_to_spectra validates required input columns", {
  expect_error(
    masstools::df_to_spectra(matrix(1:4, ncol = 2)),
    "spectrum_df must be a data.frame"
  )

  expect_error(
    masstools::df_to_spectra(data.frame(a = 1:3, b = 4:6)),
    "must contain 'mz' and 'intensity' columns"
  )
})

test_that("spectra_to_df validates object type and index", {
  expect_error(
    masstools::spectra_to_df(data.frame(mz = 1, intensity = 1)),
    "spectra must be a Spectra object"
  )

  spec <- masstools::df_to_spectra(data.frame(mz = 1:2, intensity = 3:4))
  expect_error(
    masstools::spectra_to_df(spec, index = 2),
    "index exceeds number of spectra"
  )
})

test_that("internal spectrum conversion helpers preserve legacy formats", {
  spectrum_df <- data.frame(
    mz = c(100, 120),
    intensity = c(1000, 500)
  )
  spec <- masstools::df_to_spectra(spectrum_df, precursor_mz = 150, precursor_rt = 5)

  expect_equal(
    getFromNamespace("check_spectrum_type", "masstools")(spec),
    "Spectra"
  )
  expect_equal(
    getFromNamespace("check_spectrum_type", "masstools")(spectrum_df),
    "data.frame"
  )
  expect_equal(
    getFromNamespace("check_spectrum_type", "masstools")(as.matrix(spectrum_df)),
    "matrix"
  )
  expect_equal(
    getFromNamespace("check_spectrum_type", "masstools")("x"),
    "unknown"
  )

  expect_equal(
    getFromNamespace("as_spectrum_df", "masstools")(spec),
    spectrum_df
  )
  expect_equal(
    getFromNamespace("as_spectrum_df", "masstools")(as.matrix(spectrum_df)),
    spectrum_df
  )

  legacy <- getFromNamespace("spectra_to_list", "masstools")(spec)
  expect_type(legacy, "list")
  expect_true(all(c("info", "spec") %in% names(legacy[[1]])))

  spec_roundtrip <- getFromNamespace("list_to_spectra", "masstools")(legacy)
  expect_s4_class(spec_roundtrip, "Spectra")
  expect_equal(masstools::spectra_to_df(spec_roundtrip), spectrum_df)
})

test_that("internal helpers reject unsupported spectrum inputs", {
  expect_error(
    getFromNamespace("as_spectrum_df", "masstools")(data.frame(a = 1, b = 2)),
    "must contain 'mz' and 'intensity' columns"
  )

  expect_error(
    getFromNamespace("as_spectrum_df", "masstools")(matrix(1:6, ncol = 3)),
    "matrix must have 2 columns"
  )

  expect_error(
    getFromNamespace("list_to_spectra", "masstools")("bad"),
    "spectrum_list must be a list"
  )
})
