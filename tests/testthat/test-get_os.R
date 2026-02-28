
test_that("detect_os returns a supported OS label", {
 expect_true(detect_os() %in% c("osx", "linux", "windows"))
})
