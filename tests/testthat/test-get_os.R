
test_that("get_os", {
 expect_true(get_os() %in% c("osx", "linux", "windows"))
})
