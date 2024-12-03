data1 <- data.frame(mz = 1:10, rt = 1:10)
data2 <- data.frame(mz = 1:10, rt = 1:10)

test_that("mz_rt_match", {
  result <- 
    mz_rt_match(data1, data2, mz.tol = 10)
  testthat::expect_s3_class(result, "data.frame")
})
