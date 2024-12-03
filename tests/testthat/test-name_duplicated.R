test_that("name_duplicated", {
  expect_length(object = name_duplicated(c(rep(1, 5), 2)), 6)
  expect_length(object = name_duplicated(1:5), 5)
})
