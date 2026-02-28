test_that("deduplicate_names preserves length", {
  expect_length(object = deduplicate_names(c(rep(1, 5), 2)), 6)
  expect_length(object = deduplicate_names(1:5), 5)
})
