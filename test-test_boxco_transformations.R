test_that("boxcox_transform works correctly", {
  # Positive cases
  expect_equal(boxcox_transform(c(1, 10, 100), lambda = 0), log(c(1, 10, 100)))
  expect_equal(boxcox_transform(c(1, 10, 100), lambda = 1), c(1, 10, 100) - 1)

  # Error cases
  expect_error(boxcox_transform(c(-1, 0, 10), lambda = 0), "Values must be positive")
})
