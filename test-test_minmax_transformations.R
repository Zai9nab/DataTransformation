test_that("minmax_normalization works correctly", {
  x <- c(1, 2, 3, 4, 5)
  normalized <- minmax_normalization(x)

  # Check values are between 0 and 1
  expect_true(all(normalized >= 0 & normalized <= 1))

  # Check first and last values
  expect_equal(normalized[1], 0)
  expect_equal(normalized[length(normalized)], 1)
})
