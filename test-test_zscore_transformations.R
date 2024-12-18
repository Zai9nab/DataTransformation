test_that("zscore_transform works correctly", {
  x <- c(1, 2, 3, 4, 5)
  standardized <- zscore_transform(x)

  # Check mean is approximately 0
  expect_equal(mean(standardized), 0, tolerance = 1e-8)

  # Check standard deviation is approximately 1
  expect_equal(sd(standardized), 1, tolerance = 1e-8)
})
