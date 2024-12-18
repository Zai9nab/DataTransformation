test_that("polynomial_transform works correctly", {
  x <- c(1, 2, 3)
  degree <- 2
  result <- polynomial_transform(x, degree)

  # Check dimensions
  expect_equal(ncol(result), degree + 1)
  expect_equal(nrow(result), length(x))

  # Check first row for correctness
  expect_equal(result[1, ], c(1, 1, 1))  # 1^0, 1^1, 1^2
})
