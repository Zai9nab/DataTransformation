test_that("moving_average works correctly", {
  x <- c(1, 2, 3, 4, 5)
  window_size <- 3
  result <- moving_average(x, window_size)

  # Check length of result
  expect_equal(length(result), length(x))

  # Check correctness of first few values
  expected <- as.numeric(c(NA, NA, mean(c(1, 2, 3)), mean(c(2, 3, 4)), mean(c(3, 4, 5))))
  expect_equal(result, expected)
})
