test_that("weighted_aggregation works correctly", {
  x <- c(1, 2, 3)
  weights <- c(0.1, 0.3, 0.6)
  result <- weighted_aggregation(x, weights)

  # Check weighted sum
  expect_equal(result$weighted_sum, sum(x * weights))

  # Check weighted average
  expect_equal(result$weighted_avg, sum(x * weights) / sum(weights))

  # Error case
  expect_error(weighted_aggregation(c(1, 2), c(0.1, 0.2, 0.3)), "x and weights must have the same length")
})
